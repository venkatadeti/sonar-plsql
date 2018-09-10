create or replace PACKAGE BODY utl_global_inbox AS
 ----------------------------------------------------------------------------
	--	Package name		:	UTL_GLOBAL_INBOXUTL_GLOBAL_INBOX
	--	Copyright			:	Sultan-Center
	--	Project				:	RBS
	--	Purpose				:	Library Functions/procedures to set or get GlobalInbox object
	--	CreatedBY			:	Bhavana Konda
	--	CreatedDate			:	13-OCT-2017
	--	LastModifiedDate	:	03-NOV-2017
	--	Version				:	1.0.1
	--	Pending				:
	----------------------------------------------------------------------------

 gv_program		errlog.pkg_name%TYPE := '';
 dml_errors		EXCEPTION;
 lv_program		errlog.proc_name%TYPE;
 lv_phase		errlog.event_phase%TYPE := 'Declare Variables';
 lv_error		VARCHAR2(100):= ' ';
 lv_errcode		VARCHAR2(100):= ' ';
 lv_rec			VARCHAR2(30) := ' ';
 lv_retval		BOOLEAN := TRUE;
 lv_logparam	INTEGER := 1;
 lv_count       NUMBER;
FUNCTION get_inbox( userid		IN VARCHAR2,
					inbox_tab	IN VARCHAR2,
					p_inbox_tbl	OUT NOCOPY inbox_tbl)
RETURN BOOLEAN AS
 lv_sql_stmt	CLOB;
 lv_ref_cur		utl_lib.t_ref_typ;
 jdate_list		utl_lib.num_list := utl_lib.num_list();
   
BEGIN
 lv_retval	:= TRUE; 
 lv_program	:= 'GET_INBOX';
 lv_phase	:= 'Phase 1 Startup';
 lv_error	:= '';
 lv_rec		:= 'INBOX_OBJ';

 jdate_list.EXTEND;
 jdate_list := utl_lib.date_group_thresholds(SYSDATE);
 
 lv_sql_stmt := 
 'SELECT inbox_obj(
		sender_id			=> gi.senderid,
		sender_name			=> gi.sendername,
		recipient_id		=> gi.recipientid,
		recipient_name		=> gi.recipientname,
		dataprofile_id		=> gi.dataprofileid,
		profileinstance_id	=> TO_CHAR(gi.profileinstanceid),
		workflow_id			=> gi.workflowid,   
		workflowinstance_id	=> gi.workflowinstanceid,
		req_date			=> TRUNC(gi.req_recieved_date),
		due_date			=> gi.duedate,
		due_days			=> (CASE WHEN (gi.actiontype NOT IN(''C'',''S'')
								AND gi.DueDate >= SYSDATE)
								THEN TO_CHAR(TRUNC(gi.DueDate),''DD-MON-YYYY'')
								WHEN (gi.actiontype NOT IN (''C'',''S'',''R'')
								AND gi.DueDate < SYSDATE)
								THEN ''Overdue'' END),
		status				=> gi.status,
		req_createdby		=> gi.req_createdby,
		req_createdbyname	=> gi.req_createdbyname,
		req_datecreated		=> TRUNC(gi.req_datecreated),
		action_name			=> (CASE WHEN gi.actiontype = ''A''
								THEN ''Submitted for Approval''
								WHEN gi.actiontype = ''B''
								THEN ''Sent back for Review''
								WHEN gi.actiontype = ''R''
								THEN ''Rejected''
								WHEN gi.actiontype = ''C''
								THEN ''Completed''
                                WHEN gi.actiontype = ''S''
                                THEN ''Worksheet''
								ELSE NULL
								END),
		actiontype			=> gi.actiontype,
		requestfor			=> gi.requestfor,
		requestforname		=> gi.requestforname,
		amount				=> gi.amount,
		purpose				=> gi.purpose,
		stateid				=> gi.stateid,
		tostateid			=> gi.tostateid,
		requestread			=> gi.requestread,
		requestreadby		=> gi.requestreadby,
		issnoozed			=> gi.issnoozed,
		issnoozedby			=> gi.issnoozedby,
		activitycompleted	=> gi.activitycompleted,
		workflowcompleted	=> gi.workflowcompleted,
		isdeclined			=> gi.isdeclined,
		req_doc_loc			=> gi.req_doc_loc,
		proxyindicator		=> gi.proxyindicator,
		proxyuser			=> gi.proxyuser,
		last_modifiedon		=> gi.last_modifiedon,
		last_modifiedby		=> gi.last_modifiedby,
		comments			=> gi.comments,
		date_group			=> CASE WHEN TO_NUMBER(TO_CHAR(req_recieved_date,''j'')) = :jdate_list
								THEN '''||'TODAY'||'''
								WHEN TO_NUMBER(TO_CHAR(req_recieved_date,''j'')) > :jdate_list
								THEN '''||'YESTERDAY'||'''
								WHEN TO_NUMBER(TO_CHAR(req_recieved_date,''j'')) > :jdate_list
								THEN '''||'THIS WEEK'||'''
								WHEN TO_NUMBER(TO_CHAR(req_recieved_date,''j'')) > :jdate_list
								THEN '''||'LAST WEEK'||'''
								WHEN TO_NUMBER(TO_CHAR(req_recieved_date,''j'')) > :jdate_list
								THEN '''||'THIS MONTH'||'''
								WHEN TO_NUMBER(TO_CHAR(req_recieved_date,''j'')) > :jdate_list
								THEN '''||'LAST MONTH'||'''
								WHEN TO_NUMBER(TO_CHAR(req_recieved_date,''j'')) > :jdate_list
								THEN '''||'THIS YEAR'||'''
								ELSE '''||'OLDER'||'''
								END,
		operation			=> NULL,
		lang_id				=> 1,
		subject				=> gi.subject,
		user_pic_url		=> (SELECT user_pic_url 
								FROM users 
								WHERE user_id = '''||userid||'''),
        file_cnt            =>(SELECT COUNT(*)
                                FROM document_detail
                                WHERE file_value = TO_CHAR(gi.profileinstanceid)),
        module_name         => gi.module_name
		program_phase		=> NULL,
		program_message		=> NULL,
		p_error				=> NULL )
	FROM globalinbox gi
	WHERE ';
 IF(inbox_tab = 'PDG') THEN
	lv_sql_stmt := lv_sql_stmt ||' activitycompleted = 0
							AND workflowcompleted = 0
							AND actiontype IN (''A'',''B'',''R'')
							AND recipientid = '''||userid||'''';
 ELSIF(inbox_tab = 'SNZ') THEN
	lv_sql_stmt := lv_sql_stmt ||' recipientid = '''||userid||'''
							 AND activitycompleted = 0
							 AND workflowcompleted = 0';

 ELSIF(inbox_tab = 'SNT') THEN
	lv_sql_stmt := lv_sql_stmt ||' senderid = '''||userid||'''
							AND workflowcompleted = 0
							AND actiontype IN (''A'',''B'',''R'')
							AND workflowinstanceid IN 
							(SELECT workflowinstanceid
							FROM (SELECT workflowinstanceid,
									MAX(workflowinstanceid)
									OVER( PARTITION BY profileinstanceid,
														senderid) AS max_work
									FROM globalinbox
									WHERE senderid = '''||userid||''')
							WHERE workflowinstanceid = max_work)';
 ELSIF (inbox_tab = 'COM') THEN
	lv_sql_stmt := lv_sql_stmt ||' senderid = '''||userid||'''
							AND workflowcompleted = 1
							AND workflowinstanceid IN 
							(SELECT workflowinstanceid
							FROM (SELECT workflowinstanceid,
									MAX(workflowinstanceid)
									OVER( PARTITION BY profileinstanceid,
														senderid) AS max_work
									FROM globalinbox
									WHERE senderid = '''||userid||''')
							WHERE workflowinstanceid = max_work)';
ELSIF (inbox_tab = 'DFT') THEN
	lv_sql_stmt := lv_sql_stmt ||'	recipientid = '''||userid||'''
									AND activitycompleted = 0
									AND workflowcompleted = 0
									AND actiontype = ''S''';
 ELSE
		lv_error := 'Invalid Inbox_tab type';
		lv_errcode:= 'ORA-000';
		RAISE dml_errors;
 END IF;
    lv_sql_stmt := lv_sql_stmt || ' ORDER BY Req_Recieved_Date DESC '; 
	OPEN lv_ref_cur FOR lv_sql_stmt
					USING	jdate_list(1),
							jdate_list(2),
							jdate_list(3),
							jdate_list(4),
							jdate_list(5),
							jdate_list(6),
							jdate_list(7);
	FETCH lv_ref_cur BULK COLLECT INTO p_inbox_tbl;

	RETURN lv_retval;
 EXCEPTION
 WHEN dml_errors THEN
	DBMS_OUTPUT.PUT_LINE(utl_log.setutllog(lv_program
											,gv_program
											,lv_phase
											,lv_errcode
											,lv_error
											,lv_logparam));
	lv_ref_cur := utl_log.geterrdetail(lv_rec
										,lv_phase
										,lv_errcode
										,lv_error);

	lv_retval   := FALSE; 
	return lv_retval;
 WHEN OTHERS THEN
	DBMS_OUTPUT.PUT_LINE(utl_log.setutllog(lv_program
											,gv_program
											,lv_phase
											,SQLCODE
											,SQLERRM
											,lv_logparam));
		lv_ref_cur := utl_log.geterrdetail(lv_rec
										,lv_phase
										,SQLCODE
										,SQLERRM);
	FETCH lv_ref_cur BULK COLLECT INTO p_inbox_tbl;
	CLOSE lv_ref_cur;
	lv_retval := FALSE;
	RETURN lv_retval;
END get_inbox;

FUNCTION set_inbox(p_inbox_tbl IN OUT inbox_tbl)
RETURN BOOLEAN AS
	lv_ref_cur				utl_lib.t_ref_typ;
	lv_username_tab			username_tab	:= username_tab();
	lv_state_id				NVARCHAR2(50)	:= NULL;
	lv_sql_stmt				VARCHAR2(4000)	:= NULL;
	lv_transitionclaim		fxo.WorkFlowTransition."TransitionClaim"%TYPE := NULL;
	lv_item_status			VARCHAR2(5)		:= NULL;
    lv_subject              globalinbox.subject%TYPE := NULL;

BEGIN
	lv_retval	:= TRUE;
	lv_program	:= 'set_inbox';
	lv_phase	:= 'Phase 1 Startup';

 IF p_inbox_tbl(1).operation IN ('CHGS','DFT') THEN
 
  IF p_inbox_tbl(1).operation = 'CHGS' THEN

	SELECT "StateId" 
	INTO lv_state_id
	FROM fxo.WorkFlowState
	WHERE "WorkflowStateId" = p_inbox_tbl(1).tostateid;

	SELECT "TransitionClaim"
	INTO lv_transitionclaim
	FROM fxo.WorkFlowTransition
	WHERE "DataProfileId"	= TO_CHAR(p_inbox_tbl(1).dataprofile_id)
	AND "FromStateId"		= p_inbox_tbl(1).tostateid
	AND "TransitionType"	= (SELECT CASE WHEN "StateId" = utl_lib.finalstate
										THEN 'ST'
										ELSE 'NT'
										END
								FROM fxo.WorkFlowState
								WHERE "WorkflowStateId" = p_inbox_tbl(1).tostateid);

	lv_sql_stmt := 
		'SELECT username_rec(id => "Id",
					username => "UserName")
		FROM sec.IdentityUser
		WHERE ';
	IF lv_transitionclaim = utl_lib.divisionmanager THEN
 IF p_inbox_tbl(1).module_name = 'ITEM' THEN
		lv_sql_stmt := lv_sql_stmt ||
		'"Id" =
		(SELECT DISTINCT dm.division_mgr
		FROM division_mst dm
		INNER JOIN item_basics ib
		ON ( dm.division_id  = ib.division_id)
        INNER JOIN users us
        ON dm.division_mgr = us.user_id
		WHERE ib.item = '''||p_inbox_tbl(1).profileinstance_id||''')';
     ELSE
		lv_sql_stmt := lv_sql_stmt ||
		'"Id" IN
		(SELECT DISTINCT dm.division_mgr
		FROM division_mst dm
		INNER JOIN item_basics ib
		ON ( dm.division_id  = ib.division_id)
        INNER JOIN price_change_detail pcd
        ON ib.item = pcd.item
        INNER JOIN users us
        ON dm.division_mgr = us.user_id
		WHERE pcd.price_change_id = '''||p_inbox_tbl(1).profileinstance_id||''')';      
     END IF;      
	ELSIF lv_transitionclaim = utl_lib.categorybuyer THEN
      IF p_inbox_tbl(1).module_name = 'ITEM' THEN    
        lv_sql_stmt := lv_sql_stmt ||
	'"Id" =
	(SELECT DISTINCT cm.buyer_id
		FROM category_mst cm
		INNER JOIN item_basics ib         
        ON ( cm.division_id = ib.division_id
        AND cm.dept_id  = ib.dept_id
		AND cm.category_id = ib.category_id )
        INNER JOIN users us
        ON cm.buyer_id = us.user_id
		WHERE ib.item = '''||p_inbox_tbl(1).profileinstance_id||''')';
     ELSE
        lv_sql_stmt := lv_sql_stmt ||
	'"Id" IN
	(SELECT DISTINCT cm.buyer_id
		FROM category_mst cm
		INNER JOIN item_basics ib         
        ON ( cm.division_id = ib.division_id
        AND cm.dept_id  = ib.dept_id
		AND cm.category_id = ib.category_id )
        INNER JOIN price_change_detail pcd
        ON ib.item = pcd.item
        INNER JOIN users us
        ON cm.buyer_id = us.user_id
		WHERE pcd.price_change_id = '''||p_inbox_tbl(1).profileinstance_id||''')';    
     END IF;   
	ELSE
	lv_sql_stmt := lv_sql_stmt ||
	'"Id" IN ( SELECT "UserId"
				FROM sec.IdentityUserRole
				WHERE "RoleId" IN (
				SELECT "RoleId"
				FROM sec.IdentityRoleClaim
				WHERE "ClaimValue" = '''||lv_transitionclaim||'''))';
	END IF;
 ELSE 
 lv_sql_stmt := 
		'SELECT username_rec(id => '''||p_inbox_tbl(1).sender_id||''',
					username => '''||p_inbox_tbl(1).sender_name||''')
		FROM dual ';
 END IF; 
	OPEN lv_ref_cur FOR lv_sql_stmt;
	FETCH lv_ref_cur BULK COLLECT INTO lv_username_tab;

    SELECT desc_long INTO lv_subject
    FROM item_basics 
    WHERE item = p_inbox_tbl(1).profileinstance_id;

	IF lv_username_tab.COUNT > 0 THEN
	FOR i IN lv_username_tab.FIRST..lv_username_tab.LAST LOOP
	EXIT WHEN lv_username_tab.COUNT = 0;
	INSERT INTO globalinbox
				(globalinboxid,
				senderid,
				sendername,
				recipientid,
				recipientname,
				dataprofileid,
				profileinstanceid,
				workflowid,
				workflowinstanceid,
				req_recieved_date,
				duedate,
				status,
				req_createdby,
				req_createdbyname,
				req_datecreated,
				actiontype,
				requestfor,
				requestforname,
				amount,
				purpose,
				stateid,
				tostateid,
				requestread,
				requestreadby,
				issnoozed,
				issnoozedby,
				activitycompleted,
				workflowcompleted,
				isdeclined,
				req_doc_loc,
				proxyindicator,
				proxyuser,
				last_modifiedon,
				last_modifiedby,
				comments,
                module_name,
				subject)
	VALUES(		global_inbox_seq.NEXTVAL,
				p_inbox_tbl(1).sender_id,
				p_inbox_tbl(1).sender_name,
				lv_username_tab(i).id,
				lv_username_tab(i).username,
				p_inbox_tbl(1).dataprofile_id,
				p_inbox_tbl(1).profileinstance_id,
				p_inbox_tbl(1).workflow_id,   
				p_inbox_tbl(1).workflowinstance_id,
				p_inbox_tbl(1).req_date,
				p_inbox_tbl(1).req_datecreated + 3,
				p_inbox_tbl(1).status,
				p_inbox_tbl(1).req_createdby, 
				p_inbox_tbl(1).req_createdbyname, 
				p_inbox_tbl(1).req_datecreated,
				CASE WHEN p_inbox_tbl(1).operation = 'DFT'
				THEN 'S' ELSE 'A' END,
				p_inbox_tbl(1).requestfor,
				p_inbox_tbl(1).requestforname,
				p_inbox_tbl(1).amount,
				p_inbox_tbl(1).purpose,
				p_inbox_tbl(1).stateid,
				p_inbox_tbl(1).tostateid,
				NVL(p_inbox_tbl(1).requestread,0),
				p_inbox_tbl(1).requestreadby,
				NVL(p_inbox_tbl(1).issnoozed,0),
				p_inbox_tbl(1).issnoozedby,
				NVL(p_inbox_tbl(1).activitycompleted,0),
				NVL(p_inbox_tbl(1).workflowcompleted,0),
				NVL(p_inbox_tbl(1).isdeclined,0),
				p_inbox_tbl(1).req_doc_loc,
				p_inbox_tbl(1).proxyindicator,
				p_inbox_tbl(1).proxyuser,
				SYSDATE,
				USER,
				p_inbox_tbl(1).Comments,
                p_inbox_tbl(1).module_name,                
        lv_subject);
	END LOOP;
	END IF; 
    
   IF  p_inbox_tbl(1).operation <> 'DFT' THEN

	UPDATE globalinbox
	SET activitycompleted	= 1
		,last_modifiedon	= SYSDATE
		,last_modifiedby	= USER
	WHERE (workflowcompleted = '0'
	AND		activitycompleted = '0')
	AND		profileinstanceid	= p_inbox_tbl(1).profileinstance_id
	AND		workflowid			= p_inbox_tbl(1).workflow_id
	AND		recipientid			= p_inbox_tbl(1).sender_id;

	DELETE FROM globalinbox
	WHERE	profileinstanceid	= p_inbox_tbl(1).profileinstance_id
	AND		workflowid			= p_inbox_tbl(1).workflow_id
	AND		activitycompleted	= '0'
	AND		senderid			!= p_inbox_tbl(1).sender_id;
    
    END IF;

	lv_item_status := CASE WHEN p_inbox_tbl(1).operation = 'DFT'
                        THEN 'S' ELSE 'A' END;
  
	IF lv_state_id = utl_lib.finalstate THEN
		UPDATE globalinbox
		SET workflowcompleted = 1,
			actiontype = 'C'
		WHERE	profileinstanceid	= p_inbox_tbl(1).profileinstance_id
		AND		workflowid			= p_inbox_tbl(1).workflow_id
		AND		actiontype			NOT IN ('B','R');

		DELETE FROM globalinbox gi
		WHERE gi.profileinstanceid	= p_inbox_tbl(1).profileinstance_id
		AND gi.workflowid			= p_inbox_tbl(1).workflow_id
		AND gi.activitycompleted	= '0'
		AND NOT EXISTS
				( SELECT 1
				FROM globalinbox gi2
				WHERE gi2.profileinstanceid	    = p_inbox_tbl(1).profileinstance_id
				AND gi2.workflowid				= p_inbox_tbl(1).workflow_id
				AND gi2.activitycompleted		= '1'
				AND gi.recipientid				= gi2.senderid);

		lv_item_status := 'C';
    END IF;

	ELSIF p_inbox_tbl(1).operation IN ('SBR','REJ') THEN
		UPDATE globalinbox
		SET  activitycompleted	= 1
			,actiontype			= 'A'
			,last_modifiedon	= SYSDATE
			,last_modifiedby	= USER
		WHERE (workflowcompleted = '0'
		AND		activitycompleted = '0')
		AND		profileinstanceid	= p_inbox_tbl(1).profileinstance_id
		AND		workflowid			= p_inbox_tbl(1).workflow_id
		AND		recipientid			= p_inbox_tbl(1).sender_id;

		DELETE FROM globalinbox 
		WHERE	profileinstanceid	= p_inbox_tbl(1).profileinstance_id
		AND		workflowid			= p_inbox_tbl(1).workflow_id
		AND		activitycompleted	= '0'
		AND		senderid			!= p_inbox_tbl(1).sender_id;

		INSERT INTO globalinbox
				(globalinboxid,
				senderid,
				sendername,
				recipientid,
				recipientname,
				dataprofileid,
				profileinstanceid,
				workflowid,
				workflowinstanceid,
				req_recieved_date,
				duedate,
				status,
				req_createdby,
				req_createdbyname,
				req_datecreated,
				actiontype,
				requestfor,
				requestforname,
				amount,
				purpose,
				stateid,
				tostateid,
				requestread,
				requestreadby,
				issnoozed,
				issnoozedby,
				activitycompleted,
				workflowcompleted,
				isdeclined,
				req_doc_loc,
				proxyindicator,
				proxyuser,
				last_modifiedon,
				last_modifiedby,
                module_name,
				comments)
	SELECT		global_inbox_seq.NEXTVAL,
				p_inbox_tbl(1).sender_id,
				p_inbox_tbl(1).sender_name,
				senderid,
				sendername,
				p_inbox_tbl(1).dataprofile_id,
				p_inbox_tbl(1).profileinstance_id,
				p_inbox_tbl(1).workflow_id,
				p_inbox_tbl(1).workflowinstance_id,
				p_inbox_tbl(1).req_date,
				p_inbox_tbl(1).req_datecreated + 3,
				p_inbox_tbl(1).status,
				p_inbox_tbl(1).req_createdby,
				p_inbox_tbl(1).req_createdbyname,
				p_inbox_tbl(1).req_datecreated,
				CASE WHEN p_inbox_tbl(1).operation = 'SBR'
				THEN 'B' ELSE 'R' END,
				p_inbox_tbl(1).requestfor,
				p_inbox_tbl(1).requestforname,
				p_inbox_tbl(1).amount, 
				p_inbox_tbl(1).purpose,
				p_inbox_tbl(1).stateid,
				p_inbox_tbl(1).tostateid,
				NVL(p_inbox_tbl(1).requestread,0),
				p_inbox_tbl(1).requestreadby,
				NVL(p_inbox_tbl(1).issnoozed,0),
				p_inbox_tbl(1).issnoozedby,
				'0',
				NVL(p_inbox_tbl(1).workflowcompleted,0),
				NVL(p_inbox_tbl(1).isdeclined,0),
				p_inbox_tbl(1).req_doc_loc,
				p_inbox_tbl(1).ProxyIndicator,
				p_inbox_tbl(1).ProxyUser,
				SYSDATE,
				USER,
                p_inbox_tbl(1).module_name,                
				p_inbox_tbl(1).Comments
				FROM globalinbox
				WHERE profileinstanceid	= p_inbox_tbl(1).profileinstance_id
				AND workflowid			= p_inbox_tbl(1).workflow_id
				AND stateid				= p_inbox_tbl(1).tostateid
                AND ROWNUM              = 1; 

		lv_item_status := CASE WHEN p_inbox_tbl(1).operation = 'SBR'
							THEN NULL ELSE 'R' END;

/*	ELSE
		UPDATE globalinbox gi
			SET requestread = 1
			,requestreadby = p_inbox_tbl(1).recipient_id
			,issnoozed = CASE WHEN(p_inbox_tbl(1).operation = 'SNZ') 
								THEN '1' ELSE gi.issnoozed END
			,issnoozedBy = CASE WHEN(p_inbox_tbl(1).operation = 'SNZ') 
								THEN p_inbox_tbl(1).recipient_id ELSE gi.issnoozed END
			,activitycompleted = CASE WHEN(p_inbox_tbl(1).operation <> 'SNZ') 
									THEN '1' ELSE gi.activitycompleted END
			,workflowcompleted = CASE WHEN(p_inbox_tbl(1).operation = 'COM') 
									THEN '1' ELSE gi.workflowcompleted END
			,isdeclined = CASE WHEN(p_inbox_tbl(1).operation = 'DEC') 
								THEN '1' ELSE gi.isdeclined END
			,last_modifiedon = SYSDATE
			,last_modifiedby = USER
			WHERE gi.workflowcompleted = '0' AND gi.activitycompleted = '0'
			AND ((gi.recipientid = p_inbox_tbl(1).sender_id) 
			OR  (gi.senderid = p_inbox_tbl(1).sender_id
			AND gi.recipientid = p_inbox_tbl(1).recipient_id));*/
	END IF;
--- item status changes

	lv_item_status :=
		CASE WHEN lv_item_status = 'C'
			THEN 'APD'
			WHEN lv_item_status IN ('A','R','B')
			THEN 'SUB'
			END;
   IF lv_item_status IN ('APD','SUB') THEN         
    FOR rec IN (SELECT column_name
			FROM user_tab_columns
			WHERE table_name = 'ITEM_WORKFLOW_TAB_LIST'
			AND column_name NOT IN ('ITEM','FASHION')) LOOP
        EXECUTE IMMEDIATE ' UPDATE '
			||rec.column_name
			||' SET status = '''
			||lv_item_status
			||''' WHERE item = '''
			||p_inbox_tbl(1).profileinstance_id||'''';
    END LOOP;
   END IF; 
	RETURN lv_retval;
EXCEPTION
	WHEN OTHERS THEN
		DBMS_OUTPUT.PUT_LINE(utl_log.setutllog(lv_program
								,gv_program
								,lv_phase
								,SQLCODE
								,SQLERRM
								,lv_logparam));
		lv_ref_cur := utl_log.geterrdetail(lv_rec
										,lv_phase
										,SQLCODE
										,SQLERRM);
		FETCH lv_ref_cur BULK COLLECT INTO p_inbox_tbl;
		CLOSE lv_ref_cur;
		lv_retval := FALSE;
	RETURN lv_retval;
END set_inbox;

FUNCTION get_inbox_quickview(dataprofile_id			IN VARCHAR2
							,workflow_id			IN VARCHAR2
							,profile_instance_id	IN VARCHAR2
							,p_quickveiw			IN OUT quickview_tab)
RETURN BOOLEAN AS
	lv_sql_stmt			VARCHAR2(4000);
	lv_ref_cur			utl_lib.t_ref_typ;
	lv_col_val			VARCHAR2(250) := NULL;
	lv_quickview_rec	quickview_rec;
	lv_quickview_tab	quickview_tab := quickview_tab();
BEGIN
	lv_count := 1;
	FOR rec IN (SELECT	ref_table_name,
						ref_column_name,
						qk_column_name
				FROM quick_view
				WHERE dataprofile_id = dataprofile_id
				AND workflow_id = workflow_id)
	LOOP
	lv_sql_stmt :=  'SELECT '
			|| rec.qk_column_name
			||' FROM '
			||rec.ref_table_name 
			||' WHERE TO_CHAR('
			||rec.ref_column_name
			||') = '''
			||profile_instance_id||'''';
		OPEN lv_ref_cur FOR lv_sql_stmt;
		FETCH lv_ref_cur INTO lv_col_val; 
		CLOSE lv_ref_cur;
		lv_quickview_tab.EXTEND;
		lv_quickview_tab(lv_count):=
						quickview_rec(col_name => (CASE WHEN rec.qk_column_name = 'ITEM_TYPE'
                                                  THEN 'ITEM TYPE'
                                                  WHEN rec.qk_column_name = 'DESC_LONG'
                                                  THEN 'LONG DESCRIPTION'
                                                  ELSE rec.qk_column_name END),
								col_value => lv_col_val);
	lv_count := lv_count + 1;
	END LOOP; 
		p_quickveiw := lv_quickview_tab;
	RETURN lv_retval;
EXCEPTION
	WHEN OTHERS THEN
		DBMS_OUTPUT.PUT_LINE(utl_log.setutllog(lv_program
								,gv_program
								,lv_phase
								,SQLCODE
								,SQLERRM
								,lv_logparam));
		lv_ref_cur := utl_log.geterrdetail(lv_rec
										,lv_phase
										,SQLCODE
										,SQLERRM);
		FETCH lv_ref_cur BULK COLLECT INTO p_quickveiw;
		CLOSE lv_ref_cur;
		lv_retval := FALSE;
	RETURN lv_retval;
END get_inbox_Quickview;

    -- 
  -- ---------------------------------------------------------------------------- 
  -- |------------------------< getGlobalInboxCnt     >-------------------------| 
  -- ---------------------------------------------------------------------------- 
  --

FUNCTION getglobalinboxcnt	(p_userid				IN VARCHAR2
							,p_global_inbox_cnt_tab	IN OUT NOCOPY global_inbox_cnt_tab)
RETURN BOOLEAN IS
-- Declare local varaibles to be used in the applcations.
	lv_retval		BOOLEAN := TRUE;
	lv_ref_cur		utl_lib.t_ref_typ;

BEGIN
	lv_retval  := TRUE; 
	Lv_Program := 'getglobalinboxcnt';
	Lv_Rec := 'GLOBAL_INBOX_CNT_REC';
	Lv_Phase := 'Phase 1 Started';

	SELECT global_inbox_cnt_rec
		(pending_cnt		=> gi.pending_cnt,
		snoozed_cnt			=> gi.snoozed_cnt,
		completed_cnt		=> gi.completed_cnt,
		sent_cnt			=> gi.sent_cnt,
		percent_cnt			=> gi.percent_cnt,
		duedate_cnt			=> gi.duedate_cnt,
		highpriority_cnt	=> gi.highpriority_cnt,
        draft_cnt       => gi.draft_cnt,
		program_phase		=> NULL,
		program_message		=> NULL,
		p_error				=> null)
	BULK COLLECT INTO p_global_inbox_cnt_tab
	FROM (SELECT SUM(NVL(pending_cnt,0)) AS pending_cnt,
				SUM(NVL(completed_cnt,0)) AS completed_cnt,
				SUM(NVL(snoozed_cnt,0)) AS snoozed_cnt,
				SUM(NVL(sent_cnt,0)) AS sent_cnt,
				SUM(NVL(percent_cnt,0)) AS percent_cnt,
				SUM(NVL(duedate_cnt,0)) AS duedate_cnt,
				SUM(NVL(highpriority_cnt,0)) AS highpriority_cnt,
                SUM(NVL(draft_cnt,0)) AS draft_cnt        
		FROM ( SELECT SUM(CASE WHEN activitycompleted = 0
						AND workflowcompleted = 0 AND actiontype IN ('A','B','R')
						THEN 1 ELSE 0 END ) AS pending_cnt,
					0 AS completed_cnt,
					SUM(CASE WHEN activitycompleted = 0
					AND workflowcompleted = 0
					THEN 1 ELSE 0 END) AS snoozed_cnt,
					0 AS sent_cnt,
					ROUND((SUM( CASE WHEN req_datecreated <= duedate 
										AND actiontype = 'C' 
										THEN 1 ELSE 0 END)/
								SUM( CASE WHEN req_datecreated <= duedate 
										THEN 1 ELSE 0 END) )*100 ) AS percent_cnt,
						SUM( CASE WHEN (SYSDATE > duedate
								AND actiontype IN ('A','B','R')
								AND activitycompleted = 0
								AND workflowcompleted = 0)
								THEN 1 ELSE 0 END) AS duedate_cnt,
						SUM( CASE WHEN status = 'H'
								AND activitycompleted = 0
								AND workflowcompleted = 0
								THEN 1 ELSE 0 END) AS highpriority_cnt,
						SUM( CASE WHEN actiontype = 'S'
								AND activitycompleted = 0
								AND workflowcompleted = 0
								THEN 1 ELSE 0 END) AS draft_cnt
			FROM globalinbox 
			WHERE recipientid = p_userid
			UNION ALL
				SELECT 0 AS pending_cnt,
				SUM(CASE WHEN workflowcompleted = 1 
					THEN 1 ELSE 0 END) AS completed_cnt,
					0 AS snoozed_cnt,
					SUM(CASE WHEN workflowcompleted = 0 AND actiontype IN ('A','B','R')
					THEN 1 ELSE 0 END) AS sent_cnt,
					0 AS percent_cnt,
					0 AS duedate_cnt,
					0 AS highpriority_cnt,
					0 AS draft_cnt
				FROM GlobalInbox 
				WHERE senderid = p_userid 
                AND workflowinstanceid IN 
			(SELECT  workflowinstanceid
				FROM ( SELECT workflowinstanceid,
					MAX(workflowinstanceid)
					OVER( PARTITION BY profileinstanceid
								,senderid) AS max_work
			FROM globalinbox
			WHERE senderid = p_userid )
		WHERE workflowinstanceid = max_work ))) gi;
	RETURN lv_retval;
EXCEPTION 
	WHEN OTHERS THEN
		DBMS_OUTPUT.PUT_LINE(utl_log.setutllog(lv_program
								,gv_program
								,lv_phase
								,SQLCODE
								,SQLERRM
								,lv_logparam));
		lv_ref_cur := utl_log.geterrdetail(lv_rec
										,lv_phase
										,SQLCODE
										,SQLERRM);
		FETCH lv_ref_cur BULK COLLECT INTO p_global_inbox_cnt_tab;
		CLOSE lv_ref_cur;
		lv_retval := FALSE;
	RETURN lv_retval;
END getglobalinboxcnt;
  
FUNCTION getinboxhistory (p_item IN NUMBER
			,p_globalinbox_hist_tab IN OUT NOCOPY globalinbox_hist_tab)
RETURN BOOLEAN AS
	lv_retval		BOOLEAN := TRUE;
	lv_ref_cur		utl_lib.t_ref_typ;
BEGIN
	Lv_Rec := 'GLOBALINBOX_HIST_REC';

	SELECT  globalinbox_hist_rec
		(sendername			=> gi.sendername,
		recipientname		=> gi.recipientname,
		action_name			=> gi.action_name,
		req_recieved_date	=> gi.req_recieved_date,
		comments			=> gi.comments,
		in_progress			=> TO_NUMBER(TRUNC(SYSDATE-req_recieved_date))
								||' Day(s)',
		item				=> gi.profileinstanceid
		)
	BULK COLLECT INTO p_globalinbox_hist_tab
	FROM( SELECT sendername,
				recipientname,
				req_recieved_date,
				comments,
				(SELECT UPPER("StateName")
				FROM fxo.WorkFlowState 
				WHERE "WorkflowStateId" = tostateid) AS action_name,
				profileinstanceid 
		FROM globalinbox
		WHERE profileinstanceid = p_Item
        AND actiontype <> 'S') gi
		ORDER BY req_recieved_date DESC;
	RETURN lv_retval;
EXCEPTION
	WHEN OTHERS THEN
		DBMS_OUTPUT.PUT_LINE(utl_log.setutllog(lv_program
								,gv_program
								,lv_phase
								,SQLCODE
								,SQLERRM
								,lv_logparam));
		lv_ref_cur := utl_log.geterrdetail(lv_rec
										,lv_phase
										,SQLCODE
										,SQLERRM);
		FETCH lv_ref_cur BULK COLLECT INTO p_globalinbox_hist_tab;
		CLOSE lv_ref_cur;
		lv_retval := FALSE;
	RETURN lv_retval;
END getinboxhistory;

FUNCTION getstateid (p_operation			IN VARCHAR2,
					p_stateid				IN VARCHAR2,
					p_profileinstance_id	IN VARCHAR2,
					p_workflowid			IN VARCHAR2)
RETURN VARCHAR2 AS
lv_stateid		globalinbox.stateid%TYPE := NULL;
lv_ref_cur		utl_lib.t_ref_typ;
lv_sql_stmt		VARCHAR2(4000) := NULL;
BEGIN
	IF p_operation = 'SBR' THEN
		lv_sql_stmt :=
		'SELECT "FromStateId"
        FROM fxo.WorkFlowTransition
        WHERE "ToStateId" =	'''
		||p_stateid||'''
        AND "TransitionType" = ''NT''';
	ELSIF p_operation = 'REJ' THEN
		lv_sql_stmt :=
		'SELECT "WorkflowStateId"
		FROM fxo.workflowstate
		WHERE "StateId" = ''CreatedState''
		AND "WorkflowId" = '''||p_workflowid||'''';
	END IF;
	OPEN lv_ref_cur FOR lv_sql_stmt;
	FETCH lv_ref_cur INTO lv_stateid;
	CLOSE lv_ref_cur;
	RETURN lv_stateid;
EXCEPTION
WHEN OTHERS THEN
	CLOSE lv_ref_cur;
	RETURN lv_stateid;
END getstateid;
END utl_global_inbox;
