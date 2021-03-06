/*
 * Sonar PL/SQL Plugin (Community)
 * Copyright (C) 2015-2018 Felipe Zorzo
 * mailto:felipebzorzo AT gmail DOT com
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */
package org.sonar.plugins.plsqlopen.api.ddl;

import static org.sonar.sslr.tests.Assertions.assertThat;

import org.junit.Before;
import org.junit.Test;
import org.sonar.plugins.plsqlopen.api.DdlGrammar;
import org.sonar.plugins.plsqlopen.api.RuleTest;

public class CreateSequenceTest extends RuleTest {

    @Before
    public void init() {
        setRootRule(DdlGrammar.CREATE_SEQUENCE);
    }
    
    @Test
    public void matchesSimpleCreateSequence() {
        assertThat(p).matches("create sequence seq_name;");
    }
    
    @Test
    public void matchesCreateSequenceStart() {
    	assertThat(p).matches("create sequence seq_name start with 1;");
    }
    
    @Test
    public void matchesCreateSequenceIncrement() {
    	assertThat(p).matches("create sequence seq_name increment by 1;");
    }
    
    @Test
    public void matchesCreateSequenceStartIncrement() {
    	assertThat(p).matches("create sequence seq_name start with 1 increment by 1;");
    }
    
    @Test
    public void matchesCreateSequenceCache() {
    	assertThat(p).matches("create sequence seq_name cache 10;");
    }
    
    @Test
    public void matchesCreateSequenceNoCache() {
    	assertThat(p).matches("create sequence seq_name nocache;");
    }
    
    @Test
    public void matchesCreateSequenceXXX() {
    	assertThat(p).matches("create sequence seq_name start with 1 MAXVALUE 9999 MINVALUE 1 NOCYCLE CACHE 20 ORDER;");
    }
}
