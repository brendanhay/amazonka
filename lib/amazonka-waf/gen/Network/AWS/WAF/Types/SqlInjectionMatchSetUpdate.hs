{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.SqlInjectionMatchSetUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.SqlInjectionMatchSetUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAF.Types.ChangeAction
import Network.AWS.WAF.Types.SqlInjectionMatchTuple

-- | Specifies the part of a web request that you want to inspect for snippets of malicious SQL code and indicates whether you want to add the specification to a 'SqlInjectionMatchSet' or delete it from a @SqlInjectionMatchSet@ .
--
--
--
-- /See:/ 'sqlInjectionMatchSetUpdate' smart constructor.
data SqlInjectionMatchSetUpdate = SqlInjectionMatchSetUpdate'
  { _simsuAction ::
      !ChangeAction,
    _simsuSqlInjectionMatchTuple ::
      !SqlInjectionMatchTuple
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SqlInjectionMatchSetUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'simsuAction' - Specify @INSERT@ to add a 'SqlInjectionMatchSetUpdate' to a 'SqlInjectionMatchSet' . Use @DELETE@ to remove a @SqlInjectionMatchSetUpdate@ from a @SqlInjectionMatchSet@ .
--
-- * 'simsuSqlInjectionMatchTuple' - Specifies the part of a web request that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header.
sqlInjectionMatchSetUpdate ::
  -- | 'simsuAction'
  ChangeAction ->
  -- | 'simsuSqlInjectionMatchTuple'
  SqlInjectionMatchTuple ->
  SqlInjectionMatchSetUpdate
sqlInjectionMatchSetUpdate pAction_ pSqlInjectionMatchTuple_ =
  SqlInjectionMatchSetUpdate'
    { _simsuAction = pAction_,
      _simsuSqlInjectionMatchTuple = pSqlInjectionMatchTuple_
    }

-- | Specify @INSERT@ to add a 'SqlInjectionMatchSetUpdate' to a 'SqlInjectionMatchSet' . Use @DELETE@ to remove a @SqlInjectionMatchSetUpdate@ from a @SqlInjectionMatchSet@ .
simsuAction :: Lens' SqlInjectionMatchSetUpdate ChangeAction
simsuAction = lens _simsuAction (\s a -> s {_simsuAction = a})

-- | Specifies the part of a web request that you want AWS WAF to inspect for snippets of malicious SQL code and, if you want AWS WAF to inspect a header, the name of the header.
simsuSqlInjectionMatchTuple :: Lens' SqlInjectionMatchSetUpdate SqlInjectionMatchTuple
simsuSqlInjectionMatchTuple = lens _simsuSqlInjectionMatchTuple (\s a -> s {_simsuSqlInjectionMatchTuple = a})

instance Hashable SqlInjectionMatchSetUpdate

instance NFData SqlInjectionMatchSetUpdate

instance ToJSON SqlInjectionMatchSetUpdate where
  toJSON SqlInjectionMatchSetUpdate' {..} =
    object
      ( catMaybes
          [ Just ("Action" .= _simsuAction),
            Just ("SqlInjectionMatchTuple" .= _simsuSqlInjectionMatchTuple)
          ]
      )
