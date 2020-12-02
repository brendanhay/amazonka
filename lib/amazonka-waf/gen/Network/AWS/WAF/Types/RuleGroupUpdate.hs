{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.RuleGroupUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.RuleGroupUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAF.Types.ActivatedRule
import Network.AWS.WAF.Types.ChangeAction

-- | Specifies an @ActivatedRule@ and indicates whether you want to add it to a @RuleGroup@ or delete it from a @RuleGroup@ .
--
--
--
-- /See:/ 'ruleGroupUpdate' smart constructor.
data RuleGroupUpdate = RuleGroupUpdate'
  { _rguAction ::
      !ChangeAction,
    _rguActivatedRule :: !ActivatedRule
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RuleGroupUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rguAction' - Specify @INSERT@ to add an @ActivatedRule@ to a @RuleGroup@ . Use @DELETE@ to remove an @ActivatedRule@ from a @RuleGroup@ .
--
-- * 'rguActivatedRule' - The @ActivatedRule@ object specifies a @Rule@ that you want to insert or delete, the priority of the @Rule@ in the @WebACL@ , and the action that you want AWS WAF to take when a web request matches the @Rule@ (@ALLOW@ , @BLOCK@ , or @COUNT@ ).
ruleGroupUpdate ::
  -- | 'rguAction'
  ChangeAction ->
  -- | 'rguActivatedRule'
  ActivatedRule ->
  RuleGroupUpdate
ruleGroupUpdate pAction_ pActivatedRule_ =
  RuleGroupUpdate'
    { _rguAction = pAction_,
      _rguActivatedRule = pActivatedRule_
    }

-- | Specify @INSERT@ to add an @ActivatedRule@ to a @RuleGroup@ . Use @DELETE@ to remove an @ActivatedRule@ from a @RuleGroup@ .
rguAction :: Lens' RuleGroupUpdate ChangeAction
rguAction = lens _rguAction (\s a -> s {_rguAction = a})

-- | The @ActivatedRule@ object specifies a @Rule@ that you want to insert or delete, the priority of the @Rule@ in the @WebACL@ , and the action that you want AWS WAF to take when a web request matches the @Rule@ (@ALLOW@ , @BLOCK@ , or @COUNT@ ).
rguActivatedRule :: Lens' RuleGroupUpdate ActivatedRule
rguActivatedRule = lens _rguActivatedRule (\s a -> s {_rguActivatedRule = a})

instance Hashable RuleGroupUpdate

instance NFData RuleGroupUpdate

instance ToJSON RuleGroupUpdate where
  toJSON RuleGroupUpdate' {..} =
    object
      ( catMaybes
          [ Just ("Action" .= _rguAction),
            Just ("ActivatedRule" .= _rguActivatedRule)
          ]
      )
