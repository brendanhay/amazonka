{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.RuleGroupUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.RuleGroupUpdate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.WAF.Types.ActivatedRule
import Network.AWS.WAF.Types.ChangeAction

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Specifies an @ActivatedRule@ and indicates whether you want to add it to
-- a @RuleGroup@ or delete it from a @RuleGroup@.
--
-- /See:/ 'newRuleGroupUpdate' smart constructor.
data RuleGroupUpdate = RuleGroupUpdate'
  { -- | Specify @INSERT@ to add an @ActivatedRule@ to a @RuleGroup@. Use
    -- @DELETE@ to remove an @ActivatedRule@ from a @RuleGroup@.
    action :: ChangeAction,
    -- | The @ActivatedRule@ object specifies a @Rule@ that you want to insert or
    -- delete, the priority of the @Rule@ in the @WebACL@, and the action that
    -- you want AWS WAF to take when a web request matches the @Rule@ (@ALLOW@,
    -- @BLOCK@, or @COUNT@).
    activatedRule :: ActivatedRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'ruleGroupUpdate_action' - Specify @INSERT@ to add an @ActivatedRule@ to a @RuleGroup@. Use
-- @DELETE@ to remove an @ActivatedRule@ from a @RuleGroup@.
--
-- 'activatedRule', 'ruleGroupUpdate_activatedRule' - The @ActivatedRule@ object specifies a @Rule@ that you want to insert or
-- delete, the priority of the @Rule@ in the @WebACL@, and the action that
-- you want AWS WAF to take when a web request matches the @Rule@ (@ALLOW@,
-- @BLOCK@, or @COUNT@).
newRuleGroupUpdate ::
  -- | 'action'
  ChangeAction ->
  -- | 'activatedRule'
  ActivatedRule ->
  RuleGroupUpdate
newRuleGroupUpdate pAction_ pActivatedRule_ =
  RuleGroupUpdate'
    { action = pAction_,
      activatedRule = pActivatedRule_
    }

-- | Specify @INSERT@ to add an @ActivatedRule@ to a @RuleGroup@. Use
-- @DELETE@ to remove an @ActivatedRule@ from a @RuleGroup@.
ruleGroupUpdate_action :: Lens.Lens' RuleGroupUpdate ChangeAction
ruleGroupUpdate_action = Lens.lens (\RuleGroupUpdate' {action} -> action) (\s@RuleGroupUpdate' {} a -> s {action = a} :: RuleGroupUpdate)

-- | The @ActivatedRule@ object specifies a @Rule@ that you want to insert or
-- delete, the priority of the @Rule@ in the @WebACL@, and the action that
-- you want AWS WAF to take when a web request matches the @Rule@ (@ALLOW@,
-- @BLOCK@, or @COUNT@).
ruleGroupUpdate_activatedRule :: Lens.Lens' RuleGroupUpdate ActivatedRule
ruleGroupUpdate_activatedRule = Lens.lens (\RuleGroupUpdate' {activatedRule} -> activatedRule) (\s@RuleGroupUpdate' {} a -> s {activatedRule = a} :: RuleGroupUpdate)

instance Prelude.Hashable RuleGroupUpdate

instance Prelude.NFData RuleGroupUpdate

instance Prelude.ToJSON RuleGroupUpdate where
  toJSON RuleGroupUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Action" Prelude..= action),
            Prelude.Just
              ("ActivatedRule" Prelude..= activatedRule)
          ]
      )
