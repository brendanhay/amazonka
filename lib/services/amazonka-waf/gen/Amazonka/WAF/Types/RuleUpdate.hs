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
-- Module      : Amazonka.WAF.Types.RuleUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAF.Types.RuleUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAF.Types.ChangeAction
import Amazonka.WAF.Types.Predicate

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Specifies a @Predicate@ (such as an @IPSet@) and indicates whether you
-- want to add it to a @Rule@ or delete it from a @Rule@.
--
-- /See:/ 'newRuleUpdate' smart constructor.
data RuleUpdate = RuleUpdate'
  { -- | Specify @INSERT@ to add a @Predicate@ to a @Rule@. Use @DELETE@ to
    -- remove a @Predicate@ from a @Rule@.
    action :: ChangeAction,
    -- | The ID of the @Predicate@ (such as an @IPSet@) that you want to add to a
    -- @Rule@.
    predicate :: Predicate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuleUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'ruleUpdate_action' - Specify @INSERT@ to add a @Predicate@ to a @Rule@. Use @DELETE@ to
-- remove a @Predicate@ from a @Rule@.
--
-- 'predicate', 'ruleUpdate_predicate' - The ID of the @Predicate@ (such as an @IPSet@) that you want to add to a
-- @Rule@.
newRuleUpdate ::
  -- | 'action'
  ChangeAction ->
  -- | 'predicate'
  Predicate ->
  RuleUpdate
newRuleUpdate pAction_ pPredicate_ =
  RuleUpdate'
    { action = pAction_,
      predicate = pPredicate_
    }

-- | Specify @INSERT@ to add a @Predicate@ to a @Rule@. Use @DELETE@ to
-- remove a @Predicate@ from a @Rule@.
ruleUpdate_action :: Lens.Lens' RuleUpdate ChangeAction
ruleUpdate_action = Lens.lens (\RuleUpdate' {action} -> action) (\s@RuleUpdate' {} a -> s {action = a} :: RuleUpdate)

-- | The ID of the @Predicate@ (such as an @IPSet@) that you want to add to a
-- @Rule@.
ruleUpdate_predicate :: Lens.Lens' RuleUpdate Predicate
ruleUpdate_predicate = Lens.lens (\RuleUpdate' {predicate} -> predicate) (\s@RuleUpdate' {} a -> s {predicate = a} :: RuleUpdate)

instance Prelude.Hashable RuleUpdate where
  hashWithSalt _salt RuleUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` predicate

instance Prelude.NFData RuleUpdate where
  rnf RuleUpdate' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf predicate

instance Data.ToJSON RuleUpdate where
  toJSON RuleUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Action" Data..= action),
            Prelude.Just ("Predicate" Data..= predicate)
          ]
      )
