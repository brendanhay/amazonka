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
-- Module      : Network.AWS.WAFRegional.Types.ExcludedRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.ExcludedRule where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- The rule to exclude from a rule group. This is applicable only when the
-- @ActivatedRule@ refers to a @RuleGroup@. The rule must belong to the
-- @RuleGroup@ that is specified by the @ActivatedRule@.
--
-- /See:/ 'newExcludedRule' smart constructor.
data ExcludedRule = ExcludedRule'
  { -- | The unique identifier for the rule to exclude from the rule group.
    ruleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ExcludedRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleId', 'excludedRule_ruleId' - The unique identifier for the rule to exclude from the rule group.
newExcludedRule ::
  -- | 'ruleId'
  Prelude.Text ->
  ExcludedRule
newExcludedRule pRuleId_ =
  ExcludedRule' {ruleId = pRuleId_}

-- | The unique identifier for the rule to exclude from the rule group.
excludedRule_ruleId :: Lens.Lens' ExcludedRule Prelude.Text
excludedRule_ruleId = Lens.lens (\ExcludedRule' {ruleId} -> ruleId) (\s@ExcludedRule' {} a -> s {ruleId = a} :: ExcludedRule)

instance Prelude.FromJSON ExcludedRule where
  parseJSON =
    Prelude.withObject
      "ExcludedRule"
      ( \x ->
          ExcludedRule' Prelude.<$> (x Prelude..: "RuleId")
      )

instance Prelude.Hashable ExcludedRule

instance Prelude.NFData ExcludedRule

instance Prelude.ToJSON ExcludedRule where
  toJSON ExcludedRule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("RuleId" Prelude..= ruleId)]
      )
