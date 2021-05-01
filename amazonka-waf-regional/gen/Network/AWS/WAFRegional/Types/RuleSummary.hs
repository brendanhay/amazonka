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
-- Module      : Network.AWS.WAFRegional.Types.RuleSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.RuleSummary where

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
-- Contains the identifier and the friendly name or description of the
-- @Rule@.
--
-- /See:/ 'newRuleSummary' smart constructor.
data RuleSummary = RuleSummary'
  { -- | A unique identifier for a @Rule@. You use @RuleId@ to get more
    -- information about a @Rule@ (see GetRule), update a @Rule@ (see
    -- UpdateRule), insert a @Rule@ into a @WebACL@ or delete one from a
    -- @WebACL@ (see UpdateWebACL), or delete a @Rule@ from AWS WAF (see
    -- DeleteRule).
    --
    -- @RuleId@ is returned by CreateRule and by ListRules.
    ruleId :: Prelude.Text,
    -- | A friendly name or description of the Rule. You can\'t change the name
    -- of a @Rule@ after you create it.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RuleSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleId', 'ruleSummary_ruleId' - A unique identifier for a @Rule@. You use @RuleId@ to get more
-- information about a @Rule@ (see GetRule), update a @Rule@ (see
-- UpdateRule), insert a @Rule@ into a @WebACL@ or delete one from a
-- @WebACL@ (see UpdateWebACL), or delete a @Rule@ from AWS WAF (see
-- DeleteRule).
--
-- @RuleId@ is returned by CreateRule and by ListRules.
--
-- 'name', 'ruleSummary_name' - A friendly name or description of the Rule. You can\'t change the name
-- of a @Rule@ after you create it.
newRuleSummary ::
  -- | 'ruleId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  RuleSummary
newRuleSummary pRuleId_ pName_ =
  RuleSummary' {ruleId = pRuleId_, name = pName_}

-- | A unique identifier for a @Rule@. You use @RuleId@ to get more
-- information about a @Rule@ (see GetRule), update a @Rule@ (see
-- UpdateRule), insert a @Rule@ into a @WebACL@ or delete one from a
-- @WebACL@ (see UpdateWebACL), or delete a @Rule@ from AWS WAF (see
-- DeleteRule).
--
-- @RuleId@ is returned by CreateRule and by ListRules.
ruleSummary_ruleId :: Lens.Lens' RuleSummary Prelude.Text
ruleSummary_ruleId = Lens.lens (\RuleSummary' {ruleId} -> ruleId) (\s@RuleSummary' {} a -> s {ruleId = a} :: RuleSummary)

-- | A friendly name or description of the Rule. You can\'t change the name
-- of a @Rule@ after you create it.
ruleSummary_name :: Lens.Lens' RuleSummary Prelude.Text
ruleSummary_name = Lens.lens (\RuleSummary' {name} -> name) (\s@RuleSummary' {} a -> s {name = a} :: RuleSummary)

instance Prelude.FromJSON RuleSummary where
  parseJSON =
    Prelude.withObject
      "RuleSummary"
      ( \x ->
          RuleSummary'
            Prelude.<$> (x Prelude..: "RuleId")
            Prelude.<*> (x Prelude..: "Name")
      )

instance Prelude.Hashable RuleSummary

instance Prelude.NFData RuleSummary
