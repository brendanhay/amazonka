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
-- Module      : Network.AWS.WAFRegional.Types.RuleGroupSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.RuleGroupSummary where

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
-- @RuleGroup@.
--
-- /See:/ 'newRuleGroupSummary' smart constructor.
data RuleGroupSummary = RuleGroupSummary'
  { -- | A unique identifier for a @RuleGroup@. You use @RuleGroupId@ to get more
    -- information about a @RuleGroup@ (see GetRuleGroup), update a @RuleGroup@
    -- (see UpdateRuleGroup), insert a @RuleGroup@ into a @WebACL@ or delete
    -- one from a @WebACL@ (see UpdateWebACL), or delete a @RuleGroup@ from AWS
    -- WAF (see DeleteRuleGroup).
    --
    -- @RuleGroupId@ is returned by CreateRuleGroup and by ListRuleGroups.
    ruleGroupId :: Prelude.Text,
    -- | A friendly name or description of the RuleGroup. You can\'t change the
    -- name of a @RuleGroup@ after you create it.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RuleGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleGroupId', 'ruleGroupSummary_ruleGroupId' - A unique identifier for a @RuleGroup@. You use @RuleGroupId@ to get more
-- information about a @RuleGroup@ (see GetRuleGroup), update a @RuleGroup@
-- (see UpdateRuleGroup), insert a @RuleGroup@ into a @WebACL@ or delete
-- one from a @WebACL@ (see UpdateWebACL), or delete a @RuleGroup@ from AWS
-- WAF (see DeleteRuleGroup).
--
-- @RuleGroupId@ is returned by CreateRuleGroup and by ListRuleGroups.
--
-- 'name', 'ruleGroupSummary_name' - A friendly name or description of the RuleGroup. You can\'t change the
-- name of a @RuleGroup@ after you create it.
newRuleGroupSummary ::
  -- | 'ruleGroupId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  RuleGroupSummary
newRuleGroupSummary pRuleGroupId_ pName_ =
  RuleGroupSummary'
    { ruleGroupId = pRuleGroupId_,
      name = pName_
    }

-- | A unique identifier for a @RuleGroup@. You use @RuleGroupId@ to get more
-- information about a @RuleGroup@ (see GetRuleGroup), update a @RuleGroup@
-- (see UpdateRuleGroup), insert a @RuleGroup@ into a @WebACL@ or delete
-- one from a @WebACL@ (see UpdateWebACL), or delete a @RuleGroup@ from AWS
-- WAF (see DeleteRuleGroup).
--
-- @RuleGroupId@ is returned by CreateRuleGroup and by ListRuleGroups.
ruleGroupSummary_ruleGroupId :: Lens.Lens' RuleGroupSummary Prelude.Text
ruleGroupSummary_ruleGroupId = Lens.lens (\RuleGroupSummary' {ruleGroupId} -> ruleGroupId) (\s@RuleGroupSummary' {} a -> s {ruleGroupId = a} :: RuleGroupSummary)

-- | A friendly name or description of the RuleGroup. You can\'t change the
-- name of a @RuleGroup@ after you create it.
ruleGroupSummary_name :: Lens.Lens' RuleGroupSummary Prelude.Text
ruleGroupSummary_name = Lens.lens (\RuleGroupSummary' {name} -> name) (\s@RuleGroupSummary' {} a -> s {name = a} :: RuleGroupSummary)

instance Prelude.FromJSON RuleGroupSummary where
  parseJSON =
    Prelude.withObject
      "RuleGroupSummary"
      ( \x ->
          RuleGroupSummary'
            Prelude.<$> (x Prelude..: "RuleGroupId")
            Prelude.<*> (x Prelude..: "Name")
      )

instance Prelude.Hashable RuleGroupSummary

instance Prelude.NFData RuleGroupSummary
