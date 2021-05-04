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
-- Module      : Network.AWS.WAFRegional.Types.RuleGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.RuleGroup where

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
-- A collection of predefined rules that you can add to a web ACL.
--
-- Rule groups are subject to the following limits:
--
-- -   Three rule groups per account. You can request an increase to this
--     limit by contacting customer support.
--
-- -   One rule group per web ACL.
--
-- -   Ten rules per rule group.
--
-- /See:/ 'newRuleGroup' smart constructor.
data RuleGroup = RuleGroup'
  { -- | A friendly name or description for the metrics for this @RuleGroup@. The
    -- name can contain only alphanumeric characters (A-Z, a-z, 0-9), with
    -- maximum length 128 and minimum length one. It can\'t contain whitespace
    -- or metric names reserved for AWS WAF, including \"All\" and
    -- \"Default_Action.\" You can\'t change the name of the metric after you
    -- create the @RuleGroup@.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The friendly name or description for the @RuleGroup@. You can\'t change
    -- the name of a @RuleGroup@ after you create it.
    name :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a @RuleGroup@. You use @RuleGroupId@ to get more
    -- information about a @RuleGroup@ (see GetRuleGroup), update a @RuleGroup@
    -- (see UpdateRuleGroup), insert a @RuleGroup@ into a @WebACL@ or delete a
    -- one from a @WebACL@ (see UpdateWebACL), or delete a @RuleGroup@ from AWS
    -- WAF (see DeleteRuleGroup).
    --
    -- @RuleGroupId@ is returned by CreateRuleGroup and by ListRuleGroups.
    ruleGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricName', 'ruleGroup_metricName' - A friendly name or description for the metrics for this @RuleGroup@. The
-- name can contain only alphanumeric characters (A-Z, a-z, 0-9), with
-- maximum length 128 and minimum length one. It can\'t contain whitespace
-- or metric names reserved for AWS WAF, including \"All\" and
-- \"Default_Action.\" You can\'t change the name of the metric after you
-- create the @RuleGroup@.
--
-- 'name', 'ruleGroup_name' - The friendly name or description for the @RuleGroup@. You can\'t change
-- the name of a @RuleGroup@ after you create it.
--
-- 'ruleGroupId', 'ruleGroup_ruleGroupId' - A unique identifier for a @RuleGroup@. You use @RuleGroupId@ to get more
-- information about a @RuleGroup@ (see GetRuleGroup), update a @RuleGroup@
-- (see UpdateRuleGroup), insert a @RuleGroup@ into a @WebACL@ or delete a
-- one from a @WebACL@ (see UpdateWebACL), or delete a @RuleGroup@ from AWS
-- WAF (see DeleteRuleGroup).
--
-- @RuleGroupId@ is returned by CreateRuleGroup and by ListRuleGroups.
newRuleGroup ::
  -- | 'ruleGroupId'
  Prelude.Text ->
  RuleGroup
newRuleGroup pRuleGroupId_ =
  RuleGroup'
    { metricName = Prelude.Nothing,
      name = Prelude.Nothing,
      ruleGroupId = pRuleGroupId_
    }

-- | A friendly name or description for the metrics for this @RuleGroup@. The
-- name can contain only alphanumeric characters (A-Z, a-z, 0-9), with
-- maximum length 128 and minimum length one. It can\'t contain whitespace
-- or metric names reserved for AWS WAF, including \"All\" and
-- \"Default_Action.\" You can\'t change the name of the metric after you
-- create the @RuleGroup@.
ruleGroup_metricName :: Lens.Lens' RuleGroup (Prelude.Maybe Prelude.Text)
ruleGroup_metricName = Lens.lens (\RuleGroup' {metricName} -> metricName) (\s@RuleGroup' {} a -> s {metricName = a} :: RuleGroup)

-- | The friendly name or description for the @RuleGroup@. You can\'t change
-- the name of a @RuleGroup@ after you create it.
ruleGroup_name :: Lens.Lens' RuleGroup (Prelude.Maybe Prelude.Text)
ruleGroup_name = Lens.lens (\RuleGroup' {name} -> name) (\s@RuleGroup' {} a -> s {name = a} :: RuleGroup)

-- | A unique identifier for a @RuleGroup@. You use @RuleGroupId@ to get more
-- information about a @RuleGroup@ (see GetRuleGroup), update a @RuleGroup@
-- (see UpdateRuleGroup), insert a @RuleGroup@ into a @WebACL@ or delete a
-- one from a @WebACL@ (see UpdateWebACL), or delete a @RuleGroup@ from AWS
-- WAF (see DeleteRuleGroup).
--
-- @RuleGroupId@ is returned by CreateRuleGroup and by ListRuleGroups.
ruleGroup_ruleGroupId :: Lens.Lens' RuleGroup Prelude.Text
ruleGroup_ruleGroupId = Lens.lens (\RuleGroup' {ruleGroupId} -> ruleGroupId) (\s@RuleGroup' {} a -> s {ruleGroupId = a} :: RuleGroup)

instance Prelude.FromJSON RuleGroup where
  parseJSON =
    Prelude.withObject
      "RuleGroup"
      ( \x ->
          RuleGroup'
            Prelude.<$> (x Prelude..:? "MetricName")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..: "RuleGroupId")
      )

instance Prelude.Hashable RuleGroup

instance Prelude.NFData RuleGroup
