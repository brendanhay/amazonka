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
-- Module      : Network.AWS.CloudWatch.Types.InsightRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.InsightRule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | This structure contains the definition for a Contributor Insights rule.
--
-- /See:/ 'newInsightRule' smart constructor.
data InsightRule = InsightRule'
  { -- | The name of the rule.
    name :: Core.Text,
    -- | Indicates whether the rule is enabled or disabled.
    state :: Core.Text,
    -- | For rules that you create, this is always
    -- @{\"Name\": \"CloudWatchLogRule\", \"Version\": 1}@. For built-in rules,
    -- this is @{\"Name\": \"ServiceLogRule\", \"Version\": 1}@
    schema :: Core.Text,
    -- | The definition of the rule, as a JSON object. The definition contains
    -- the keywords used to define contributors, the value to aggregate on if
    -- this rule returns a sum instead of a count, and the filters. For details
    -- on the valid syntax, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights-RuleSyntax.html Contributor Insights Rule Syntax>.
    definition :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InsightRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'insightRule_name' - The name of the rule.
--
-- 'state', 'insightRule_state' - Indicates whether the rule is enabled or disabled.
--
-- 'schema', 'insightRule_schema' - For rules that you create, this is always
-- @{\"Name\": \"CloudWatchLogRule\", \"Version\": 1}@. For built-in rules,
-- this is @{\"Name\": \"ServiceLogRule\", \"Version\": 1}@
--
-- 'definition', 'insightRule_definition' - The definition of the rule, as a JSON object. The definition contains
-- the keywords used to define contributors, the value to aggregate on if
-- this rule returns a sum instead of a count, and the filters. For details
-- on the valid syntax, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights-RuleSyntax.html Contributor Insights Rule Syntax>.
newInsightRule ::
  -- | 'name'
  Core.Text ->
  -- | 'state'
  Core.Text ->
  -- | 'schema'
  Core.Text ->
  -- | 'definition'
  Core.Text ->
  InsightRule
newInsightRule pName_ pState_ pSchema_ pDefinition_ =
  InsightRule'
    { name = pName_,
      state = pState_,
      schema = pSchema_,
      definition = pDefinition_
    }

-- | The name of the rule.
insightRule_name :: Lens.Lens' InsightRule Core.Text
insightRule_name = Lens.lens (\InsightRule' {name} -> name) (\s@InsightRule' {} a -> s {name = a} :: InsightRule)

-- | Indicates whether the rule is enabled or disabled.
insightRule_state :: Lens.Lens' InsightRule Core.Text
insightRule_state = Lens.lens (\InsightRule' {state} -> state) (\s@InsightRule' {} a -> s {state = a} :: InsightRule)

-- | For rules that you create, this is always
-- @{\"Name\": \"CloudWatchLogRule\", \"Version\": 1}@. For built-in rules,
-- this is @{\"Name\": \"ServiceLogRule\", \"Version\": 1}@
insightRule_schema :: Lens.Lens' InsightRule Core.Text
insightRule_schema = Lens.lens (\InsightRule' {schema} -> schema) (\s@InsightRule' {} a -> s {schema = a} :: InsightRule)

-- | The definition of the rule, as a JSON object. The definition contains
-- the keywords used to define contributors, the value to aggregate on if
-- this rule returns a sum instead of a count, and the filters. For details
-- on the valid syntax, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights-RuleSyntax.html Contributor Insights Rule Syntax>.
insightRule_definition :: Lens.Lens' InsightRule Core.Text
insightRule_definition = Lens.lens (\InsightRule' {definition} -> definition) (\s@InsightRule' {} a -> s {definition = a} :: InsightRule)

instance Core.FromXML InsightRule where
  parseXML x =
    InsightRule'
      Core.<$> (x Core..@ "Name")
      Core.<*> (x Core..@ "State")
      Core.<*> (x Core..@ "Schema")
      Core.<*> (x Core..@ "Definition")

instance Core.Hashable InsightRule

instance Core.NFData InsightRule
