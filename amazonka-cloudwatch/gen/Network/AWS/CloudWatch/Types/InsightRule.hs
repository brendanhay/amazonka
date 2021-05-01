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
-- Module      : Network.AWS.CloudWatch.Types.InsightRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.InsightRule where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This structure contains the definition for a Contributor Insights rule.
--
-- /See:/ 'newInsightRule' smart constructor.
data InsightRule = InsightRule'
  { -- | The name of the rule.
    name :: Prelude.Text,
    -- | Indicates whether the rule is enabled or disabled.
    state :: Prelude.Text,
    -- | For rules that you create, this is always
    -- @{\"Name\": \"CloudWatchLogRule\", \"Version\": 1}@. For built-in rules,
    -- this is @{\"Name\": \"ServiceLogRule\", \"Version\": 1}@
    schema :: Prelude.Text,
    -- | The definition of the rule, as a JSON object. The definition contains
    -- the keywords used to define contributors, the value to aggregate on if
    -- this rule returns a sum instead of a count, and the filters. For details
    -- on the valid syntax, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights-RuleSyntax.html Contributor Insights Rule Syntax>.
    definition :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'state'
  Prelude.Text ->
  -- | 'schema'
  Prelude.Text ->
  -- | 'definition'
  Prelude.Text ->
  InsightRule
newInsightRule pName_ pState_ pSchema_ pDefinition_ =
  InsightRule'
    { name = pName_,
      state = pState_,
      schema = pSchema_,
      definition = pDefinition_
    }

-- | The name of the rule.
insightRule_name :: Lens.Lens' InsightRule Prelude.Text
insightRule_name = Lens.lens (\InsightRule' {name} -> name) (\s@InsightRule' {} a -> s {name = a} :: InsightRule)

-- | Indicates whether the rule is enabled or disabled.
insightRule_state :: Lens.Lens' InsightRule Prelude.Text
insightRule_state = Lens.lens (\InsightRule' {state} -> state) (\s@InsightRule' {} a -> s {state = a} :: InsightRule)

-- | For rules that you create, this is always
-- @{\"Name\": \"CloudWatchLogRule\", \"Version\": 1}@. For built-in rules,
-- this is @{\"Name\": \"ServiceLogRule\", \"Version\": 1}@
insightRule_schema :: Lens.Lens' InsightRule Prelude.Text
insightRule_schema = Lens.lens (\InsightRule' {schema} -> schema) (\s@InsightRule' {} a -> s {schema = a} :: InsightRule)

-- | The definition of the rule, as a JSON object. The definition contains
-- the keywords used to define contributors, the value to aggregate on if
-- this rule returns a sum instead of a count, and the filters. For details
-- on the valid syntax, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights-RuleSyntax.html Contributor Insights Rule Syntax>.
insightRule_definition :: Lens.Lens' InsightRule Prelude.Text
insightRule_definition = Lens.lens (\InsightRule' {definition} -> definition) (\s@InsightRule' {} a -> s {definition = a} :: InsightRule)

instance Prelude.FromXML InsightRule where
  parseXML x =
    InsightRule'
      Prelude.<$> (x Prelude..@ "Name")
      Prelude.<*> (x Prelude..@ "State")
      Prelude.<*> (x Prelude..@ "Schema")
      Prelude.<*> (x Prelude..@ "Definition")

instance Prelude.Hashable InsightRule

instance Prelude.NFData InsightRule
