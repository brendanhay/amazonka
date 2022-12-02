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
-- Module      : Amazonka.CloudWatch.Types.InsightRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.InsightRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This structure contains the definition for a Contributor Insights rule.
-- For more information about this rule, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights.html Using Constributor Insights to analyze high-cardinality data>
-- in the /Amazon CloudWatch User Guide/.
--
-- /See:/ 'newInsightRule' smart constructor.
data InsightRule = InsightRule'
  { -- | An optional built-in rule that Amazon Web Services manages.
    managedRule :: Prelude.Maybe Prelude.Bool,
    -- | The name of the rule.
    name :: Prelude.Text,
    -- | Indicates whether the rule is enabled or disabled.
    state :: Prelude.Text,
    -- | For rules that you create, this is always
    -- @{\"Name\": \"CloudWatchLogRule\", \"Version\": 1}@. For managed rules,
    -- this is @{\"Name\": \"ServiceLogRule\", \"Version\": 1}@
    schema :: Prelude.Text,
    -- | The definition of the rule, as a JSON object. The definition contains
    -- the keywords used to define contributors, the value to aggregate on if
    -- this rule returns a sum instead of a count, and the filters. For details
    -- on the valid syntax, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/ContributorInsights-RuleSyntax.html Contributor Insights Rule Syntax>.
    definition :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InsightRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managedRule', 'insightRule_managedRule' - An optional built-in rule that Amazon Web Services manages.
--
-- 'name', 'insightRule_name' - The name of the rule.
--
-- 'state', 'insightRule_state' - Indicates whether the rule is enabled or disabled.
--
-- 'schema', 'insightRule_schema' - For rules that you create, this is always
-- @{\"Name\": \"CloudWatchLogRule\", \"Version\": 1}@. For managed rules,
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
    { managedRule = Prelude.Nothing,
      name = pName_,
      state = pState_,
      schema = pSchema_,
      definition = pDefinition_
    }

-- | An optional built-in rule that Amazon Web Services manages.
insightRule_managedRule :: Lens.Lens' InsightRule (Prelude.Maybe Prelude.Bool)
insightRule_managedRule = Lens.lens (\InsightRule' {managedRule} -> managedRule) (\s@InsightRule' {} a -> s {managedRule = a} :: InsightRule)

-- | The name of the rule.
insightRule_name :: Lens.Lens' InsightRule Prelude.Text
insightRule_name = Lens.lens (\InsightRule' {name} -> name) (\s@InsightRule' {} a -> s {name = a} :: InsightRule)

-- | Indicates whether the rule is enabled or disabled.
insightRule_state :: Lens.Lens' InsightRule Prelude.Text
insightRule_state = Lens.lens (\InsightRule' {state} -> state) (\s@InsightRule' {} a -> s {state = a} :: InsightRule)

-- | For rules that you create, this is always
-- @{\"Name\": \"CloudWatchLogRule\", \"Version\": 1}@. For managed rules,
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

instance Data.FromXML InsightRule where
  parseXML x =
    InsightRule'
      Prelude.<$> (x Data..@? "ManagedRule")
      Prelude.<*> (x Data..@ "Name")
      Prelude.<*> (x Data..@ "State")
      Prelude.<*> (x Data..@ "Schema")
      Prelude.<*> (x Data..@ "Definition")

instance Prelude.Hashable InsightRule where
  hashWithSalt _salt InsightRule' {..} =
    _salt `Prelude.hashWithSalt` managedRule
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` definition

instance Prelude.NFData InsightRule where
  rnf InsightRule' {..} =
    Prelude.rnf managedRule
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf definition
