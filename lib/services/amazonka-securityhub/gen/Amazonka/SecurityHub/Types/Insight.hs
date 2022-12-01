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
-- Module      : Amazonka.SecurityHub.Types.Insight
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.Insight where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsSecurityFindingFilters

-- | Contains information about a Security Hub insight.
--
-- /See:/ 'newInsight' smart constructor.
data Insight = Insight'
  { -- | The ARN of a Security Hub insight.
    insightArn :: Prelude.Text,
    -- | The name of a Security Hub insight.
    name :: Prelude.Text,
    -- | One or more attributes used to filter the findings included in the
    -- insight. The insight only includes findings that match the criteria
    -- defined in the filters.
    filters :: AwsSecurityFindingFilters,
    -- | The grouping attribute for the insight\'s findings. Indicates how to
    -- group the matching findings, and identifies the type of item that the
    -- insight applies to. For example, if an insight is grouped by resource
    -- identifier, then the insight produces a list of resource identifiers.
    groupByAttribute :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Insight' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insightArn', 'insight_insightArn' - The ARN of a Security Hub insight.
--
-- 'name', 'insight_name' - The name of a Security Hub insight.
--
-- 'filters', 'insight_filters' - One or more attributes used to filter the findings included in the
-- insight. The insight only includes findings that match the criteria
-- defined in the filters.
--
-- 'groupByAttribute', 'insight_groupByAttribute' - The grouping attribute for the insight\'s findings. Indicates how to
-- group the matching findings, and identifies the type of item that the
-- insight applies to. For example, if an insight is grouped by resource
-- identifier, then the insight produces a list of resource identifiers.
newInsight ::
  -- | 'insightArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'filters'
  AwsSecurityFindingFilters ->
  -- | 'groupByAttribute'
  Prelude.Text ->
  Insight
newInsight
  pInsightArn_
  pName_
  pFilters_
  pGroupByAttribute_ =
    Insight'
      { insightArn = pInsightArn_,
        name = pName_,
        filters = pFilters_,
        groupByAttribute = pGroupByAttribute_
      }

-- | The ARN of a Security Hub insight.
insight_insightArn :: Lens.Lens' Insight Prelude.Text
insight_insightArn = Lens.lens (\Insight' {insightArn} -> insightArn) (\s@Insight' {} a -> s {insightArn = a} :: Insight)

-- | The name of a Security Hub insight.
insight_name :: Lens.Lens' Insight Prelude.Text
insight_name = Lens.lens (\Insight' {name} -> name) (\s@Insight' {} a -> s {name = a} :: Insight)

-- | One or more attributes used to filter the findings included in the
-- insight. The insight only includes findings that match the criteria
-- defined in the filters.
insight_filters :: Lens.Lens' Insight AwsSecurityFindingFilters
insight_filters = Lens.lens (\Insight' {filters} -> filters) (\s@Insight' {} a -> s {filters = a} :: Insight)

-- | The grouping attribute for the insight\'s findings. Indicates how to
-- group the matching findings, and identifies the type of item that the
-- insight applies to. For example, if an insight is grouped by resource
-- identifier, then the insight produces a list of resource identifiers.
insight_groupByAttribute :: Lens.Lens' Insight Prelude.Text
insight_groupByAttribute = Lens.lens (\Insight' {groupByAttribute} -> groupByAttribute) (\s@Insight' {} a -> s {groupByAttribute = a} :: Insight)

instance Core.FromJSON Insight where
  parseJSON =
    Core.withObject
      "Insight"
      ( \x ->
          Insight'
            Prelude.<$> (x Core..: "InsightArn")
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> (x Core..: "Filters")
            Prelude.<*> (x Core..: "GroupByAttribute")
      )

instance Prelude.Hashable Insight where
  hashWithSalt _salt Insight' {..} =
    _salt `Prelude.hashWithSalt` insightArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` groupByAttribute

instance Prelude.NFData Insight where
  rnf Insight' {..} =
    Prelude.rnf insightArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf groupByAttribute
