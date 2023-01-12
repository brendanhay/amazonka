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
-- Module      : Amazonka.Glue.Types.DataQualityRulesetListDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DataQualityRulesetListDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.DataQualityTargetTable
import qualified Amazonka.Prelude as Prelude

-- | Describes a data quality ruleset returned by @GetDataQualityRuleset@.
--
-- /See:/ 'newDataQualityRulesetListDetails' smart constructor.
data DataQualityRulesetListDetails = DataQualityRulesetListDetails'
  { -- | The date and time the data quality ruleset was created.
    createdOn :: Prelude.Maybe Data.POSIX,
    -- | A description of the data quality ruleset.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date and time the data quality ruleset was last modified.
    lastModifiedOn :: Prelude.Maybe Data.POSIX,
    -- | The name of the data quality ruleset.
    name :: Prelude.Maybe Prelude.Text,
    -- | When a ruleset was created from a recommendation run, this run ID is
    -- generated to link the two together.
    recommendationRunId :: Prelude.Maybe Prelude.Text,
    -- | The number of rules in the ruleset.
    ruleCount :: Prelude.Maybe Prelude.Int,
    -- | An object representing an Glue table.
    targetTable :: Prelude.Maybe DataQualityTargetTable
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataQualityRulesetListDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdOn', 'dataQualityRulesetListDetails_createdOn' - The date and time the data quality ruleset was created.
--
-- 'description', 'dataQualityRulesetListDetails_description' - A description of the data quality ruleset.
--
-- 'lastModifiedOn', 'dataQualityRulesetListDetails_lastModifiedOn' - The date and time the data quality ruleset was last modified.
--
-- 'name', 'dataQualityRulesetListDetails_name' - The name of the data quality ruleset.
--
-- 'recommendationRunId', 'dataQualityRulesetListDetails_recommendationRunId' - When a ruleset was created from a recommendation run, this run ID is
-- generated to link the two together.
--
-- 'ruleCount', 'dataQualityRulesetListDetails_ruleCount' - The number of rules in the ruleset.
--
-- 'targetTable', 'dataQualityRulesetListDetails_targetTable' - An object representing an Glue table.
newDataQualityRulesetListDetails ::
  DataQualityRulesetListDetails
newDataQualityRulesetListDetails =
  DataQualityRulesetListDetails'
    { createdOn =
        Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedOn = Prelude.Nothing,
      name = Prelude.Nothing,
      recommendationRunId = Prelude.Nothing,
      ruleCount = Prelude.Nothing,
      targetTable = Prelude.Nothing
    }

-- | The date and time the data quality ruleset was created.
dataQualityRulesetListDetails_createdOn :: Lens.Lens' DataQualityRulesetListDetails (Prelude.Maybe Prelude.UTCTime)
dataQualityRulesetListDetails_createdOn = Lens.lens (\DataQualityRulesetListDetails' {createdOn} -> createdOn) (\s@DataQualityRulesetListDetails' {} a -> s {createdOn = a} :: DataQualityRulesetListDetails) Prelude.. Lens.mapping Data._Time

-- | A description of the data quality ruleset.
dataQualityRulesetListDetails_description :: Lens.Lens' DataQualityRulesetListDetails (Prelude.Maybe Prelude.Text)
dataQualityRulesetListDetails_description = Lens.lens (\DataQualityRulesetListDetails' {description} -> description) (\s@DataQualityRulesetListDetails' {} a -> s {description = a} :: DataQualityRulesetListDetails)

-- | The date and time the data quality ruleset was last modified.
dataQualityRulesetListDetails_lastModifiedOn :: Lens.Lens' DataQualityRulesetListDetails (Prelude.Maybe Prelude.UTCTime)
dataQualityRulesetListDetails_lastModifiedOn = Lens.lens (\DataQualityRulesetListDetails' {lastModifiedOn} -> lastModifiedOn) (\s@DataQualityRulesetListDetails' {} a -> s {lastModifiedOn = a} :: DataQualityRulesetListDetails) Prelude.. Lens.mapping Data._Time

-- | The name of the data quality ruleset.
dataQualityRulesetListDetails_name :: Lens.Lens' DataQualityRulesetListDetails (Prelude.Maybe Prelude.Text)
dataQualityRulesetListDetails_name = Lens.lens (\DataQualityRulesetListDetails' {name} -> name) (\s@DataQualityRulesetListDetails' {} a -> s {name = a} :: DataQualityRulesetListDetails)

-- | When a ruleset was created from a recommendation run, this run ID is
-- generated to link the two together.
dataQualityRulesetListDetails_recommendationRunId :: Lens.Lens' DataQualityRulesetListDetails (Prelude.Maybe Prelude.Text)
dataQualityRulesetListDetails_recommendationRunId = Lens.lens (\DataQualityRulesetListDetails' {recommendationRunId} -> recommendationRunId) (\s@DataQualityRulesetListDetails' {} a -> s {recommendationRunId = a} :: DataQualityRulesetListDetails)

-- | The number of rules in the ruleset.
dataQualityRulesetListDetails_ruleCount :: Lens.Lens' DataQualityRulesetListDetails (Prelude.Maybe Prelude.Int)
dataQualityRulesetListDetails_ruleCount = Lens.lens (\DataQualityRulesetListDetails' {ruleCount} -> ruleCount) (\s@DataQualityRulesetListDetails' {} a -> s {ruleCount = a} :: DataQualityRulesetListDetails)

-- | An object representing an Glue table.
dataQualityRulesetListDetails_targetTable :: Lens.Lens' DataQualityRulesetListDetails (Prelude.Maybe DataQualityTargetTable)
dataQualityRulesetListDetails_targetTable = Lens.lens (\DataQualityRulesetListDetails' {targetTable} -> targetTable) (\s@DataQualityRulesetListDetails' {} a -> s {targetTable = a} :: DataQualityRulesetListDetails)

instance Data.FromJSON DataQualityRulesetListDetails where
  parseJSON =
    Data.withObject
      "DataQualityRulesetListDetails"
      ( \x ->
          DataQualityRulesetListDetails'
            Prelude.<$> (x Data..:? "CreatedOn")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LastModifiedOn")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "RecommendationRunId")
            Prelude.<*> (x Data..:? "RuleCount")
            Prelude.<*> (x Data..:? "TargetTable")
      )

instance
  Prelude.Hashable
    DataQualityRulesetListDetails
  where
  hashWithSalt _salt DataQualityRulesetListDetails' {..} =
    _salt `Prelude.hashWithSalt` createdOn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastModifiedOn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` recommendationRunId
      `Prelude.hashWithSalt` ruleCount
      `Prelude.hashWithSalt` targetTable

instance Prelude.NFData DataQualityRulesetListDetails where
  rnf DataQualityRulesetListDetails' {..} =
    Prelude.rnf createdOn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedOn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf recommendationRunId
      `Prelude.seq` Prelude.rnf ruleCount
      `Prelude.seq` Prelude.rnf targetTable
