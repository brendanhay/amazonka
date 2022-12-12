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
-- Module      : Amazonka.Glue.Types.DataQualityRulesetFilterCriteria
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DataQualityRulesetFilterCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.DataQualityTargetTable
import qualified Amazonka.Prelude as Prelude

-- | The criteria used to filter data quality rulesets.
--
-- /See:/ 'newDataQualityRulesetFilterCriteria' smart constructor.
data DataQualityRulesetFilterCriteria = DataQualityRulesetFilterCriteria'
  { -- | Filter on rulesets created after this date.
    createdAfter :: Prelude.Maybe Data.POSIX,
    -- | Filter on rulesets created before this date.
    createdBefore :: Prelude.Maybe Data.POSIX,
    -- | The description of the ruleset filter criteria.
    description :: Prelude.Maybe Prelude.Text,
    -- | Filter on rulesets last modified after this date.
    lastModifiedAfter :: Prelude.Maybe Data.POSIX,
    -- | Filter on rulesets last modified before this date.
    lastModifiedBefore :: Prelude.Maybe Data.POSIX,
    -- | The name of the ruleset filter criteria.
    name :: Prelude.Maybe Prelude.Text,
    -- | The name and database name of the target table.
    targetTable :: Prelude.Maybe DataQualityTargetTable
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataQualityRulesetFilterCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAfter', 'dataQualityRulesetFilterCriteria_createdAfter' - Filter on rulesets created after this date.
--
-- 'createdBefore', 'dataQualityRulesetFilterCriteria_createdBefore' - Filter on rulesets created before this date.
--
-- 'description', 'dataQualityRulesetFilterCriteria_description' - The description of the ruleset filter criteria.
--
-- 'lastModifiedAfter', 'dataQualityRulesetFilterCriteria_lastModifiedAfter' - Filter on rulesets last modified after this date.
--
-- 'lastModifiedBefore', 'dataQualityRulesetFilterCriteria_lastModifiedBefore' - Filter on rulesets last modified before this date.
--
-- 'name', 'dataQualityRulesetFilterCriteria_name' - The name of the ruleset filter criteria.
--
-- 'targetTable', 'dataQualityRulesetFilterCriteria_targetTable' - The name and database name of the target table.
newDataQualityRulesetFilterCriteria ::
  DataQualityRulesetFilterCriteria
newDataQualityRulesetFilterCriteria =
  DataQualityRulesetFilterCriteria'
    { createdAfter =
        Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedAfter = Prelude.Nothing,
      lastModifiedBefore = Prelude.Nothing,
      name = Prelude.Nothing,
      targetTable = Prelude.Nothing
    }

-- | Filter on rulesets created after this date.
dataQualityRulesetFilterCriteria_createdAfter :: Lens.Lens' DataQualityRulesetFilterCriteria (Prelude.Maybe Prelude.UTCTime)
dataQualityRulesetFilterCriteria_createdAfter = Lens.lens (\DataQualityRulesetFilterCriteria' {createdAfter} -> createdAfter) (\s@DataQualityRulesetFilterCriteria' {} a -> s {createdAfter = a} :: DataQualityRulesetFilterCriteria) Prelude.. Lens.mapping Data._Time

-- | Filter on rulesets created before this date.
dataQualityRulesetFilterCriteria_createdBefore :: Lens.Lens' DataQualityRulesetFilterCriteria (Prelude.Maybe Prelude.UTCTime)
dataQualityRulesetFilterCriteria_createdBefore = Lens.lens (\DataQualityRulesetFilterCriteria' {createdBefore} -> createdBefore) (\s@DataQualityRulesetFilterCriteria' {} a -> s {createdBefore = a} :: DataQualityRulesetFilterCriteria) Prelude.. Lens.mapping Data._Time

-- | The description of the ruleset filter criteria.
dataQualityRulesetFilterCriteria_description :: Lens.Lens' DataQualityRulesetFilterCriteria (Prelude.Maybe Prelude.Text)
dataQualityRulesetFilterCriteria_description = Lens.lens (\DataQualityRulesetFilterCriteria' {description} -> description) (\s@DataQualityRulesetFilterCriteria' {} a -> s {description = a} :: DataQualityRulesetFilterCriteria)

-- | Filter on rulesets last modified after this date.
dataQualityRulesetFilterCriteria_lastModifiedAfter :: Lens.Lens' DataQualityRulesetFilterCriteria (Prelude.Maybe Prelude.UTCTime)
dataQualityRulesetFilterCriteria_lastModifiedAfter = Lens.lens (\DataQualityRulesetFilterCriteria' {lastModifiedAfter} -> lastModifiedAfter) (\s@DataQualityRulesetFilterCriteria' {} a -> s {lastModifiedAfter = a} :: DataQualityRulesetFilterCriteria) Prelude.. Lens.mapping Data._Time

-- | Filter on rulesets last modified before this date.
dataQualityRulesetFilterCriteria_lastModifiedBefore :: Lens.Lens' DataQualityRulesetFilterCriteria (Prelude.Maybe Prelude.UTCTime)
dataQualityRulesetFilterCriteria_lastModifiedBefore = Lens.lens (\DataQualityRulesetFilterCriteria' {lastModifiedBefore} -> lastModifiedBefore) (\s@DataQualityRulesetFilterCriteria' {} a -> s {lastModifiedBefore = a} :: DataQualityRulesetFilterCriteria) Prelude.. Lens.mapping Data._Time

-- | The name of the ruleset filter criteria.
dataQualityRulesetFilterCriteria_name :: Lens.Lens' DataQualityRulesetFilterCriteria (Prelude.Maybe Prelude.Text)
dataQualityRulesetFilterCriteria_name = Lens.lens (\DataQualityRulesetFilterCriteria' {name} -> name) (\s@DataQualityRulesetFilterCriteria' {} a -> s {name = a} :: DataQualityRulesetFilterCriteria)

-- | The name and database name of the target table.
dataQualityRulesetFilterCriteria_targetTable :: Lens.Lens' DataQualityRulesetFilterCriteria (Prelude.Maybe DataQualityTargetTable)
dataQualityRulesetFilterCriteria_targetTable = Lens.lens (\DataQualityRulesetFilterCriteria' {targetTable} -> targetTable) (\s@DataQualityRulesetFilterCriteria' {} a -> s {targetTable = a} :: DataQualityRulesetFilterCriteria)

instance
  Prelude.Hashable
    DataQualityRulesetFilterCriteria
  where
  hashWithSalt
    _salt
    DataQualityRulesetFilterCriteria' {..} =
      _salt `Prelude.hashWithSalt` createdAfter
        `Prelude.hashWithSalt` createdBefore
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` lastModifiedAfter
        `Prelude.hashWithSalt` lastModifiedBefore
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` targetTable

instance
  Prelude.NFData
    DataQualityRulesetFilterCriteria
  where
  rnf DataQualityRulesetFilterCriteria' {..} =
    Prelude.rnf createdAfter
      `Prelude.seq` Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedAfter
      `Prelude.seq` Prelude.rnf lastModifiedBefore
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf targetTable

instance Data.ToJSON DataQualityRulesetFilterCriteria where
  toJSON DataQualityRulesetFilterCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreatedAfter" Data..=) Prelude.<$> createdAfter,
            ("CreatedBefore" Data..=) Prelude.<$> createdBefore,
            ("Description" Data..=) Prelude.<$> description,
            ("LastModifiedAfter" Data..=)
              Prelude.<$> lastModifiedAfter,
            ("LastModifiedBefore" Data..=)
              Prelude.<$> lastModifiedBefore,
            ("Name" Data..=) Prelude.<$> name,
            ("TargetTable" Data..=) Prelude.<$> targetTable
          ]
      )
