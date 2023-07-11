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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.SqlApplicationConfigurationUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.SqlApplicationConfigurationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.InputUpdate
import Amazonka.KinesisAnalyticsV2.Types.OutputUpdate
import Amazonka.KinesisAnalyticsV2.Types.ReferenceDataSourceUpdate
import qualified Amazonka.Prelude as Prelude

-- | Describes updates to the input streams, destination streams, and
-- reference data sources for a SQL-based Kinesis Data Analytics
-- application.
--
-- /See:/ 'newSqlApplicationConfigurationUpdate' smart constructor.
data SqlApplicationConfigurationUpdate = SqlApplicationConfigurationUpdate'
  { -- | The array of InputUpdate objects describing the new input streams used
    -- by the application.
    inputUpdates :: Prelude.Maybe [InputUpdate],
    -- | The array of OutputUpdate objects describing the new destination streams
    -- used by the application.
    outputUpdates :: Prelude.Maybe [OutputUpdate],
    -- | The array of ReferenceDataSourceUpdate objects describing the new
    -- reference data sources used by the application.
    referenceDataSourceUpdates :: Prelude.Maybe [ReferenceDataSourceUpdate]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SqlApplicationConfigurationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputUpdates', 'sqlApplicationConfigurationUpdate_inputUpdates' - The array of InputUpdate objects describing the new input streams used
-- by the application.
--
-- 'outputUpdates', 'sqlApplicationConfigurationUpdate_outputUpdates' - The array of OutputUpdate objects describing the new destination streams
-- used by the application.
--
-- 'referenceDataSourceUpdates', 'sqlApplicationConfigurationUpdate_referenceDataSourceUpdates' - The array of ReferenceDataSourceUpdate objects describing the new
-- reference data sources used by the application.
newSqlApplicationConfigurationUpdate ::
  SqlApplicationConfigurationUpdate
newSqlApplicationConfigurationUpdate =
  SqlApplicationConfigurationUpdate'
    { inputUpdates =
        Prelude.Nothing,
      outputUpdates = Prelude.Nothing,
      referenceDataSourceUpdates =
        Prelude.Nothing
    }

-- | The array of InputUpdate objects describing the new input streams used
-- by the application.
sqlApplicationConfigurationUpdate_inputUpdates :: Lens.Lens' SqlApplicationConfigurationUpdate (Prelude.Maybe [InputUpdate])
sqlApplicationConfigurationUpdate_inputUpdates = Lens.lens (\SqlApplicationConfigurationUpdate' {inputUpdates} -> inputUpdates) (\s@SqlApplicationConfigurationUpdate' {} a -> s {inputUpdates = a} :: SqlApplicationConfigurationUpdate) Prelude.. Lens.mapping Lens.coerced

-- | The array of OutputUpdate objects describing the new destination streams
-- used by the application.
sqlApplicationConfigurationUpdate_outputUpdates :: Lens.Lens' SqlApplicationConfigurationUpdate (Prelude.Maybe [OutputUpdate])
sqlApplicationConfigurationUpdate_outputUpdates = Lens.lens (\SqlApplicationConfigurationUpdate' {outputUpdates} -> outputUpdates) (\s@SqlApplicationConfigurationUpdate' {} a -> s {outputUpdates = a} :: SqlApplicationConfigurationUpdate) Prelude.. Lens.mapping Lens.coerced

-- | The array of ReferenceDataSourceUpdate objects describing the new
-- reference data sources used by the application.
sqlApplicationConfigurationUpdate_referenceDataSourceUpdates :: Lens.Lens' SqlApplicationConfigurationUpdate (Prelude.Maybe [ReferenceDataSourceUpdate])
sqlApplicationConfigurationUpdate_referenceDataSourceUpdates = Lens.lens (\SqlApplicationConfigurationUpdate' {referenceDataSourceUpdates} -> referenceDataSourceUpdates) (\s@SqlApplicationConfigurationUpdate' {} a -> s {referenceDataSourceUpdates = a} :: SqlApplicationConfigurationUpdate) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    SqlApplicationConfigurationUpdate
  where
  hashWithSalt
    _salt
    SqlApplicationConfigurationUpdate' {..} =
      _salt
        `Prelude.hashWithSalt` inputUpdates
        `Prelude.hashWithSalt` outputUpdates
        `Prelude.hashWithSalt` referenceDataSourceUpdates

instance
  Prelude.NFData
    SqlApplicationConfigurationUpdate
  where
  rnf SqlApplicationConfigurationUpdate' {..} =
    Prelude.rnf inputUpdates
      `Prelude.seq` Prelude.rnf outputUpdates
      `Prelude.seq` Prelude.rnf referenceDataSourceUpdates

instance
  Data.ToJSON
    SqlApplicationConfigurationUpdate
  where
  toJSON SqlApplicationConfigurationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InputUpdates" Data..=) Prelude.<$> inputUpdates,
            ("OutputUpdates" Data..=) Prelude.<$> outputUpdates,
            ("ReferenceDataSourceUpdates" Data..=)
              Prelude.<$> referenceDataSourceUpdates
          ]
      )
