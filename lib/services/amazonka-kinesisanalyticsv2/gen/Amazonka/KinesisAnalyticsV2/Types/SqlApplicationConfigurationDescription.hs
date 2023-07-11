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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.SqlApplicationConfigurationDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.SqlApplicationConfigurationDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.InputDescription
import Amazonka.KinesisAnalyticsV2.Types.OutputDescription
import Amazonka.KinesisAnalyticsV2.Types.ReferenceDataSourceDescription
import qualified Amazonka.Prelude as Prelude

-- | Describes the inputs, outputs, and reference data sources for a
-- SQL-based Kinesis Data Analytics application.
--
-- /See:/ 'newSqlApplicationConfigurationDescription' smart constructor.
data SqlApplicationConfigurationDescription = SqlApplicationConfigurationDescription'
  { -- | The array of InputDescription objects describing the input streams used
    -- by the application.
    inputDescriptions :: Prelude.Maybe [InputDescription],
    -- | The array of OutputDescription objects describing the destination
    -- streams used by the application.
    outputDescriptions :: Prelude.Maybe [OutputDescription],
    -- | The array of ReferenceDataSourceDescription objects describing the
    -- reference data sources used by the application.
    referenceDataSourceDescriptions :: Prelude.Maybe [ReferenceDataSourceDescription]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SqlApplicationConfigurationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputDescriptions', 'sqlApplicationConfigurationDescription_inputDescriptions' - The array of InputDescription objects describing the input streams used
-- by the application.
--
-- 'outputDescriptions', 'sqlApplicationConfigurationDescription_outputDescriptions' - The array of OutputDescription objects describing the destination
-- streams used by the application.
--
-- 'referenceDataSourceDescriptions', 'sqlApplicationConfigurationDescription_referenceDataSourceDescriptions' - The array of ReferenceDataSourceDescription objects describing the
-- reference data sources used by the application.
newSqlApplicationConfigurationDescription ::
  SqlApplicationConfigurationDescription
newSqlApplicationConfigurationDescription =
  SqlApplicationConfigurationDescription'
    { inputDescriptions =
        Prelude.Nothing,
      outputDescriptions =
        Prelude.Nothing,
      referenceDataSourceDescriptions =
        Prelude.Nothing
    }

-- | The array of InputDescription objects describing the input streams used
-- by the application.
sqlApplicationConfigurationDescription_inputDescriptions :: Lens.Lens' SqlApplicationConfigurationDescription (Prelude.Maybe [InputDescription])
sqlApplicationConfigurationDescription_inputDescriptions = Lens.lens (\SqlApplicationConfigurationDescription' {inputDescriptions} -> inputDescriptions) (\s@SqlApplicationConfigurationDescription' {} a -> s {inputDescriptions = a} :: SqlApplicationConfigurationDescription) Prelude.. Lens.mapping Lens.coerced

-- | The array of OutputDescription objects describing the destination
-- streams used by the application.
sqlApplicationConfigurationDescription_outputDescriptions :: Lens.Lens' SqlApplicationConfigurationDescription (Prelude.Maybe [OutputDescription])
sqlApplicationConfigurationDescription_outputDescriptions = Lens.lens (\SqlApplicationConfigurationDescription' {outputDescriptions} -> outputDescriptions) (\s@SqlApplicationConfigurationDescription' {} a -> s {outputDescriptions = a} :: SqlApplicationConfigurationDescription) Prelude.. Lens.mapping Lens.coerced

-- | The array of ReferenceDataSourceDescription objects describing the
-- reference data sources used by the application.
sqlApplicationConfigurationDescription_referenceDataSourceDescriptions :: Lens.Lens' SqlApplicationConfigurationDescription (Prelude.Maybe [ReferenceDataSourceDescription])
sqlApplicationConfigurationDescription_referenceDataSourceDescriptions = Lens.lens (\SqlApplicationConfigurationDescription' {referenceDataSourceDescriptions} -> referenceDataSourceDescriptions) (\s@SqlApplicationConfigurationDescription' {} a -> s {referenceDataSourceDescriptions = a} :: SqlApplicationConfigurationDescription) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    SqlApplicationConfigurationDescription
  where
  parseJSON =
    Data.withObject
      "SqlApplicationConfigurationDescription"
      ( \x ->
          SqlApplicationConfigurationDescription'
            Prelude.<$> ( x
                            Data..:? "InputDescriptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "OutputDescriptions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "ReferenceDataSourceDescriptions"
                            Data..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    SqlApplicationConfigurationDescription
  where
  hashWithSalt
    _salt
    SqlApplicationConfigurationDescription' {..} =
      _salt
        `Prelude.hashWithSalt` inputDescriptions
        `Prelude.hashWithSalt` outputDescriptions
        `Prelude.hashWithSalt` referenceDataSourceDescriptions

instance
  Prelude.NFData
    SqlApplicationConfigurationDescription
  where
  rnf SqlApplicationConfigurationDescription' {..} =
    Prelude.rnf inputDescriptions
      `Prelude.seq` Prelude.rnf outputDescriptions
      `Prelude.seq` Prelude.rnf referenceDataSourceDescriptions
