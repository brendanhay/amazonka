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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.SqlApplicationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.SqlApplicationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.Input
import Amazonka.KinesisAnalyticsV2.Types.Output
import Amazonka.KinesisAnalyticsV2.Types.ReferenceDataSource
import qualified Amazonka.Prelude as Prelude

-- | Describes the inputs, outputs, and reference data sources for a
-- SQL-based Kinesis Data Analytics application.
--
-- /See:/ 'newSqlApplicationConfiguration' smart constructor.
data SqlApplicationConfiguration = SqlApplicationConfiguration'
  { -- | The array of Input objects describing the input streams used by the
    -- application.
    inputs :: Prelude.Maybe [Input],
    -- | The array of Output objects describing the destination streams used by
    -- the application.
    outputs :: Prelude.Maybe [Output],
    -- | The array of ReferenceDataSource objects describing the reference data
    -- sources used by the application.
    referenceDataSources :: Prelude.Maybe [ReferenceDataSource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SqlApplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputs', 'sqlApplicationConfiguration_inputs' - The array of Input objects describing the input streams used by the
-- application.
--
-- 'outputs', 'sqlApplicationConfiguration_outputs' - The array of Output objects describing the destination streams used by
-- the application.
--
-- 'referenceDataSources', 'sqlApplicationConfiguration_referenceDataSources' - The array of ReferenceDataSource objects describing the reference data
-- sources used by the application.
newSqlApplicationConfiguration ::
  SqlApplicationConfiguration
newSqlApplicationConfiguration =
  SqlApplicationConfiguration'
    { inputs =
        Prelude.Nothing,
      outputs = Prelude.Nothing,
      referenceDataSources = Prelude.Nothing
    }

-- | The array of Input objects describing the input streams used by the
-- application.
sqlApplicationConfiguration_inputs :: Lens.Lens' SqlApplicationConfiguration (Prelude.Maybe [Input])
sqlApplicationConfiguration_inputs = Lens.lens (\SqlApplicationConfiguration' {inputs} -> inputs) (\s@SqlApplicationConfiguration' {} a -> s {inputs = a} :: SqlApplicationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The array of Output objects describing the destination streams used by
-- the application.
sqlApplicationConfiguration_outputs :: Lens.Lens' SqlApplicationConfiguration (Prelude.Maybe [Output])
sqlApplicationConfiguration_outputs = Lens.lens (\SqlApplicationConfiguration' {outputs} -> outputs) (\s@SqlApplicationConfiguration' {} a -> s {outputs = a} :: SqlApplicationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The array of ReferenceDataSource objects describing the reference data
-- sources used by the application.
sqlApplicationConfiguration_referenceDataSources :: Lens.Lens' SqlApplicationConfiguration (Prelude.Maybe [ReferenceDataSource])
sqlApplicationConfiguration_referenceDataSources = Lens.lens (\SqlApplicationConfiguration' {referenceDataSources} -> referenceDataSources) (\s@SqlApplicationConfiguration' {} a -> s {referenceDataSources = a} :: SqlApplicationConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable SqlApplicationConfiguration where
  hashWithSalt _salt SqlApplicationConfiguration' {..} =
    _salt `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` referenceDataSources

instance Prelude.NFData SqlApplicationConfiguration where
  rnf SqlApplicationConfiguration' {..} =
    Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf referenceDataSources

instance Data.ToJSON SqlApplicationConfiguration where
  toJSON SqlApplicationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Inputs" Data..=) Prelude.<$> inputs,
            ("Outputs" Data..=) Prelude.<$> outputs,
            ("ReferenceDataSources" Data..=)
              Prelude.<$> referenceDataSources
          ]
      )
