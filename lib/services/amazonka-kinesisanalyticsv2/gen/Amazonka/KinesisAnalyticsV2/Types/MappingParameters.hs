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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.MappingParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.MappingParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.CSVMappingParameters
import Amazonka.KinesisAnalyticsV2.Types.JSONMappingParameters
import qualified Amazonka.Prelude as Prelude

-- | When you configure a SQL-based Kinesis Data Analytics application\'s
-- input at the time of creating or updating an application, provides
-- additional mapping information specific to the record format (such as
-- JSON, CSV, or record fields delimited by some delimiter) on the
-- streaming source.
--
-- /See:/ 'newMappingParameters' smart constructor.
data MappingParameters = MappingParameters'
  { -- | Provides additional mapping information when the record format uses
    -- delimiters (for example, CSV).
    cSVMappingParameters :: Prelude.Maybe CSVMappingParameters,
    -- | Provides additional mapping information when JSON is the record format
    -- on the streaming source.
    jSONMappingParameters :: Prelude.Maybe JSONMappingParameters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MappingParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cSVMappingParameters', 'mappingParameters_cSVMappingParameters' - Provides additional mapping information when the record format uses
-- delimiters (for example, CSV).
--
-- 'jSONMappingParameters', 'mappingParameters_jSONMappingParameters' - Provides additional mapping information when JSON is the record format
-- on the streaming source.
newMappingParameters ::
  MappingParameters
newMappingParameters =
  MappingParameters'
    { cSVMappingParameters =
        Prelude.Nothing,
      jSONMappingParameters = Prelude.Nothing
    }

-- | Provides additional mapping information when the record format uses
-- delimiters (for example, CSV).
mappingParameters_cSVMappingParameters :: Lens.Lens' MappingParameters (Prelude.Maybe CSVMappingParameters)
mappingParameters_cSVMappingParameters = Lens.lens (\MappingParameters' {cSVMappingParameters} -> cSVMappingParameters) (\s@MappingParameters' {} a -> s {cSVMappingParameters = a} :: MappingParameters)

-- | Provides additional mapping information when JSON is the record format
-- on the streaming source.
mappingParameters_jSONMappingParameters :: Lens.Lens' MappingParameters (Prelude.Maybe JSONMappingParameters)
mappingParameters_jSONMappingParameters = Lens.lens (\MappingParameters' {jSONMappingParameters} -> jSONMappingParameters) (\s@MappingParameters' {} a -> s {jSONMappingParameters = a} :: MappingParameters)

instance Data.FromJSON MappingParameters where
  parseJSON =
    Data.withObject
      "MappingParameters"
      ( \x ->
          MappingParameters'
            Prelude.<$> (x Data..:? "CSVMappingParameters")
            Prelude.<*> (x Data..:? "JSONMappingParameters")
      )

instance Prelude.Hashable MappingParameters where
  hashWithSalt _salt MappingParameters' {..} =
    _salt `Prelude.hashWithSalt` cSVMappingParameters
      `Prelude.hashWithSalt` jSONMappingParameters

instance Prelude.NFData MappingParameters where
  rnf MappingParameters' {..} =
    Prelude.rnf cSVMappingParameters
      `Prelude.seq` Prelude.rnf jSONMappingParameters

instance Data.ToJSON MappingParameters where
  toJSON MappingParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CSVMappingParameters" Data..=)
              Prelude.<$> cSVMappingParameters,
            ("JSONMappingParameters" Data..=)
              Prelude.<$> jSONMappingParameters
          ]
      )
