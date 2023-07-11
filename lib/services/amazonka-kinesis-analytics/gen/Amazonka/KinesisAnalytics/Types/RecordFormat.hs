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
-- Module      : Amazonka.KinesisAnalytics.Types.RecordFormat
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.RecordFormat where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalytics.Types.MappingParameters
import Amazonka.KinesisAnalytics.Types.RecordFormatType
import qualified Amazonka.Prelude as Prelude

-- | Describes the record format and relevant mapping information that should
-- be applied to schematize the records on the stream.
--
-- /See:/ 'newRecordFormat' smart constructor.
data RecordFormat = RecordFormat'
  { -- | When configuring application input at the time of creating or updating
    -- an application, provides additional mapping information specific to the
    -- record format (such as JSON, CSV, or record fields delimited by some
    -- delimiter) on the streaming source.
    mappingParameters :: Prelude.Maybe MappingParameters,
    -- | The type of record format.
    recordFormatType :: RecordFormatType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecordFormat' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mappingParameters', 'recordFormat_mappingParameters' - When configuring application input at the time of creating or updating
-- an application, provides additional mapping information specific to the
-- record format (such as JSON, CSV, or record fields delimited by some
-- delimiter) on the streaming source.
--
-- 'recordFormatType', 'recordFormat_recordFormatType' - The type of record format.
newRecordFormat ::
  -- | 'recordFormatType'
  RecordFormatType ->
  RecordFormat
newRecordFormat pRecordFormatType_ =
  RecordFormat'
    { mappingParameters = Prelude.Nothing,
      recordFormatType = pRecordFormatType_
    }

-- | When configuring application input at the time of creating or updating
-- an application, provides additional mapping information specific to the
-- record format (such as JSON, CSV, or record fields delimited by some
-- delimiter) on the streaming source.
recordFormat_mappingParameters :: Lens.Lens' RecordFormat (Prelude.Maybe MappingParameters)
recordFormat_mappingParameters = Lens.lens (\RecordFormat' {mappingParameters} -> mappingParameters) (\s@RecordFormat' {} a -> s {mappingParameters = a} :: RecordFormat)

-- | The type of record format.
recordFormat_recordFormatType :: Lens.Lens' RecordFormat RecordFormatType
recordFormat_recordFormatType = Lens.lens (\RecordFormat' {recordFormatType} -> recordFormatType) (\s@RecordFormat' {} a -> s {recordFormatType = a} :: RecordFormat)

instance Data.FromJSON RecordFormat where
  parseJSON =
    Data.withObject
      "RecordFormat"
      ( \x ->
          RecordFormat'
            Prelude.<$> (x Data..:? "MappingParameters")
            Prelude.<*> (x Data..: "RecordFormatType")
      )

instance Prelude.Hashable RecordFormat where
  hashWithSalt _salt RecordFormat' {..} =
    _salt
      `Prelude.hashWithSalt` mappingParameters
      `Prelude.hashWithSalt` recordFormatType

instance Prelude.NFData RecordFormat where
  rnf RecordFormat' {..} =
    Prelude.rnf mappingParameters
      `Prelude.seq` Prelude.rnf recordFormatType

instance Data.ToJSON RecordFormat where
  toJSON RecordFormat' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MappingParameters" Data..=)
              Prelude.<$> mappingParameters,
            Prelude.Just
              ("RecordFormatType" Data..= recordFormatType)
          ]
      )
