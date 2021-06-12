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
-- Module      : Network.AWS.KinesisAnalytics.Types.RecordFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.RecordFormat where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisAnalytics.Types.MappingParameters
import Network.AWS.KinesisAnalytics.Types.RecordFormatType
import qualified Network.AWS.Lens as Lens

-- | Describes the record format and relevant mapping information that should
-- be applied to schematize the records on the stream.
--
-- /See:/ 'newRecordFormat' smart constructor.
data RecordFormat = RecordFormat'
  { -- | When configuring application input at the time of creating or updating
    -- an application, provides additional mapping information specific to the
    -- record format (such as JSON, CSV, or record fields delimited by some
    -- delimiter) on the streaming source.
    mappingParameters :: Core.Maybe MappingParameters,
    -- | The type of record format.
    recordFormatType :: RecordFormatType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { mappingParameters = Core.Nothing,
      recordFormatType = pRecordFormatType_
    }

-- | When configuring application input at the time of creating or updating
-- an application, provides additional mapping information specific to the
-- record format (such as JSON, CSV, or record fields delimited by some
-- delimiter) on the streaming source.
recordFormat_mappingParameters :: Lens.Lens' RecordFormat (Core.Maybe MappingParameters)
recordFormat_mappingParameters = Lens.lens (\RecordFormat' {mappingParameters} -> mappingParameters) (\s@RecordFormat' {} a -> s {mappingParameters = a} :: RecordFormat)

-- | The type of record format.
recordFormat_recordFormatType :: Lens.Lens' RecordFormat RecordFormatType
recordFormat_recordFormatType = Lens.lens (\RecordFormat' {recordFormatType} -> recordFormatType) (\s@RecordFormat' {} a -> s {recordFormatType = a} :: RecordFormat)

instance Core.FromJSON RecordFormat where
  parseJSON =
    Core.withObject
      "RecordFormat"
      ( \x ->
          RecordFormat'
            Core.<$> (x Core..:? "MappingParameters")
            Core.<*> (x Core..: "RecordFormatType")
      )

instance Core.Hashable RecordFormat

instance Core.NFData RecordFormat

instance Core.ToJSON RecordFormat where
  toJSON RecordFormat' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MappingParameters" Core..=)
              Core.<$> mappingParameters,
            Core.Just
              ("RecordFormatType" Core..= recordFormatType)
          ]
      )
