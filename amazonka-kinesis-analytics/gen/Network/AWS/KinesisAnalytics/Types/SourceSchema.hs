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
-- Module      : Network.AWS.KinesisAnalytics.Types.SourceSchema
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.SourceSchema where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisAnalytics.Types.RecordColumn
import Network.AWS.KinesisAnalytics.Types.RecordFormat
import qualified Network.AWS.Lens as Lens

-- | Describes the format of the data in the streaming source, and how each
-- data element maps to corresponding columns created in the in-application
-- stream.
--
-- /See:/ 'newSourceSchema' smart constructor.
data SourceSchema = SourceSchema'
  { -- | Specifies the encoding of the records in the streaming source. For
    -- example, UTF-8.
    recordEncoding :: Core.Maybe Core.Text,
    -- | Specifies the format of the records on the streaming source.
    recordFormat :: RecordFormat,
    -- | A list of @RecordColumn@ objects.
    recordColumns :: Core.NonEmpty RecordColumn
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SourceSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordEncoding', 'sourceSchema_recordEncoding' - Specifies the encoding of the records in the streaming source. For
-- example, UTF-8.
--
-- 'recordFormat', 'sourceSchema_recordFormat' - Specifies the format of the records on the streaming source.
--
-- 'recordColumns', 'sourceSchema_recordColumns' - A list of @RecordColumn@ objects.
newSourceSchema ::
  -- | 'recordFormat'
  RecordFormat ->
  -- | 'recordColumns'
  Core.NonEmpty RecordColumn ->
  SourceSchema
newSourceSchema pRecordFormat_ pRecordColumns_ =
  SourceSchema'
    { recordEncoding = Core.Nothing,
      recordFormat = pRecordFormat_,
      recordColumns = Lens._Coerce Lens.# pRecordColumns_
    }

-- | Specifies the encoding of the records in the streaming source. For
-- example, UTF-8.
sourceSchema_recordEncoding :: Lens.Lens' SourceSchema (Core.Maybe Core.Text)
sourceSchema_recordEncoding = Lens.lens (\SourceSchema' {recordEncoding} -> recordEncoding) (\s@SourceSchema' {} a -> s {recordEncoding = a} :: SourceSchema)

-- | Specifies the format of the records on the streaming source.
sourceSchema_recordFormat :: Lens.Lens' SourceSchema RecordFormat
sourceSchema_recordFormat = Lens.lens (\SourceSchema' {recordFormat} -> recordFormat) (\s@SourceSchema' {} a -> s {recordFormat = a} :: SourceSchema)

-- | A list of @RecordColumn@ objects.
sourceSchema_recordColumns :: Lens.Lens' SourceSchema (Core.NonEmpty RecordColumn)
sourceSchema_recordColumns = Lens.lens (\SourceSchema' {recordColumns} -> recordColumns) (\s@SourceSchema' {} a -> s {recordColumns = a} :: SourceSchema) Core.. Lens._Coerce

instance Core.FromJSON SourceSchema where
  parseJSON =
    Core.withObject
      "SourceSchema"
      ( \x ->
          SourceSchema'
            Core.<$> (x Core..:? "RecordEncoding")
            Core.<*> (x Core..: "RecordFormat")
            Core.<*> (x Core..: "RecordColumns")
      )

instance Core.Hashable SourceSchema

instance Core.NFData SourceSchema

instance Core.ToJSON SourceSchema where
  toJSON SourceSchema' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RecordEncoding" Core..=) Core.<$> recordEncoding,
            Core.Just ("RecordFormat" Core..= recordFormat),
            Core.Just ("RecordColumns" Core..= recordColumns)
          ]
      )
