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
-- Module      : Amazonka.KinesisAnalytics.Types.SourceSchema
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.SourceSchema where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalytics.Types.RecordColumn
import Amazonka.KinesisAnalytics.Types.RecordFormat
import qualified Amazonka.Prelude as Prelude

-- | Describes the format of the data in the streaming source, and how each
-- data element maps to corresponding columns created in the in-application
-- stream.
--
-- /See:/ 'newSourceSchema' smart constructor.
data SourceSchema = SourceSchema'
  { -- | Specifies the encoding of the records in the streaming source. For
    -- example, UTF-8.
    recordEncoding :: Prelude.Maybe Prelude.Text,
    -- | Specifies the format of the records on the streaming source.
    recordFormat :: RecordFormat,
    -- | A list of @RecordColumn@ objects.
    recordColumns :: Prelude.NonEmpty RecordColumn
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.NonEmpty RecordColumn ->
  SourceSchema
newSourceSchema pRecordFormat_ pRecordColumns_ =
  SourceSchema'
    { recordEncoding = Prelude.Nothing,
      recordFormat = pRecordFormat_,
      recordColumns = Lens.coerced Lens.# pRecordColumns_
    }

-- | Specifies the encoding of the records in the streaming source. For
-- example, UTF-8.
sourceSchema_recordEncoding :: Lens.Lens' SourceSchema (Prelude.Maybe Prelude.Text)
sourceSchema_recordEncoding = Lens.lens (\SourceSchema' {recordEncoding} -> recordEncoding) (\s@SourceSchema' {} a -> s {recordEncoding = a} :: SourceSchema)

-- | Specifies the format of the records on the streaming source.
sourceSchema_recordFormat :: Lens.Lens' SourceSchema RecordFormat
sourceSchema_recordFormat = Lens.lens (\SourceSchema' {recordFormat} -> recordFormat) (\s@SourceSchema' {} a -> s {recordFormat = a} :: SourceSchema)

-- | A list of @RecordColumn@ objects.
sourceSchema_recordColumns :: Lens.Lens' SourceSchema (Prelude.NonEmpty RecordColumn)
sourceSchema_recordColumns = Lens.lens (\SourceSchema' {recordColumns} -> recordColumns) (\s@SourceSchema' {} a -> s {recordColumns = a} :: SourceSchema) Prelude.. Lens.coerced

instance Data.FromJSON SourceSchema where
  parseJSON =
    Data.withObject
      "SourceSchema"
      ( \x ->
          SourceSchema'
            Prelude.<$> (x Data..:? "RecordEncoding")
            Prelude.<*> (x Data..: "RecordFormat")
            Prelude.<*> (x Data..: "RecordColumns")
      )

instance Prelude.Hashable SourceSchema where
  hashWithSalt _salt SourceSchema' {..} =
    _salt
      `Prelude.hashWithSalt` recordEncoding
      `Prelude.hashWithSalt` recordFormat
      `Prelude.hashWithSalt` recordColumns

instance Prelude.NFData SourceSchema where
  rnf SourceSchema' {..} =
    Prelude.rnf recordEncoding
      `Prelude.seq` Prelude.rnf recordFormat
      `Prelude.seq` Prelude.rnf recordColumns

instance Data.ToJSON SourceSchema where
  toJSON SourceSchema' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RecordEncoding" Data..=)
              Prelude.<$> recordEncoding,
            Prelude.Just ("RecordFormat" Data..= recordFormat),
            Prelude.Just
              ("RecordColumns" Data..= recordColumns)
          ]
      )
