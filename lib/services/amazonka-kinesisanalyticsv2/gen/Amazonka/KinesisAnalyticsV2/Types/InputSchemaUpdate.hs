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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.InputSchemaUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.InputSchemaUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.KinesisAnalyticsV2.Types.RecordColumn
import Amazonka.KinesisAnalyticsV2.Types.RecordFormat
import qualified Amazonka.Prelude as Prelude

-- | Describes updates for an SQL-based Kinesis Data Analytics application\'s
-- input schema.
--
-- /See:/ 'newInputSchemaUpdate' smart constructor.
data InputSchemaUpdate = InputSchemaUpdate'
  { -- | A list of @RecordColumn@ objects. Each object describes the mapping of
    -- the streaming source element to the corresponding column in the
    -- in-application stream.
    recordColumnUpdates :: Prelude.Maybe (Prelude.NonEmpty RecordColumn),
    -- | Specifies the encoding of the records in the streaming source; for
    -- example, UTF-8.
    recordEncodingUpdate :: Prelude.Maybe Prelude.Text,
    -- | Specifies the format of the records on the streaming source.
    recordFormatUpdate :: Prelude.Maybe RecordFormat
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputSchemaUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordColumnUpdates', 'inputSchemaUpdate_recordColumnUpdates' - A list of @RecordColumn@ objects. Each object describes the mapping of
-- the streaming source element to the corresponding column in the
-- in-application stream.
--
-- 'recordEncodingUpdate', 'inputSchemaUpdate_recordEncodingUpdate' - Specifies the encoding of the records in the streaming source; for
-- example, UTF-8.
--
-- 'recordFormatUpdate', 'inputSchemaUpdate_recordFormatUpdate' - Specifies the format of the records on the streaming source.
newInputSchemaUpdate ::
  InputSchemaUpdate
newInputSchemaUpdate =
  InputSchemaUpdate'
    { recordColumnUpdates =
        Prelude.Nothing,
      recordEncodingUpdate = Prelude.Nothing,
      recordFormatUpdate = Prelude.Nothing
    }

-- | A list of @RecordColumn@ objects. Each object describes the mapping of
-- the streaming source element to the corresponding column in the
-- in-application stream.
inputSchemaUpdate_recordColumnUpdates :: Lens.Lens' InputSchemaUpdate (Prelude.Maybe (Prelude.NonEmpty RecordColumn))
inputSchemaUpdate_recordColumnUpdates = Lens.lens (\InputSchemaUpdate' {recordColumnUpdates} -> recordColumnUpdates) (\s@InputSchemaUpdate' {} a -> s {recordColumnUpdates = a} :: InputSchemaUpdate) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the encoding of the records in the streaming source; for
-- example, UTF-8.
inputSchemaUpdate_recordEncodingUpdate :: Lens.Lens' InputSchemaUpdate (Prelude.Maybe Prelude.Text)
inputSchemaUpdate_recordEncodingUpdate = Lens.lens (\InputSchemaUpdate' {recordEncodingUpdate} -> recordEncodingUpdate) (\s@InputSchemaUpdate' {} a -> s {recordEncodingUpdate = a} :: InputSchemaUpdate)

-- | Specifies the format of the records on the streaming source.
inputSchemaUpdate_recordFormatUpdate :: Lens.Lens' InputSchemaUpdate (Prelude.Maybe RecordFormat)
inputSchemaUpdate_recordFormatUpdate = Lens.lens (\InputSchemaUpdate' {recordFormatUpdate} -> recordFormatUpdate) (\s@InputSchemaUpdate' {} a -> s {recordFormatUpdate = a} :: InputSchemaUpdate)

instance Prelude.Hashable InputSchemaUpdate where
  hashWithSalt _salt InputSchemaUpdate' {..} =
    _salt
      `Prelude.hashWithSalt` recordColumnUpdates
      `Prelude.hashWithSalt` recordEncodingUpdate
      `Prelude.hashWithSalt` recordFormatUpdate

instance Prelude.NFData InputSchemaUpdate where
  rnf InputSchemaUpdate' {..} =
    Prelude.rnf recordColumnUpdates
      `Prelude.seq` Prelude.rnf recordEncodingUpdate
      `Prelude.seq` Prelude.rnf recordFormatUpdate

instance Data.ToJSON InputSchemaUpdate where
  toJSON InputSchemaUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RecordColumnUpdates" Data..=)
              Prelude.<$> recordColumnUpdates,
            ("RecordEncodingUpdate" Data..=)
              Prelude.<$> recordEncodingUpdate,
            ("RecordFormatUpdate" Data..=)
              Prelude.<$> recordFormatUpdate
          ]
      )
