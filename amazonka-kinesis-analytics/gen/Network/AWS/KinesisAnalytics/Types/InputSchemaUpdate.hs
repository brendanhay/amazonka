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
-- Module      : Network.AWS.KinesisAnalytics.Types.InputSchemaUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputSchemaUpdate where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisAnalytics.Types.RecordColumn
import Network.AWS.KinesisAnalytics.Types.RecordFormat
import qualified Network.AWS.Lens as Lens

-- | Describes updates for the application\'s input schema.
--
-- /See:/ 'newInputSchemaUpdate' smart constructor.
data InputSchemaUpdate = InputSchemaUpdate'
  { -- | Specifies the format of the records on the streaming source.
    recordFormatUpdate :: Core.Maybe RecordFormat,
    -- | A list of @RecordColumn@ objects. Each object describes the mapping of
    -- the streaming source element to the corresponding column in the
    -- in-application stream.
    recordColumnUpdates :: Core.Maybe (Core.NonEmpty RecordColumn),
    -- | Specifies the encoding of the records in the streaming source. For
    -- example, UTF-8.
    recordEncodingUpdate :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InputSchemaUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordFormatUpdate', 'inputSchemaUpdate_recordFormatUpdate' - Specifies the format of the records on the streaming source.
--
-- 'recordColumnUpdates', 'inputSchemaUpdate_recordColumnUpdates' - A list of @RecordColumn@ objects. Each object describes the mapping of
-- the streaming source element to the corresponding column in the
-- in-application stream.
--
-- 'recordEncodingUpdate', 'inputSchemaUpdate_recordEncodingUpdate' - Specifies the encoding of the records in the streaming source. For
-- example, UTF-8.
newInputSchemaUpdate ::
  InputSchemaUpdate
newInputSchemaUpdate =
  InputSchemaUpdate'
    { recordFormatUpdate =
        Core.Nothing,
      recordColumnUpdates = Core.Nothing,
      recordEncodingUpdate = Core.Nothing
    }

-- | Specifies the format of the records on the streaming source.
inputSchemaUpdate_recordFormatUpdate :: Lens.Lens' InputSchemaUpdate (Core.Maybe RecordFormat)
inputSchemaUpdate_recordFormatUpdate = Lens.lens (\InputSchemaUpdate' {recordFormatUpdate} -> recordFormatUpdate) (\s@InputSchemaUpdate' {} a -> s {recordFormatUpdate = a} :: InputSchemaUpdate)

-- | A list of @RecordColumn@ objects. Each object describes the mapping of
-- the streaming source element to the corresponding column in the
-- in-application stream.
inputSchemaUpdate_recordColumnUpdates :: Lens.Lens' InputSchemaUpdate (Core.Maybe (Core.NonEmpty RecordColumn))
inputSchemaUpdate_recordColumnUpdates = Lens.lens (\InputSchemaUpdate' {recordColumnUpdates} -> recordColumnUpdates) (\s@InputSchemaUpdate' {} a -> s {recordColumnUpdates = a} :: InputSchemaUpdate) Core.. Lens.mapping Lens._Coerce

-- | Specifies the encoding of the records in the streaming source. For
-- example, UTF-8.
inputSchemaUpdate_recordEncodingUpdate :: Lens.Lens' InputSchemaUpdate (Core.Maybe Core.Text)
inputSchemaUpdate_recordEncodingUpdate = Lens.lens (\InputSchemaUpdate' {recordEncodingUpdate} -> recordEncodingUpdate) (\s@InputSchemaUpdate' {} a -> s {recordEncodingUpdate = a} :: InputSchemaUpdate)

instance Core.Hashable InputSchemaUpdate

instance Core.NFData InputSchemaUpdate

instance Core.ToJSON InputSchemaUpdate where
  toJSON InputSchemaUpdate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RecordFormatUpdate" Core..=)
              Core.<$> recordFormatUpdate,
            ("RecordColumnUpdates" Core..=)
              Core.<$> recordColumnUpdates,
            ("RecordEncodingUpdate" Core..=)
              Core.<$> recordEncodingUpdate
          ]
      )
