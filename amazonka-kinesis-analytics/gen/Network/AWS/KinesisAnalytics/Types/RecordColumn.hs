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
-- Module      : Network.AWS.KinesisAnalytics.Types.RecordColumn
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.RecordColumn where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the mapping of each data element in the streaming source to
-- the corresponding column in the in-application stream.
--
-- Also used to describe the format of the reference data source.
--
-- /See:/ 'newRecordColumn' smart constructor.
data RecordColumn = RecordColumn'
  { -- | Reference to the data element in the streaming input or the reference
    -- data source. This element is required if the
    -- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_RecordFormat.html#analytics-Type-RecordFormat-RecordFormatTypel RecordFormatType>
    -- is @JSON@.
    mapping :: Core.Maybe Core.Text,
    -- | Name of the column created in the in-application input stream or
    -- reference table.
    name :: Core.Text,
    -- | Type of column created in the in-application input stream or reference
    -- table.
    sqlType :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RecordColumn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mapping', 'recordColumn_mapping' - Reference to the data element in the streaming input or the reference
-- data source. This element is required if the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_RecordFormat.html#analytics-Type-RecordFormat-RecordFormatTypel RecordFormatType>
-- is @JSON@.
--
-- 'name', 'recordColumn_name' - Name of the column created in the in-application input stream or
-- reference table.
--
-- 'sqlType', 'recordColumn_sqlType' - Type of column created in the in-application input stream or reference
-- table.
newRecordColumn ::
  -- | 'name'
  Core.Text ->
  -- | 'sqlType'
  Core.Text ->
  RecordColumn
newRecordColumn pName_ pSqlType_ =
  RecordColumn'
    { mapping = Core.Nothing,
      name = pName_,
      sqlType = pSqlType_
    }

-- | Reference to the data element in the streaming input or the reference
-- data source. This element is required if the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_RecordFormat.html#analytics-Type-RecordFormat-RecordFormatTypel RecordFormatType>
-- is @JSON@.
recordColumn_mapping :: Lens.Lens' RecordColumn (Core.Maybe Core.Text)
recordColumn_mapping = Lens.lens (\RecordColumn' {mapping} -> mapping) (\s@RecordColumn' {} a -> s {mapping = a} :: RecordColumn)

-- | Name of the column created in the in-application input stream or
-- reference table.
recordColumn_name :: Lens.Lens' RecordColumn Core.Text
recordColumn_name = Lens.lens (\RecordColumn' {name} -> name) (\s@RecordColumn' {} a -> s {name = a} :: RecordColumn)

-- | Type of column created in the in-application input stream or reference
-- table.
recordColumn_sqlType :: Lens.Lens' RecordColumn Core.Text
recordColumn_sqlType = Lens.lens (\RecordColumn' {sqlType} -> sqlType) (\s@RecordColumn' {} a -> s {sqlType = a} :: RecordColumn)

instance Core.FromJSON RecordColumn where
  parseJSON =
    Core.withObject
      "RecordColumn"
      ( \x ->
          RecordColumn'
            Core.<$> (x Core..:? "Mapping")
            Core.<*> (x Core..: "Name")
            Core.<*> (x Core..: "SqlType")
      )

instance Core.Hashable RecordColumn

instance Core.NFData RecordColumn

instance Core.ToJSON RecordColumn where
  toJSON RecordColumn' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Mapping" Core..=) Core.<$> mapping,
            Core.Just ("Name" Core..= name),
            Core.Just ("SqlType" Core..= sqlType)
          ]
      )
