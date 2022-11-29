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
-- Module      : Amazonka.KinesisAnalytics.Types.RecordColumn
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalytics.Types.RecordColumn where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

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
    mapping :: Prelude.Maybe Prelude.Text,
    -- | Name of the column created in the in-application input stream or
    -- reference table.
    name :: Prelude.Text,
    -- | Type of column created in the in-application input stream or reference
    -- table.
    sqlType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'sqlType'
  Prelude.Text ->
  RecordColumn
newRecordColumn pName_ pSqlType_ =
  RecordColumn'
    { mapping = Prelude.Nothing,
      name = pName_,
      sqlType = pSqlType_
    }

-- | Reference to the data element in the streaming input or the reference
-- data source. This element is required if the
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_RecordFormat.html#analytics-Type-RecordFormat-RecordFormatTypel RecordFormatType>
-- is @JSON@.
recordColumn_mapping :: Lens.Lens' RecordColumn (Prelude.Maybe Prelude.Text)
recordColumn_mapping = Lens.lens (\RecordColumn' {mapping} -> mapping) (\s@RecordColumn' {} a -> s {mapping = a} :: RecordColumn)

-- | Name of the column created in the in-application input stream or
-- reference table.
recordColumn_name :: Lens.Lens' RecordColumn Prelude.Text
recordColumn_name = Lens.lens (\RecordColumn' {name} -> name) (\s@RecordColumn' {} a -> s {name = a} :: RecordColumn)

-- | Type of column created in the in-application input stream or reference
-- table.
recordColumn_sqlType :: Lens.Lens' RecordColumn Prelude.Text
recordColumn_sqlType = Lens.lens (\RecordColumn' {sqlType} -> sqlType) (\s@RecordColumn' {} a -> s {sqlType = a} :: RecordColumn)

instance Core.FromJSON RecordColumn where
  parseJSON =
    Core.withObject
      "RecordColumn"
      ( \x ->
          RecordColumn'
            Prelude.<$> (x Core..:? "Mapping")
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> (x Core..: "SqlType")
      )

instance Prelude.Hashable RecordColumn where
  hashWithSalt _salt RecordColumn' {..} =
    _salt `Prelude.hashWithSalt` mapping
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sqlType

instance Prelude.NFData RecordColumn where
  rnf RecordColumn' {..} =
    Prelude.rnf mapping
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sqlType

instance Core.ToJSON RecordColumn where
  toJSON RecordColumn' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Mapping" Core..=) Prelude.<$> mapping,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("SqlType" Core..= sqlType)
          ]
      )
