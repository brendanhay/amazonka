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
-- Module      : Amazonka.FinSpaceData.Types.ColumnDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FinSpaceData.Types.ColumnDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types.ColumnDataType
import qualified Amazonka.Prelude as Prelude

-- | The definition of a column in a tabular Dataset.
--
-- /See:/ 'newColumnDefinition' smart constructor.
data ColumnDefinition = ColumnDefinition'
  { -- | Description for a column.
    columnDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of a column.
    columnName :: Prelude.Maybe Prelude.Text,
    -- | Data type of a column.
    --
    -- -   @STRING@ – A String data type.
    --
    --     @CHAR@ – A char data type.
    --
    --     @INTEGER@ – An integer data type.
    --
    --     @TINYINT@ – A tinyint data type.
    --
    --     @SMALLINT@ – A smallint data type.
    --
    --     @BIGINT@ – A bigint data type.
    --
    --     @FLOAT@ – A float data type.
    --
    --     @DOUBLE@ – A double data type.
    --
    --     @DATE@ – A date data type.
    --
    --     @DATETIME@ – A datetime data type.
    --
    --     @BOOLEAN@ – A boolean data type.
    --
    --     @BINARY@ – A binary data type.
    dataType :: Prelude.Maybe ColumnDataType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnDescription', 'columnDefinition_columnDescription' - Description for a column.
--
-- 'columnName', 'columnDefinition_columnName' - The name of a column.
--
-- 'dataType', 'columnDefinition_dataType' - Data type of a column.
--
-- -   @STRING@ – A String data type.
--
--     @CHAR@ – A char data type.
--
--     @INTEGER@ – An integer data type.
--
--     @TINYINT@ – A tinyint data type.
--
--     @SMALLINT@ – A smallint data type.
--
--     @BIGINT@ – A bigint data type.
--
--     @FLOAT@ – A float data type.
--
--     @DOUBLE@ – A double data type.
--
--     @DATE@ – A date data type.
--
--     @DATETIME@ – A datetime data type.
--
--     @BOOLEAN@ – A boolean data type.
--
--     @BINARY@ – A binary data type.
newColumnDefinition ::
  ColumnDefinition
newColumnDefinition =
  ColumnDefinition'
    { columnDescription =
        Prelude.Nothing,
      columnName = Prelude.Nothing,
      dataType = Prelude.Nothing
    }

-- | Description for a column.
columnDefinition_columnDescription :: Lens.Lens' ColumnDefinition (Prelude.Maybe Prelude.Text)
columnDefinition_columnDescription = Lens.lens (\ColumnDefinition' {columnDescription} -> columnDescription) (\s@ColumnDefinition' {} a -> s {columnDescription = a} :: ColumnDefinition)

-- | The name of a column.
columnDefinition_columnName :: Lens.Lens' ColumnDefinition (Prelude.Maybe Prelude.Text)
columnDefinition_columnName = Lens.lens (\ColumnDefinition' {columnName} -> columnName) (\s@ColumnDefinition' {} a -> s {columnName = a} :: ColumnDefinition)

-- | Data type of a column.
--
-- -   @STRING@ – A String data type.
--
--     @CHAR@ – A char data type.
--
--     @INTEGER@ – An integer data type.
--
--     @TINYINT@ – A tinyint data type.
--
--     @SMALLINT@ – A smallint data type.
--
--     @BIGINT@ – A bigint data type.
--
--     @FLOAT@ – A float data type.
--
--     @DOUBLE@ – A double data type.
--
--     @DATE@ – A date data type.
--
--     @DATETIME@ – A datetime data type.
--
--     @BOOLEAN@ – A boolean data type.
--
--     @BINARY@ – A binary data type.
columnDefinition_dataType :: Lens.Lens' ColumnDefinition (Prelude.Maybe ColumnDataType)
columnDefinition_dataType = Lens.lens (\ColumnDefinition' {dataType} -> dataType) (\s@ColumnDefinition' {} a -> s {dataType = a} :: ColumnDefinition)

instance Data.FromJSON ColumnDefinition where
  parseJSON =
    Data.withObject
      "ColumnDefinition"
      ( \x ->
          ColumnDefinition'
            Prelude.<$> (x Data..:? "columnDescription")
            Prelude.<*> (x Data..:? "columnName")
            Prelude.<*> (x Data..:? "dataType")
      )

instance Prelude.Hashable ColumnDefinition where
  hashWithSalt _salt ColumnDefinition' {..} =
    _salt `Prelude.hashWithSalt` columnDescription
      `Prelude.hashWithSalt` columnName
      `Prelude.hashWithSalt` dataType

instance Prelude.NFData ColumnDefinition where
  rnf ColumnDefinition' {..} =
    Prelude.rnf columnDescription
      `Prelude.seq` Prelude.rnf columnName
      `Prelude.seq` Prelude.rnf dataType

instance Data.ToJSON ColumnDefinition where
  toJSON ColumnDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("columnDescription" Data..=)
              Prelude.<$> columnDescription,
            ("columnName" Data..=) Prelude.<$> columnName,
            ("dataType" Data..=) Prelude.<$> dataType
          ]
      )
