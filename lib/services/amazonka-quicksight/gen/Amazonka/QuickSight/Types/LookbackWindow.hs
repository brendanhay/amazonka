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
-- Module      : Amazonka.QuickSight.Types.LookbackWindow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LookbackWindow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.LookbackWindowSizeUnit

-- | The lookback window setup of an incremental refresh configuration.
--
-- /See:/ 'newLookbackWindow' smart constructor.
data LookbackWindow = LookbackWindow'
  { -- | The name of the lookback window column.
    columnName :: Prelude.Text,
    -- | The lookback window column size.
    size :: Prelude.Natural,
    -- | The size unit that is used for the lookback window column. Valid values
    -- for this structure are @HOUR@, @DAY@, and @WEEK@.
    sizeUnit :: LookbackWindowSizeUnit
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LookbackWindow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnName', 'lookbackWindow_columnName' - The name of the lookback window column.
--
-- 'size', 'lookbackWindow_size' - The lookback window column size.
--
-- 'sizeUnit', 'lookbackWindow_sizeUnit' - The size unit that is used for the lookback window column. Valid values
-- for this structure are @HOUR@, @DAY@, and @WEEK@.
newLookbackWindow ::
  -- | 'columnName'
  Prelude.Text ->
  -- | 'size'
  Prelude.Natural ->
  -- | 'sizeUnit'
  LookbackWindowSizeUnit ->
  LookbackWindow
newLookbackWindow pColumnName_ pSize_ pSizeUnit_ =
  LookbackWindow'
    { columnName = pColumnName_,
      size = pSize_,
      sizeUnit = pSizeUnit_
    }

-- | The name of the lookback window column.
lookbackWindow_columnName :: Lens.Lens' LookbackWindow Prelude.Text
lookbackWindow_columnName = Lens.lens (\LookbackWindow' {columnName} -> columnName) (\s@LookbackWindow' {} a -> s {columnName = a} :: LookbackWindow)

-- | The lookback window column size.
lookbackWindow_size :: Lens.Lens' LookbackWindow Prelude.Natural
lookbackWindow_size = Lens.lens (\LookbackWindow' {size} -> size) (\s@LookbackWindow' {} a -> s {size = a} :: LookbackWindow)

-- | The size unit that is used for the lookback window column. Valid values
-- for this structure are @HOUR@, @DAY@, and @WEEK@.
lookbackWindow_sizeUnit :: Lens.Lens' LookbackWindow LookbackWindowSizeUnit
lookbackWindow_sizeUnit = Lens.lens (\LookbackWindow' {sizeUnit} -> sizeUnit) (\s@LookbackWindow' {} a -> s {sizeUnit = a} :: LookbackWindow)

instance Data.FromJSON LookbackWindow where
  parseJSON =
    Data.withObject
      "LookbackWindow"
      ( \x ->
          LookbackWindow'
            Prelude.<$> (x Data..: "ColumnName")
            Prelude.<*> (x Data..: "Size")
            Prelude.<*> (x Data..: "SizeUnit")
      )

instance Prelude.Hashable LookbackWindow where
  hashWithSalt _salt LookbackWindow' {..} =
    _salt
      `Prelude.hashWithSalt` columnName
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` sizeUnit

instance Prelude.NFData LookbackWindow where
  rnf LookbackWindow' {..} =
    Prelude.rnf columnName
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf sizeUnit

instance Data.ToJSON LookbackWindow where
  toJSON LookbackWindow' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ColumnName" Data..= columnName),
            Prelude.Just ("Size" Data..= size),
            Prelude.Just ("SizeUnit" Data..= sizeUnit)
          ]
      )
