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
-- Module      : Amazonka.HoneyCode.Types.DataItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.DataItem where

import qualified Amazonka.Core as Core
import Amazonka.HoneyCode.Types.Format
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The data in a particular data cell defined on the screen.
--
-- /See:/ 'newDataItem' smart constructor.
data DataItem = DataItem'
  { -- | The raw value of the data. e.g. jsmith\@example.com
    rawValue :: Prelude.Maybe Prelude.Text,
    -- | The overrideFormat is optional and is specified only if a particular row
    -- of data has a different format for the data than the default format
    -- defined on the screen or the table.
    overrideFormat :: Prelude.Maybe Format,
    -- | The formatted value of the data. e.g. John Smith.
    formattedValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rawValue', 'dataItem_rawValue' - The raw value of the data. e.g. jsmith\@example.com
--
-- 'overrideFormat', 'dataItem_overrideFormat' - The overrideFormat is optional and is specified only if a particular row
-- of data has a different format for the data than the default format
-- defined on the screen or the table.
--
-- 'formattedValue', 'dataItem_formattedValue' - The formatted value of the data. e.g. John Smith.
newDataItem ::
  DataItem
newDataItem =
  DataItem'
    { rawValue = Prelude.Nothing,
      overrideFormat = Prelude.Nothing,
      formattedValue = Prelude.Nothing
    }

-- | The raw value of the data. e.g. jsmith\@example.com
dataItem_rawValue :: Lens.Lens' DataItem (Prelude.Maybe Prelude.Text)
dataItem_rawValue = Lens.lens (\DataItem' {rawValue} -> rawValue) (\s@DataItem' {} a -> s {rawValue = a} :: DataItem)

-- | The overrideFormat is optional and is specified only if a particular row
-- of data has a different format for the data than the default format
-- defined on the screen or the table.
dataItem_overrideFormat :: Lens.Lens' DataItem (Prelude.Maybe Format)
dataItem_overrideFormat = Lens.lens (\DataItem' {overrideFormat} -> overrideFormat) (\s@DataItem' {} a -> s {overrideFormat = a} :: DataItem)

-- | The formatted value of the data. e.g. John Smith.
dataItem_formattedValue :: Lens.Lens' DataItem (Prelude.Maybe Prelude.Text)
dataItem_formattedValue = Lens.lens (\DataItem' {formattedValue} -> formattedValue) (\s@DataItem' {} a -> s {formattedValue = a} :: DataItem)

instance Core.FromJSON DataItem where
  parseJSON =
    Core.withObject
      "DataItem"
      ( \x ->
          DataItem'
            Prelude.<$> (x Core..:? "rawValue")
            Prelude.<*> (x Core..:? "overrideFormat")
            Prelude.<*> (x Core..:? "formattedValue")
      )

instance Prelude.Hashable DataItem

instance Prelude.NFData DataItem
