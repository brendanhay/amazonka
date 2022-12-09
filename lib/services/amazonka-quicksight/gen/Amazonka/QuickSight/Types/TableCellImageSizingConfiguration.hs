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
-- Module      : Amazonka.QuickSight.Types.TableCellImageSizingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableCellImageSizingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TableCellImageScalingConfiguration

-- | The sizing options for the table image configuration.
--
-- /See:/ 'newTableCellImageSizingConfiguration' smart constructor.
data TableCellImageSizingConfiguration = TableCellImageSizingConfiguration'
  { -- | The cell scaling configuration of the sizing options for the table image
    -- configuration.
    tableCellImageScalingConfiguration :: Prelude.Maybe TableCellImageScalingConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableCellImageSizingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableCellImageScalingConfiguration', 'tableCellImageSizingConfiguration_tableCellImageScalingConfiguration' - The cell scaling configuration of the sizing options for the table image
-- configuration.
newTableCellImageSizingConfiguration ::
  TableCellImageSizingConfiguration
newTableCellImageSizingConfiguration =
  TableCellImageSizingConfiguration'
    { tableCellImageScalingConfiguration =
        Prelude.Nothing
    }

-- | The cell scaling configuration of the sizing options for the table image
-- configuration.
tableCellImageSizingConfiguration_tableCellImageScalingConfiguration :: Lens.Lens' TableCellImageSizingConfiguration (Prelude.Maybe TableCellImageScalingConfiguration)
tableCellImageSizingConfiguration_tableCellImageScalingConfiguration = Lens.lens (\TableCellImageSizingConfiguration' {tableCellImageScalingConfiguration} -> tableCellImageScalingConfiguration) (\s@TableCellImageSizingConfiguration' {} a -> s {tableCellImageScalingConfiguration = a} :: TableCellImageSizingConfiguration)

instance
  Data.FromJSON
    TableCellImageSizingConfiguration
  where
  parseJSON =
    Data.withObject
      "TableCellImageSizingConfiguration"
      ( \x ->
          TableCellImageSizingConfiguration'
            Prelude.<$> (x Data..:? "TableCellImageScalingConfiguration")
      )

instance
  Prelude.Hashable
    TableCellImageSizingConfiguration
  where
  hashWithSalt
    _salt
    TableCellImageSizingConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` tableCellImageScalingConfiguration

instance
  Prelude.NFData
    TableCellImageSizingConfiguration
  where
  rnf TableCellImageSizingConfiguration' {..} =
    Prelude.rnf tableCellImageScalingConfiguration

instance
  Data.ToJSON
    TableCellImageSizingConfiguration
  where
  toJSON TableCellImageSizingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TableCellImageScalingConfiguration" Data..=)
              Prelude.<$> tableCellImageScalingConfiguration
          ]
      )
