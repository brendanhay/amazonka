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
-- Module      : Amazonka.QuickSight.Types.TableFieldImageConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableFieldImageConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TableCellImageSizingConfiguration

-- | The image configuration of a table field URL.
--
-- /See:/ 'newTableFieldImageConfiguration' smart constructor.
data TableFieldImageConfiguration = TableFieldImageConfiguration'
  { -- | The sizing options for the table image configuration.
    sizingOptions :: Prelude.Maybe TableCellImageSizingConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableFieldImageConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizingOptions', 'tableFieldImageConfiguration_sizingOptions' - The sizing options for the table image configuration.
newTableFieldImageConfiguration ::
  TableFieldImageConfiguration
newTableFieldImageConfiguration =
  TableFieldImageConfiguration'
    { sizingOptions =
        Prelude.Nothing
    }

-- | The sizing options for the table image configuration.
tableFieldImageConfiguration_sizingOptions :: Lens.Lens' TableFieldImageConfiguration (Prelude.Maybe TableCellImageSizingConfiguration)
tableFieldImageConfiguration_sizingOptions = Lens.lens (\TableFieldImageConfiguration' {sizingOptions} -> sizingOptions) (\s@TableFieldImageConfiguration' {} a -> s {sizingOptions = a} :: TableFieldImageConfiguration)

instance Data.FromJSON TableFieldImageConfiguration where
  parseJSON =
    Data.withObject
      "TableFieldImageConfiguration"
      ( \x ->
          TableFieldImageConfiguration'
            Prelude.<$> (x Data..:? "SizingOptions")
      )

instance
  Prelude.Hashable
    TableFieldImageConfiguration
  where
  hashWithSalt _salt TableFieldImageConfiguration' {..} =
    _salt `Prelude.hashWithSalt` sizingOptions

instance Prelude.NFData TableFieldImageConfiguration where
  rnf TableFieldImageConfiguration' {..} =
    Prelude.rnf sizingOptions

instance Data.ToJSON TableFieldImageConfiguration where
  toJSON TableFieldImageConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SizingOptions" Data..=)
              Prelude.<$> sizingOptions
          ]
      )
