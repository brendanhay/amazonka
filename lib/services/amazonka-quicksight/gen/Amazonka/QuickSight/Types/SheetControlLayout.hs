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
-- Module      : Amazonka.QuickSight.Types.SheetControlLayout
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SheetControlLayout where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.SheetControlLayoutConfiguration

-- | A grid layout to define the placement of sheet control.
--
-- /See:/ 'newSheetControlLayout' smart constructor.
data SheetControlLayout = SheetControlLayout'
  { -- | The configuration that determines the elements and canvas size options
    -- of sheet control.
    configuration :: SheetControlLayoutConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SheetControlLayout' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuration', 'sheetControlLayout_configuration' - The configuration that determines the elements and canvas size options
-- of sheet control.
newSheetControlLayout ::
  -- | 'configuration'
  SheetControlLayoutConfiguration ->
  SheetControlLayout
newSheetControlLayout pConfiguration_ =
  SheetControlLayout'
    { configuration =
        pConfiguration_
    }

-- | The configuration that determines the elements and canvas size options
-- of sheet control.
sheetControlLayout_configuration :: Lens.Lens' SheetControlLayout SheetControlLayoutConfiguration
sheetControlLayout_configuration = Lens.lens (\SheetControlLayout' {configuration} -> configuration) (\s@SheetControlLayout' {} a -> s {configuration = a} :: SheetControlLayout)

instance Data.FromJSON SheetControlLayout where
  parseJSON =
    Data.withObject
      "SheetControlLayout"
      ( \x ->
          SheetControlLayout'
            Prelude.<$> (x Data..: "Configuration")
      )

instance Prelude.Hashable SheetControlLayout where
  hashWithSalt _salt SheetControlLayout' {..} =
    _salt `Prelude.hashWithSalt` configuration

instance Prelude.NFData SheetControlLayout where
  rnf SheetControlLayout' {..} =
    Prelude.rnf configuration

instance Data.ToJSON SheetControlLayout where
  toJSON SheetControlLayout' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("Configuration" Data..= configuration)
          ]
      )
