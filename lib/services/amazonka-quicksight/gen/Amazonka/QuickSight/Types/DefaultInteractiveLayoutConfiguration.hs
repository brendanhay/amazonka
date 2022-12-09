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
-- Module      : Amazonka.QuickSight.Types.DefaultInteractiveLayoutConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DefaultInteractiveLayoutConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DefaultFreeFormLayoutConfiguration
import Amazonka.QuickSight.Types.DefaultGridLayoutConfiguration

-- | The options that determine the default settings for interactive layout
-- configuration.
--
-- /See:/ 'newDefaultInteractiveLayoutConfiguration' smart constructor.
data DefaultInteractiveLayoutConfiguration = DefaultInteractiveLayoutConfiguration'
  { -- | The options that determine the default settings of a free-form layout
    -- configuration.
    freeForm :: Prelude.Maybe DefaultFreeFormLayoutConfiguration,
    -- | The options that determine the default settings for a grid layout
    -- configuration.
    grid :: Prelude.Maybe DefaultGridLayoutConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultInteractiveLayoutConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'freeForm', 'defaultInteractiveLayoutConfiguration_freeForm' - The options that determine the default settings of a free-form layout
-- configuration.
--
-- 'grid', 'defaultInteractiveLayoutConfiguration_grid' - The options that determine the default settings for a grid layout
-- configuration.
newDefaultInteractiveLayoutConfiguration ::
  DefaultInteractiveLayoutConfiguration
newDefaultInteractiveLayoutConfiguration =
  DefaultInteractiveLayoutConfiguration'
    { freeForm =
        Prelude.Nothing,
      grid = Prelude.Nothing
    }

-- | The options that determine the default settings of a free-form layout
-- configuration.
defaultInteractiveLayoutConfiguration_freeForm :: Lens.Lens' DefaultInteractiveLayoutConfiguration (Prelude.Maybe DefaultFreeFormLayoutConfiguration)
defaultInteractiveLayoutConfiguration_freeForm = Lens.lens (\DefaultInteractiveLayoutConfiguration' {freeForm} -> freeForm) (\s@DefaultInteractiveLayoutConfiguration' {} a -> s {freeForm = a} :: DefaultInteractiveLayoutConfiguration)

-- | The options that determine the default settings for a grid layout
-- configuration.
defaultInteractiveLayoutConfiguration_grid :: Lens.Lens' DefaultInteractiveLayoutConfiguration (Prelude.Maybe DefaultGridLayoutConfiguration)
defaultInteractiveLayoutConfiguration_grid = Lens.lens (\DefaultInteractiveLayoutConfiguration' {grid} -> grid) (\s@DefaultInteractiveLayoutConfiguration' {} a -> s {grid = a} :: DefaultInteractiveLayoutConfiguration)

instance
  Data.FromJSON
    DefaultInteractiveLayoutConfiguration
  where
  parseJSON =
    Data.withObject
      "DefaultInteractiveLayoutConfiguration"
      ( \x ->
          DefaultInteractiveLayoutConfiguration'
            Prelude.<$> (x Data..:? "FreeForm")
            Prelude.<*> (x Data..:? "Grid")
      )

instance
  Prelude.Hashable
    DefaultInteractiveLayoutConfiguration
  where
  hashWithSalt
    _salt
    DefaultInteractiveLayoutConfiguration' {..} =
      _salt `Prelude.hashWithSalt` freeForm
        `Prelude.hashWithSalt` grid

instance
  Prelude.NFData
    DefaultInteractiveLayoutConfiguration
  where
  rnf DefaultInteractiveLayoutConfiguration' {..} =
    Prelude.rnf freeForm `Prelude.seq` Prelude.rnf grid

instance
  Data.ToJSON
    DefaultInteractiveLayoutConfiguration
  where
  toJSON DefaultInteractiveLayoutConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FreeForm" Data..=) Prelude.<$> freeForm,
            ("Grid" Data..=) Prelude.<$> grid
          ]
      )
