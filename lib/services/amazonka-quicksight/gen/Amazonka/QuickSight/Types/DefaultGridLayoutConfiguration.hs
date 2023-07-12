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
-- Module      : Amazonka.QuickSight.Types.DefaultGridLayoutConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DefaultGridLayoutConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.GridLayoutCanvasSizeOptions

-- | The options that determine the default settings for a grid layout
-- configuration.
--
-- /See:/ 'newDefaultGridLayoutConfiguration' smart constructor.
data DefaultGridLayoutConfiguration = DefaultGridLayoutConfiguration'
  { -- | Determines the screen canvas size options for a grid layout.
    canvasSizeOptions :: GridLayoutCanvasSizeOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultGridLayoutConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'canvasSizeOptions', 'defaultGridLayoutConfiguration_canvasSizeOptions' - Determines the screen canvas size options for a grid layout.
newDefaultGridLayoutConfiguration ::
  -- | 'canvasSizeOptions'
  GridLayoutCanvasSizeOptions ->
  DefaultGridLayoutConfiguration
newDefaultGridLayoutConfiguration pCanvasSizeOptions_ =
  DefaultGridLayoutConfiguration'
    { canvasSizeOptions =
        pCanvasSizeOptions_
    }

-- | Determines the screen canvas size options for a grid layout.
defaultGridLayoutConfiguration_canvasSizeOptions :: Lens.Lens' DefaultGridLayoutConfiguration GridLayoutCanvasSizeOptions
defaultGridLayoutConfiguration_canvasSizeOptions = Lens.lens (\DefaultGridLayoutConfiguration' {canvasSizeOptions} -> canvasSizeOptions) (\s@DefaultGridLayoutConfiguration' {} a -> s {canvasSizeOptions = a} :: DefaultGridLayoutConfiguration)

instance Data.FromJSON DefaultGridLayoutConfiguration where
  parseJSON =
    Data.withObject
      "DefaultGridLayoutConfiguration"
      ( \x ->
          DefaultGridLayoutConfiguration'
            Prelude.<$> (x Data..: "CanvasSizeOptions")
      )

instance
  Prelude.Hashable
    DefaultGridLayoutConfiguration
  where
  hashWithSalt
    _salt
    DefaultGridLayoutConfiguration' {..} =
      _salt `Prelude.hashWithSalt` canvasSizeOptions

instance
  Prelude.NFData
    DefaultGridLayoutConfiguration
  where
  rnf DefaultGridLayoutConfiguration' {..} =
    Prelude.rnf canvasSizeOptions

instance Data.ToJSON DefaultGridLayoutConfiguration where
  toJSON DefaultGridLayoutConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CanvasSizeOptions" Data..= canvasSizeOptions)
          ]
      )
