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
-- Module      : Amazonka.QuickSight.Types.DefaultFreeFormLayoutConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DefaultFreeFormLayoutConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FreeFormLayoutCanvasSizeOptions

-- | The options that determine the default settings of a free-form layout
-- configuration.
--
-- /See:/ 'newDefaultFreeFormLayoutConfiguration' smart constructor.
data DefaultFreeFormLayoutConfiguration = DefaultFreeFormLayoutConfiguration'
  { -- | Determines the screen canvas size options for a free-form layout.
    canvasSizeOptions :: FreeFormLayoutCanvasSizeOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultFreeFormLayoutConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'canvasSizeOptions', 'defaultFreeFormLayoutConfiguration_canvasSizeOptions' - Determines the screen canvas size options for a free-form layout.
newDefaultFreeFormLayoutConfiguration ::
  -- | 'canvasSizeOptions'
  FreeFormLayoutCanvasSizeOptions ->
  DefaultFreeFormLayoutConfiguration
newDefaultFreeFormLayoutConfiguration
  pCanvasSizeOptions_ =
    DefaultFreeFormLayoutConfiguration'
      { canvasSizeOptions =
          pCanvasSizeOptions_
      }

-- | Determines the screen canvas size options for a free-form layout.
defaultFreeFormLayoutConfiguration_canvasSizeOptions :: Lens.Lens' DefaultFreeFormLayoutConfiguration FreeFormLayoutCanvasSizeOptions
defaultFreeFormLayoutConfiguration_canvasSizeOptions = Lens.lens (\DefaultFreeFormLayoutConfiguration' {canvasSizeOptions} -> canvasSizeOptions) (\s@DefaultFreeFormLayoutConfiguration' {} a -> s {canvasSizeOptions = a} :: DefaultFreeFormLayoutConfiguration)

instance
  Data.FromJSON
    DefaultFreeFormLayoutConfiguration
  where
  parseJSON =
    Data.withObject
      "DefaultFreeFormLayoutConfiguration"
      ( \x ->
          DefaultFreeFormLayoutConfiguration'
            Prelude.<$> (x Data..: "CanvasSizeOptions")
      )

instance
  Prelude.Hashable
    DefaultFreeFormLayoutConfiguration
  where
  hashWithSalt
    _salt
    DefaultFreeFormLayoutConfiguration' {..} =
      _salt `Prelude.hashWithSalt` canvasSizeOptions

instance
  Prelude.NFData
    DefaultFreeFormLayoutConfiguration
  where
  rnf DefaultFreeFormLayoutConfiguration' {..} =
    Prelude.rnf canvasSizeOptions

instance
  Data.ToJSON
    DefaultFreeFormLayoutConfiguration
  where
  toJSON DefaultFreeFormLayoutConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CanvasSizeOptions" Data..= canvasSizeOptions)
          ]
      )
