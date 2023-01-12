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
-- Module      : Amazonka.QuickSight.Types.FreeFormLayoutCanvasSizeOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FreeFormLayoutCanvasSizeOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FreeFormLayoutScreenCanvasSizeOptions

-- | Configuration options for the canvas of a free-form layout.
--
-- /See:/ 'newFreeFormLayoutCanvasSizeOptions' smart constructor.
data FreeFormLayoutCanvasSizeOptions = FreeFormLayoutCanvasSizeOptions'
  { -- | The options that determine the sizing of the canvas used in a free-form
    -- layout.
    screenCanvasSizeOptions :: Prelude.Maybe FreeFormLayoutScreenCanvasSizeOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FreeFormLayoutCanvasSizeOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'screenCanvasSizeOptions', 'freeFormLayoutCanvasSizeOptions_screenCanvasSizeOptions' - The options that determine the sizing of the canvas used in a free-form
-- layout.
newFreeFormLayoutCanvasSizeOptions ::
  FreeFormLayoutCanvasSizeOptions
newFreeFormLayoutCanvasSizeOptions =
  FreeFormLayoutCanvasSizeOptions'
    { screenCanvasSizeOptions =
        Prelude.Nothing
    }

-- | The options that determine the sizing of the canvas used in a free-form
-- layout.
freeFormLayoutCanvasSizeOptions_screenCanvasSizeOptions :: Lens.Lens' FreeFormLayoutCanvasSizeOptions (Prelude.Maybe FreeFormLayoutScreenCanvasSizeOptions)
freeFormLayoutCanvasSizeOptions_screenCanvasSizeOptions = Lens.lens (\FreeFormLayoutCanvasSizeOptions' {screenCanvasSizeOptions} -> screenCanvasSizeOptions) (\s@FreeFormLayoutCanvasSizeOptions' {} a -> s {screenCanvasSizeOptions = a} :: FreeFormLayoutCanvasSizeOptions)

instance
  Data.FromJSON
    FreeFormLayoutCanvasSizeOptions
  where
  parseJSON =
    Data.withObject
      "FreeFormLayoutCanvasSizeOptions"
      ( \x ->
          FreeFormLayoutCanvasSizeOptions'
            Prelude.<$> (x Data..:? "ScreenCanvasSizeOptions")
      )

instance
  Prelude.Hashable
    FreeFormLayoutCanvasSizeOptions
  where
  hashWithSalt
    _salt
    FreeFormLayoutCanvasSizeOptions' {..} =
      _salt
        `Prelude.hashWithSalt` screenCanvasSizeOptions

instance
  Prelude.NFData
    FreeFormLayoutCanvasSizeOptions
  where
  rnf FreeFormLayoutCanvasSizeOptions' {..} =
    Prelude.rnf screenCanvasSizeOptions

instance Data.ToJSON FreeFormLayoutCanvasSizeOptions where
  toJSON FreeFormLayoutCanvasSizeOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ScreenCanvasSizeOptions" Data..=)
              Prelude.<$> screenCanvasSizeOptions
          ]
      )
