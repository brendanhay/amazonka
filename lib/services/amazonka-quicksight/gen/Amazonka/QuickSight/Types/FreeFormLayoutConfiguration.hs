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
-- Module      : Amazonka.QuickSight.Types.FreeFormLayoutConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FreeFormLayoutConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FreeFormLayoutCanvasSizeOptions
import Amazonka.QuickSight.Types.FreeFormLayoutElement

-- | The configuration of a free-form layout.
--
-- /See:/ 'newFreeFormLayoutConfiguration' smart constructor.
data FreeFormLayoutConfiguration = FreeFormLayoutConfiguration'
  { canvasSizeOptions :: Prelude.Maybe FreeFormLayoutCanvasSizeOptions,
    -- | The elements that are included in a free-form layout.
    elements :: [FreeFormLayoutElement]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FreeFormLayoutConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'canvasSizeOptions', 'freeFormLayoutConfiguration_canvasSizeOptions' - Undocumented member.
--
-- 'elements', 'freeFormLayoutConfiguration_elements' - The elements that are included in a free-form layout.
newFreeFormLayoutConfiguration ::
  FreeFormLayoutConfiguration
newFreeFormLayoutConfiguration =
  FreeFormLayoutConfiguration'
    { canvasSizeOptions =
        Prelude.Nothing,
      elements = Prelude.mempty
    }

-- | Undocumented member.
freeFormLayoutConfiguration_canvasSizeOptions :: Lens.Lens' FreeFormLayoutConfiguration (Prelude.Maybe FreeFormLayoutCanvasSizeOptions)
freeFormLayoutConfiguration_canvasSizeOptions = Lens.lens (\FreeFormLayoutConfiguration' {canvasSizeOptions} -> canvasSizeOptions) (\s@FreeFormLayoutConfiguration' {} a -> s {canvasSizeOptions = a} :: FreeFormLayoutConfiguration)

-- | The elements that are included in a free-form layout.
freeFormLayoutConfiguration_elements :: Lens.Lens' FreeFormLayoutConfiguration [FreeFormLayoutElement]
freeFormLayoutConfiguration_elements = Lens.lens (\FreeFormLayoutConfiguration' {elements} -> elements) (\s@FreeFormLayoutConfiguration' {} a -> s {elements = a} :: FreeFormLayoutConfiguration) Prelude.. Lens.coerced

instance Data.FromJSON FreeFormLayoutConfiguration where
  parseJSON =
    Data.withObject
      "FreeFormLayoutConfiguration"
      ( \x ->
          FreeFormLayoutConfiguration'
            Prelude.<$> (x Data..:? "CanvasSizeOptions")
            Prelude.<*> (x Data..:? "Elements" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FreeFormLayoutConfiguration where
  hashWithSalt _salt FreeFormLayoutConfiguration' {..} =
    _salt `Prelude.hashWithSalt` canvasSizeOptions
      `Prelude.hashWithSalt` elements

instance Prelude.NFData FreeFormLayoutConfiguration where
  rnf FreeFormLayoutConfiguration' {..} =
    Prelude.rnf canvasSizeOptions
      `Prelude.seq` Prelude.rnf elements

instance Data.ToJSON FreeFormLayoutConfiguration where
  toJSON FreeFormLayoutConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CanvasSizeOptions" Data..=)
              Prelude.<$> canvasSizeOptions,
            Prelude.Just ("Elements" Data..= elements)
          ]
      )
