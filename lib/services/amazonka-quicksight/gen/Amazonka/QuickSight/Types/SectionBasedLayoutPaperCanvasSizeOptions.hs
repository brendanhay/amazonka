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
-- Module      : Amazonka.QuickSight.Types.SectionBasedLayoutPaperCanvasSizeOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SectionBasedLayoutPaperCanvasSizeOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.PaperOrientation
import Amazonka.QuickSight.Types.PaperSize
import Amazonka.QuickSight.Types.Spacing

-- | The options for a paper canvas of a section-based layout.
--
-- /See:/ 'newSectionBasedLayoutPaperCanvasSizeOptions' smart constructor.
data SectionBasedLayoutPaperCanvasSizeOptions = SectionBasedLayoutPaperCanvasSizeOptions'
  { -- | Defines the spacing between the canvas content and the top, bottom,
    -- left, and right edges.
    paperMargin :: Prelude.Maybe Spacing,
    -- | The paper orientation that is used to define canvas dimensions. Choose
    -- one of the following options:
    --
    -- -   PORTRAIT
    --
    -- -   LANDSCAPE
    paperOrientation :: Prelude.Maybe PaperOrientation,
    -- | The paper size that is used to define canvas dimensions.
    paperSize :: Prelude.Maybe PaperSize
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SectionBasedLayoutPaperCanvasSizeOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'paperMargin', 'sectionBasedLayoutPaperCanvasSizeOptions_paperMargin' - Defines the spacing between the canvas content and the top, bottom,
-- left, and right edges.
--
-- 'paperOrientation', 'sectionBasedLayoutPaperCanvasSizeOptions_paperOrientation' - The paper orientation that is used to define canvas dimensions. Choose
-- one of the following options:
--
-- -   PORTRAIT
--
-- -   LANDSCAPE
--
-- 'paperSize', 'sectionBasedLayoutPaperCanvasSizeOptions_paperSize' - The paper size that is used to define canvas dimensions.
newSectionBasedLayoutPaperCanvasSizeOptions ::
  SectionBasedLayoutPaperCanvasSizeOptions
newSectionBasedLayoutPaperCanvasSizeOptions =
  SectionBasedLayoutPaperCanvasSizeOptions'
    { paperMargin =
        Prelude.Nothing,
      paperOrientation =
        Prelude.Nothing,
      paperSize = Prelude.Nothing
    }

-- | Defines the spacing between the canvas content and the top, bottom,
-- left, and right edges.
sectionBasedLayoutPaperCanvasSizeOptions_paperMargin :: Lens.Lens' SectionBasedLayoutPaperCanvasSizeOptions (Prelude.Maybe Spacing)
sectionBasedLayoutPaperCanvasSizeOptions_paperMargin = Lens.lens (\SectionBasedLayoutPaperCanvasSizeOptions' {paperMargin} -> paperMargin) (\s@SectionBasedLayoutPaperCanvasSizeOptions' {} a -> s {paperMargin = a} :: SectionBasedLayoutPaperCanvasSizeOptions)

-- | The paper orientation that is used to define canvas dimensions. Choose
-- one of the following options:
--
-- -   PORTRAIT
--
-- -   LANDSCAPE
sectionBasedLayoutPaperCanvasSizeOptions_paperOrientation :: Lens.Lens' SectionBasedLayoutPaperCanvasSizeOptions (Prelude.Maybe PaperOrientation)
sectionBasedLayoutPaperCanvasSizeOptions_paperOrientation = Lens.lens (\SectionBasedLayoutPaperCanvasSizeOptions' {paperOrientation} -> paperOrientation) (\s@SectionBasedLayoutPaperCanvasSizeOptions' {} a -> s {paperOrientation = a} :: SectionBasedLayoutPaperCanvasSizeOptions)

-- | The paper size that is used to define canvas dimensions.
sectionBasedLayoutPaperCanvasSizeOptions_paperSize :: Lens.Lens' SectionBasedLayoutPaperCanvasSizeOptions (Prelude.Maybe PaperSize)
sectionBasedLayoutPaperCanvasSizeOptions_paperSize = Lens.lens (\SectionBasedLayoutPaperCanvasSizeOptions' {paperSize} -> paperSize) (\s@SectionBasedLayoutPaperCanvasSizeOptions' {} a -> s {paperSize = a} :: SectionBasedLayoutPaperCanvasSizeOptions)

instance
  Data.FromJSON
    SectionBasedLayoutPaperCanvasSizeOptions
  where
  parseJSON =
    Data.withObject
      "SectionBasedLayoutPaperCanvasSizeOptions"
      ( \x ->
          SectionBasedLayoutPaperCanvasSizeOptions'
            Prelude.<$> (x Data..:? "PaperMargin")
            Prelude.<*> (x Data..:? "PaperOrientation")
            Prelude.<*> (x Data..:? "PaperSize")
      )

instance
  Prelude.Hashable
    SectionBasedLayoutPaperCanvasSizeOptions
  where
  hashWithSalt
    _salt
    SectionBasedLayoutPaperCanvasSizeOptions' {..} =
      _salt
        `Prelude.hashWithSalt` paperMargin
        `Prelude.hashWithSalt` paperOrientation
        `Prelude.hashWithSalt` paperSize

instance
  Prelude.NFData
    SectionBasedLayoutPaperCanvasSizeOptions
  where
  rnf SectionBasedLayoutPaperCanvasSizeOptions' {..} =
    Prelude.rnf paperMargin
      `Prelude.seq` Prelude.rnf paperOrientation
      `Prelude.seq` Prelude.rnf paperSize

instance
  Data.ToJSON
    SectionBasedLayoutPaperCanvasSizeOptions
  where
  toJSON SectionBasedLayoutPaperCanvasSizeOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PaperMargin" Data..=) Prelude.<$> paperMargin,
            ("PaperOrientation" Data..=)
              Prelude.<$> paperOrientation,
            ("PaperSize" Data..=) Prelude.<$> paperSize
          ]
      )
