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
-- Module      : Amazonka.QuickSight.Types.FreeFormLayoutElement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FreeFormLayoutElement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FreeFormLayoutElementBackgroundStyle
import Amazonka.QuickSight.Types.FreeFormLayoutElementBorderStyle
import Amazonka.QuickSight.Types.LayoutElementType
import Amazonka.QuickSight.Types.LoadingAnimation
import Amazonka.QuickSight.Types.SheetElementRenderingRule
import Amazonka.QuickSight.Types.Visibility

-- | An element within a free-form layout.
--
-- /See:/ 'newFreeFormLayoutElement' smart constructor.
data FreeFormLayoutElement = FreeFormLayoutElement'
  { -- | The background style configuration of a free-form layout element.
    backgroundStyle :: Prelude.Maybe FreeFormLayoutElementBackgroundStyle,
    -- | The border style configuration of a free-form layout element.
    borderStyle :: Prelude.Maybe FreeFormLayoutElementBorderStyle,
    -- | The loading animation configuration of a free-form layout element.
    loadingAnimation :: Prelude.Maybe LoadingAnimation,
    -- | The rendering rules that determine when an element should be displayed
    -- within a free-form layout.
    renderingRules :: Prelude.Maybe [SheetElementRenderingRule],
    -- | The border style configuration of a free-form layout element. This
    -- border style is used when the element is selected.
    selectedBorderStyle :: Prelude.Maybe FreeFormLayoutElementBorderStyle,
    -- | The visibility of an element within a free-form layout.
    visibility :: Prelude.Maybe Visibility,
    -- | A unique identifier for an element within a free-form layout.
    elementId :: Prelude.Text,
    -- | The type of element.
    elementType :: LayoutElementType,
    -- | The x-axis coordinate of the element.
    xAxisLocation :: Prelude.Text,
    -- | The y-axis coordinate of the element.
    yAxisLocation :: Prelude.Text,
    -- | The width of an element within a free-form layout.
    width :: Prelude.Text,
    -- | The height of an element within a free-form layout.
    height :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FreeFormLayoutElement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backgroundStyle', 'freeFormLayoutElement_backgroundStyle' - The background style configuration of a free-form layout element.
--
-- 'borderStyle', 'freeFormLayoutElement_borderStyle' - The border style configuration of a free-form layout element.
--
-- 'loadingAnimation', 'freeFormLayoutElement_loadingAnimation' - The loading animation configuration of a free-form layout element.
--
-- 'renderingRules', 'freeFormLayoutElement_renderingRules' - The rendering rules that determine when an element should be displayed
-- within a free-form layout.
--
-- 'selectedBorderStyle', 'freeFormLayoutElement_selectedBorderStyle' - The border style configuration of a free-form layout element. This
-- border style is used when the element is selected.
--
-- 'visibility', 'freeFormLayoutElement_visibility' - The visibility of an element within a free-form layout.
--
-- 'elementId', 'freeFormLayoutElement_elementId' - A unique identifier for an element within a free-form layout.
--
-- 'elementType', 'freeFormLayoutElement_elementType' - The type of element.
--
-- 'xAxisLocation', 'freeFormLayoutElement_xAxisLocation' - The x-axis coordinate of the element.
--
-- 'yAxisLocation', 'freeFormLayoutElement_yAxisLocation' - The y-axis coordinate of the element.
--
-- 'width', 'freeFormLayoutElement_width' - The width of an element within a free-form layout.
--
-- 'height', 'freeFormLayoutElement_height' - The height of an element within a free-form layout.
newFreeFormLayoutElement ::
  -- | 'elementId'
  Prelude.Text ->
  -- | 'elementType'
  LayoutElementType ->
  -- | 'xAxisLocation'
  Prelude.Text ->
  -- | 'yAxisLocation'
  Prelude.Text ->
  -- | 'width'
  Prelude.Text ->
  -- | 'height'
  Prelude.Text ->
  FreeFormLayoutElement
newFreeFormLayoutElement
  pElementId_
  pElementType_
  pXAxisLocation_
  pYAxisLocation_
  pWidth_
  pHeight_ =
    FreeFormLayoutElement'
      { backgroundStyle =
          Prelude.Nothing,
        borderStyle = Prelude.Nothing,
        loadingAnimation = Prelude.Nothing,
        renderingRules = Prelude.Nothing,
        selectedBorderStyle = Prelude.Nothing,
        visibility = Prelude.Nothing,
        elementId = pElementId_,
        elementType = pElementType_,
        xAxisLocation = pXAxisLocation_,
        yAxisLocation = pYAxisLocation_,
        width = pWidth_,
        height = pHeight_
      }

-- | The background style configuration of a free-form layout element.
freeFormLayoutElement_backgroundStyle :: Lens.Lens' FreeFormLayoutElement (Prelude.Maybe FreeFormLayoutElementBackgroundStyle)
freeFormLayoutElement_backgroundStyle = Lens.lens (\FreeFormLayoutElement' {backgroundStyle} -> backgroundStyle) (\s@FreeFormLayoutElement' {} a -> s {backgroundStyle = a} :: FreeFormLayoutElement)

-- | The border style configuration of a free-form layout element.
freeFormLayoutElement_borderStyle :: Lens.Lens' FreeFormLayoutElement (Prelude.Maybe FreeFormLayoutElementBorderStyle)
freeFormLayoutElement_borderStyle = Lens.lens (\FreeFormLayoutElement' {borderStyle} -> borderStyle) (\s@FreeFormLayoutElement' {} a -> s {borderStyle = a} :: FreeFormLayoutElement)

-- | The loading animation configuration of a free-form layout element.
freeFormLayoutElement_loadingAnimation :: Lens.Lens' FreeFormLayoutElement (Prelude.Maybe LoadingAnimation)
freeFormLayoutElement_loadingAnimation = Lens.lens (\FreeFormLayoutElement' {loadingAnimation} -> loadingAnimation) (\s@FreeFormLayoutElement' {} a -> s {loadingAnimation = a} :: FreeFormLayoutElement)

-- | The rendering rules that determine when an element should be displayed
-- within a free-form layout.
freeFormLayoutElement_renderingRules :: Lens.Lens' FreeFormLayoutElement (Prelude.Maybe [SheetElementRenderingRule])
freeFormLayoutElement_renderingRules = Lens.lens (\FreeFormLayoutElement' {renderingRules} -> renderingRules) (\s@FreeFormLayoutElement' {} a -> s {renderingRules = a} :: FreeFormLayoutElement) Prelude.. Lens.mapping Lens.coerced

-- | The border style configuration of a free-form layout element. This
-- border style is used when the element is selected.
freeFormLayoutElement_selectedBorderStyle :: Lens.Lens' FreeFormLayoutElement (Prelude.Maybe FreeFormLayoutElementBorderStyle)
freeFormLayoutElement_selectedBorderStyle = Lens.lens (\FreeFormLayoutElement' {selectedBorderStyle} -> selectedBorderStyle) (\s@FreeFormLayoutElement' {} a -> s {selectedBorderStyle = a} :: FreeFormLayoutElement)

-- | The visibility of an element within a free-form layout.
freeFormLayoutElement_visibility :: Lens.Lens' FreeFormLayoutElement (Prelude.Maybe Visibility)
freeFormLayoutElement_visibility = Lens.lens (\FreeFormLayoutElement' {visibility} -> visibility) (\s@FreeFormLayoutElement' {} a -> s {visibility = a} :: FreeFormLayoutElement)

-- | A unique identifier for an element within a free-form layout.
freeFormLayoutElement_elementId :: Lens.Lens' FreeFormLayoutElement Prelude.Text
freeFormLayoutElement_elementId = Lens.lens (\FreeFormLayoutElement' {elementId} -> elementId) (\s@FreeFormLayoutElement' {} a -> s {elementId = a} :: FreeFormLayoutElement)

-- | The type of element.
freeFormLayoutElement_elementType :: Lens.Lens' FreeFormLayoutElement LayoutElementType
freeFormLayoutElement_elementType = Lens.lens (\FreeFormLayoutElement' {elementType} -> elementType) (\s@FreeFormLayoutElement' {} a -> s {elementType = a} :: FreeFormLayoutElement)

-- | The x-axis coordinate of the element.
freeFormLayoutElement_xAxisLocation :: Lens.Lens' FreeFormLayoutElement Prelude.Text
freeFormLayoutElement_xAxisLocation = Lens.lens (\FreeFormLayoutElement' {xAxisLocation} -> xAxisLocation) (\s@FreeFormLayoutElement' {} a -> s {xAxisLocation = a} :: FreeFormLayoutElement)

-- | The y-axis coordinate of the element.
freeFormLayoutElement_yAxisLocation :: Lens.Lens' FreeFormLayoutElement Prelude.Text
freeFormLayoutElement_yAxisLocation = Lens.lens (\FreeFormLayoutElement' {yAxisLocation} -> yAxisLocation) (\s@FreeFormLayoutElement' {} a -> s {yAxisLocation = a} :: FreeFormLayoutElement)

-- | The width of an element within a free-form layout.
freeFormLayoutElement_width :: Lens.Lens' FreeFormLayoutElement Prelude.Text
freeFormLayoutElement_width = Lens.lens (\FreeFormLayoutElement' {width} -> width) (\s@FreeFormLayoutElement' {} a -> s {width = a} :: FreeFormLayoutElement)

-- | The height of an element within a free-form layout.
freeFormLayoutElement_height :: Lens.Lens' FreeFormLayoutElement Prelude.Text
freeFormLayoutElement_height = Lens.lens (\FreeFormLayoutElement' {height} -> height) (\s@FreeFormLayoutElement' {} a -> s {height = a} :: FreeFormLayoutElement)

instance Data.FromJSON FreeFormLayoutElement where
  parseJSON =
    Data.withObject
      "FreeFormLayoutElement"
      ( \x ->
          FreeFormLayoutElement'
            Prelude.<$> (x Data..:? "BackgroundStyle")
            Prelude.<*> (x Data..:? "BorderStyle")
            Prelude.<*> (x Data..:? "LoadingAnimation")
            Prelude.<*> (x Data..:? "RenderingRules" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SelectedBorderStyle")
            Prelude.<*> (x Data..:? "Visibility")
            Prelude.<*> (x Data..: "ElementId")
            Prelude.<*> (x Data..: "ElementType")
            Prelude.<*> (x Data..: "XAxisLocation")
            Prelude.<*> (x Data..: "YAxisLocation")
            Prelude.<*> (x Data..: "Width")
            Prelude.<*> (x Data..: "Height")
      )

instance Prelude.Hashable FreeFormLayoutElement where
  hashWithSalt _salt FreeFormLayoutElement' {..} =
    _salt `Prelude.hashWithSalt` backgroundStyle
      `Prelude.hashWithSalt` borderStyle
      `Prelude.hashWithSalt` loadingAnimation
      `Prelude.hashWithSalt` renderingRules
      `Prelude.hashWithSalt` selectedBorderStyle
      `Prelude.hashWithSalt` visibility
      `Prelude.hashWithSalt` elementId
      `Prelude.hashWithSalt` elementType
      `Prelude.hashWithSalt` xAxisLocation
      `Prelude.hashWithSalt` yAxisLocation
      `Prelude.hashWithSalt` width
      `Prelude.hashWithSalt` height

instance Prelude.NFData FreeFormLayoutElement where
  rnf FreeFormLayoutElement' {..} =
    Prelude.rnf backgroundStyle
      `Prelude.seq` Prelude.rnf borderStyle
      `Prelude.seq` Prelude.rnf loadingAnimation
      `Prelude.seq` Prelude.rnf renderingRules
      `Prelude.seq` Prelude.rnf selectedBorderStyle
      `Prelude.seq` Prelude.rnf visibility
      `Prelude.seq` Prelude.rnf elementId
      `Prelude.seq` Prelude.rnf elementType
      `Prelude.seq` Prelude.rnf xAxisLocation
      `Prelude.seq` Prelude.rnf yAxisLocation
      `Prelude.seq` Prelude.rnf width
      `Prelude.seq` Prelude.rnf height

instance Data.ToJSON FreeFormLayoutElement where
  toJSON FreeFormLayoutElement' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BackgroundStyle" Data..=)
              Prelude.<$> backgroundStyle,
            ("BorderStyle" Data..=) Prelude.<$> borderStyle,
            ("LoadingAnimation" Data..=)
              Prelude.<$> loadingAnimation,
            ("RenderingRules" Data..=)
              Prelude.<$> renderingRules,
            ("SelectedBorderStyle" Data..=)
              Prelude.<$> selectedBorderStyle,
            ("Visibility" Data..=) Prelude.<$> visibility,
            Prelude.Just ("ElementId" Data..= elementId),
            Prelude.Just ("ElementType" Data..= elementType),
            Prelude.Just ("XAxisLocation" Data..= xAxisLocation),
            Prelude.Just ("YAxisLocation" Data..= yAxisLocation),
            Prelude.Just ("Width" Data..= width),
            Prelude.Just ("Height" Data..= height)
          ]
      )
