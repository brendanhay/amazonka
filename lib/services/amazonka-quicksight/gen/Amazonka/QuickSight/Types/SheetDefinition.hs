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
-- Module      : Amazonka.QuickSight.Types.SheetDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SheetDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FilterControl
import Amazonka.QuickSight.Types.Layout
import Amazonka.QuickSight.Types.ParameterControl
import Amazonka.QuickSight.Types.SheetContentType
import Amazonka.QuickSight.Types.SheetControlLayout
import Amazonka.QuickSight.Types.SheetTextBox
import Amazonka.QuickSight.Types.Visual

-- | A sheet is an object that contains a set of visuals that are viewed
-- together on one page in a paginated report. Every analysis and dashboard
-- must contain at least one sheet.
--
-- /See:/ 'newSheetDefinition' smart constructor.
data SheetDefinition = SheetDefinition'
  { -- | The layout content type of the sheet. Choose one of the following
    -- options:
    --
    -- -   @PAGINATED@: Creates a sheet for a paginated report.
    --
    -- -   @INTERACTIVE@: Creates a sheet for an interactive dashboard.
    contentType :: Prelude.Maybe SheetContentType,
    -- | A description of the sheet.
    description :: Prelude.Maybe Prelude.Text,
    -- | The list of filter controls that are on a sheet.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/filter-controls.html Adding filter controls to analysis sheets>
    -- in the /Amazon QuickSight User Guide/.
    filterControls :: Prelude.Maybe [FilterControl],
    -- | Layouts define how the components of a sheet are arranged.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/types-of-layout.html Types of layout>
    -- in the /Amazon QuickSight User Guide/.
    layouts :: Prelude.Maybe (Prelude.NonEmpty Layout),
    -- | The name of the sheet. This name is displayed on the sheet\'s tab in the
    -- Amazon QuickSight console.
    name :: Prelude.Maybe Prelude.Text,
    -- | The list of parameter controls that are on a sheet.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/quicksight/latest/user/parameters-controls.html Using a Control with a Parameter in Amazon QuickSight>
    -- in the /Amazon QuickSight User Guide/.
    parameterControls :: Prelude.Maybe [ParameterControl],
    -- | The control layouts of the sheet.
    sheetControlLayouts :: Prelude.Maybe [SheetControlLayout],
    -- | The text boxes that are on a sheet.
    textBoxes :: Prelude.Maybe [SheetTextBox],
    -- | The title of the sheet.
    title :: Prelude.Maybe Prelude.Text,
    -- | A list of the visuals that are on a sheet. Visual placement is
    -- determined by the layout of the sheet.
    visuals :: Prelude.Maybe [Visual],
    -- | The unique identifier of a sheet.
    sheetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SheetDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'sheetDefinition_contentType' - The layout content type of the sheet. Choose one of the following
-- options:
--
-- -   @PAGINATED@: Creates a sheet for a paginated report.
--
-- -   @INTERACTIVE@: Creates a sheet for an interactive dashboard.
--
-- 'description', 'sheetDefinition_description' - A description of the sheet.
--
-- 'filterControls', 'sheetDefinition_filterControls' - The list of filter controls that are on a sheet.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/filter-controls.html Adding filter controls to analysis sheets>
-- in the /Amazon QuickSight User Guide/.
--
-- 'layouts', 'sheetDefinition_layouts' - Layouts define how the components of a sheet are arranged.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/types-of-layout.html Types of layout>
-- in the /Amazon QuickSight User Guide/.
--
-- 'name', 'sheetDefinition_name' - The name of the sheet. This name is displayed on the sheet\'s tab in the
-- Amazon QuickSight console.
--
-- 'parameterControls', 'sheetDefinition_parameterControls' - The list of parameter controls that are on a sheet.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/parameters-controls.html Using a Control with a Parameter in Amazon QuickSight>
-- in the /Amazon QuickSight User Guide/.
--
-- 'sheetControlLayouts', 'sheetDefinition_sheetControlLayouts' - The control layouts of the sheet.
--
-- 'textBoxes', 'sheetDefinition_textBoxes' - The text boxes that are on a sheet.
--
-- 'title', 'sheetDefinition_title' - The title of the sheet.
--
-- 'visuals', 'sheetDefinition_visuals' - A list of the visuals that are on a sheet. Visual placement is
-- determined by the layout of the sheet.
--
-- 'sheetId', 'sheetDefinition_sheetId' - The unique identifier of a sheet.
newSheetDefinition ::
  -- | 'sheetId'
  Prelude.Text ->
  SheetDefinition
newSheetDefinition pSheetId_ =
  SheetDefinition'
    { contentType = Prelude.Nothing,
      description = Prelude.Nothing,
      filterControls = Prelude.Nothing,
      layouts = Prelude.Nothing,
      name = Prelude.Nothing,
      parameterControls = Prelude.Nothing,
      sheetControlLayouts = Prelude.Nothing,
      textBoxes = Prelude.Nothing,
      title = Prelude.Nothing,
      visuals = Prelude.Nothing,
      sheetId = pSheetId_
    }

-- | The layout content type of the sheet. Choose one of the following
-- options:
--
-- -   @PAGINATED@: Creates a sheet for a paginated report.
--
-- -   @INTERACTIVE@: Creates a sheet for an interactive dashboard.
sheetDefinition_contentType :: Lens.Lens' SheetDefinition (Prelude.Maybe SheetContentType)
sheetDefinition_contentType = Lens.lens (\SheetDefinition' {contentType} -> contentType) (\s@SheetDefinition' {} a -> s {contentType = a} :: SheetDefinition)

-- | A description of the sheet.
sheetDefinition_description :: Lens.Lens' SheetDefinition (Prelude.Maybe Prelude.Text)
sheetDefinition_description = Lens.lens (\SheetDefinition' {description} -> description) (\s@SheetDefinition' {} a -> s {description = a} :: SheetDefinition)

-- | The list of filter controls that are on a sheet.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/filter-controls.html Adding filter controls to analysis sheets>
-- in the /Amazon QuickSight User Guide/.
sheetDefinition_filterControls :: Lens.Lens' SheetDefinition (Prelude.Maybe [FilterControl])
sheetDefinition_filterControls = Lens.lens (\SheetDefinition' {filterControls} -> filterControls) (\s@SheetDefinition' {} a -> s {filterControls = a} :: SheetDefinition) Prelude.. Lens.mapping Lens.coerced

-- | Layouts define how the components of a sheet are arranged.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/types-of-layout.html Types of layout>
-- in the /Amazon QuickSight User Guide/.
sheetDefinition_layouts :: Lens.Lens' SheetDefinition (Prelude.Maybe (Prelude.NonEmpty Layout))
sheetDefinition_layouts = Lens.lens (\SheetDefinition' {layouts} -> layouts) (\s@SheetDefinition' {} a -> s {layouts = a} :: SheetDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The name of the sheet. This name is displayed on the sheet\'s tab in the
-- Amazon QuickSight console.
sheetDefinition_name :: Lens.Lens' SheetDefinition (Prelude.Maybe Prelude.Text)
sheetDefinition_name = Lens.lens (\SheetDefinition' {name} -> name) (\s@SheetDefinition' {} a -> s {name = a} :: SheetDefinition)

-- | The list of parameter controls that are on a sheet.
--
-- For more information, see
-- <https://docs.aws.amazon.com/quicksight/latest/user/parameters-controls.html Using a Control with a Parameter in Amazon QuickSight>
-- in the /Amazon QuickSight User Guide/.
sheetDefinition_parameterControls :: Lens.Lens' SheetDefinition (Prelude.Maybe [ParameterControl])
sheetDefinition_parameterControls = Lens.lens (\SheetDefinition' {parameterControls} -> parameterControls) (\s@SheetDefinition' {} a -> s {parameterControls = a} :: SheetDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The control layouts of the sheet.
sheetDefinition_sheetControlLayouts :: Lens.Lens' SheetDefinition (Prelude.Maybe [SheetControlLayout])
sheetDefinition_sheetControlLayouts = Lens.lens (\SheetDefinition' {sheetControlLayouts} -> sheetControlLayouts) (\s@SheetDefinition' {} a -> s {sheetControlLayouts = a} :: SheetDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The text boxes that are on a sheet.
sheetDefinition_textBoxes :: Lens.Lens' SheetDefinition (Prelude.Maybe [SheetTextBox])
sheetDefinition_textBoxes = Lens.lens (\SheetDefinition' {textBoxes} -> textBoxes) (\s@SheetDefinition' {} a -> s {textBoxes = a} :: SheetDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The title of the sheet.
sheetDefinition_title :: Lens.Lens' SheetDefinition (Prelude.Maybe Prelude.Text)
sheetDefinition_title = Lens.lens (\SheetDefinition' {title} -> title) (\s@SheetDefinition' {} a -> s {title = a} :: SheetDefinition)

-- | A list of the visuals that are on a sheet. Visual placement is
-- determined by the layout of the sheet.
sheetDefinition_visuals :: Lens.Lens' SheetDefinition (Prelude.Maybe [Visual])
sheetDefinition_visuals = Lens.lens (\SheetDefinition' {visuals} -> visuals) (\s@SheetDefinition' {} a -> s {visuals = a} :: SheetDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier of a sheet.
sheetDefinition_sheetId :: Lens.Lens' SheetDefinition Prelude.Text
sheetDefinition_sheetId = Lens.lens (\SheetDefinition' {sheetId} -> sheetId) (\s@SheetDefinition' {} a -> s {sheetId = a} :: SheetDefinition)

instance Data.FromJSON SheetDefinition where
  parseJSON =
    Data.withObject
      "SheetDefinition"
      ( \x ->
          SheetDefinition'
            Prelude.<$> (x Data..:? "ContentType")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "FilterControls" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Layouts")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> ( x
                            Data..:? "ParameterControls"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "SheetControlLayouts"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "TextBoxes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Title")
            Prelude.<*> (x Data..:? "Visuals" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "SheetId")
      )

instance Prelude.Hashable SheetDefinition where
  hashWithSalt _salt SheetDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` contentType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` filterControls
      `Prelude.hashWithSalt` layouts
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` parameterControls
      `Prelude.hashWithSalt` sheetControlLayouts
      `Prelude.hashWithSalt` textBoxes
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` visuals
      `Prelude.hashWithSalt` sheetId

instance Prelude.NFData SheetDefinition where
  rnf SheetDefinition' {..} =
    Prelude.rnf contentType `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf filterControls `Prelude.seq`
          Prelude.rnf layouts `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf parameterControls `Prelude.seq`
                Prelude.rnf sheetControlLayouts `Prelude.seq`
                  Prelude.rnf textBoxes `Prelude.seq`
                    Prelude.rnf title `Prelude.seq`
                      Prelude.rnf visuals `Prelude.seq`
                        Prelude.rnf sheetId

instance Data.ToJSON SheetDefinition where
  toJSON SheetDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContentType" Data..=) Prelude.<$> contentType,
            ("Description" Data..=) Prelude.<$> description,
            ("FilterControls" Data..=)
              Prelude.<$> filterControls,
            ("Layouts" Data..=) Prelude.<$> layouts,
            ("Name" Data..=) Prelude.<$> name,
            ("ParameterControls" Data..=)
              Prelude.<$> parameterControls,
            ("SheetControlLayouts" Data..=)
              Prelude.<$> sheetControlLayouts,
            ("TextBoxes" Data..=) Prelude.<$> textBoxes,
            ("Title" Data..=) Prelude.<$> title,
            ("Visuals" Data..=) Prelude.<$> visuals,
            Prelude.Just ("SheetId" Data..= sheetId)
          ]
      )
