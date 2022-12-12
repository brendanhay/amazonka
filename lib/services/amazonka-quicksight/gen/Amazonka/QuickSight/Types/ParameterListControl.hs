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
-- Module      : Amazonka.QuickSight.Types.ParameterListControl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ParameterListControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CascadingControlConfiguration
import Amazonka.QuickSight.Types.ListControlDisplayOptions
import Amazonka.QuickSight.Types.ParameterSelectableValues
import Amazonka.QuickSight.Types.SheetControlListType

-- | A control to display a list with buttons or boxes that are used to
-- select either a single value or multiple values.
--
-- /See:/ 'newParameterListControl' smart constructor.
data ParameterListControl = ParameterListControl'
  { -- | The values that are displayed in a control can be configured to only
    -- show values that are valid based on what\'s selected in other controls.
    cascadingControlConfiguration :: Prelude.Maybe CascadingControlConfiguration,
    -- | The display options of a control.
    displayOptions :: Prelude.Maybe ListControlDisplayOptions,
    -- | A list of selectable values that are used in a control.
    selectableValues :: Prelude.Maybe ParameterSelectableValues,
    -- | The type of @ParameterListControl@.
    type' :: Prelude.Maybe SheetControlListType,
    -- | The ID of the @ParameterListControl@.
    parameterControlId :: Prelude.Text,
    -- | The title of the @ParameterListControl@.
    title :: Prelude.Text,
    -- | The source parameter name of the @ParameterListControl@.
    sourceParameterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterListControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cascadingControlConfiguration', 'parameterListControl_cascadingControlConfiguration' - The values that are displayed in a control can be configured to only
-- show values that are valid based on what\'s selected in other controls.
--
-- 'displayOptions', 'parameterListControl_displayOptions' - The display options of a control.
--
-- 'selectableValues', 'parameterListControl_selectableValues' - A list of selectable values that are used in a control.
--
-- 'type'', 'parameterListControl_type' - The type of @ParameterListControl@.
--
-- 'parameterControlId', 'parameterListControl_parameterControlId' - The ID of the @ParameterListControl@.
--
-- 'title', 'parameterListControl_title' - The title of the @ParameterListControl@.
--
-- 'sourceParameterName', 'parameterListControl_sourceParameterName' - The source parameter name of the @ParameterListControl@.
newParameterListControl ::
  -- | 'parameterControlId'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  -- | 'sourceParameterName'
  Prelude.Text ->
  ParameterListControl
newParameterListControl
  pParameterControlId_
  pTitle_
  pSourceParameterName_ =
    ParameterListControl'
      { cascadingControlConfiguration =
          Prelude.Nothing,
        displayOptions = Prelude.Nothing,
        selectableValues = Prelude.Nothing,
        type' = Prelude.Nothing,
        parameterControlId = pParameterControlId_,
        title = pTitle_,
        sourceParameterName = pSourceParameterName_
      }

-- | The values that are displayed in a control can be configured to only
-- show values that are valid based on what\'s selected in other controls.
parameterListControl_cascadingControlConfiguration :: Lens.Lens' ParameterListControl (Prelude.Maybe CascadingControlConfiguration)
parameterListControl_cascadingControlConfiguration = Lens.lens (\ParameterListControl' {cascadingControlConfiguration} -> cascadingControlConfiguration) (\s@ParameterListControl' {} a -> s {cascadingControlConfiguration = a} :: ParameterListControl)

-- | The display options of a control.
parameterListControl_displayOptions :: Lens.Lens' ParameterListControl (Prelude.Maybe ListControlDisplayOptions)
parameterListControl_displayOptions = Lens.lens (\ParameterListControl' {displayOptions} -> displayOptions) (\s@ParameterListControl' {} a -> s {displayOptions = a} :: ParameterListControl)

-- | A list of selectable values that are used in a control.
parameterListControl_selectableValues :: Lens.Lens' ParameterListControl (Prelude.Maybe ParameterSelectableValues)
parameterListControl_selectableValues = Lens.lens (\ParameterListControl' {selectableValues} -> selectableValues) (\s@ParameterListControl' {} a -> s {selectableValues = a} :: ParameterListControl)

-- | The type of @ParameterListControl@.
parameterListControl_type :: Lens.Lens' ParameterListControl (Prelude.Maybe SheetControlListType)
parameterListControl_type = Lens.lens (\ParameterListControl' {type'} -> type') (\s@ParameterListControl' {} a -> s {type' = a} :: ParameterListControl)

-- | The ID of the @ParameterListControl@.
parameterListControl_parameterControlId :: Lens.Lens' ParameterListControl Prelude.Text
parameterListControl_parameterControlId = Lens.lens (\ParameterListControl' {parameterControlId} -> parameterControlId) (\s@ParameterListControl' {} a -> s {parameterControlId = a} :: ParameterListControl)

-- | The title of the @ParameterListControl@.
parameterListControl_title :: Lens.Lens' ParameterListControl Prelude.Text
parameterListControl_title = Lens.lens (\ParameterListControl' {title} -> title) (\s@ParameterListControl' {} a -> s {title = a} :: ParameterListControl)

-- | The source parameter name of the @ParameterListControl@.
parameterListControl_sourceParameterName :: Lens.Lens' ParameterListControl Prelude.Text
parameterListControl_sourceParameterName = Lens.lens (\ParameterListControl' {sourceParameterName} -> sourceParameterName) (\s@ParameterListControl' {} a -> s {sourceParameterName = a} :: ParameterListControl)

instance Data.FromJSON ParameterListControl where
  parseJSON =
    Data.withObject
      "ParameterListControl"
      ( \x ->
          ParameterListControl'
            Prelude.<$> (x Data..:? "CascadingControlConfiguration")
            Prelude.<*> (x Data..:? "DisplayOptions")
            Prelude.<*> (x Data..:? "SelectableValues")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..: "ParameterControlId")
            Prelude.<*> (x Data..: "Title")
            Prelude.<*> (x Data..: "SourceParameterName")
      )

instance Prelude.Hashable ParameterListControl where
  hashWithSalt _salt ParameterListControl' {..} =
    _salt
      `Prelude.hashWithSalt` cascadingControlConfiguration
      `Prelude.hashWithSalt` displayOptions
      `Prelude.hashWithSalt` selectableValues
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` parameterControlId
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` sourceParameterName

instance Prelude.NFData ParameterListControl where
  rnf ParameterListControl' {..} =
    Prelude.rnf cascadingControlConfiguration
      `Prelude.seq` Prelude.rnf displayOptions
      `Prelude.seq` Prelude.rnf selectableValues
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf parameterControlId
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf sourceParameterName

instance Data.ToJSON ParameterListControl where
  toJSON ParameterListControl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CascadingControlConfiguration" Data..=)
              Prelude.<$> cascadingControlConfiguration,
            ("DisplayOptions" Data..=)
              Prelude.<$> displayOptions,
            ("SelectableValues" Data..=)
              Prelude.<$> selectableValues,
            ("Type" Data..=) Prelude.<$> type',
            Prelude.Just
              ("ParameterControlId" Data..= parameterControlId),
            Prelude.Just ("Title" Data..= title),
            Prelude.Just
              ("SourceParameterName" Data..= sourceParameterName)
          ]
      )
