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
-- Module      : Amazonka.QuickSight.Types.ParameterDropDownControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ParameterDropDownControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CascadingControlConfiguration
import Amazonka.QuickSight.Types.DropDownControlDisplayOptions
import Amazonka.QuickSight.Types.ParameterSelectableValues
import Amazonka.QuickSight.Types.SheetControlListType

-- | A control to display a dropdown list with buttons that are used to
-- select a single value.
--
-- /See:/ 'newParameterDropDownControl' smart constructor.
data ParameterDropDownControl = ParameterDropDownControl'
  { -- | The values that are displayed in a control can be configured to only
    -- show values that are valid based on what\'s selected in other controls.
    cascadingControlConfiguration :: Prelude.Maybe CascadingControlConfiguration,
    -- | The display options of a control.
    displayOptions :: Prelude.Maybe DropDownControlDisplayOptions,
    -- | A list of selectable values that are used in a control.
    selectableValues :: Prelude.Maybe ParameterSelectableValues,
    -- | The type parameter name of the @ParameterDropDownControl@.
    type' :: Prelude.Maybe SheetControlListType,
    -- | The ID of the @ParameterDropDownControl@.
    parameterControlId :: Prelude.Text,
    -- | The title of the @ParameterDropDownControl@.
    title :: Prelude.Text,
    -- | The source parameter name of the @ParameterDropDownControl@.
    sourceParameterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterDropDownControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cascadingControlConfiguration', 'parameterDropDownControl_cascadingControlConfiguration' - The values that are displayed in a control can be configured to only
-- show values that are valid based on what\'s selected in other controls.
--
-- 'displayOptions', 'parameterDropDownControl_displayOptions' - The display options of a control.
--
-- 'selectableValues', 'parameterDropDownControl_selectableValues' - A list of selectable values that are used in a control.
--
-- 'type'', 'parameterDropDownControl_type' - The type parameter name of the @ParameterDropDownControl@.
--
-- 'parameterControlId', 'parameterDropDownControl_parameterControlId' - The ID of the @ParameterDropDownControl@.
--
-- 'title', 'parameterDropDownControl_title' - The title of the @ParameterDropDownControl@.
--
-- 'sourceParameterName', 'parameterDropDownControl_sourceParameterName' - The source parameter name of the @ParameterDropDownControl@.
newParameterDropDownControl ::
  -- | 'parameterControlId'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  -- | 'sourceParameterName'
  Prelude.Text ->
  ParameterDropDownControl
newParameterDropDownControl
  pParameterControlId_
  pTitle_
  pSourceParameterName_ =
    ParameterDropDownControl'
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
parameterDropDownControl_cascadingControlConfiguration :: Lens.Lens' ParameterDropDownControl (Prelude.Maybe CascadingControlConfiguration)
parameterDropDownControl_cascadingControlConfiguration = Lens.lens (\ParameterDropDownControl' {cascadingControlConfiguration} -> cascadingControlConfiguration) (\s@ParameterDropDownControl' {} a -> s {cascadingControlConfiguration = a} :: ParameterDropDownControl)

-- | The display options of a control.
parameterDropDownControl_displayOptions :: Lens.Lens' ParameterDropDownControl (Prelude.Maybe DropDownControlDisplayOptions)
parameterDropDownControl_displayOptions = Lens.lens (\ParameterDropDownControl' {displayOptions} -> displayOptions) (\s@ParameterDropDownControl' {} a -> s {displayOptions = a} :: ParameterDropDownControl)

-- | A list of selectable values that are used in a control.
parameterDropDownControl_selectableValues :: Lens.Lens' ParameterDropDownControl (Prelude.Maybe ParameterSelectableValues)
parameterDropDownControl_selectableValues = Lens.lens (\ParameterDropDownControl' {selectableValues} -> selectableValues) (\s@ParameterDropDownControl' {} a -> s {selectableValues = a} :: ParameterDropDownControl)

-- | The type parameter name of the @ParameterDropDownControl@.
parameterDropDownControl_type :: Lens.Lens' ParameterDropDownControl (Prelude.Maybe SheetControlListType)
parameterDropDownControl_type = Lens.lens (\ParameterDropDownControl' {type'} -> type') (\s@ParameterDropDownControl' {} a -> s {type' = a} :: ParameterDropDownControl)

-- | The ID of the @ParameterDropDownControl@.
parameterDropDownControl_parameterControlId :: Lens.Lens' ParameterDropDownControl Prelude.Text
parameterDropDownControl_parameterControlId = Lens.lens (\ParameterDropDownControl' {parameterControlId} -> parameterControlId) (\s@ParameterDropDownControl' {} a -> s {parameterControlId = a} :: ParameterDropDownControl)

-- | The title of the @ParameterDropDownControl@.
parameterDropDownControl_title :: Lens.Lens' ParameterDropDownControl Prelude.Text
parameterDropDownControl_title = Lens.lens (\ParameterDropDownControl' {title} -> title) (\s@ParameterDropDownControl' {} a -> s {title = a} :: ParameterDropDownControl)

-- | The source parameter name of the @ParameterDropDownControl@.
parameterDropDownControl_sourceParameterName :: Lens.Lens' ParameterDropDownControl Prelude.Text
parameterDropDownControl_sourceParameterName = Lens.lens (\ParameterDropDownControl' {sourceParameterName} -> sourceParameterName) (\s@ParameterDropDownControl' {} a -> s {sourceParameterName = a} :: ParameterDropDownControl)

instance Data.FromJSON ParameterDropDownControl where
  parseJSON =
    Data.withObject
      "ParameterDropDownControl"
      ( \x ->
          ParameterDropDownControl'
            Prelude.<$> (x Data..:? "CascadingControlConfiguration")
            Prelude.<*> (x Data..:? "DisplayOptions")
            Prelude.<*> (x Data..:? "SelectableValues")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..: "ParameterControlId")
            Prelude.<*> (x Data..: "Title")
            Prelude.<*> (x Data..: "SourceParameterName")
      )

instance Prelude.Hashable ParameterDropDownControl where
  hashWithSalt _salt ParameterDropDownControl' {..} =
    _salt
      `Prelude.hashWithSalt` cascadingControlConfiguration
      `Prelude.hashWithSalt` displayOptions
      `Prelude.hashWithSalt` selectableValues
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` parameterControlId
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` sourceParameterName

instance Prelude.NFData ParameterDropDownControl where
  rnf ParameterDropDownControl' {..} =
    Prelude.rnf cascadingControlConfiguration `Prelude.seq`
      Prelude.rnf displayOptions `Prelude.seq`
        Prelude.rnf selectableValues `Prelude.seq`
          Prelude.rnf type' `Prelude.seq`
            Prelude.rnf parameterControlId `Prelude.seq`
              Prelude.rnf title `Prelude.seq`
                Prelude.rnf sourceParameterName

instance Data.ToJSON ParameterDropDownControl where
  toJSON ParameterDropDownControl' {..} =
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
