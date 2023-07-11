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
-- Module      : Amazonka.QuickSight.Types.ParameterDateTimePickerControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ParameterDateTimePickerControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DateTimePickerControlDisplayOptions

-- | A control from a date parameter that specifies date and time.
--
-- /See:/ 'newParameterDateTimePickerControl' smart constructor.
data ParameterDateTimePickerControl = ParameterDateTimePickerControl'
  { -- | The display options of a control.
    displayOptions :: Prelude.Maybe DateTimePickerControlDisplayOptions,
    -- | The ID of the @ParameterDateTimePickerControl@.
    parameterControlId :: Prelude.Text,
    -- | The title of the @ParameterDateTimePickerControl@.
    title :: Prelude.Text,
    -- | The name of the @ParameterDateTimePickerControl@.
    sourceParameterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterDateTimePickerControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayOptions', 'parameterDateTimePickerControl_displayOptions' - The display options of a control.
--
-- 'parameterControlId', 'parameterDateTimePickerControl_parameterControlId' - The ID of the @ParameterDateTimePickerControl@.
--
-- 'title', 'parameterDateTimePickerControl_title' - The title of the @ParameterDateTimePickerControl@.
--
-- 'sourceParameterName', 'parameterDateTimePickerControl_sourceParameterName' - The name of the @ParameterDateTimePickerControl@.
newParameterDateTimePickerControl ::
  -- | 'parameterControlId'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  -- | 'sourceParameterName'
  Prelude.Text ->
  ParameterDateTimePickerControl
newParameterDateTimePickerControl
  pParameterControlId_
  pTitle_
  pSourceParameterName_ =
    ParameterDateTimePickerControl'
      { displayOptions =
          Prelude.Nothing,
        parameterControlId = pParameterControlId_,
        title = pTitle_,
        sourceParameterName = pSourceParameterName_
      }

-- | The display options of a control.
parameterDateTimePickerControl_displayOptions :: Lens.Lens' ParameterDateTimePickerControl (Prelude.Maybe DateTimePickerControlDisplayOptions)
parameterDateTimePickerControl_displayOptions = Lens.lens (\ParameterDateTimePickerControl' {displayOptions} -> displayOptions) (\s@ParameterDateTimePickerControl' {} a -> s {displayOptions = a} :: ParameterDateTimePickerControl)

-- | The ID of the @ParameterDateTimePickerControl@.
parameterDateTimePickerControl_parameterControlId :: Lens.Lens' ParameterDateTimePickerControl Prelude.Text
parameterDateTimePickerControl_parameterControlId = Lens.lens (\ParameterDateTimePickerControl' {parameterControlId} -> parameterControlId) (\s@ParameterDateTimePickerControl' {} a -> s {parameterControlId = a} :: ParameterDateTimePickerControl)

-- | The title of the @ParameterDateTimePickerControl@.
parameterDateTimePickerControl_title :: Lens.Lens' ParameterDateTimePickerControl Prelude.Text
parameterDateTimePickerControl_title = Lens.lens (\ParameterDateTimePickerControl' {title} -> title) (\s@ParameterDateTimePickerControl' {} a -> s {title = a} :: ParameterDateTimePickerControl)

-- | The name of the @ParameterDateTimePickerControl@.
parameterDateTimePickerControl_sourceParameterName :: Lens.Lens' ParameterDateTimePickerControl Prelude.Text
parameterDateTimePickerControl_sourceParameterName = Lens.lens (\ParameterDateTimePickerControl' {sourceParameterName} -> sourceParameterName) (\s@ParameterDateTimePickerControl' {} a -> s {sourceParameterName = a} :: ParameterDateTimePickerControl)

instance Data.FromJSON ParameterDateTimePickerControl where
  parseJSON =
    Data.withObject
      "ParameterDateTimePickerControl"
      ( \x ->
          ParameterDateTimePickerControl'
            Prelude.<$> (x Data..:? "DisplayOptions")
            Prelude.<*> (x Data..: "ParameterControlId")
            Prelude.<*> (x Data..: "Title")
            Prelude.<*> (x Data..: "SourceParameterName")
      )

instance
  Prelude.Hashable
    ParameterDateTimePickerControl
  where
  hashWithSalt
    _salt
    ParameterDateTimePickerControl' {..} =
      _salt
        `Prelude.hashWithSalt` displayOptions
        `Prelude.hashWithSalt` parameterControlId
        `Prelude.hashWithSalt` title
        `Prelude.hashWithSalt` sourceParameterName

instance
  Prelude.NFData
    ParameterDateTimePickerControl
  where
  rnf ParameterDateTimePickerControl' {..} =
    Prelude.rnf displayOptions
      `Prelude.seq` Prelude.rnf parameterControlId
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf sourceParameterName

instance Data.ToJSON ParameterDateTimePickerControl where
  toJSON ParameterDateTimePickerControl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DisplayOptions" Data..=)
              Prelude.<$> displayOptions,
            Prelude.Just
              ("ParameterControlId" Data..= parameterControlId),
            Prelude.Just ("Title" Data..= title),
            Prelude.Just
              ("SourceParameterName" Data..= sourceParameterName)
          ]
      )
