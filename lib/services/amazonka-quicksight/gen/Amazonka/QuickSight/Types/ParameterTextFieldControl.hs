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
-- Module      : Amazonka.QuickSight.Types.ParameterTextFieldControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ParameterTextFieldControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TextFieldControlDisplayOptions

-- | A control to display a text box that is used to enter a single entry.
--
-- /See:/ 'newParameterTextFieldControl' smart constructor.
data ParameterTextFieldControl = ParameterTextFieldControl'
  { -- | The display options of a control.
    displayOptions :: Prelude.Maybe TextFieldControlDisplayOptions,
    -- | The ID of the @ParameterTextFieldControl@.
    parameterControlId :: Prelude.Text,
    -- | The title of the @ParameterTextFieldControl@.
    title :: Prelude.Text,
    -- | The source parameter name of the @ParameterTextFieldControl@.
    sourceParameterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterTextFieldControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayOptions', 'parameterTextFieldControl_displayOptions' - The display options of a control.
--
-- 'parameterControlId', 'parameterTextFieldControl_parameterControlId' - The ID of the @ParameterTextFieldControl@.
--
-- 'title', 'parameterTextFieldControl_title' - The title of the @ParameterTextFieldControl@.
--
-- 'sourceParameterName', 'parameterTextFieldControl_sourceParameterName' - The source parameter name of the @ParameterTextFieldControl@.
newParameterTextFieldControl ::
  -- | 'parameterControlId'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  -- | 'sourceParameterName'
  Prelude.Text ->
  ParameterTextFieldControl
newParameterTextFieldControl
  pParameterControlId_
  pTitle_
  pSourceParameterName_ =
    ParameterTextFieldControl'
      { displayOptions =
          Prelude.Nothing,
        parameterControlId = pParameterControlId_,
        title = pTitle_,
        sourceParameterName = pSourceParameterName_
      }

-- | The display options of a control.
parameterTextFieldControl_displayOptions :: Lens.Lens' ParameterTextFieldControl (Prelude.Maybe TextFieldControlDisplayOptions)
parameterTextFieldControl_displayOptions = Lens.lens (\ParameterTextFieldControl' {displayOptions} -> displayOptions) (\s@ParameterTextFieldControl' {} a -> s {displayOptions = a} :: ParameterTextFieldControl)

-- | The ID of the @ParameterTextFieldControl@.
parameterTextFieldControl_parameterControlId :: Lens.Lens' ParameterTextFieldControl Prelude.Text
parameterTextFieldControl_parameterControlId = Lens.lens (\ParameterTextFieldControl' {parameterControlId} -> parameterControlId) (\s@ParameterTextFieldControl' {} a -> s {parameterControlId = a} :: ParameterTextFieldControl)

-- | The title of the @ParameterTextFieldControl@.
parameterTextFieldControl_title :: Lens.Lens' ParameterTextFieldControl Prelude.Text
parameterTextFieldControl_title = Lens.lens (\ParameterTextFieldControl' {title} -> title) (\s@ParameterTextFieldControl' {} a -> s {title = a} :: ParameterTextFieldControl)

-- | The source parameter name of the @ParameterTextFieldControl@.
parameterTextFieldControl_sourceParameterName :: Lens.Lens' ParameterTextFieldControl Prelude.Text
parameterTextFieldControl_sourceParameterName = Lens.lens (\ParameterTextFieldControl' {sourceParameterName} -> sourceParameterName) (\s@ParameterTextFieldControl' {} a -> s {sourceParameterName = a} :: ParameterTextFieldControl)

instance Data.FromJSON ParameterTextFieldControl where
  parseJSON =
    Data.withObject
      "ParameterTextFieldControl"
      ( \x ->
          ParameterTextFieldControl'
            Prelude.<$> (x Data..:? "DisplayOptions")
            Prelude.<*> (x Data..: "ParameterControlId")
            Prelude.<*> (x Data..: "Title")
            Prelude.<*> (x Data..: "SourceParameterName")
      )

instance Prelude.Hashable ParameterTextFieldControl where
  hashWithSalt _salt ParameterTextFieldControl' {..} =
    _salt
      `Prelude.hashWithSalt` displayOptions
      `Prelude.hashWithSalt` parameterControlId
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` sourceParameterName

instance Prelude.NFData ParameterTextFieldControl where
  rnf ParameterTextFieldControl' {..} =
    Prelude.rnf displayOptions
      `Prelude.seq` Prelude.rnf parameterControlId
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf sourceParameterName

instance Data.ToJSON ParameterTextFieldControl where
  toJSON ParameterTextFieldControl' {..} =
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
