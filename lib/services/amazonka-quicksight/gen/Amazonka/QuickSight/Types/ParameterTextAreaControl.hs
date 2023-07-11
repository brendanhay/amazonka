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
-- Module      : Amazonka.QuickSight.Types.ParameterTextAreaControl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ParameterTextAreaControl where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TextAreaControlDisplayOptions

-- | A control to display a text box that is used to enter multiple entries.
--
-- /See:/ 'newParameterTextAreaControl' smart constructor.
data ParameterTextAreaControl = ParameterTextAreaControl'
  { -- | The delimiter that is used to separate the lines in text.
    delimiter :: Prelude.Maybe Prelude.Text,
    -- | The display options of a control.
    displayOptions :: Prelude.Maybe TextAreaControlDisplayOptions,
    -- | The ID of the @ParameterTextAreaControl@.
    parameterControlId :: Prelude.Text,
    -- | The title of the @ParameterTextAreaControl@.
    title :: Prelude.Text,
    -- | The source parameter name of the @ParameterTextAreaControl@.
    sourceParameterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterTextAreaControl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'delimiter', 'parameterTextAreaControl_delimiter' - The delimiter that is used to separate the lines in text.
--
-- 'displayOptions', 'parameterTextAreaControl_displayOptions' - The display options of a control.
--
-- 'parameterControlId', 'parameterTextAreaControl_parameterControlId' - The ID of the @ParameterTextAreaControl@.
--
-- 'title', 'parameterTextAreaControl_title' - The title of the @ParameterTextAreaControl@.
--
-- 'sourceParameterName', 'parameterTextAreaControl_sourceParameterName' - The source parameter name of the @ParameterTextAreaControl@.
newParameterTextAreaControl ::
  -- | 'parameterControlId'
  Prelude.Text ->
  -- | 'title'
  Prelude.Text ->
  -- | 'sourceParameterName'
  Prelude.Text ->
  ParameterTextAreaControl
newParameterTextAreaControl
  pParameterControlId_
  pTitle_
  pSourceParameterName_ =
    ParameterTextAreaControl'
      { delimiter =
          Prelude.Nothing,
        displayOptions = Prelude.Nothing,
        parameterControlId = pParameterControlId_,
        title = pTitle_,
        sourceParameterName = pSourceParameterName_
      }

-- | The delimiter that is used to separate the lines in text.
parameterTextAreaControl_delimiter :: Lens.Lens' ParameterTextAreaControl (Prelude.Maybe Prelude.Text)
parameterTextAreaControl_delimiter = Lens.lens (\ParameterTextAreaControl' {delimiter} -> delimiter) (\s@ParameterTextAreaControl' {} a -> s {delimiter = a} :: ParameterTextAreaControl)

-- | The display options of a control.
parameterTextAreaControl_displayOptions :: Lens.Lens' ParameterTextAreaControl (Prelude.Maybe TextAreaControlDisplayOptions)
parameterTextAreaControl_displayOptions = Lens.lens (\ParameterTextAreaControl' {displayOptions} -> displayOptions) (\s@ParameterTextAreaControl' {} a -> s {displayOptions = a} :: ParameterTextAreaControl)

-- | The ID of the @ParameterTextAreaControl@.
parameterTextAreaControl_parameterControlId :: Lens.Lens' ParameterTextAreaControl Prelude.Text
parameterTextAreaControl_parameterControlId = Lens.lens (\ParameterTextAreaControl' {parameterControlId} -> parameterControlId) (\s@ParameterTextAreaControl' {} a -> s {parameterControlId = a} :: ParameterTextAreaControl)

-- | The title of the @ParameterTextAreaControl@.
parameterTextAreaControl_title :: Lens.Lens' ParameterTextAreaControl Prelude.Text
parameterTextAreaControl_title = Lens.lens (\ParameterTextAreaControl' {title} -> title) (\s@ParameterTextAreaControl' {} a -> s {title = a} :: ParameterTextAreaControl)

-- | The source parameter name of the @ParameterTextAreaControl@.
parameterTextAreaControl_sourceParameterName :: Lens.Lens' ParameterTextAreaControl Prelude.Text
parameterTextAreaControl_sourceParameterName = Lens.lens (\ParameterTextAreaControl' {sourceParameterName} -> sourceParameterName) (\s@ParameterTextAreaControl' {} a -> s {sourceParameterName = a} :: ParameterTextAreaControl)

instance Data.FromJSON ParameterTextAreaControl where
  parseJSON =
    Data.withObject
      "ParameterTextAreaControl"
      ( \x ->
          ParameterTextAreaControl'
            Prelude.<$> (x Data..:? "Delimiter")
            Prelude.<*> (x Data..:? "DisplayOptions")
            Prelude.<*> (x Data..: "ParameterControlId")
            Prelude.<*> (x Data..: "Title")
            Prelude.<*> (x Data..: "SourceParameterName")
      )

instance Prelude.Hashable ParameterTextAreaControl where
  hashWithSalt _salt ParameterTextAreaControl' {..} =
    _salt
      `Prelude.hashWithSalt` delimiter
      `Prelude.hashWithSalt` displayOptions
      `Prelude.hashWithSalt` parameterControlId
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` sourceParameterName

instance Prelude.NFData ParameterTextAreaControl where
  rnf ParameterTextAreaControl' {..} =
    Prelude.rnf delimiter
      `Prelude.seq` Prelude.rnf displayOptions
      `Prelude.seq` Prelude.rnf parameterControlId
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf sourceParameterName

instance Data.ToJSON ParameterTextAreaControl where
  toJSON ParameterTextAreaControl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Delimiter" Data..=) Prelude.<$> delimiter,
            ("DisplayOptions" Data..=)
              Prelude.<$> displayOptions,
            Prelude.Just
              ("ParameterControlId" Data..= parameterControlId),
            Prelude.Just ("Title" Data..= title),
            Prelude.Just
              ("SourceParameterName" Data..= sourceParameterName)
          ]
      )
