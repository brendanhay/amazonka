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
-- Module      : Amazonka.MediaLive.Types.InputPrepareScheduleActionSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputPrepareScheduleActionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.InputClippingSettings
import qualified Amazonka.Prelude as Prelude

-- | Action to prepare an input for a future immediate input switch.
--
-- /See:/ 'newInputPrepareScheduleActionSettings' smart constructor.
data InputPrepareScheduleActionSettings = InputPrepareScheduleActionSettings'
  { -- | The name of the input attachment that should be prepared by this action.
    -- If no name is provided, the action will stop the most recent prepare (if
    -- any) when activated.
    inputAttachmentNameReference :: Prelude.Maybe Prelude.Text,
    -- | Settings to let you create a clip of the file input, in order to set up
    -- the input to ingest only a portion of the file.
    inputClippingSettings :: Prelude.Maybe InputClippingSettings,
    -- | The value for the variable portion of the URL for the dynamic input, for
    -- this instance of the input. Each time you use the same dynamic input in
    -- an input switch action, you can provide a different value, in order to
    -- connect the input to a different content source.
    urlPath :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputPrepareScheduleActionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputAttachmentNameReference', 'inputPrepareScheduleActionSettings_inputAttachmentNameReference' - The name of the input attachment that should be prepared by this action.
-- If no name is provided, the action will stop the most recent prepare (if
-- any) when activated.
--
-- 'inputClippingSettings', 'inputPrepareScheduleActionSettings_inputClippingSettings' - Settings to let you create a clip of the file input, in order to set up
-- the input to ingest only a portion of the file.
--
-- 'urlPath', 'inputPrepareScheduleActionSettings_urlPath' - The value for the variable portion of the URL for the dynamic input, for
-- this instance of the input. Each time you use the same dynamic input in
-- an input switch action, you can provide a different value, in order to
-- connect the input to a different content source.
newInputPrepareScheduleActionSettings ::
  InputPrepareScheduleActionSettings
newInputPrepareScheduleActionSettings =
  InputPrepareScheduleActionSettings'
    { inputAttachmentNameReference =
        Prelude.Nothing,
      inputClippingSettings = Prelude.Nothing,
      urlPath = Prelude.Nothing
    }

-- | The name of the input attachment that should be prepared by this action.
-- If no name is provided, the action will stop the most recent prepare (if
-- any) when activated.
inputPrepareScheduleActionSettings_inputAttachmentNameReference :: Lens.Lens' InputPrepareScheduleActionSettings (Prelude.Maybe Prelude.Text)
inputPrepareScheduleActionSettings_inputAttachmentNameReference = Lens.lens (\InputPrepareScheduleActionSettings' {inputAttachmentNameReference} -> inputAttachmentNameReference) (\s@InputPrepareScheduleActionSettings' {} a -> s {inputAttachmentNameReference = a} :: InputPrepareScheduleActionSettings)

-- | Settings to let you create a clip of the file input, in order to set up
-- the input to ingest only a portion of the file.
inputPrepareScheduleActionSettings_inputClippingSettings :: Lens.Lens' InputPrepareScheduleActionSettings (Prelude.Maybe InputClippingSettings)
inputPrepareScheduleActionSettings_inputClippingSettings = Lens.lens (\InputPrepareScheduleActionSettings' {inputClippingSettings} -> inputClippingSettings) (\s@InputPrepareScheduleActionSettings' {} a -> s {inputClippingSettings = a} :: InputPrepareScheduleActionSettings)

-- | The value for the variable portion of the URL for the dynamic input, for
-- this instance of the input. Each time you use the same dynamic input in
-- an input switch action, you can provide a different value, in order to
-- connect the input to a different content source.
inputPrepareScheduleActionSettings_urlPath :: Lens.Lens' InputPrepareScheduleActionSettings (Prelude.Maybe [Prelude.Text])
inputPrepareScheduleActionSettings_urlPath = Lens.lens (\InputPrepareScheduleActionSettings' {urlPath} -> urlPath) (\s@InputPrepareScheduleActionSettings' {} a -> s {urlPath = a} :: InputPrepareScheduleActionSettings) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    InputPrepareScheduleActionSettings
  where
  parseJSON =
    Data.withObject
      "InputPrepareScheduleActionSettings"
      ( \x ->
          InputPrepareScheduleActionSettings'
            Prelude.<$> (x Data..:? "inputAttachmentNameReference")
            Prelude.<*> (x Data..:? "inputClippingSettings")
            Prelude.<*> (x Data..:? "urlPath" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    InputPrepareScheduleActionSettings
  where
  hashWithSalt
    _salt
    InputPrepareScheduleActionSettings' {..} =
      _salt
        `Prelude.hashWithSalt` inputAttachmentNameReference
        `Prelude.hashWithSalt` inputClippingSettings
        `Prelude.hashWithSalt` urlPath

instance
  Prelude.NFData
    InputPrepareScheduleActionSettings
  where
  rnf InputPrepareScheduleActionSettings' {..} =
    Prelude.rnf inputAttachmentNameReference `Prelude.seq`
      Prelude.rnf inputClippingSettings `Prelude.seq`
        Prelude.rnf urlPath

instance
  Data.ToJSON
    InputPrepareScheduleActionSettings
  where
  toJSON InputPrepareScheduleActionSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("inputAttachmentNameReference" Data..=)
              Prelude.<$> inputAttachmentNameReference,
            ("inputClippingSettings" Data..=)
              Prelude.<$> inputClippingSettings,
            ("urlPath" Data..=) Prelude.<$> urlPath
          ]
      )
