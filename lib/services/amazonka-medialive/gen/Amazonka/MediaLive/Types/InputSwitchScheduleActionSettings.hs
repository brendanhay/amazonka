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
-- Module      : Amazonka.MediaLive.Types.InputSwitchScheduleActionSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputSwitchScheduleActionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.InputClippingSettings
import qualified Amazonka.Prelude as Prelude

-- | Settings for the \"switch input\" action: to switch from ingesting one
-- input to ingesting another input.
--
-- /See:/ 'newInputSwitchScheduleActionSettings' smart constructor.
data InputSwitchScheduleActionSettings = InputSwitchScheduleActionSettings'
  { -- | Settings to let you create a clip of the file input, in order to set up
    -- the input to ingest only a portion of the file.
    inputClippingSettings :: Prelude.Maybe InputClippingSettings,
    -- | The value for the variable portion of the URL for the dynamic input, for
    -- this instance of the input. Each time you use the same dynamic input in
    -- an input switch action, you can provide a different value, in order to
    -- connect the input to a different content source.
    urlPath :: Prelude.Maybe [Prelude.Text],
    -- | The name of the input attachment (not the name of the input!) to switch
    -- to. The name is specified in the channel configuration.
    inputAttachmentNameReference :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputSwitchScheduleActionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputClippingSettings', 'inputSwitchScheduleActionSettings_inputClippingSettings' - Settings to let you create a clip of the file input, in order to set up
-- the input to ingest only a portion of the file.
--
-- 'urlPath', 'inputSwitchScheduleActionSettings_urlPath' - The value for the variable portion of the URL for the dynamic input, for
-- this instance of the input. Each time you use the same dynamic input in
-- an input switch action, you can provide a different value, in order to
-- connect the input to a different content source.
--
-- 'inputAttachmentNameReference', 'inputSwitchScheduleActionSettings_inputAttachmentNameReference' - The name of the input attachment (not the name of the input!) to switch
-- to. The name is specified in the channel configuration.
newInputSwitchScheduleActionSettings ::
  -- | 'inputAttachmentNameReference'
  Prelude.Text ->
  InputSwitchScheduleActionSettings
newInputSwitchScheduleActionSettings
  pInputAttachmentNameReference_ =
    InputSwitchScheduleActionSettings'
      { inputClippingSettings =
          Prelude.Nothing,
        urlPath = Prelude.Nothing,
        inputAttachmentNameReference =
          pInputAttachmentNameReference_
      }

-- | Settings to let you create a clip of the file input, in order to set up
-- the input to ingest only a portion of the file.
inputSwitchScheduleActionSettings_inputClippingSettings :: Lens.Lens' InputSwitchScheduleActionSettings (Prelude.Maybe InputClippingSettings)
inputSwitchScheduleActionSettings_inputClippingSettings = Lens.lens (\InputSwitchScheduleActionSettings' {inputClippingSettings} -> inputClippingSettings) (\s@InputSwitchScheduleActionSettings' {} a -> s {inputClippingSettings = a} :: InputSwitchScheduleActionSettings)

-- | The value for the variable portion of the URL for the dynamic input, for
-- this instance of the input. Each time you use the same dynamic input in
-- an input switch action, you can provide a different value, in order to
-- connect the input to a different content source.
inputSwitchScheduleActionSettings_urlPath :: Lens.Lens' InputSwitchScheduleActionSettings (Prelude.Maybe [Prelude.Text])
inputSwitchScheduleActionSettings_urlPath = Lens.lens (\InputSwitchScheduleActionSettings' {urlPath} -> urlPath) (\s@InputSwitchScheduleActionSettings' {} a -> s {urlPath = a} :: InputSwitchScheduleActionSettings) Prelude.. Lens.mapping Lens.coerced

-- | The name of the input attachment (not the name of the input!) to switch
-- to. The name is specified in the channel configuration.
inputSwitchScheduleActionSettings_inputAttachmentNameReference :: Lens.Lens' InputSwitchScheduleActionSettings Prelude.Text
inputSwitchScheduleActionSettings_inputAttachmentNameReference = Lens.lens (\InputSwitchScheduleActionSettings' {inputAttachmentNameReference} -> inputAttachmentNameReference) (\s@InputSwitchScheduleActionSettings' {} a -> s {inputAttachmentNameReference = a} :: InputSwitchScheduleActionSettings)

instance
  Data.FromJSON
    InputSwitchScheduleActionSettings
  where
  parseJSON =
    Data.withObject
      "InputSwitchScheduleActionSettings"
      ( \x ->
          InputSwitchScheduleActionSettings'
            Prelude.<$> (x Data..:? "inputClippingSettings")
            Prelude.<*> (x Data..:? "urlPath" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "inputAttachmentNameReference")
      )

instance
  Prelude.Hashable
    InputSwitchScheduleActionSettings
  where
  hashWithSalt
    _salt
    InputSwitchScheduleActionSettings' {..} =
      _salt `Prelude.hashWithSalt` inputClippingSettings
        `Prelude.hashWithSalt` urlPath
        `Prelude.hashWithSalt` inputAttachmentNameReference

instance
  Prelude.NFData
    InputSwitchScheduleActionSettings
  where
  rnf InputSwitchScheduleActionSettings' {..} =
    Prelude.rnf inputClippingSettings
      `Prelude.seq` Prelude.rnf urlPath
      `Prelude.seq` Prelude.rnf inputAttachmentNameReference

instance
  Data.ToJSON
    InputSwitchScheduleActionSettings
  where
  toJSON InputSwitchScheduleActionSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("inputClippingSettings" Data..=)
              Prelude.<$> inputClippingSettings,
            ("urlPath" Data..=) Prelude.<$> urlPath,
            Prelude.Just
              ( "inputAttachmentNameReference"
                  Data..= inputAttachmentNameReference
              )
          ]
      )
