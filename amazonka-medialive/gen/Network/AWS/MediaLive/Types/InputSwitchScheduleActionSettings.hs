{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaLive.Types.InputSwitchScheduleActionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSwitchScheduleActionSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputClippingSettings
import qualified Network.AWS.Prelude as Prelude

-- | Settings for the \"switch input\" action: to switch from ingesting one
-- input to ingesting another input.
--
-- /See:/ 'newInputSwitchScheduleActionSettings' smart constructor.
data InputSwitchScheduleActionSettings = InputSwitchScheduleActionSettings'
  { -- | The value for the variable portion of the URL for the dynamic input, for
    -- this instance of the input. Each time you use the same dynamic input in
    -- an input switch action, you can provide a different value, in order to
    -- connect the input to a different content source.
    urlPath :: Prelude.Maybe [Prelude.Text],
    -- | Settings to let you create a clip of the file input, in order to set up
    -- the input to ingest only a portion of the file.
    inputClippingSettings :: Prelude.Maybe InputClippingSettings,
    -- | The name of the input attachment (not the name of the input!) to switch
    -- to. The name is specified in the channel configuration.
    inputAttachmentNameReference :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InputSwitchScheduleActionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'urlPath', 'inputSwitchScheduleActionSettings_urlPath' - The value for the variable portion of the URL for the dynamic input, for
-- this instance of the input. Each time you use the same dynamic input in
-- an input switch action, you can provide a different value, in order to
-- connect the input to a different content source.
--
-- 'inputClippingSettings', 'inputSwitchScheduleActionSettings_inputClippingSettings' - Settings to let you create a clip of the file input, in order to set up
-- the input to ingest only a portion of the file.
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
      { urlPath =
          Prelude.Nothing,
        inputClippingSettings = Prelude.Nothing,
        inputAttachmentNameReference =
          pInputAttachmentNameReference_
      }

-- | The value for the variable portion of the URL for the dynamic input, for
-- this instance of the input. Each time you use the same dynamic input in
-- an input switch action, you can provide a different value, in order to
-- connect the input to a different content source.
inputSwitchScheduleActionSettings_urlPath :: Lens.Lens' InputSwitchScheduleActionSettings (Prelude.Maybe [Prelude.Text])
inputSwitchScheduleActionSettings_urlPath = Lens.lens (\InputSwitchScheduleActionSettings' {urlPath} -> urlPath) (\s@InputSwitchScheduleActionSettings' {} a -> s {urlPath = a} :: InputSwitchScheduleActionSettings) Prelude.. Lens.mapping Prelude._Coerce

-- | Settings to let you create a clip of the file input, in order to set up
-- the input to ingest only a portion of the file.
inputSwitchScheduleActionSettings_inputClippingSettings :: Lens.Lens' InputSwitchScheduleActionSettings (Prelude.Maybe InputClippingSettings)
inputSwitchScheduleActionSettings_inputClippingSettings = Lens.lens (\InputSwitchScheduleActionSettings' {inputClippingSettings} -> inputClippingSettings) (\s@InputSwitchScheduleActionSettings' {} a -> s {inputClippingSettings = a} :: InputSwitchScheduleActionSettings)

-- | The name of the input attachment (not the name of the input!) to switch
-- to. The name is specified in the channel configuration.
inputSwitchScheduleActionSettings_inputAttachmentNameReference :: Lens.Lens' InputSwitchScheduleActionSettings Prelude.Text
inputSwitchScheduleActionSettings_inputAttachmentNameReference = Lens.lens (\InputSwitchScheduleActionSettings' {inputAttachmentNameReference} -> inputAttachmentNameReference) (\s@InputSwitchScheduleActionSettings' {} a -> s {inputAttachmentNameReference = a} :: InputSwitchScheduleActionSettings)

instance
  Prelude.FromJSON
    InputSwitchScheduleActionSettings
  where
  parseJSON =
    Prelude.withObject
      "InputSwitchScheduleActionSettings"
      ( \x ->
          InputSwitchScheduleActionSettings'
            Prelude.<$> (x Prelude..:? "urlPath" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "inputClippingSettings")
            Prelude.<*> (x Prelude..: "inputAttachmentNameReference")
      )

instance
  Prelude.Hashable
    InputSwitchScheduleActionSettings

instance
  Prelude.NFData
    InputSwitchScheduleActionSettings

instance
  Prelude.ToJSON
    InputSwitchScheduleActionSettings
  where
  toJSON InputSwitchScheduleActionSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("urlPath" Prelude..=) Prelude.<$> urlPath,
            ("inputClippingSettings" Prelude..=)
              Prelude.<$> inputClippingSettings,
            Prelude.Just
              ( "inputAttachmentNameReference"
                  Prelude..= inputAttachmentNameReference
              )
          ]
      )
