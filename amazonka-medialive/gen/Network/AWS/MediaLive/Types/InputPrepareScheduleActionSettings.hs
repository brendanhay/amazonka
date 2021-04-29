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
-- Module      : Network.AWS.MediaLive.Types.InputPrepareScheduleActionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputPrepareScheduleActionSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputClippingSettings
import qualified Network.AWS.Prelude as Prelude

-- | Action to prepare an input for a future immediate input switch.
--
-- /See:/ 'newInputPrepareScheduleActionSettings' smart constructor.
data InputPrepareScheduleActionSettings = InputPrepareScheduleActionSettings'
  { -- | The name of the input attachment that should be prepared by this action.
    -- If no name is provided, the action will stop the most recent prepare (if
    -- any) when activated.
    inputAttachmentNameReference :: Prelude.Maybe Prelude.Text,
    -- | The value for the variable portion of the URL for the dynamic input, for
    -- this instance of the input. Each time you use the same dynamic input in
    -- an input switch action, you can provide a different value, in order to
    -- connect the input to a different content source.
    urlPath :: Prelude.Maybe [Prelude.Text],
    -- | Settings to let you create a clip of the file input, in order to set up
    -- the input to ingest only a portion of the file.
    inputClippingSettings :: Prelude.Maybe InputClippingSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'urlPath', 'inputPrepareScheduleActionSettings_urlPath' - The value for the variable portion of the URL for the dynamic input, for
-- this instance of the input. Each time you use the same dynamic input in
-- an input switch action, you can provide a different value, in order to
-- connect the input to a different content source.
--
-- 'inputClippingSettings', 'inputPrepareScheduleActionSettings_inputClippingSettings' - Settings to let you create a clip of the file input, in order to set up
-- the input to ingest only a portion of the file.
newInputPrepareScheduleActionSettings ::
  InputPrepareScheduleActionSettings
newInputPrepareScheduleActionSettings =
  InputPrepareScheduleActionSettings'
    { inputAttachmentNameReference =
        Prelude.Nothing,
      urlPath = Prelude.Nothing,
      inputClippingSettings = Prelude.Nothing
    }

-- | The name of the input attachment that should be prepared by this action.
-- If no name is provided, the action will stop the most recent prepare (if
-- any) when activated.
inputPrepareScheduleActionSettings_inputAttachmentNameReference :: Lens.Lens' InputPrepareScheduleActionSettings (Prelude.Maybe Prelude.Text)
inputPrepareScheduleActionSettings_inputAttachmentNameReference = Lens.lens (\InputPrepareScheduleActionSettings' {inputAttachmentNameReference} -> inputAttachmentNameReference) (\s@InputPrepareScheduleActionSettings' {} a -> s {inputAttachmentNameReference = a} :: InputPrepareScheduleActionSettings)

-- | The value for the variable portion of the URL for the dynamic input, for
-- this instance of the input. Each time you use the same dynamic input in
-- an input switch action, you can provide a different value, in order to
-- connect the input to a different content source.
inputPrepareScheduleActionSettings_urlPath :: Lens.Lens' InputPrepareScheduleActionSettings (Prelude.Maybe [Prelude.Text])
inputPrepareScheduleActionSettings_urlPath = Lens.lens (\InputPrepareScheduleActionSettings' {urlPath} -> urlPath) (\s@InputPrepareScheduleActionSettings' {} a -> s {urlPath = a} :: InputPrepareScheduleActionSettings) Prelude.. Lens.mapping Prelude._Coerce

-- | Settings to let you create a clip of the file input, in order to set up
-- the input to ingest only a portion of the file.
inputPrepareScheduleActionSettings_inputClippingSettings :: Lens.Lens' InputPrepareScheduleActionSettings (Prelude.Maybe InputClippingSettings)
inputPrepareScheduleActionSettings_inputClippingSettings = Lens.lens (\InputPrepareScheduleActionSettings' {inputClippingSettings} -> inputClippingSettings) (\s@InputPrepareScheduleActionSettings' {} a -> s {inputClippingSettings = a} :: InputPrepareScheduleActionSettings)

instance
  Prelude.FromJSON
    InputPrepareScheduleActionSettings
  where
  parseJSON =
    Prelude.withObject
      "InputPrepareScheduleActionSettings"
      ( \x ->
          InputPrepareScheduleActionSettings'
            Prelude.<$> (x Prelude..:? "inputAttachmentNameReference")
            Prelude.<*> (x Prelude..:? "urlPath" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "inputClippingSettings")
      )

instance
  Prelude.Hashable
    InputPrepareScheduleActionSettings

instance
  Prelude.NFData
    InputPrepareScheduleActionSettings

instance
  Prelude.ToJSON
    InputPrepareScheduleActionSettings
  where
  toJSON InputPrepareScheduleActionSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("inputAttachmentNameReference" Prelude..=)
              Prelude.<$> inputAttachmentNameReference,
            ("urlPath" Prelude..=) Prelude.<$> urlPath,
            ("inputClippingSettings" Prelude..=)
              Prelude.<$> inputClippingSettings
          ]
      )
