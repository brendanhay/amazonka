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
-- Module      : Network.AWS.MediaConvert.Types.AudioChannelTaggingSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AudioChannelTaggingSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.AudioChannelTag
import qualified Network.AWS.Prelude as Prelude

-- | When you mimic a multi-channel audio layout with multiple mono-channel
-- tracks, you can tag each channel layout manually. For example, you would
-- tag the tracks that contain your left, right, and center audio with Left
-- (L), Right (R), and Center (C), respectively. When you don\'t specify a
-- value, MediaConvert labels your track as Center (C) by default. To use
-- audio layout tagging, your output must be in a QuickTime (.mov)
-- container; your audio codec must be AAC, WAV, or AIFF; and you must set
-- up your audio track to have only one channel.
--
-- /See:/ 'newAudioChannelTaggingSettings' smart constructor.
data AudioChannelTaggingSettings = AudioChannelTaggingSettings'
  { -- | You can add a tag for this mono-channel audio track to mimic its
    -- placement in a multi-channel layout. For example, if this track is the
    -- left surround channel, choose Left surround (LS).
    channelTag :: Prelude.Maybe AudioChannelTag
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AudioChannelTaggingSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelTag', 'audioChannelTaggingSettings_channelTag' - You can add a tag for this mono-channel audio track to mimic its
-- placement in a multi-channel layout. For example, if this track is the
-- left surround channel, choose Left surround (LS).
newAudioChannelTaggingSettings ::
  AudioChannelTaggingSettings
newAudioChannelTaggingSettings =
  AudioChannelTaggingSettings'
    { channelTag =
        Prelude.Nothing
    }

-- | You can add a tag for this mono-channel audio track to mimic its
-- placement in a multi-channel layout. For example, if this track is the
-- left surround channel, choose Left surround (LS).
audioChannelTaggingSettings_channelTag :: Lens.Lens' AudioChannelTaggingSettings (Prelude.Maybe AudioChannelTag)
audioChannelTaggingSettings_channelTag = Lens.lens (\AudioChannelTaggingSettings' {channelTag} -> channelTag) (\s@AudioChannelTaggingSettings' {} a -> s {channelTag = a} :: AudioChannelTaggingSettings)

instance Prelude.FromJSON AudioChannelTaggingSettings where
  parseJSON =
    Prelude.withObject
      "AudioChannelTaggingSettings"
      ( \x ->
          AudioChannelTaggingSettings'
            Prelude.<$> (x Prelude..:? "channelTag")
      )

instance Prelude.Hashable AudioChannelTaggingSettings

instance Prelude.NFData AudioChannelTaggingSettings

instance Prelude.ToJSON AudioChannelTaggingSettings where
  toJSON AudioChannelTaggingSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("channelTag" Prelude..=) Prelude.<$> channelTag]
      )
