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
-- Module      : Network.AWS.MediaLive.Types.AudioTrack
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AudioTrack where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Audio Track
--
-- /See:/ 'newAudioTrack' smart constructor.
data AudioTrack = AudioTrack'
  { -- | 1-based integer value that maps to a specific audio track
    track :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AudioTrack' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'track', 'audioTrack_track' - 1-based integer value that maps to a specific audio track
newAudioTrack ::
  -- | 'track'
  Core.Natural ->
  AudioTrack
newAudioTrack pTrack_ = AudioTrack' {track = pTrack_}

-- | 1-based integer value that maps to a specific audio track
audioTrack_track :: Lens.Lens' AudioTrack Core.Natural
audioTrack_track = Lens.lens (\AudioTrack' {track} -> track) (\s@AudioTrack' {} a -> s {track = a} :: AudioTrack)

instance Core.FromJSON AudioTrack where
  parseJSON =
    Core.withObject
      "AudioTrack"
      (\x -> AudioTrack' Core.<$> (x Core..: "track"))

instance Core.Hashable AudioTrack

instance Core.NFData AudioTrack

instance Core.ToJSON AudioTrack where
  toJSON AudioTrack' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("track" Core..= track)])
