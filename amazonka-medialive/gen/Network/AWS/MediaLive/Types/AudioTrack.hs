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
import qualified Network.AWS.Prelude as Prelude

-- | Audio Track
--
-- /See:/ 'newAudioTrack' smart constructor.
data AudioTrack = AudioTrack'
  { -- | 1-based integer value that maps to a specific audio track
    track :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Natural ->
  AudioTrack
newAudioTrack pTrack_ = AudioTrack' {track = pTrack_}

-- | 1-based integer value that maps to a specific audio track
audioTrack_track :: Lens.Lens' AudioTrack Prelude.Natural
audioTrack_track = Lens.lens (\AudioTrack' {track} -> track) (\s@AudioTrack' {} a -> s {track = a} :: AudioTrack)

instance Core.FromJSON AudioTrack where
  parseJSON =
    Core.withObject
      "AudioTrack"
      (\x -> AudioTrack' Prelude.<$> (x Core..: "track"))

instance Prelude.Hashable AudioTrack

instance Prelude.NFData AudioTrack

instance Core.ToJSON AudioTrack where
  toJSON AudioTrack' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("track" Core..= track)]
      )
