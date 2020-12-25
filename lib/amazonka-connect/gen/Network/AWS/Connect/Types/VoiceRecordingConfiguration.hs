{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.VoiceRecordingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.VoiceRecordingConfiguration
  ( VoiceRecordingConfiguration (..),

    -- * Smart constructor
    mkVoiceRecordingConfiguration,

    -- * Lenses
    vrcVoiceRecordingTrack,
  )
where

import qualified Network.AWS.Connect.Types.VoiceRecordingTrack as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the recording configuration settings.
--
-- /See:/ 'mkVoiceRecordingConfiguration' smart constructor.
newtype VoiceRecordingConfiguration = VoiceRecordingConfiguration'
  { -- | Identifies which track is being recorded.
    voiceRecordingTrack :: Core.Maybe Types.VoiceRecordingTrack
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'VoiceRecordingConfiguration' value with any optional fields omitted.
mkVoiceRecordingConfiguration ::
  VoiceRecordingConfiguration
mkVoiceRecordingConfiguration =
  VoiceRecordingConfiguration' {voiceRecordingTrack = Core.Nothing}

-- | Identifies which track is being recorded.
--
-- /Note:/ Consider using 'voiceRecordingTrack' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vrcVoiceRecordingTrack :: Lens.Lens' VoiceRecordingConfiguration (Core.Maybe Types.VoiceRecordingTrack)
vrcVoiceRecordingTrack = Lens.field @"voiceRecordingTrack"
{-# DEPRECATED vrcVoiceRecordingTrack "Use generic-lens or generic-optics with 'voiceRecordingTrack' instead." #-}

instance Core.FromJSON VoiceRecordingConfiguration where
  toJSON VoiceRecordingConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [("VoiceRecordingTrack" Core..=) Core.<$> voiceRecordingTrack]
      )
