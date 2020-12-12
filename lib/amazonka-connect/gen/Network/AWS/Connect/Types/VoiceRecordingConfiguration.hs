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

import Network.AWS.Connect.Types.VoiceRecordingTrack
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the recording configuration settings.
--
-- /See:/ 'mkVoiceRecordingConfiguration' smart constructor.
newtype VoiceRecordingConfiguration = VoiceRecordingConfiguration'
  { voiceRecordingTrack ::
      Lude.Maybe VoiceRecordingTrack
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VoiceRecordingConfiguration' with the minimum fields required to make a request.
--
-- * 'voiceRecordingTrack' - Identifies which track is being recorded.
mkVoiceRecordingConfiguration ::
  VoiceRecordingConfiguration
mkVoiceRecordingConfiguration =
  VoiceRecordingConfiguration' {voiceRecordingTrack = Lude.Nothing}

-- | Identifies which track is being recorded.
--
-- /Note:/ Consider using 'voiceRecordingTrack' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vrcVoiceRecordingTrack :: Lens.Lens' VoiceRecordingConfiguration (Lude.Maybe VoiceRecordingTrack)
vrcVoiceRecordingTrack = Lens.lens (voiceRecordingTrack :: VoiceRecordingConfiguration -> Lude.Maybe VoiceRecordingTrack) (\s a -> s {voiceRecordingTrack = a} :: VoiceRecordingConfiguration)
{-# DEPRECATED vrcVoiceRecordingTrack "Use generic-lens or generic-optics with 'voiceRecordingTrack' instead." #-}

instance Lude.ToJSON VoiceRecordingConfiguration where
  toJSON VoiceRecordingConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [("VoiceRecordingTrack" Lude..=) Lude.<$> voiceRecordingTrack]
      )
