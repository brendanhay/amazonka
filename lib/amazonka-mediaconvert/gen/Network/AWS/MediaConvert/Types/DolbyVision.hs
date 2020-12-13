{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DolbyVision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DolbyVision
  ( DolbyVision (..),

    -- * Smart constructor
    mkDolbyVision,

    -- * Lenses
    dvProfile,
    dvL6Mode,
    dvL6Metadata,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.DolbyVisionLevel6Metadata
import Network.AWS.MediaConvert.Types.DolbyVisionLevel6Mode
import Network.AWS.MediaConvert.Types.DolbyVisionProfile
import qualified Network.AWS.Prelude as Lude

-- | Settings for Dolby Vision
--
-- /See:/ 'mkDolbyVision' smart constructor.
data DolbyVision = DolbyVision'
  { -- | In the current MediaConvert implementation, the Dolby Vision profile is always 5 (PROFILE_5). Therefore, all of your inputs must contain Dolby Vision frame interleaved data.
    profile :: Lude.Maybe DolbyVisionProfile,
    -- | Use Dolby Vision Mode to choose how the service will handle Dolby Vision MaxCLL and MaxFALL properies.
    l6Mode :: Lude.Maybe DolbyVisionLevel6Mode,
    -- | Use these settings when you set DolbyVisionLevel6Mode to SPECIFY to override the MaxCLL and MaxFALL values in your input with new values.
    l6Metadata :: Lude.Maybe DolbyVisionLevel6Metadata
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DolbyVision' with the minimum fields required to make a request.
--
-- * 'profile' - In the current MediaConvert implementation, the Dolby Vision profile is always 5 (PROFILE_5). Therefore, all of your inputs must contain Dolby Vision frame interleaved data.
-- * 'l6Mode' - Use Dolby Vision Mode to choose how the service will handle Dolby Vision MaxCLL and MaxFALL properies.
-- * 'l6Metadata' - Use these settings when you set DolbyVisionLevel6Mode to SPECIFY to override the MaxCLL and MaxFALL values in your input with new values.
mkDolbyVision ::
  DolbyVision
mkDolbyVision =
  DolbyVision'
    { profile = Lude.Nothing,
      l6Mode = Lude.Nothing,
      l6Metadata = Lude.Nothing
    }

-- | In the current MediaConvert implementation, the Dolby Vision profile is always 5 (PROFILE_5). Therefore, all of your inputs must contain Dolby Vision frame interleaved data.
--
-- /Note:/ Consider using 'profile' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvProfile :: Lens.Lens' DolbyVision (Lude.Maybe DolbyVisionProfile)
dvProfile = Lens.lens (profile :: DolbyVision -> Lude.Maybe DolbyVisionProfile) (\s a -> s {profile = a} :: DolbyVision)
{-# DEPRECATED dvProfile "Use generic-lens or generic-optics with 'profile' instead." #-}

-- | Use Dolby Vision Mode to choose how the service will handle Dolby Vision MaxCLL and MaxFALL properies.
--
-- /Note:/ Consider using 'l6Mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvL6Mode :: Lens.Lens' DolbyVision (Lude.Maybe DolbyVisionLevel6Mode)
dvL6Mode = Lens.lens (l6Mode :: DolbyVision -> Lude.Maybe DolbyVisionLevel6Mode) (\s a -> s {l6Mode = a} :: DolbyVision)
{-# DEPRECATED dvL6Mode "Use generic-lens or generic-optics with 'l6Mode' instead." #-}

-- | Use these settings when you set DolbyVisionLevel6Mode to SPECIFY to override the MaxCLL and MaxFALL values in your input with new values.
--
-- /Note:/ Consider using 'l6Metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvL6Metadata :: Lens.Lens' DolbyVision (Lude.Maybe DolbyVisionLevel6Metadata)
dvL6Metadata = Lens.lens (l6Metadata :: DolbyVision -> Lude.Maybe DolbyVisionLevel6Metadata) (\s a -> s {l6Metadata = a} :: DolbyVision)
{-# DEPRECATED dvL6Metadata "Use generic-lens or generic-optics with 'l6Metadata' instead." #-}

instance Lude.FromJSON DolbyVision where
  parseJSON =
    Lude.withObject
      "DolbyVision"
      ( \x ->
          DolbyVision'
            Lude.<$> (x Lude..:? "profile")
            Lude.<*> (x Lude..:? "l6Mode")
            Lude.<*> (x Lude..:? "l6Metadata")
      )

instance Lude.ToJSON DolbyVision where
  toJSON DolbyVision' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("profile" Lude..=) Lude.<$> profile,
            ("l6Mode" Lude..=) Lude.<$> l6Mode,
            ("l6Metadata" Lude..=) Lude.<$> l6Metadata
          ]
      )
