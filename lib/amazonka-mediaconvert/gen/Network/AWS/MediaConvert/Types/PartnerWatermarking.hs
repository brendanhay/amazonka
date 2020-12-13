{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.PartnerWatermarking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.PartnerWatermarking
  ( PartnerWatermarking (..),

    -- * Smart constructor
    mkPartnerWatermarking,

    -- * Lenses
    pwNexguardFileMarkerSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.NexGuardFileMarkerSettings
import qualified Network.AWS.Prelude as Lude

-- | If you work with a third party video watermarking partner, use the group of settings that correspond with your watermarking partner to include watermarks in your output.
--
-- /See:/ 'mkPartnerWatermarking' smart constructor.
newtype PartnerWatermarking = PartnerWatermarking'
  { -- | For forensic video watermarking, MediaConvert supports Nagra NexGuard File Marker watermarking. MediaConvert supports both PreRelease Content (NGPR/G2) and OTT Streaming workflows.
    nexguardFileMarkerSettings :: Lude.Maybe NexGuardFileMarkerSettings
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PartnerWatermarking' with the minimum fields required to make a request.
--
-- * 'nexguardFileMarkerSettings' - For forensic video watermarking, MediaConvert supports Nagra NexGuard File Marker watermarking. MediaConvert supports both PreRelease Content (NGPR/G2) and OTT Streaming workflows.
mkPartnerWatermarking ::
  PartnerWatermarking
mkPartnerWatermarking =
  PartnerWatermarking' {nexguardFileMarkerSettings = Lude.Nothing}

-- | For forensic video watermarking, MediaConvert supports Nagra NexGuard File Marker watermarking. MediaConvert supports both PreRelease Content (NGPR/G2) and OTT Streaming workflows.
--
-- /Note:/ Consider using 'nexguardFileMarkerSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwNexguardFileMarkerSettings :: Lens.Lens' PartnerWatermarking (Lude.Maybe NexGuardFileMarkerSettings)
pwNexguardFileMarkerSettings = Lens.lens (nexguardFileMarkerSettings :: PartnerWatermarking -> Lude.Maybe NexGuardFileMarkerSettings) (\s a -> s {nexguardFileMarkerSettings = a} :: PartnerWatermarking)
{-# DEPRECATED pwNexguardFileMarkerSettings "Use generic-lens or generic-optics with 'nexguardFileMarkerSettings' instead." #-}

instance Lude.FromJSON PartnerWatermarking where
  parseJSON =
    Lude.withObject
      "PartnerWatermarking"
      ( \x ->
          PartnerWatermarking'
            Lude.<$> (x Lude..:? "nexguardFileMarkerSettings")
      )

instance Lude.ToJSON PartnerWatermarking where
  toJSON PartnerWatermarking' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nexguardFileMarkerSettings" Lude..=)
              Lude.<$> nexguardFileMarkerSettings
          ]
      )
