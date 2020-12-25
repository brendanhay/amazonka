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
import qualified Network.AWS.MediaConvert.Types.NexGuardFileMarkerSettings as Types
import qualified Network.AWS.Prelude as Core

-- | If you work with a third party video watermarking partner, use the group of settings that correspond with your watermarking partner to include watermarks in your output.
--
-- /See:/ 'mkPartnerWatermarking' smart constructor.
newtype PartnerWatermarking = PartnerWatermarking'
  { -- | For forensic video watermarking, MediaConvert supports Nagra NexGuard File Marker watermarking. MediaConvert supports both PreRelease Content (NGPR/G2) and OTT Streaming workflows.
    nexguardFileMarkerSettings :: Core.Maybe Types.NexGuardFileMarkerSettings
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PartnerWatermarking' value with any optional fields omitted.
mkPartnerWatermarking ::
  PartnerWatermarking
mkPartnerWatermarking =
  PartnerWatermarking' {nexguardFileMarkerSettings = Core.Nothing}

-- | For forensic video watermarking, MediaConvert supports Nagra NexGuard File Marker watermarking. MediaConvert supports both PreRelease Content (NGPR/G2) and OTT Streaming workflows.
--
-- /Note:/ Consider using 'nexguardFileMarkerSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwNexguardFileMarkerSettings :: Lens.Lens' PartnerWatermarking (Core.Maybe Types.NexGuardFileMarkerSettings)
pwNexguardFileMarkerSettings = Lens.field @"nexguardFileMarkerSettings"
{-# DEPRECATED pwNexguardFileMarkerSettings "Use generic-lens or generic-optics with 'nexguardFileMarkerSettings' instead." #-}

instance Core.FromJSON PartnerWatermarking where
  toJSON PartnerWatermarking {..} =
    Core.object
      ( Core.catMaybes
          [ ("nexguardFileMarkerSettings" Core..=)
              Core.<$> nexguardFileMarkerSettings
          ]
      )

instance Core.FromJSON PartnerWatermarking where
  parseJSON =
    Core.withObject "PartnerWatermarking" Core.$
      \x ->
        PartnerWatermarking'
          Core.<$> (x Core..:? "nexguardFileMarkerSettings")
