{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentDemographics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.SegmentDemographics
  ( SegmentDemographics (..)
  -- * Smart constructor
  , mkSegmentDemographics
  -- * Lenses
  , sdAppVersion
  , sdChannel
  , sdDeviceType
  , sdMake
  , sdModel
  , sdPlatform
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.SetDimension as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies demographic-based dimension settings for including or excluding endpoints from a segment. These settings derive from characteristics of endpoint devices, such as platform, make, and model.
--
-- /See:/ 'mkSegmentDemographics' smart constructor.
data SegmentDemographics = SegmentDemographics'
  { appVersion :: Core.Maybe Types.SetDimension
    -- ^ The app version criteria for the segment.
  , channel :: Core.Maybe Types.SetDimension
    -- ^ The channel criteria for the segment.
  , deviceType :: Core.Maybe Types.SetDimension
    -- ^ The device type criteria for the segment.
  , make :: Core.Maybe Types.SetDimension
    -- ^ The device make criteria for the segment.
  , model :: Core.Maybe Types.SetDimension
    -- ^ The device model criteria for the segment.
  , platform :: Core.Maybe Types.SetDimension
    -- ^ The device platform criteria for the segment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SegmentDemographics' value with any optional fields omitted.
mkSegmentDemographics
    :: SegmentDemographics
mkSegmentDemographics
  = SegmentDemographics'{appVersion = Core.Nothing,
                         channel = Core.Nothing, deviceType = Core.Nothing,
                         make = Core.Nothing, model = Core.Nothing, platform = Core.Nothing}

-- | The app version criteria for the segment.
--
-- /Note:/ Consider using 'appVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdAppVersion :: Lens.Lens' SegmentDemographics (Core.Maybe Types.SetDimension)
sdAppVersion = Lens.field @"appVersion"
{-# INLINEABLE sdAppVersion #-}
{-# DEPRECATED appVersion "Use generic-lens or generic-optics with 'appVersion' instead"  #-}

-- | The channel criteria for the segment.
--
-- /Note:/ Consider using 'channel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdChannel :: Lens.Lens' SegmentDemographics (Core.Maybe Types.SetDimension)
sdChannel = Lens.field @"channel"
{-# INLINEABLE sdChannel #-}
{-# DEPRECATED channel "Use generic-lens or generic-optics with 'channel' instead"  #-}

-- | The device type criteria for the segment.
--
-- /Note:/ Consider using 'deviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdDeviceType :: Lens.Lens' SegmentDemographics (Core.Maybe Types.SetDimension)
sdDeviceType = Lens.field @"deviceType"
{-# INLINEABLE sdDeviceType #-}
{-# DEPRECATED deviceType "Use generic-lens or generic-optics with 'deviceType' instead"  #-}

-- | The device make criteria for the segment.
--
-- /Note:/ Consider using 'make' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdMake :: Lens.Lens' SegmentDemographics (Core.Maybe Types.SetDimension)
sdMake = Lens.field @"make"
{-# INLINEABLE sdMake #-}
{-# DEPRECATED make "Use generic-lens or generic-optics with 'make' instead"  #-}

-- | The device model criteria for the segment.
--
-- /Note:/ Consider using 'model' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdModel :: Lens.Lens' SegmentDemographics (Core.Maybe Types.SetDimension)
sdModel = Lens.field @"model"
{-# INLINEABLE sdModel #-}
{-# DEPRECATED model "Use generic-lens or generic-optics with 'model' instead"  #-}

-- | The device platform criteria for the segment.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdPlatform :: Lens.Lens' SegmentDemographics (Core.Maybe Types.SetDimension)
sdPlatform = Lens.field @"platform"
{-# INLINEABLE sdPlatform #-}
{-# DEPRECATED platform "Use generic-lens or generic-optics with 'platform' instead"  #-}

instance Core.FromJSON SegmentDemographics where
        toJSON SegmentDemographics{..}
          = Core.object
              (Core.catMaybes
                 [("AppVersion" Core..=) Core.<$> appVersion,
                  ("Channel" Core..=) Core.<$> channel,
                  ("DeviceType" Core..=) Core.<$> deviceType,
                  ("Make" Core..=) Core.<$> make, ("Model" Core..=) Core.<$> model,
                  ("Platform" Core..=) Core.<$> platform])

instance Core.FromJSON SegmentDemographics where
        parseJSON
          = Core.withObject "SegmentDemographics" Core.$
              \ x ->
                SegmentDemographics' Core.<$>
                  (x Core..:? "AppVersion") Core.<*> x Core..:? "Channel" Core.<*>
                    x Core..:? "DeviceType"
                    Core.<*> x Core..:? "Make"
                    Core.<*> x Core..:? "Model"
                    Core.<*> x Core..:? "Platform"
