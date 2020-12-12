{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentDemographics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentDemographics
  ( SegmentDemographics (..),

    -- * Smart constructor
    mkSegmentDemographics,

    -- * Lenses
    sdPlatform,
    sdAppVersion,
    sdChannel,
    sdModel,
    sdMake,
    sdDeviceType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.SetDimension
import qualified Network.AWS.Prelude as Lude

-- | Specifies demographic-based dimension settings for including or excluding endpoints from a segment. These settings derive from characteristics of endpoint devices, such as platform, make, and model.
--
-- /See:/ 'mkSegmentDemographics' smart constructor.
data SegmentDemographics = SegmentDemographics'
  { platform ::
      Lude.Maybe SetDimension,
    appVersion :: Lude.Maybe SetDimension,
    channel :: Lude.Maybe SetDimension,
    model :: Lude.Maybe SetDimension,
    make :: Lude.Maybe SetDimension,
    deviceType :: Lude.Maybe SetDimension
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SegmentDemographics' with the minimum fields required to make a request.
--
-- * 'appVersion' - The app version criteria for the segment.
-- * 'channel' - The channel criteria for the segment.
-- * 'deviceType' - The device type criteria for the segment.
-- * 'make' - The device make criteria for the segment.
-- * 'model' - The device model criteria for the segment.
-- * 'platform' - The device platform criteria for the segment.
mkSegmentDemographics ::
  SegmentDemographics
mkSegmentDemographics =
  SegmentDemographics'
    { platform = Lude.Nothing,
      appVersion = Lude.Nothing,
      channel = Lude.Nothing,
      model = Lude.Nothing,
      make = Lude.Nothing,
      deviceType = Lude.Nothing
    }

-- | The device platform criteria for the segment.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdPlatform :: Lens.Lens' SegmentDemographics (Lude.Maybe SetDimension)
sdPlatform = Lens.lens (platform :: SegmentDemographics -> Lude.Maybe SetDimension) (\s a -> s {platform = a} :: SegmentDemographics)
{-# DEPRECATED sdPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The app version criteria for the segment.
--
-- /Note:/ Consider using 'appVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdAppVersion :: Lens.Lens' SegmentDemographics (Lude.Maybe SetDimension)
sdAppVersion = Lens.lens (appVersion :: SegmentDemographics -> Lude.Maybe SetDimension) (\s a -> s {appVersion = a} :: SegmentDemographics)
{-# DEPRECATED sdAppVersion "Use generic-lens or generic-optics with 'appVersion' instead." #-}

-- | The channel criteria for the segment.
--
-- /Note:/ Consider using 'channel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdChannel :: Lens.Lens' SegmentDemographics (Lude.Maybe SetDimension)
sdChannel = Lens.lens (channel :: SegmentDemographics -> Lude.Maybe SetDimension) (\s a -> s {channel = a} :: SegmentDemographics)
{-# DEPRECATED sdChannel "Use generic-lens or generic-optics with 'channel' instead." #-}

-- | The device model criteria for the segment.
--
-- /Note:/ Consider using 'model' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdModel :: Lens.Lens' SegmentDemographics (Lude.Maybe SetDimension)
sdModel = Lens.lens (model :: SegmentDemographics -> Lude.Maybe SetDimension) (\s a -> s {model = a} :: SegmentDemographics)
{-# DEPRECATED sdModel "Use generic-lens or generic-optics with 'model' instead." #-}

-- | The device make criteria for the segment.
--
-- /Note:/ Consider using 'make' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdMake :: Lens.Lens' SegmentDemographics (Lude.Maybe SetDimension)
sdMake = Lens.lens (make :: SegmentDemographics -> Lude.Maybe SetDimension) (\s a -> s {make = a} :: SegmentDemographics)
{-# DEPRECATED sdMake "Use generic-lens or generic-optics with 'make' instead." #-}

-- | The device type criteria for the segment.
--
-- /Note:/ Consider using 'deviceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdDeviceType :: Lens.Lens' SegmentDemographics (Lude.Maybe SetDimension)
sdDeviceType = Lens.lens (deviceType :: SegmentDemographics -> Lude.Maybe SetDimension) (\s a -> s {deviceType = a} :: SegmentDemographics)
{-# DEPRECATED sdDeviceType "Use generic-lens or generic-optics with 'deviceType' instead." #-}

instance Lude.FromJSON SegmentDemographics where
  parseJSON =
    Lude.withObject
      "SegmentDemographics"
      ( \x ->
          SegmentDemographics'
            Lude.<$> (x Lude..:? "Platform")
            Lude.<*> (x Lude..:? "AppVersion")
            Lude.<*> (x Lude..:? "Channel")
            Lude.<*> (x Lude..:? "Model")
            Lude.<*> (x Lude..:? "Make")
            Lude.<*> (x Lude..:? "DeviceType")
      )

instance Lude.ToJSON SegmentDemographics where
  toJSON SegmentDemographics' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Platform" Lude..=) Lude.<$> platform,
            ("AppVersion" Lude..=) Lude.<$> appVersion,
            ("Channel" Lude..=) Lude.<$> channel,
            ("Model" Lude..=) Lude.<$> model,
            ("Make" Lude..=) Lude.<$> make,
            ("DeviceType" Lude..=) Lude.<$> deviceType
          ]
      )
