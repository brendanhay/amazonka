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
-- Module      : Network.AWS.Pinpoint.Types.SegmentDemographics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentDemographics where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.SetDimension

-- | Specifies demographic-based dimension settings for including or
-- excluding endpoints from a segment. These settings derive from
-- characteristics of endpoint devices, such as platform, make, and model.
--
-- /See:/ 'newSegmentDemographics' smart constructor.
data SegmentDemographics = SegmentDemographics'
  { -- | The device model criteria for the segment.
    model :: Core.Maybe SetDimension,
    -- | The device platform criteria for the segment.
    platform :: Core.Maybe SetDimension,
    -- | The app version criteria for the segment.
    appVersion :: Core.Maybe SetDimension,
    -- | The channel criteria for the segment.
    channel :: Core.Maybe SetDimension,
    -- | The device type criteria for the segment.
    deviceType :: Core.Maybe SetDimension,
    -- | The device make criteria for the segment.
    make :: Core.Maybe SetDimension
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SegmentDemographics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'model', 'segmentDemographics_model' - The device model criteria for the segment.
--
-- 'platform', 'segmentDemographics_platform' - The device platform criteria for the segment.
--
-- 'appVersion', 'segmentDemographics_appVersion' - The app version criteria for the segment.
--
-- 'channel', 'segmentDemographics_channel' - The channel criteria for the segment.
--
-- 'deviceType', 'segmentDemographics_deviceType' - The device type criteria for the segment.
--
-- 'make', 'segmentDemographics_make' - The device make criteria for the segment.
newSegmentDemographics ::
  SegmentDemographics
newSegmentDemographics =
  SegmentDemographics'
    { model = Core.Nothing,
      platform = Core.Nothing,
      appVersion = Core.Nothing,
      channel = Core.Nothing,
      deviceType = Core.Nothing,
      make = Core.Nothing
    }

-- | The device model criteria for the segment.
segmentDemographics_model :: Lens.Lens' SegmentDemographics (Core.Maybe SetDimension)
segmentDemographics_model = Lens.lens (\SegmentDemographics' {model} -> model) (\s@SegmentDemographics' {} a -> s {model = a} :: SegmentDemographics)

-- | The device platform criteria for the segment.
segmentDemographics_platform :: Lens.Lens' SegmentDemographics (Core.Maybe SetDimension)
segmentDemographics_platform = Lens.lens (\SegmentDemographics' {platform} -> platform) (\s@SegmentDemographics' {} a -> s {platform = a} :: SegmentDemographics)

-- | The app version criteria for the segment.
segmentDemographics_appVersion :: Lens.Lens' SegmentDemographics (Core.Maybe SetDimension)
segmentDemographics_appVersion = Lens.lens (\SegmentDemographics' {appVersion} -> appVersion) (\s@SegmentDemographics' {} a -> s {appVersion = a} :: SegmentDemographics)

-- | The channel criteria for the segment.
segmentDemographics_channel :: Lens.Lens' SegmentDemographics (Core.Maybe SetDimension)
segmentDemographics_channel = Lens.lens (\SegmentDemographics' {channel} -> channel) (\s@SegmentDemographics' {} a -> s {channel = a} :: SegmentDemographics)

-- | The device type criteria for the segment.
segmentDemographics_deviceType :: Lens.Lens' SegmentDemographics (Core.Maybe SetDimension)
segmentDemographics_deviceType = Lens.lens (\SegmentDemographics' {deviceType} -> deviceType) (\s@SegmentDemographics' {} a -> s {deviceType = a} :: SegmentDemographics)

-- | The device make criteria for the segment.
segmentDemographics_make :: Lens.Lens' SegmentDemographics (Core.Maybe SetDimension)
segmentDemographics_make = Lens.lens (\SegmentDemographics' {make} -> make) (\s@SegmentDemographics' {} a -> s {make = a} :: SegmentDemographics)

instance Core.FromJSON SegmentDemographics where
  parseJSON =
    Core.withObject
      "SegmentDemographics"
      ( \x ->
          SegmentDemographics'
            Core.<$> (x Core..:? "Model")
            Core.<*> (x Core..:? "Platform")
            Core.<*> (x Core..:? "AppVersion")
            Core.<*> (x Core..:? "Channel")
            Core.<*> (x Core..:? "DeviceType")
            Core.<*> (x Core..:? "Make")
      )

instance Core.Hashable SegmentDemographics

instance Core.NFData SegmentDemographics

instance Core.ToJSON SegmentDemographics where
  toJSON SegmentDemographics' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Model" Core..=) Core.<$> model,
            ("Platform" Core..=) Core.<$> platform,
            ("AppVersion" Core..=) Core.<$> appVersion,
            ("Channel" Core..=) Core.<$> channel,
            ("DeviceType" Core..=) Core.<$> deviceType,
            ("Make" Core..=) Core.<$> make
          ]
      )
