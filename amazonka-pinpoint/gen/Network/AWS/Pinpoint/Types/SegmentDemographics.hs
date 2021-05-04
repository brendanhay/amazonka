{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.SetDimension
import qualified Network.AWS.Prelude as Prelude

-- | Specifies demographic-based dimension settings for including or
-- excluding endpoints from a segment. These settings derive from
-- characteristics of endpoint devices, such as platform, make, and model.
--
-- /See:/ 'newSegmentDemographics' smart constructor.
data SegmentDemographics = SegmentDemographics'
  { -- | The device model criteria for the segment.
    model :: Prelude.Maybe SetDimension,
    -- | The device platform criteria for the segment.
    platform :: Prelude.Maybe SetDimension,
    -- | The app version criteria for the segment.
    appVersion :: Prelude.Maybe SetDimension,
    -- | The channel criteria for the segment.
    channel :: Prelude.Maybe SetDimension,
    -- | The device type criteria for the segment.
    deviceType :: Prelude.Maybe SetDimension,
    -- | The device make criteria for the segment.
    make :: Prelude.Maybe SetDimension
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { model = Prelude.Nothing,
      platform = Prelude.Nothing,
      appVersion = Prelude.Nothing,
      channel = Prelude.Nothing,
      deviceType = Prelude.Nothing,
      make = Prelude.Nothing
    }

-- | The device model criteria for the segment.
segmentDemographics_model :: Lens.Lens' SegmentDemographics (Prelude.Maybe SetDimension)
segmentDemographics_model = Lens.lens (\SegmentDemographics' {model} -> model) (\s@SegmentDemographics' {} a -> s {model = a} :: SegmentDemographics)

-- | The device platform criteria for the segment.
segmentDemographics_platform :: Lens.Lens' SegmentDemographics (Prelude.Maybe SetDimension)
segmentDemographics_platform = Lens.lens (\SegmentDemographics' {platform} -> platform) (\s@SegmentDemographics' {} a -> s {platform = a} :: SegmentDemographics)

-- | The app version criteria for the segment.
segmentDemographics_appVersion :: Lens.Lens' SegmentDemographics (Prelude.Maybe SetDimension)
segmentDemographics_appVersion = Lens.lens (\SegmentDemographics' {appVersion} -> appVersion) (\s@SegmentDemographics' {} a -> s {appVersion = a} :: SegmentDemographics)

-- | The channel criteria for the segment.
segmentDemographics_channel :: Lens.Lens' SegmentDemographics (Prelude.Maybe SetDimension)
segmentDemographics_channel = Lens.lens (\SegmentDemographics' {channel} -> channel) (\s@SegmentDemographics' {} a -> s {channel = a} :: SegmentDemographics)

-- | The device type criteria for the segment.
segmentDemographics_deviceType :: Lens.Lens' SegmentDemographics (Prelude.Maybe SetDimension)
segmentDemographics_deviceType = Lens.lens (\SegmentDemographics' {deviceType} -> deviceType) (\s@SegmentDemographics' {} a -> s {deviceType = a} :: SegmentDemographics)

-- | The device make criteria for the segment.
segmentDemographics_make :: Lens.Lens' SegmentDemographics (Prelude.Maybe SetDimension)
segmentDemographics_make = Lens.lens (\SegmentDemographics' {make} -> make) (\s@SegmentDemographics' {} a -> s {make = a} :: SegmentDemographics)

instance Prelude.FromJSON SegmentDemographics where
  parseJSON =
    Prelude.withObject
      "SegmentDemographics"
      ( \x ->
          SegmentDemographics'
            Prelude.<$> (x Prelude..:? "Model")
            Prelude.<*> (x Prelude..:? "Platform")
            Prelude.<*> (x Prelude..:? "AppVersion")
            Prelude.<*> (x Prelude..:? "Channel")
            Prelude.<*> (x Prelude..:? "DeviceType")
            Prelude.<*> (x Prelude..:? "Make")
      )

instance Prelude.Hashable SegmentDemographics

instance Prelude.NFData SegmentDemographics

instance Prelude.ToJSON SegmentDemographics where
  toJSON SegmentDemographics' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Model" Prelude..=) Prelude.<$> model,
            ("Platform" Prelude..=) Prelude.<$> platform,
            ("AppVersion" Prelude..=) Prelude.<$> appVersion,
            ("Channel" Prelude..=) Prelude.<$> channel,
            ("DeviceType" Prelude..=) Prelude.<$> deviceType,
            ("Make" Prelude..=) Prelude.<$> make
          ]
      )
