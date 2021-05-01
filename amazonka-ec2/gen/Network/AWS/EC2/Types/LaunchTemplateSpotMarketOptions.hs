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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptions where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceInterruptionBehavior
import Network.AWS.EC2.Types.SpotInstanceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The options for Spot Instances.
--
-- /See:/ 'newLaunchTemplateSpotMarketOptions' smart constructor.
data LaunchTemplateSpotMarketOptions = LaunchTemplateSpotMarketOptions'
  { -- | The required duration for the Spot Instances (also known as Spot
    -- blocks), in minutes. This value must be a multiple of 60 (60, 120, 180,
    -- 240, 300, or 360).
    blockDurationMinutes :: Prelude.Maybe Prelude.Int,
    -- | The behavior when a Spot Instance is interrupted.
    instanceInterruptionBehavior :: Prelude.Maybe InstanceInterruptionBehavior,
    -- | The end date of the request. For a one-time request, the request remains
    -- active until all instances launch, the request is canceled, or this date
    -- is reached. If the request is persistent, it remains active until it is
    -- canceled or this date and time is reached.
    validUntil :: Prelude.Maybe Prelude.ISO8601,
    -- | The Spot Instance request type.
    spotInstanceType :: Prelude.Maybe SpotInstanceType,
    -- | The maximum hourly price you\'re willing to pay for the Spot Instances.
    maxPrice :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateSpotMarketOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockDurationMinutes', 'launchTemplateSpotMarketOptions_blockDurationMinutes' - The required duration for the Spot Instances (also known as Spot
-- blocks), in minutes. This value must be a multiple of 60 (60, 120, 180,
-- 240, 300, or 360).
--
-- 'instanceInterruptionBehavior', 'launchTemplateSpotMarketOptions_instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted.
--
-- 'validUntil', 'launchTemplateSpotMarketOptions_validUntil' - The end date of the request. For a one-time request, the request remains
-- active until all instances launch, the request is canceled, or this date
-- is reached. If the request is persistent, it remains active until it is
-- canceled or this date and time is reached.
--
-- 'spotInstanceType', 'launchTemplateSpotMarketOptions_spotInstanceType' - The Spot Instance request type.
--
-- 'maxPrice', 'launchTemplateSpotMarketOptions_maxPrice' - The maximum hourly price you\'re willing to pay for the Spot Instances.
newLaunchTemplateSpotMarketOptions ::
  LaunchTemplateSpotMarketOptions
newLaunchTemplateSpotMarketOptions =
  LaunchTemplateSpotMarketOptions'
    { blockDurationMinutes =
        Prelude.Nothing,
      instanceInterruptionBehavior =
        Prelude.Nothing,
      validUntil = Prelude.Nothing,
      spotInstanceType = Prelude.Nothing,
      maxPrice = Prelude.Nothing
    }

-- | The required duration for the Spot Instances (also known as Spot
-- blocks), in minutes. This value must be a multiple of 60 (60, 120, 180,
-- 240, 300, or 360).
launchTemplateSpotMarketOptions_blockDurationMinutes :: Lens.Lens' LaunchTemplateSpotMarketOptions (Prelude.Maybe Prelude.Int)
launchTemplateSpotMarketOptions_blockDurationMinutes = Lens.lens (\LaunchTemplateSpotMarketOptions' {blockDurationMinutes} -> blockDurationMinutes) (\s@LaunchTemplateSpotMarketOptions' {} a -> s {blockDurationMinutes = a} :: LaunchTemplateSpotMarketOptions)

-- | The behavior when a Spot Instance is interrupted.
launchTemplateSpotMarketOptions_instanceInterruptionBehavior :: Lens.Lens' LaunchTemplateSpotMarketOptions (Prelude.Maybe InstanceInterruptionBehavior)
launchTemplateSpotMarketOptions_instanceInterruptionBehavior = Lens.lens (\LaunchTemplateSpotMarketOptions' {instanceInterruptionBehavior} -> instanceInterruptionBehavior) (\s@LaunchTemplateSpotMarketOptions' {} a -> s {instanceInterruptionBehavior = a} :: LaunchTemplateSpotMarketOptions)

-- | The end date of the request. For a one-time request, the request remains
-- active until all instances launch, the request is canceled, or this date
-- is reached. If the request is persistent, it remains active until it is
-- canceled or this date and time is reached.
launchTemplateSpotMarketOptions_validUntil :: Lens.Lens' LaunchTemplateSpotMarketOptions (Prelude.Maybe Prelude.UTCTime)
launchTemplateSpotMarketOptions_validUntil = Lens.lens (\LaunchTemplateSpotMarketOptions' {validUntil} -> validUntil) (\s@LaunchTemplateSpotMarketOptions' {} a -> s {validUntil = a} :: LaunchTemplateSpotMarketOptions) Prelude.. Lens.mapping Prelude._Time

-- | The Spot Instance request type.
launchTemplateSpotMarketOptions_spotInstanceType :: Lens.Lens' LaunchTemplateSpotMarketOptions (Prelude.Maybe SpotInstanceType)
launchTemplateSpotMarketOptions_spotInstanceType = Lens.lens (\LaunchTemplateSpotMarketOptions' {spotInstanceType} -> spotInstanceType) (\s@LaunchTemplateSpotMarketOptions' {} a -> s {spotInstanceType = a} :: LaunchTemplateSpotMarketOptions)

-- | The maximum hourly price you\'re willing to pay for the Spot Instances.
launchTemplateSpotMarketOptions_maxPrice :: Lens.Lens' LaunchTemplateSpotMarketOptions (Prelude.Maybe Prelude.Text)
launchTemplateSpotMarketOptions_maxPrice = Lens.lens (\LaunchTemplateSpotMarketOptions' {maxPrice} -> maxPrice) (\s@LaunchTemplateSpotMarketOptions' {} a -> s {maxPrice = a} :: LaunchTemplateSpotMarketOptions)

instance
  Prelude.FromXML
    LaunchTemplateSpotMarketOptions
  where
  parseXML x =
    LaunchTemplateSpotMarketOptions'
      Prelude.<$> (x Prelude..@? "blockDurationMinutes")
      Prelude.<*> (x Prelude..@? "instanceInterruptionBehavior")
      Prelude.<*> (x Prelude..@? "validUntil")
      Prelude.<*> (x Prelude..@? "spotInstanceType")
      Prelude.<*> (x Prelude..@? "maxPrice")

instance
  Prelude.Hashable
    LaunchTemplateSpotMarketOptions

instance
  Prelude.NFData
    LaunchTemplateSpotMarketOptions
