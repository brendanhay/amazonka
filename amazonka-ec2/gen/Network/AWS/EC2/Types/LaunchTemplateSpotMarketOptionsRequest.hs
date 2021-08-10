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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptionsRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateSpotMarketOptionsRequest where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceInterruptionBehavior
import Network.AWS.EC2.Types.SpotInstanceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The options for Spot Instances.
--
-- /See:/ 'newLaunchTemplateSpotMarketOptionsRequest' smart constructor.
data LaunchTemplateSpotMarketOptionsRequest = LaunchTemplateSpotMarketOptionsRequest'
  { -- | The required duration for the Spot Instances (also known as Spot
    -- blocks), in minutes. This value must be a multiple of 60 (60, 120, 180,
    -- 240, 300, or 360).
    blockDurationMinutes :: Prelude.Maybe Prelude.Int,
    -- | The behavior when a Spot Instance is interrupted. The default is
    -- @terminate@.
    instanceInterruptionBehavior :: Prelude.Maybe InstanceInterruptionBehavior,
    -- | The end date of the request. For a one-time request, the request remains
    -- active until all instances launch, the request is canceled, or this date
    -- is reached. If the request is persistent, it remains active until it is
    -- canceled or this date and time is reached. The default end date is 7
    -- days from the current date.
    validUntil :: Prelude.Maybe Core.ISO8601,
    -- | The Spot Instance request type.
    spotInstanceType :: Prelude.Maybe SpotInstanceType,
    -- | The maximum hourly price you\'re willing to pay for the Spot Instances.
    maxPrice :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateSpotMarketOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockDurationMinutes', 'launchTemplateSpotMarketOptionsRequest_blockDurationMinutes' - The required duration for the Spot Instances (also known as Spot
-- blocks), in minutes. This value must be a multiple of 60 (60, 120, 180,
-- 240, 300, or 360).
--
-- 'instanceInterruptionBehavior', 'launchTemplateSpotMarketOptionsRequest_instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is
-- @terminate@.
--
-- 'validUntil', 'launchTemplateSpotMarketOptionsRequest_validUntil' - The end date of the request. For a one-time request, the request remains
-- active until all instances launch, the request is canceled, or this date
-- is reached. If the request is persistent, it remains active until it is
-- canceled or this date and time is reached. The default end date is 7
-- days from the current date.
--
-- 'spotInstanceType', 'launchTemplateSpotMarketOptionsRequest_spotInstanceType' - The Spot Instance request type.
--
-- 'maxPrice', 'launchTemplateSpotMarketOptionsRequest_maxPrice' - The maximum hourly price you\'re willing to pay for the Spot Instances.
newLaunchTemplateSpotMarketOptionsRequest ::
  LaunchTemplateSpotMarketOptionsRequest
newLaunchTemplateSpotMarketOptionsRequest =
  LaunchTemplateSpotMarketOptionsRequest'
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
launchTemplateSpotMarketOptionsRequest_blockDurationMinutes :: Lens.Lens' LaunchTemplateSpotMarketOptionsRequest (Prelude.Maybe Prelude.Int)
launchTemplateSpotMarketOptionsRequest_blockDurationMinutes = Lens.lens (\LaunchTemplateSpotMarketOptionsRequest' {blockDurationMinutes} -> blockDurationMinutes) (\s@LaunchTemplateSpotMarketOptionsRequest' {} a -> s {blockDurationMinutes = a} :: LaunchTemplateSpotMarketOptionsRequest)

-- | The behavior when a Spot Instance is interrupted. The default is
-- @terminate@.
launchTemplateSpotMarketOptionsRequest_instanceInterruptionBehavior :: Lens.Lens' LaunchTemplateSpotMarketOptionsRequest (Prelude.Maybe InstanceInterruptionBehavior)
launchTemplateSpotMarketOptionsRequest_instanceInterruptionBehavior = Lens.lens (\LaunchTemplateSpotMarketOptionsRequest' {instanceInterruptionBehavior} -> instanceInterruptionBehavior) (\s@LaunchTemplateSpotMarketOptionsRequest' {} a -> s {instanceInterruptionBehavior = a} :: LaunchTemplateSpotMarketOptionsRequest)

-- | The end date of the request. For a one-time request, the request remains
-- active until all instances launch, the request is canceled, or this date
-- is reached. If the request is persistent, it remains active until it is
-- canceled or this date and time is reached. The default end date is 7
-- days from the current date.
launchTemplateSpotMarketOptionsRequest_validUntil :: Lens.Lens' LaunchTemplateSpotMarketOptionsRequest (Prelude.Maybe Prelude.UTCTime)
launchTemplateSpotMarketOptionsRequest_validUntil = Lens.lens (\LaunchTemplateSpotMarketOptionsRequest' {validUntil} -> validUntil) (\s@LaunchTemplateSpotMarketOptionsRequest' {} a -> s {validUntil = a} :: LaunchTemplateSpotMarketOptionsRequest) Prelude.. Lens.mapping Core._Time

-- | The Spot Instance request type.
launchTemplateSpotMarketOptionsRequest_spotInstanceType :: Lens.Lens' LaunchTemplateSpotMarketOptionsRequest (Prelude.Maybe SpotInstanceType)
launchTemplateSpotMarketOptionsRequest_spotInstanceType = Lens.lens (\LaunchTemplateSpotMarketOptionsRequest' {spotInstanceType} -> spotInstanceType) (\s@LaunchTemplateSpotMarketOptionsRequest' {} a -> s {spotInstanceType = a} :: LaunchTemplateSpotMarketOptionsRequest)

-- | The maximum hourly price you\'re willing to pay for the Spot Instances.
launchTemplateSpotMarketOptionsRequest_maxPrice :: Lens.Lens' LaunchTemplateSpotMarketOptionsRequest (Prelude.Maybe Prelude.Text)
launchTemplateSpotMarketOptionsRequest_maxPrice = Lens.lens (\LaunchTemplateSpotMarketOptionsRequest' {maxPrice} -> maxPrice) (\s@LaunchTemplateSpotMarketOptionsRequest' {} a -> s {maxPrice = a} :: LaunchTemplateSpotMarketOptionsRequest)

instance
  Prelude.Hashable
    LaunchTemplateSpotMarketOptionsRequest

instance
  Prelude.NFData
    LaunchTemplateSpotMarketOptionsRequest

instance
  Core.ToQuery
    LaunchTemplateSpotMarketOptionsRequest
  where
  toQuery LaunchTemplateSpotMarketOptionsRequest' {..} =
    Prelude.mconcat
      [ "BlockDurationMinutes" Core.=: blockDurationMinutes,
        "InstanceInterruptionBehavior"
          Core.=: instanceInterruptionBehavior,
        "ValidUntil" Core.=: validUntil,
        "SpotInstanceType" Core.=: spotInstanceType,
        "MaxPrice" Core.=: maxPrice
      ]
