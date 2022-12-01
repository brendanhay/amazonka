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
-- Module      : Amazonka.EC2.Types.LaunchTemplateSpotMarketOptionsRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateSpotMarketOptionsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceInterruptionBehavior
import Amazonka.EC2.Types.SpotInstanceType
import qualified Amazonka.Prelude as Prelude

-- | The options for Spot Instances.
--
-- /See:/ 'newLaunchTemplateSpotMarketOptionsRequest' smart constructor.
data LaunchTemplateSpotMarketOptionsRequest = LaunchTemplateSpotMarketOptionsRequest'
  { -- | Deprecated.
    blockDurationMinutes :: Prelude.Maybe Prelude.Int,
    -- | The maximum hourly price you\'re willing to pay for the Spot Instances.
    -- We do not recommend using this parameter because it can lead to
    -- increased interruptions. If you do not specify this parameter, you will
    -- pay the current Spot price.
    --
    -- If you specify a maximum price, your Spot Instances will be interrupted
    -- more frequently than if you do not specify this parameter.
    maxPrice :: Prelude.Maybe Prelude.Text,
    -- | The behavior when a Spot Instance is interrupted. The default is
    -- @terminate@.
    instanceInterruptionBehavior :: Prelude.Maybe InstanceInterruptionBehavior,
    -- | The Spot Instance request type.
    spotInstanceType :: Prelude.Maybe SpotInstanceType,
    -- | The end date of the request, in UTC format (/YYYY-MM-DD/T/HH:MM:SS/Z).
    -- Supported only for persistent requests.
    --
    -- -   For a persistent request, the request remains active until the
    --     @ValidUntil@ date and time is reached. Otherwise, the request
    --     remains active until you cancel it.
    --
    -- -   For a one-time request, @ValidUntil@ is not supported. The request
    --     remains active until all instances launch or you cancel the request.
    --
    -- Default: 7 days from the current date
    validUntil :: Prelude.Maybe Core.ISO8601
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
-- 'blockDurationMinutes', 'launchTemplateSpotMarketOptionsRequest_blockDurationMinutes' - Deprecated.
--
-- 'maxPrice', 'launchTemplateSpotMarketOptionsRequest_maxPrice' - The maximum hourly price you\'re willing to pay for the Spot Instances.
-- We do not recommend using this parameter because it can lead to
-- increased interruptions. If you do not specify this parameter, you will
-- pay the current Spot price.
--
-- If you specify a maximum price, your Spot Instances will be interrupted
-- more frequently than if you do not specify this parameter.
--
-- 'instanceInterruptionBehavior', 'launchTemplateSpotMarketOptionsRequest_instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is
-- @terminate@.
--
-- 'spotInstanceType', 'launchTemplateSpotMarketOptionsRequest_spotInstanceType' - The Spot Instance request type.
--
-- 'validUntil', 'launchTemplateSpotMarketOptionsRequest_validUntil' - The end date of the request, in UTC format (/YYYY-MM-DD/T/HH:MM:SS/Z).
-- Supported only for persistent requests.
--
-- -   For a persistent request, the request remains active until the
--     @ValidUntil@ date and time is reached. Otherwise, the request
--     remains active until you cancel it.
--
-- -   For a one-time request, @ValidUntil@ is not supported. The request
--     remains active until all instances launch or you cancel the request.
--
-- Default: 7 days from the current date
newLaunchTemplateSpotMarketOptionsRequest ::
  LaunchTemplateSpotMarketOptionsRequest
newLaunchTemplateSpotMarketOptionsRequest =
  LaunchTemplateSpotMarketOptionsRequest'
    { blockDurationMinutes =
        Prelude.Nothing,
      maxPrice = Prelude.Nothing,
      instanceInterruptionBehavior =
        Prelude.Nothing,
      spotInstanceType = Prelude.Nothing,
      validUntil = Prelude.Nothing
    }

-- | Deprecated.
launchTemplateSpotMarketOptionsRequest_blockDurationMinutes :: Lens.Lens' LaunchTemplateSpotMarketOptionsRequest (Prelude.Maybe Prelude.Int)
launchTemplateSpotMarketOptionsRequest_blockDurationMinutes = Lens.lens (\LaunchTemplateSpotMarketOptionsRequest' {blockDurationMinutes} -> blockDurationMinutes) (\s@LaunchTemplateSpotMarketOptionsRequest' {} a -> s {blockDurationMinutes = a} :: LaunchTemplateSpotMarketOptionsRequest)

-- | The maximum hourly price you\'re willing to pay for the Spot Instances.
-- We do not recommend using this parameter because it can lead to
-- increased interruptions. If you do not specify this parameter, you will
-- pay the current Spot price.
--
-- If you specify a maximum price, your Spot Instances will be interrupted
-- more frequently than if you do not specify this parameter.
launchTemplateSpotMarketOptionsRequest_maxPrice :: Lens.Lens' LaunchTemplateSpotMarketOptionsRequest (Prelude.Maybe Prelude.Text)
launchTemplateSpotMarketOptionsRequest_maxPrice = Lens.lens (\LaunchTemplateSpotMarketOptionsRequest' {maxPrice} -> maxPrice) (\s@LaunchTemplateSpotMarketOptionsRequest' {} a -> s {maxPrice = a} :: LaunchTemplateSpotMarketOptionsRequest)

-- | The behavior when a Spot Instance is interrupted. The default is
-- @terminate@.
launchTemplateSpotMarketOptionsRequest_instanceInterruptionBehavior :: Lens.Lens' LaunchTemplateSpotMarketOptionsRequest (Prelude.Maybe InstanceInterruptionBehavior)
launchTemplateSpotMarketOptionsRequest_instanceInterruptionBehavior = Lens.lens (\LaunchTemplateSpotMarketOptionsRequest' {instanceInterruptionBehavior} -> instanceInterruptionBehavior) (\s@LaunchTemplateSpotMarketOptionsRequest' {} a -> s {instanceInterruptionBehavior = a} :: LaunchTemplateSpotMarketOptionsRequest)

-- | The Spot Instance request type.
launchTemplateSpotMarketOptionsRequest_spotInstanceType :: Lens.Lens' LaunchTemplateSpotMarketOptionsRequest (Prelude.Maybe SpotInstanceType)
launchTemplateSpotMarketOptionsRequest_spotInstanceType = Lens.lens (\LaunchTemplateSpotMarketOptionsRequest' {spotInstanceType} -> spotInstanceType) (\s@LaunchTemplateSpotMarketOptionsRequest' {} a -> s {spotInstanceType = a} :: LaunchTemplateSpotMarketOptionsRequest)

-- | The end date of the request, in UTC format (/YYYY-MM-DD/T/HH:MM:SS/Z).
-- Supported only for persistent requests.
--
-- -   For a persistent request, the request remains active until the
--     @ValidUntil@ date and time is reached. Otherwise, the request
--     remains active until you cancel it.
--
-- -   For a one-time request, @ValidUntil@ is not supported. The request
--     remains active until all instances launch or you cancel the request.
--
-- Default: 7 days from the current date
launchTemplateSpotMarketOptionsRequest_validUntil :: Lens.Lens' LaunchTemplateSpotMarketOptionsRequest (Prelude.Maybe Prelude.UTCTime)
launchTemplateSpotMarketOptionsRequest_validUntil = Lens.lens (\LaunchTemplateSpotMarketOptionsRequest' {validUntil} -> validUntil) (\s@LaunchTemplateSpotMarketOptionsRequest' {} a -> s {validUntil = a} :: LaunchTemplateSpotMarketOptionsRequest) Prelude.. Lens.mapping Core._Time

instance
  Prelude.Hashable
    LaunchTemplateSpotMarketOptionsRequest
  where
  hashWithSalt
    _salt
    LaunchTemplateSpotMarketOptionsRequest' {..} =
      _salt `Prelude.hashWithSalt` blockDurationMinutes
        `Prelude.hashWithSalt` maxPrice
        `Prelude.hashWithSalt` instanceInterruptionBehavior
        `Prelude.hashWithSalt` spotInstanceType
        `Prelude.hashWithSalt` validUntil

instance
  Prelude.NFData
    LaunchTemplateSpotMarketOptionsRequest
  where
  rnf LaunchTemplateSpotMarketOptionsRequest' {..} =
    Prelude.rnf blockDurationMinutes
      `Prelude.seq` Prelude.rnf maxPrice
      `Prelude.seq` Prelude.rnf instanceInterruptionBehavior
      `Prelude.seq` Prelude.rnf spotInstanceType
      `Prelude.seq` Prelude.rnf validUntil

instance
  Core.ToQuery
    LaunchTemplateSpotMarketOptionsRequest
  where
  toQuery LaunchTemplateSpotMarketOptionsRequest' {..} =
    Prelude.mconcat
      [ "BlockDurationMinutes" Core.=: blockDurationMinutes,
        "MaxPrice" Core.=: maxPrice,
        "InstanceInterruptionBehavior"
          Core.=: instanceInterruptionBehavior,
        "SpotInstanceType" Core.=: spotInstanceType,
        "ValidUntil" Core.=: validUntil
      ]
