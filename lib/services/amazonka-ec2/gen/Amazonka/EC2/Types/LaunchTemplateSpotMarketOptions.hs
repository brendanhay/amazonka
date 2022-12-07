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
-- Module      : Amazonka.EC2.Types.LaunchTemplateSpotMarketOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateSpotMarketOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceInterruptionBehavior
import Amazonka.EC2.Types.SpotInstanceType
import qualified Amazonka.Prelude as Prelude

-- | The options for Spot Instances.
--
-- /See:/ 'newLaunchTemplateSpotMarketOptions' smart constructor.
data LaunchTemplateSpotMarketOptions = LaunchTemplateSpotMarketOptions'
  { -- | The required duration for the Spot Instances (also known as Spot
    -- blocks), in minutes. This value must be a multiple of 60 (60, 120, 180,
    -- 240, 300, or 360).
    blockDurationMinutes :: Prelude.Maybe Prelude.Int,
    -- | The maximum hourly price you\'re willing to pay for the Spot Instances.
    -- We do not recommend using this parameter because it can lead to
    -- increased interruptions. If you do not specify this parameter, you will
    -- pay the current Spot price.
    --
    -- If you specify a maximum price, your Spot Instances will be interrupted
    -- more frequently than if you do not specify this parameter.
    maxPrice :: Prelude.Maybe Prelude.Text,
    -- | The behavior when a Spot Instance is interrupted.
    instanceInterruptionBehavior :: Prelude.Maybe InstanceInterruptionBehavior,
    -- | The Spot Instance request type.
    spotInstanceType :: Prelude.Maybe SpotInstanceType,
    -- | The end date of the request. For a one-time request, the request remains
    -- active until all instances launch, the request is canceled, or this date
    -- is reached. If the request is persistent, it remains active until it is
    -- canceled or this date and time is reached.
    validUntil :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'maxPrice', 'launchTemplateSpotMarketOptions_maxPrice' - The maximum hourly price you\'re willing to pay for the Spot Instances.
-- We do not recommend using this parameter because it can lead to
-- increased interruptions. If you do not specify this parameter, you will
-- pay the current Spot price.
--
-- If you specify a maximum price, your Spot Instances will be interrupted
-- more frequently than if you do not specify this parameter.
--
-- 'instanceInterruptionBehavior', 'launchTemplateSpotMarketOptions_instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted.
--
-- 'spotInstanceType', 'launchTemplateSpotMarketOptions_spotInstanceType' - The Spot Instance request type.
--
-- 'validUntil', 'launchTemplateSpotMarketOptions_validUntil' - The end date of the request. For a one-time request, the request remains
-- active until all instances launch, the request is canceled, or this date
-- is reached. If the request is persistent, it remains active until it is
-- canceled or this date and time is reached.
newLaunchTemplateSpotMarketOptions ::
  LaunchTemplateSpotMarketOptions
newLaunchTemplateSpotMarketOptions =
  LaunchTemplateSpotMarketOptions'
    { blockDurationMinutes =
        Prelude.Nothing,
      maxPrice = Prelude.Nothing,
      instanceInterruptionBehavior =
        Prelude.Nothing,
      spotInstanceType = Prelude.Nothing,
      validUntil = Prelude.Nothing
    }

-- | The required duration for the Spot Instances (also known as Spot
-- blocks), in minutes. This value must be a multiple of 60 (60, 120, 180,
-- 240, 300, or 360).
launchTemplateSpotMarketOptions_blockDurationMinutes :: Lens.Lens' LaunchTemplateSpotMarketOptions (Prelude.Maybe Prelude.Int)
launchTemplateSpotMarketOptions_blockDurationMinutes = Lens.lens (\LaunchTemplateSpotMarketOptions' {blockDurationMinutes} -> blockDurationMinutes) (\s@LaunchTemplateSpotMarketOptions' {} a -> s {blockDurationMinutes = a} :: LaunchTemplateSpotMarketOptions)

-- | The maximum hourly price you\'re willing to pay for the Spot Instances.
-- We do not recommend using this parameter because it can lead to
-- increased interruptions. If you do not specify this parameter, you will
-- pay the current Spot price.
--
-- If you specify a maximum price, your Spot Instances will be interrupted
-- more frequently than if you do not specify this parameter.
launchTemplateSpotMarketOptions_maxPrice :: Lens.Lens' LaunchTemplateSpotMarketOptions (Prelude.Maybe Prelude.Text)
launchTemplateSpotMarketOptions_maxPrice = Lens.lens (\LaunchTemplateSpotMarketOptions' {maxPrice} -> maxPrice) (\s@LaunchTemplateSpotMarketOptions' {} a -> s {maxPrice = a} :: LaunchTemplateSpotMarketOptions)

-- | The behavior when a Spot Instance is interrupted.
launchTemplateSpotMarketOptions_instanceInterruptionBehavior :: Lens.Lens' LaunchTemplateSpotMarketOptions (Prelude.Maybe InstanceInterruptionBehavior)
launchTemplateSpotMarketOptions_instanceInterruptionBehavior = Lens.lens (\LaunchTemplateSpotMarketOptions' {instanceInterruptionBehavior} -> instanceInterruptionBehavior) (\s@LaunchTemplateSpotMarketOptions' {} a -> s {instanceInterruptionBehavior = a} :: LaunchTemplateSpotMarketOptions)

-- | The Spot Instance request type.
launchTemplateSpotMarketOptions_spotInstanceType :: Lens.Lens' LaunchTemplateSpotMarketOptions (Prelude.Maybe SpotInstanceType)
launchTemplateSpotMarketOptions_spotInstanceType = Lens.lens (\LaunchTemplateSpotMarketOptions' {spotInstanceType} -> spotInstanceType) (\s@LaunchTemplateSpotMarketOptions' {} a -> s {spotInstanceType = a} :: LaunchTemplateSpotMarketOptions)

-- | The end date of the request. For a one-time request, the request remains
-- active until all instances launch, the request is canceled, or this date
-- is reached. If the request is persistent, it remains active until it is
-- canceled or this date and time is reached.
launchTemplateSpotMarketOptions_validUntil :: Lens.Lens' LaunchTemplateSpotMarketOptions (Prelude.Maybe Prelude.UTCTime)
launchTemplateSpotMarketOptions_validUntil = Lens.lens (\LaunchTemplateSpotMarketOptions' {validUntil} -> validUntil) (\s@LaunchTemplateSpotMarketOptions' {} a -> s {validUntil = a} :: LaunchTemplateSpotMarketOptions) Prelude.. Lens.mapping Data._Time

instance Data.FromXML LaunchTemplateSpotMarketOptions where
  parseXML x =
    LaunchTemplateSpotMarketOptions'
      Prelude.<$> (x Data..@? "blockDurationMinutes")
      Prelude.<*> (x Data..@? "maxPrice")
      Prelude.<*> (x Data..@? "instanceInterruptionBehavior")
      Prelude.<*> (x Data..@? "spotInstanceType")
      Prelude.<*> (x Data..@? "validUntil")

instance
  Prelude.Hashable
    LaunchTemplateSpotMarketOptions
  where
  hashWithSalt
    _salt
    LaunchTemplateSpotMarketOptions' {..} =
      _salt `Prelude.hashWithSalt` blockDurationMinutes
        `Prelude.hashWithSalt` maxPrice
        `Prelude.hashWithSalt` instanceInterruptionBehavior
        `Prelude.hashWithSalt` spotInstanceType
        `Prelude.hashWithSalt` validUntil

instance
  Prelude.NFData
    LaunchTemplateSpotMarketOptions
  where
  rnf LaunchTemplateSpotMarketOptions' {..} =
    Prelude.rnf blockDurationMinutes
      `Prelude.seq` Prelude.rnf maxPrice
      `Prelude.seq` Prelude.rnf instanceInterruptionBehavior
      `Prelude.seq` Prelude.rnf spotInstanceType
      `Prelude.seq` Prelude.rnf validUntil
