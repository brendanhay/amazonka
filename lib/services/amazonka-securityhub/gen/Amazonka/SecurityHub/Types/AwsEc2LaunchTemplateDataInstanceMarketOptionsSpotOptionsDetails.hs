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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides details about the market (purchasing) options for Spot
-- Instances.
--
-- /See:/ 'newAwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails' smart constructor.
data AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails = AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails'
  { -- | Deprecated.
    blockDurationMinutes :: Prelude.Maybe Prelude.Int,
    -- | The behavior when a Spot Instance is interrupted.
    instanceInterruptionBehavior :: Prelude.Maybe Prelude.Text,
    -- | The maximum hourly price you\'re willing to pay for the Spot Instances.
    maxPrice :: Prelude.Maybe Prelude.Text,
    -- | The Spot Instance request type.
    spotInstanceType :: Prelude.Maybe Prelude.Text,
    -- | The end date of the request, in UTC format (YYYY-MM-DDTHH:MM:SSZ), for
    -- persistent requests.
    validUntil :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockDurationMinutes', 'awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_blockDurationMinutes' - Deprecated.
--
-- 'instanceInterruptionBehavior', 'awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted.
--
-- 'maxPrice', 'awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_maxPrice' - The maximum hourly price you\'re willing to pay for the Spot Instances.
--
-- 'spotInstanceType', 'awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_spotInstanceType' - The Spot Instance request type.
--
-- 'validUntil', 'awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_validUntil' - The end date of the request, in UTC format (YYYY-MM-DDTHH:MM:SSZ), for
-- persistent requests.
newAwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails ::
  AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails
newAwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails =
  AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails'
    { blockDurationMinutes =
        Prelude.Nothing,
      instanceInterruptionBehavior =
        Prelude.Nothing,
      maxPrice =
        Prelude.Nothing,
      spotInstanceType =
        Prelude.Nothing,
      validUntil =
        Prelude.Nothing
    }

-- | Deprecated.
awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_blockDurationMinutes :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails (Prelude.Maybe Prelude.Int)
awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_blockDurationMinutes = Lens.lens (\AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails' {blockDurationMinutes} -> blockDurationMinutes) (\s@AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails' {} a -> s {blockDurationMinutes = a} :: AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails)

-- | The behavior when a Spot Instance is interrupted.
awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_instanceInterruptionBehavior :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_instanceInterruptionBehavior = Lens.lens (\AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails' {instanceInterruptionBehavior} -> instanceInterruptionBehavior) (\s@AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails' {} a -> s {instanceInterruptionBehavior = a} :: AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails)

-- | The maximum hourly price you\'re willing to pay for the Spot Instances.
awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_maxPrice :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_maxPrice = Lens.lens (\AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails' {maxPrice} -> maxPrice) (\s@AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails' {} a -> s {maxPrice = a} :: AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails)

-- | The Spot Instance request type.
awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_spotInstanceType :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_spotInstanceType = Lens.lens (\AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails' {spotInstanceType} -> spotInstanceType) (\s@AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails' {} a -> s {spotInstanceType = a} :: AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails)

-- | The end date of the request, in UTC format (YYYY-MM-DDTHH:MM:SSZ), for
-- persistent requests.
awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_validUntil :: Lens.Lens' AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails (Prelude.Maybe Prelude.Text)
awsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails_validUntil = Lens.lens (\AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails' {validUntil} -> validUntil) (\s@AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails' {} a -> s {validUntil = a} :: AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails)

instance
  Data.FromJSON
    AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails"
      ( \x ->
          AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails'
            Prelude.<$> (x Data..:? "BlockDurationMinutes")
            Prelude.<*> (x Data..:? "InstanceInterruptionBehavior")
            Prelude.<*> (x Data..:? "MaxPrice")
            Prelude.<*> (x Data..:? "SpotInstanceType")
            Prelude.<*> (x Data..:? "ValidUntil")
      )

instance
  Prelude.Hashable
    AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails
  where
  hashWithSalt
    _salt
    AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails' {..} =
      _salt
        `Prelude.hashWithSalt` blockDurationMinutes
        `Prelude.hashWithSalt` instanceInterruptionBehavior
        `Prelude.hashWithSalt` maxPrice
        `Prelude.hashWithSalt` spotInstanceType
        `Prelude.hashWithSalt` validUntil

instance
  Prelude.NFData
    AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails
  where
  rnf
    AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails' {..} =
      Prelude.rnf blockDurationMinutes
        `Prelude.seq` Prelude.rnf instanceInterruptionBehavior
        `Prelude.seq` Prelude.rnf maxPrice
        `Prelude.seq` Prelude.rnf spotInstanceType
        `Prelude.seq` Prelude.rnf validUntil

instance
  Data.ToJSON
    AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails
  where
  toJSON
    AwsEc2LaunchTemplateDataInstanceMarketOptionsSpotOptionsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("BlockDurationMinutes" Data..=)
                Prelude.<$> blockDurationMinutes,
              ("InstanceInterruptionBehavior" Data..=)
                Prelude.<$> instanceInterruptionBehavior,
              ("MaxPrice" Data..=) Prelude.<$> maxPrice,
              ("SpotInstanceType" Data..=)
                Prelude.<$> spotInstanceType,
              ("ValidUntil" Data..=) Prelude.<$> validUntil
            ]
        )
