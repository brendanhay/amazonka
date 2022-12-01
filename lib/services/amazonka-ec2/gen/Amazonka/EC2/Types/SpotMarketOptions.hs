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
-- Module      : Amazonka.EC2.Types.SpotMarketOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SpotMarketOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceInterruptionBehavior
import Amazonka.EC2.Types.SpotInstanceType
import qualified Amazonka.Prelude as Prelude

-- | The options for Spot Instances.
--
-- /See:/ 'newSpotMarketOptions' smart constructor.
data SpotMarketOptions = SpotMarketOptions'
  { -- | Deprecated.
    blockDurationMinutes :: Prelude.Maybe Prelude.Int,
    -- | The maximum hourly price that you\'re willing to pay for a Spot
    -- Instance. We do not recommend using this parameter because it can lead
    -- to increased interruptions. If you do not specify this parameter, you
    -- will pay the current Spot price.
    --
    -- If you specify a maximum price, your Spot Instances will be interrupted
    -- more frequently than if you do not specify this parameter.
    maxPrice :: Prelude.Maybe Prelude.Text,
    -- | The behavior when a Spot Instance is interrupted. The default is
    -- @terminate@.
    instanceInterruptionBehavior :: Prelude.Maybe InstanceInterruptionBehavior,
    -- | The Spot Instance request type. For
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances RunInstances>,
    -- persistent Spot Instance requests are only supported when the instance
    -- interruption behavior is either @hibernate@ or @stop@.
    spotInstanceType :: Prelude.Maybe SpotInstanceType,
    -- | The end date of the request, in UTC format
    -- (/YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). Supported only for persistent
    -- requests.
    --
    -- -   For a persistent request, the request remains active until the
    --     @ValidUntil@ date and time is reached. Otherwise, the request
    --     remains active until you cancel it.
    --
    -- -   For a one-time request, @ValidUntil@ is not supported. The request
    --     remains active until all instances launch or you cancel the request.
    validUntil :: Prelude.Maybe Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpotMarketOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blockDurationMinutes', 'spotMarketOptions_blockDurationMinutes' - Deprecated.
--
-- 'maxPrice', 'spotMarketOptions_maxPrice' - The maximum hourly price that you\'re willing to pay for a Spot
-- Instance. We do not recommend using this parameter because it can lead
-- to increased interruptions. If you do not specify this parameter, you
-- will pay the current Spot price.
--
-- If you specify a maximum price, your Spot Instances will be interrupted
-- more frequently than if you do not specify this parameter.
--
-- 'instanceInterruptionBehavior', 'spotMarketOptions_instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is
-- @terminate@.
--
-- 'spotInstanceType', 'spotMarketOptions_spotInstanceType' - The Spot Instance request type. For
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances RunInstances>,
-- persistent Spot Instance requests are only supported when the instance
-- interruption behavior is either @hibernate@ or @stop@.
--
-- 'validUntil', 'spotMarketOptions_validUntil' - The end date of the request, in UTC format
-- (/YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). Supported only for persistent
-- requests.
--
-- -   For a persistent request, the request remains active until the
--     @ValidUntil@ date and time is reached. Otherwise, the request
--     remains active until you cancel it.
--
-- -   For a one-time request, @ValidUntil@ is not supported. The request
--     remains active until all instances launch or you cancel the request.
newSpotMarketOptions ::
  SpotMarketOptions
newSpotMarketOptions =
  SpotMarketOptions'
    { blockDurationMinutes =
        Prelude.Nothing,
      maxPrice = Prelude.Nothing,
      instanceInterruptionBehavior = Prelude.Nothing,
      spotInstanceType = Prelude.Nothing,
      validUntil = Prelude.Nothing
    }

-- | Deprecated.
spotMarketOptions_blockDurationMinutes :: Lens.Lens' SpotMarketOptions (Prelude.Maybe Prelude.Int)
spotMarketOptions_blockDurationMinutes = Lens.lens (\SpotMarketOptions' {blockDurationMinutes} -> blockDurationMinutes) (\s@SpotMarketOptions' {} a -> s {blockDurationMinutes = a} :: SpotMarketOptions)

-- | The maximum hourly price that you\'re willing to pay for a Spot
-- Instance. We do not recommend using this parameter because it can lead
-- to increased interruptions. If you do not specify this parameter, you
-- will pay the current Spot price.
--
-- If you specify a maximum price, your Spot Instances will be interrupted
-- more frequently than if you do not specify this parameter.
spotMarketOptions_maxPrice :: Lens.Lens' SpotMarketOptions (Prelude.Maybe Prelude.Text)
spotMarketOptions_maxPrice = Lens.lens (\SpotMarketOptions' {maxPrice} -> maxPrice) (\s@SpotMarketOptions' {} a -> s {maxPrice = a} :: SpotMarketOptions)

-- | The behavior when a Spot Instance is interrupted. The default is
-- @terminate@.
spotMarketOptions_instanceInterruptionBehavior :: Lens.Lens' SpotMarketOptions (Prelude.Maybe InstanceInterruptionBehavior)
spotMarketOptions_instanceInterruptionBehavior = Lens.lens (\SpotMarketOptions' {instanceInterruptionBehavior} -> instanceInterruptionBehavior) (\s@SpotMarketOptions' {} a -> s {instanceInterruptionBehavior = a} :: SpotMarketOptions)

-- | The Spot Instance request type. For
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances RunInstances>,
-- persistent Spot Instance requests are only supported when the instance
-- interruption behavior is either @hibernate@ or @stop@.
spotMarketOptions_spotInstanceType :: Lens.Lens' SpotMarketOptions (Prelude.Maybe SpotInstanceType)
spotMarketOptions_spotInstanceType = Lens.lens (\SpotMarketOptions' {spotInstanceType} -> spotInstanceType) (\s@SpotMarketOptions' {} a -> s {spotInstanceType = a} :: SpotMarketOptions)

-- | The end date of the request, in UTC format
-- (/YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z). Supported only for persistent
-- requests.
--
-- -   For a persistent request, the request remains active until the
--     @ValidUntil@ date and time is reached. Otherwise, the request
--     remains active until you cancel it.
--
-- -   For a one-time request, @ValidUntil@ is not supported. The request
--     remains active until all instances launch or you cancel the request.
spotMarketOptions_validUntil :: Lens.Lens' SpotMarketOptions (Prelude.Maybe Prelude.UTCTime)
spotMarketOptions_validUntil = Lens.lens (\SpotMarketOptions' {validUntil} -> validUntil) (\s@SpotMarketOptions' {} a -> s {validUntil = a} :: SpotMarketOptions) Prelude.. Lens.mapping Core._Time

instance Prelude.Hashable SpotMarketOptions where
  hashWithSalt _salt SpotMarketOptions' {..} =
    _salt `Prelude.hashWithSalt` blockDurationMinutes
      `Prelude.hashWithSalt` maxPrice
      `Prelude.hashWithSalt` instanceInterruptionBehavior
      `Prelude.hashWithSalt` spotInstanceType
      `Prelude.hashWithSalt` validUntil

instance Prelude.NFData SpotMarketOptions where
  rnf SpotMarketOptions' {..} =
    Prelude.rnf blockDurationMinutes
      `Prelude.seq` Prelude.rnf maxPrice
      `Prelude.seq` Prelude.rnf instanceInterruptionBehavior
      `Prelude.seq` Prelude.rnf spotInstanceType
      `Prelude.seq` Prelude.rnf validUntil

instance Core.ToQuery SpotMarketOptions where
  toQuery SpotMarketOptions' {..} =
    Prelude.mconcat
      [ "BlockDurationMinutes" Core.=: blockDurationMinutes,
        "MaxPrice" Core.=: maxPrice,
        "InstanceInterruptionBehavior"
          Core.=: instanceInterruptionBehavior,
        "SpotInstanceType" Core.=: spotInstanceType,
        "ValidUntil" Core.=: validUntil
      ]
