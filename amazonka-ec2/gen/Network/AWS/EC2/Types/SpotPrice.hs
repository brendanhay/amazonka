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
-- Module      : Network.AWS.EC2.Types.SpotPrice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotPrice where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceType
import Network.AWS.EC2.Types.RIProductDescription
import qualified Network.AWS.Lens as Lens

-- | Describes the maximum price per hour that you are willing to pay for a
-- Spot Instance.
--
-- /See:/ 'newSpotPrice' smart constructor.
data SpotPrice = SpotPrice'
  { -- | The instance type.
    instanceType :: Core.Maybe InstanceType,
    -- | The maximum price per hour that you are willing to pay for a Spot
    -- Instance.
    spotPrice :: Core.Maybe Core.Text,
    -- | The Availability Zone.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The date and time the request was created, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    timestamp :: Core.Maybe Core.ISO8601,
    -- | A general description of the AMI.
    productDescription :: Core.Maybe RIProductDescription
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SpotPrice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceType', 'spotPrice_instanceType' - The instance type.
--
-- 'spotPrice', 'spotPrice_spotPrice' - The maximum price per hour that you are willing to pay for a Spot
-- Instance.
--
-- 'availabilityZone', 'spotPrice_availabilityZone' - The Availability Zone.
--
-- 'timestamp', 'spotPrice_timestamp' - The date and time the request was created, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
--
-- 'productDescription', 'spotPrice_productDescription' - A general description of the AMI.
newSpotPrice ::
  SpotPrice
newSpotPrice =
  SpotPrice'
    { instanceType = Core.Nothing,
      spotPrice = Core.Nothing,
      availabilityZone = Core.Nothing,
      timestamp = Core.Nothing,
      productDescription = Core.Nothing
    }

-- | The instance type.
spotPrice_instanceType :: Lens.Lens' SpotPrice (Core.Maybe InstanceType)
spotPrice_instanceType = Lens.lens (\SpotPrice' {instanceType} -> instanceType) (\s@SpotPrice' {} a -> s {instanceType = a} :: SpotPrice)

-- | The maximum price per hour that you are willing to pay for a Spot
-- Instance.
spotPrice_spotPrice :: Lens.Lens' SpotPrice (Core.Maybe Core.Text)
spotPrice_spotPrice = Lens.lens (\SpotPrice' {spotPrice} -> spotPrice) (\s@SpotPrice' {} a -> s {spotPrice = a} :: SpotPrice)

-- | The Availability Zone.
spotPrice_availabilityZone :: Lens.Lens' SpotPrice (Core.Maybe Core.Text)
spotPrice_availabilityZone = Lens.lens (\SpotPrice' {availabilityZone} -> availabilityZone) (\s@SpotPrice' {} a -> s {availabilityZone = a} :: SpotPrice)

-- | The date and time the request was created, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
spotPrice_timestamp :: Lens.Lens' SpotPrice (Core.Maybe Core.UTCTime)
spotPrice_timestamp = Lens.lens (\SpotPrice' {timestamp} -> timestamp) (\s@SpotPrice' {} a -> s {timestamp = a} :: SpotPrice) Core.. Lens.mapping Core._Time

-- | A general description of the AMI.
spotPrice_productDescription :: Lens.Lens' SpotPrice (Core.Maybe RIProductDescription)
spotPrice_productDescription = Lens.lens (\SpotPrice' {productDescription} -> productDescription) (\s@SpotPrice' {} a -> s {productDescription = a} :: SpotPrice)

instance Core.FromXML SpotPrice where
  parseXML x =
    SpotPrice'
      Core.<$> (x Core..@? "instanceType")
      Core.<*> (x Core..@? "spotPrice")
      Core.<*> (x Core..@? "availabilityZone")
      Core.<*> (x Core..@? "timestamp")
      Core.<*> (x Core..@? "productDescription")

instance Core.Hashable SpotPrice

instance Core.NFData SpotPrice
