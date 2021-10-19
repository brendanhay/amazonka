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
import qualified Network.AWS.Prelude as Prelude

-- | Describes the maximum price per hour that you are willing to pay for a
-- Spot Instance.
--
-- /See:/ 'newSpotPrice' smart constructor.
data SpotPrice = SpotPrice'
  { -- | A general description of the AMI.
    productDescription :: Prelude.Maybe RIProductDescription,
    -- | The maximum price per hour that you are willing to pay for a Spot
    -- Instance.
    spotPrice :: Prelude.Maybe Prelude.Text,
    -- | The instance type.
    instanceType :: Prelude.Maybe InstanceType,
    -- | The Availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The date and time the request was created, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    timestamp :: Prelude.Maybe Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpotPrice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productDescription', 'spotPrice_productDescription' - A general description of the AMI.
--
-- 'spotPrice', 'spotPrice_spotPrice' - The maximum price per hour that you are willing to pay for a Spot
-- Instance.
--
-- 'instanceType', 'spotPrice_instanceType' - The instance type.
--
-- 'availabilityZone', 'spotPrice_availabilityZone' - The Availability Zone.
--
-- 'timestamp', 'spotPrice_timestamp' - The date and time the request was created, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
newSpotPrice ::
  SpotPrice
newSpotPrice =
  SpotPrice'
    { productDescription = Prelude.Nothing,
      spotPrice = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | A general description of the AMI.
spotPrice_productDescription :: Lens.Lens' SpotPrice (Prelude.Maybe RIProductDescription)
spotPrice_productDescription = Lens.lens (\SpotPrice' {productDescription} -> productDescription) (\s@SpotPrice' {} a -> s {productDescription = a} :: SpotPrice)

-- | The maximum price per hour that you are willing to pay for a Spot
-- Instance.
spotPrice_spotPrice :: Lens.Lens' SpotPrice (Prelude.Maybe Prelude.Text)
spotPrice_spotPrice = Lens.lens (\SpotPrice' {spotPrice} -> spotPrice) (\s@SpotPrice' {} a -> s {spotPrice = a} :: SpotPrice)

-- | The instance type.
spotPrice_instanceType :: Lens.Lens' SpotPrice (Prelude.Maybe InstanceType)
spotPrice_instanceType = Lens.lens (\SpotPrice' {instanceType} -> instanceType) (\s@SpotPrice' {} a -> s {instanceType = a} :: SpotPrice)

-- | The Availability Zone.
spotPrice_availabilityZone :: Lens.Lens' SpotPrice (Prelude.Maybe Prelude.Text)
spotPrice_availabilityZone = Lens.lens (\SpotPrice' {availabilityZone} -> availabilityZone) (\s@SpotPrice' {} a -> s {availabilityZone = a} :: SpotPrice)

-- | The date and time the request was created, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
spotPrice_timestamp :: Lens.Lens' SpotPrice (Prelude.Maybe Prelude.UTCTime)
spotPrice_timestamp = Lens.lens (\SpotPrice' {timestamp} -> timestamp) (\s@SpotPrice' {} a -> s {timestamp = a} :: SpotPrice) Prelude.. Lens.mapping Core._Time

instance Core.FromXML SpotPrice where
  parseXML x =
    SpotPrice'
      Prelude.<$> (x Core..@? "productDescription")
      Prelude.<*> (x Core..@? "spotPrice")
      Prelude.<*> (x Core..@? "instanceType")
      Prelude.<*> (x Core..@? "availabilityZone")
      Prelude.<*> (x Core..@? "timestamp")

instance Prelude.Hashable SpotPrice

instance Prelude.NFData SpotPrice
