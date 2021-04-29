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
-- Module      : Network.AWS.EC2.Types.SpotPrice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotPrice where

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
  { -- | The instance type.
    instanceType :: Prelude.Maybe InstanceType,
    -- | The maximum price per hour that you are willing to pay for a Spot
    -- Instance.
    spotPrice :: Prelude.Maybe Prelude.Text,
    -- | The Availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The date and time the request was created, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    timestamp :: Prelude.Maybe Prelude.ISO8601,
    -- | A general description of the AMI.
    productDescription :: Prelude.Maybe RIProductDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { instanceType = Prelude.Nothing,
      spotPrice = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      productDescription = Prelude.Nothing
    }

-- | The instance type.
spotPrice_instanceType :: Lens.Lens' SpotPrice (Prelude.Maybe InstanceType)
spotPrice_instanceType = Lens.lens (\SpotPrice' {instanceType} -> instanceType) (\s@SpotPrice' {} a -> s {instanceType = a} :: SpotPrice)

-- | The maximum price per hour that you are willing to pay for a Spot
-- Instance.
spotPrice_spotPrice :: Lens.Lens' SpotPrice (Prelude.Maybe Prelude.Text)
spotPrice_spotPrice = Lens.lens (\SpotPrice' {spotPrice} -> spotPrice) (\s@SpotPrice' {} a -> s {spotPrice = a} :: SpotPrice)

-- | The Availability Zone.
spotPrice_availabilityZone :: Lens.Lens' SpotPrice (Prelude.Maybe Prelude.Text)
spotPrice_availabilityZone = Lens.lens (\SpotPrice' {availabilityZone} -> availabilityZone) (\s@SpotPrice' {} a -> s {availabilityZone = a} :: SpotPrice)

-- | The date and time the request was created, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
spotPrice_timestamp :: Lens.Lens' SpotPrice (Prelude.Maybe Prelude.UTCTime)
spotPrice_timestamp = Lens.lens (\SpotPrice' {timestamp} -> timestamp) (\s@SpotPrice' {} a -> s {timestamp = a} :: SpotPrice) Prelude.. Lens.mapping Prelude._Time

-- | A general description of the AMI.
spotPrice_productDescription :: Lens.Lens' SpotPrice (Prelude.Maybe RIProductDescription)
spotPrice_productDescription = Lens.lens (\SpotPrice' {productDescription} -> productDescription) (\s@SpotPrice' {} a -> s {productDescription = a} :: SpotPrice)

instance Prelude.FromXML SpotPrice where
  parseXML x =
    SpotPrice'
      Prelude.<$> (x Prelude..@? "instanceType")
      Prelude.<*> (x Prelude..@? "spotPrice")
      Prelude.<*> (x Prelude..@? "availabilityZone")
      Prelude.<*> (x Prelude..@? "timestamp")
      Prelude.<*> (x Prelude..@? "productDescription")

instance Prelude.Hashable SpotPrice

instance Prelude.NFData SpotPrice
