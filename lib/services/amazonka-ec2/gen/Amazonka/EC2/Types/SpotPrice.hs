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
-- Module      : Amazonka.EC2.Types.SpotPrice
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SpotPrice where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceType
import Amazonka.EC2.Types.RIProductDescription
import qualified Amazonka.Prelude as Prelude

-- | The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. We do not recommend using this parameter because it can lead
-- to increased interruptions. If you do not specify this parameter, you
-- will pay the current Spot price.
--
-- If you specify a maximum price, your instances will be interrupted more
-- frequently than if you do not specify this parameter.
--
-- /See:/ 'newSpotPrice' smart constructor.
data SpotPrice = SpotPrice'
  { -- | The date and time the request was created, in UTC format (for example,
    -- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
    timestamp :: Prelude.Maybe Data.ISO8601,
    -- | The Availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The instance type.
    instanceType :: Prelude.Maybe InstanceType,
    -- | A general description of the AMI.
    productDescription :: Prelude.Maybe RIProductDescription,
    -- | The maximum price per unit hour that you are willing to pay for a Spot
    -- Instance. We do not recommend using this parameter because it can lead
    -- to increased interruptions. If you do not specify this parameter, you
    -- will pay the current Spot price.
    --
    -- If you specify a maximum price, your instances will be interrupted more
    -- frequently than if you do not specify this parameter.
    spotPrice :: Prelude.Maybe Prelude.Text
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
-- 'timestamp', 'spotPrice_timestamp' - The date and time the request was created, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
--
-- 'availabilityZone', 'spotPrice_availabilityZone' - The Availability Zone.
--
-- 'instanceType', 'spotPrice_instanceType' - The instance type.
--
-- 'productDescription', 'spotPrice_productDescription' - A general description of the AMI.
--
-- 'spotPrice', 'spotPrice_spotPrice' - The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. We do not recommend using this parameter because it can lead
-- to increased interruptions. If you do not specify this parameter, you
-- will pay the current Spot price.
--
-- If you specify a maximum price, your instances will be interrupted more
-- frequently than if you do not specify this parameter.
newSpotPrice ::
  SpotPrice
newSpotPrice =
  SpotPrice'
    { timestamp = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      productDescription = Prelude.Nothing,
      spotPrice = Prelude.Nothing
    }

-- | The date and time the request was created, in UTC format (for example,
-- /YYYY/-/MM/-/DD/T/HH/:/MM/:/SS/Z).
spotPrice_timestamp :: Lens.Lens' SpotPrice (Prelude.Maybe Prelude.UTCTime)
spotPrice_timestamp = Lens.lens (\SpotPrice' {timestamp} -> timestamp) (\s@SpotPrice' {} a -> s {timestamp = a} :: SpotPrice) Prelude.. Lens.mapping Data._Time

-- | The Availability Zone.
spotPrice_availabilityZone :: Lens.Lens' SpotPrice (Prelude.Maybe Prelude.Text)
spotPrice_availabilityZone = Lens.lens (\SpotPrice' {availabilityZone} -> availabilityZone) (\s@SpotPrice' {} a -> s {availabilityZone = a} :: SpotPrice)

-- | The instance type.
spotPrice_instanceType :: Lens.Lens' SpotPrice (Prelude.Maybe InstanceType)
spotPrice_instanceType = Lens.lens (\SpotPrice' {instanceType} -> instanceType) (\s@SpotPrice' {} a -> s {instanceType = a} :: SpotPrice)

-- | A general description of the AMI.
spotPrice_productDescription :: Lens.Lens' SpotPrice (Prelude.Maybe RIProductDescription)
spotPrice_productDescription = Lens.lens (\SpotPrice' {productDescription} -> productDescription) (\s@SpotPrice' {} a -> s {productDescription = a} :: SpotPrice)

-- | The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. We do not recommend using this parameter because it can lead
-- to increased interruptions. If you do not specify this parameter, you
-- will pay the current Spot price.
--
-- If you specify a maximum price, your instances will be interrupted more
-- frequently than if you do not specify this parameter.
spotPrice_spotPrice :: Lens.Lens' SpotPrice (Prelude.Maybe Prelude.Text)
spotPrice_spotPrice = Lens.lens (\SpotPrice' {spotPrice} -> spotPrice) (\s@SpotPrice' {} a -> s {spotPrice = a} :: SpotPrice)

instance Data.FromXML SpotPrice where
  parseXML x =
    SpotPrice'
      Prelude.<$> (x Data..@? "timestamp")
      Prelude.<*> (x Data..@? "availabilityZone")
      Prelude.<*> (x Data..@? "instanceType")
      Prelude.<*> (x Data..@? "productDescription")
      Prelude.<*> (x Data..@? "spotPrice")

instance Prelude.Hashable SpotPrice where
  hashWithSalt _salt SpotPrice' {..} =
    _salt `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` productDescription
      `Prelude.hashWithSalt` spotPrice

instance Prelude.NFData SpotPrice where
  rnf SpotPrice' {..} =
    Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf productDescription
      `Prelude.seq` Prelude.rnf spotPrice
