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
-- Module      : Network.AWS.EC2.Types.PricingDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.PricingDetail where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a Reserved Instance offering.
--
-- /See:/ 'newPricingDetail' smart constructor.
data PricingDetail = PricingDetail'
  { -- | The number of reservations available for the price.
    count :: Prelude.Maybe Prelude.Int,
    -- | The price per instance.
    price :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PricingDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'pricingDetail_count' - The number of reservations available for the price.
--
-- 'price', 'pricingDetail_price' - The price per instance.
newPricingDetail ::
  PricingDetail
newPricingDetail =
  PricingDetail'
    { count = Prelude.Nothing,
      price = Prelude.Nothing
    }

-- | The number of reservations available for the price.
pricingDetail_count :: Lens.Lens' PricingDetail (Prelude.Maybe Prelude.Int)
pricingDetail_count = Lens.lens (\PricingDetail' {count} -> count) (\s@PricingDetail' {} a -> s {count = a} :: PricingDetail)

-- | The price per instance.
pricingDetail_price :: Lens.Lens' PricingDetail (Prelude.Maybe Prelude.Double)
pricingDetail_price = Lens.lens (\PricingDetail' {price} -> price) (\s@PricingDetail' {} a -> s {price = a} :: PricingDetail)

instance Prelude.FromXML PricingDetail where
  parseXML x =
    PricingDetail'
      Prelude.<$> (x Prelude..@? "count")
      Prelude.<*> (x Prelude..@? "price")

instance Prelude.Hashable PricingDetail

instance Prelude.NFData PricingDetail
