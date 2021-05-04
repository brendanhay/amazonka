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
-- Module      : Network.AWS.Snowball.Types.ShippingDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.ShippingDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Snowball.Types.Shipment
import Network.AWS.Snowball.Types.ShippingOption

-- | A job\'s shipping information, including inbound and outbound tracking
-- numbers and shipping speed options.
--
-- /See:/ 'newShippingDetails' smart constructor.
data ShippingDetails = ShippingDetails'
  { -- | The shipping speed for a particular job. This speed doesn\'t dictate how
    -- soon you\'ll get the Snow device from the job\'s creation date. This
    -- speed represents how quickly it moves to its destination while in
    -- transit. Regional shipping speeds are as follows:
    --
    -- -   In Australia, you have access to express shipping. Typically, Snow
    --     devices shipped express are delivered in about a day.
    --
    -- -   In the European Union (EU), you have access to express shipping.
    --     Typically, Snow devices shipped express are delivered in about a
    --     day. In addition, most countries in the EU have access to standard
    --     shipping, which typically takes less than a week, one way.
    --
    -- -   In India, Snow device are delivered in one to seven days.
    --
    -- -   In the United States of America (US), you have access to one-day
    --     shipping and two-day shipping.
    shippingOption :: Prelude.Maybe ShippingOption,
    -- | The @Status@ and @TrackingNumber@ values for a Snow device being
    -- delivered to the address that you specified for a particular job.
    outboundShipment :: Prelude.Maybe Shipment,
    -- | The @Status@ and @TrackingNumber@ values for a Snow device being
    -- returned to AWS for a particular job.
    inboundShipment :: Prelude.Maybe Shipment
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ShippingDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shippingOption', 'shippingDetails_shippingOption' - The shipping speed for a particular job. This speed doesn\'t dictate how
-- soon you\'ll get the Snow device from the job\'s creation date. This
-- speed represents how quickly it moves to its destination while in
-- transit. Regional shipping speeds are as follows:
--
-- -   In Australia, you have access to express shipping. Typically, Snow
--     devices shipped express are delivered in about a day.
--
-- -   In the European Union (EU), you have access to express shipping.
--     Typically, Snow devices shipped express are delivered in about a
--     day. In addition, most countries in the EU have access to standard
--     shipping, which typically takes less than a week, one way.
--
-- -   In India, Snow device are delivered in one to seven days.
--
-- -   In the United States of America (US), you have access to one-day
--     shipping and two-day shipping.
--
-- 'outboundShipment', 'shippingDetails_outboundShipment' - The @Status@ and @TrackingNumber@ values for a Snow device being
-- delivered to the address that you specified for a particular job.
--
-- 'inboundShipment', 'shippingDetails_inboundShipment' - The @Status@ and @TrackingNumber@ values for a Snow device being
-- returned to AWS for a particular job.
newShippingDetails ::
  ShippingDetails
newShippingDetails =
  ShippingDetails'
    { shippingOption = Prelude.Nothing,
      outboundShipment = Prelude.Nothing,
      inboundShipment = Prelude.Nothing
    }

-- | The shipping speed for a particular job. This speed doesn\'t dictate how
-- soon you\'ll get the Snow device from the job\'s creation date. This
-- speed represents how quickly it moves to its destination while in
-- transit. Regional shipping speeds are as follows:
--
-- -   In Australia, you have access to express shipping. Typically, Snow
--     devices shipped express are delivered in about a day.
--
-- -   In the European Union (EU), you have access to express shipping.
--     Typically, Snow devices shipped express are delivered in about a
--     day. In addition, most countries in the EU have access to standard
--     shipping, which typically takes less than a week, one way.
--
-- -   In India, Snow device are delivered in one to seven days.
--
-- -   In the United States of America (US), you have access to one-day
--     shipping and two-day shipping.
shippingDetails_shippingOption :: Lens.Lens' ShippingDetails (Prelude.Maybe ShippingOption)
shippingDetails_shippingOption = Lens.lens (\ShippingDetails' {shippingOption} -> shippingOption) (\s@ShippingDetails' {} a -> s {shippingOption = a} :: ShippingDetails)

-- | The @Status@ and @TrackingNumber@ values for a Snow device being
-- delivered to the address that you specified for a particular job.
shippingDetails_outboundShipment :: Lens.Lens' ShippingDetails (Prelude.Maybe Shipment)
shippingDetails_outboundShipment = Lens.lens (\ShippingDetails' {outboundShipment} -> outboundShipment) (\s@ShippingDetails' {} a -> s {outboundShipment = a} :: ShippingDetails)

-- | The @Status@ and @TrackingNumber@ values for a Snow device being
-- returned to AWS for a particular job.
shippingDetails_inboundShipment :: Lens.Lens' ShippingDetails (Prelude.Maybe Shipment)
shippingDetails_inboundShipment = Lens.lens (\ShippingDetails' {inboundShipment} -> inboundShipment) (\s@ShippingDetails' {} a -> s {inboundShipment = a} :: ShippingDetails)

instance Prelude.FromJSON ShippingDetails where
  parseJSON =
    Prelude.withObject
      "ShippingDetails"
      ( \x ->
          ShippingDetails'
            Prelude.<$> (x Prelude..:? "ShippingOption")
            Prelude.<*> (x Prelude..:? "OutboundShipment")
            Prelude.<*> (x Prelude..:? "InboundShipment")
      )

instance Prelude.Hashable ShippingDetails

instance Prelude.NFData ShippingDetails
