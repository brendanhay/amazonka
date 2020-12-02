{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.ShippingDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.ShippingDetails where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Snowball.Types.Shipment
import Network.AWS.Snowball.Types.ShippingOption

-- | A job's shipping information, including inbound and outbound tracking numbers and shipping speed options.
--
--
--
-- /See:/ 'shippingDetails' smart constructor.
data ShippingDetails = ShippingDetails'
  { _sdShippingOption ::
      !(Maybe ShippingOption),
    _sdOutboundShipment :: !(Maybe Shipment),
    _sdInboundShipment :: !(Maybe Shipment)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ShippingDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdShippingOption' - The shipping speed for a particular job. This speed doesn't dictate how soon you'll get the Snow device from the job's creation date. This speed represents how quickly it moves to its destination while in transit. Regional shipping speeds are as follows:     * In Australia, you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day.     * In the European Union (EU), you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.     * In India, Snow device are delivered in one to seven days.     * In the United States of America (US), you have access to one-day shipping and two-day shipping.
--
-- * 'sdOutboundShipment' - The @Status@ and @TrackingNumber@ values for a Snow device being delivered to the address that you specified for a particular job.
--
-- * 'sdInboundShipment' - The @Status@ and @TrackingNumber@ values for a Snow device being returned to AWS for a particular job.
shippingDetails ::
  ShippingDetails
shippingDetails =
  ShippingDetails'
    { _sdShippingOption = Nothing,
      _sdOutboundShipment = Nothing,
      _sdInboundShipment = Nothing
    }

-- | The shipping speed for a particular job. This speed doesn't dictate how soon you'll get the Snow device from the job's creation date. This speed represents how quickly it moves to its destination while in transit. Regional shipping speeds are as follows:     * In Australia, you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day.     * In the European Union (EU), you have access to express shipping. Typically, Snow devices shipped express are delivered in about a day. In addition, most countries in the EU have access to standard shipping, which typically takes less than a week, one way.     * In India, Snow device are delivered in one to seven days.     * In the United States of America (US), you have access to one-day shipping and two-day shipping.
sdShippingOption :: Lens' ShippingDetails (Maybe ShippingOption)
sdShippingOption = lens _sdShippingOption (\s a -> s {_sdShippingOption = a})

-- | The @Status@ and @TrackingNumber@ values for a Snow device being delivered to the address that you specified for a particular job.
sdOutboundShipment :: Lens' ShippingDetails (Maybe Shipment)
sdOutboundShipment = lens _sdOutboundShipment (\s a -> s {_sdOutboundShipment = a})

-- | The @Status@ and @TrackingNumber@ values for a Snow device being returned to AWS for a particular job.
sdInboundShipment :: Lens' ShippingDetails (Maybe Shipment)
sdInboundShipment = lens _sdInboundShipment (\s a -> s {_sdInboundShipment = a})

instance FromJSON ShippingDetails where
  parseJSON =
    withObject
      "ShippingDetails"
      ( \x ->
          ShippingDetails'
            <$> (x .:? "ShippingOption")
            <*> (x .:? "OutboundShipment")
            <*> (x .:? "InboundShipment")
      )

instance Hashable ShippingDetails

instance NFData ShippingDetails
