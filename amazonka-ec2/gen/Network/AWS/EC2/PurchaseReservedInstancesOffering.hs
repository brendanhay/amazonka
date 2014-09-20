{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.PurchaseReservedInstancesOffering
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Purchases a Reserved Instance for use with your account. With Amazon EC2
-- Reserved Instances, you obtain a capacity reservation for a certain
-- instance configuration over a specified period of time. You pay a lower
-- usage rate than with On-Demand instances for the time that you actually use
-- the capacity reservation. For more information, see Reserved Instance
-- Marketplace in the Amazon Elastic Compute Cloud User Guide. Example 1 This
-- example uses a limit price to limit the total purchase order of Reserved
-- Instances from Reserved Instance Marketplace.
-- https://ec2.amazonaws.com/?Action=PurchaseReservedInstancesOffering
-- &amp;ReservedInstancesOfferingId=4b2293b4-5813-4cc8-9ce3-1957fEXAMPLE
-- &amp;LimitPrice.Amount=200 &amp;InstanceCount=2 &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE e5a2ff3b-7d14-494f-90af-0b5d0EXAMPLE
-- Example 2 This example illustrates a purchase of a Reserved Instances
-- offering.
-- https://ec2.amazonaws.com/?Action=PurchaseReservedInstancesOffering
-- &amp;ReservedInstancesOfferingId=4b2293b4-5813-4cc8-9ce3-1957fEXAMPLE
-- &amp;InstanceCount=2 &amp;AUTHPARAMS
-- &lt;PurchaseReservedInstancesOfferingResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;reservedInstancesId&gt;e5a2ff3b-7d14-494f-90af-0b5d0EXAMPLE&lt;/reservedInstancesId&gt;
-- &lt;/PurchaseReservedInstancesOfferingResponse&gt;.
module Network.AWS.EC2.PurchaseReservedInstancesOffering
    (
    -- * Request
      PurchaseReservedInstancesOffering
    -- ** Request constructor
    , purchaseReservedInstancesOffering
    -- ** Request lenses
    , prioReservedInstancesOfferingId
    , prioInstanceCount
    , prioLimitPrice

    -- * Response
    , PurchaseReservedInstancesOfferingResponse
    -- ** Response constructor
    , purchaseReservedInstancesOfferingResponse
    -- ** Response lenses
    , priorReservedInstancesId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data PurchaseReservedInstancesOffering = PurchaseReservedInstancesOffering
    { _prioReservedInstancesOfferingId :: Text
    , _prioInstanceCount :: !Integer
    , _prioLimitPrice :: Maybe ReservedInstanceLimitPrice
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PurchaseReservedInstancesOffering' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedInstancesOfferingId ::@ @Text@
--
-- * @InstanceCount ::@ @Integer@
--
-- * @LimitPrice ::@ @Maybe ReservedInstanceLimitPrice@
--
purchaseReservedInstancesOffering :: Text -- ^ 'prioReservedInstancesOfferingId'
                                  -> Integer -- ^ 'prioInstanceCount'
                                  -> PurchaseReservedInstancesOffering
purchaseReservedInstancesOffering p1 p2 = PurchaseReservedInstancesOffering
    { _prioReservedInstancesOfferingId = p1
    , _prioInstanceCount = p2
    , _prioLimitPrice = Nothing
    }

-- | The ID of the Reserved Instance offering to purchase.
prioReservedInstancesOfferingId :: Lens' PurchaseReservedInstancesOffering Text
prioReservedInstancesOfferingId =
    lens _prioReservedInstancesOfferingId
         (\s a -> s { _prioReservedInstancesOfferingId = a })

-- | The number of Reserved Instances to purchase.
prioInstanceCount :: Lens' PurchaseReservedInstancesOffering Integer
prioInstanceCount =
    lens _prioInstanceCount (\s a -> s { _prioInstanceCount = a })

-- | Specified for Reserved Instance Marketplace offerings to limit the total
-- order and ensure that the Reserved Instances are not purchased at
-- unexpected prices.
prioLimitPrice :: Lens' PurchaseReservedInstancesOffering (Maybe ReservedInstanceLimitPrice)
prioLimitPrice = lens _prioLimitPrice (\s a -> s { _prioLimitPrice = a })

instance ToQuery PurchaseReservedInstancesOffering where
    toQuery = genericQuery def

newtype PurchaseReservedInstancesOfferingResponse = PurchaseReservedInstancesOfferingResponse
    { _priorReservedInstancesId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PurchaseReservedInstancesOfferingResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedInstancesId ::@ @Maybe Text@
--
purchaseReservedInstancesOfferingResponse :: PurchaseReservedInstancesOfferingResponse
purchaseReservedInstancesOfferingResponse = PurchaseReservedInstancesOfferingResponse
    { _priorReservedInstancesId = Nothing
    }

-- | The IDs of the purchased Reserved Instances.
priorReservedInstancesId :: Lens' PurchaseReservedInstancesOfferingResponse (Maybe Text)
priorReservedInstancesId =
    lens _priorReservedInstancesId
         (\s a -> s { _priorReservedInstancesId = a })

instance FromXML PurchaseReservedInstancesOfferingResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest PurchaseReservedInstancesOffering where
    type Sv PurchaseReservedInstancesOffering = EC2
    type Rs PurchaseReservedInstancesOffering = PurchaseReservedInstancesOfferingResponse

    request = post "PurchaseReservedInstancesOffering"
    response _ = xmlResponse
