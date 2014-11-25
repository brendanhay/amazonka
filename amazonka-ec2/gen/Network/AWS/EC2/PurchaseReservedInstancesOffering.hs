{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
-- Reserved Instances, you obtain a capacity reservation for a certain instance
-- configuration over a specified period of time. You pay a lower usage rate
-- than with On-Demand instances for the time that you actually use the capacity
-- reservation.
--
-- Use 'DescribeReservedInstancesOfferings' to get a list of Reserved Instance
-- offerings that match your specifications. After you've purchased a Reserved
-- Instance, you can check for your new Reserved Instance with 'DescribeReservedInstances'.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/concepts-on-demand-reserved-instances.html Reserved Instances> and <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ri-market-general.html Reserved InstanceMarketplace> in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-PurchaseReservedInstancesOffering.html>
module Network.AWS.EC2.PurchaseReservedInstancesOffering
    (
    -- * Request
      PurchaseReservedInstancesOffering
    -- ** Request constructor
    , purchaseReservedInstancesOffering
    -- ** Request lenses
    , prioDryRun
    , prioInstanceCount
    , prioLimitPrice
    , prioReservedInstancesOfferingId

    -- * Response
    , PurchaseReservedInstancesOfferingResponse
    -- ** Response constructor
    , purchaseReservedInstancesOfferingResponse
    -- ** Response lenses
    , priorReservedInstancesId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data PurchaseReservedInstancesOffering = PurchaseReservedInstancesOffering
    { _prioDryRun                      :: Maybe Bool
    , _prioInstanceCount               :: Int
    , _prioLimitPrice                  :: Maybe ReservedInstanceLimitPrice
    , _prioReservedInstancesOfferingId :: Text
    } deriving (Eq, Show)

-- | 'PurchaseReservedInstancesOffering' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prioDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'prioInstanceCount' @::@ 'Int'
--
-- * 'prioLimitPrice' @::@ 'Maybe' 'ReservedInstanceLimitPrice'
--
-- * 'prioReservedInstancesOfferingId' @::@ 'Text'
--
purchaseReservedInstancesOffering :: Text -- ^ 'prioReservedInstancesOfferingId'
                                  -> Int -- ^ 'prioInstanceCount'
                                  -> PurchaseReservedInstancesOffering
purchaseReservedInstancesOffering p1 p2 = PurchaseReservedInstancesOffering
    { _prioReservedInstancesOfferingId = p1
    , _prioInstanceCount               = p2
    , _prioDryRun                      = Nothing
    , _prioLimitPrice                  = Nothing
    }

prioDryRun :: Lens' PurchaseReservedInstancesOffering (Maybe Bool)
prioDryRun = lens _prioDryRun (\s a -> s { _prioDryRun = a })

-- | The number of Reserved Instances to purchase.
--
prioInstanceCount :: Lens' PurchaseReservedInstancesOffering Int
prioInstanceCount =
    lens _prioInstanceCount (\s a -> s { _prioInstanceCount = a })

-- | Specified for Reserved Instance Marketplace offerings to limit the total
-- order and ensure that the Reserved Instances are not purchased at unexpected
-- prices.
--
prioLimitPrice :: Lens' PurchaseReservedInstancesOffering (Maybe ReservedInstanceLimitPrice)
prioLimitPrice = lens _prioLimitPrice (\s a -> s { _prioLimitPrice = a })

-- | The ID of the Reserved Instance offering to purchase.
--
prioReservedInstancesOfferingId :: Lens' PurchaseReservedInstancesOffering Text
prioReservedInstancesOfferingId =
    lens _prioReservedInstancesOfferingId
        (\s a -> s { _prioReservedInstancesOfferingId = a })

newtype PurchaseReservedInstancesOfferingResponse = PurchaseReservedInstancesOfferingResponse
    { _priorReservedInstancesId :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'PurchaseReservedInstancesOfferingResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'priorReservedInstancesId' @::@ 'Maybe' 'Text'
--
purchaseReservedInstancesOfferingResponse :: PurchaseReservedInstancesOfferingResponse
purchaseReservedInstancesOfferingResponse = PurchaseReservedInstancesOfferingResponse
    { _priorReservedInstancesId = Nothing
    }

-- | The IDs of the purchased Reserved Instances.
--
priorReservedInstancesId :: Lens' PurchaseReservedInstancesOfferingResponse (Maybe Text)
priorReservedInstancesId =
    lens _priorReservedInstancesId
        (\s a -> s { _priorReservedInstancesId = a })

instance ToPath PurchaseReservedInstancesOffering where
    toPath = const "/"

instance ToQuery PurchaseReservedInstancesOffering where
    toQuery PurchaseReservedInstancesOffering{..} = mconcat
        [ "dryRun"                      =? _prioDryRun
        , "InstanceCount"               =? _prioInstanceCount
        , "limitPrice"                  =? _prioLimitPrice
        , "ReservedInstancesOfferingId" =? _prioReservedInstancesOfferingId
        ]

instance ToHeaders PurchaseReservedInstancesOffering

instance AWSRequest PurchaseReservedInstancesOffering where
    type Sv PurchaseReservedInstancesOffering = EC2
    type Rs PurchaseReservedInstancesOffering = PurchaseReservedInstancesOfferingResponse

    request  = post "PurchaseReservedInstancesOffering"
    response = xmlResponse

instance FromXML PurchaseReservedInstancesOfferingResponse where
    parseXML x = PurchaseReservedInstancesOfferingResponse
        <$> x .@? "reservedInstancesId"
