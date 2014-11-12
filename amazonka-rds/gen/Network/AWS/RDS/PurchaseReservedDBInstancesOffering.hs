{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.RDS.PurchaseReservedDBInstancesOffering
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Purchases a reserved DB instance offering.
module Network.AWS.RDS.PurchaseReservedDBInstancesOffering
    (
    -- * Request
      PurchaseReservedDBInstancesOfferingMessage
    -- ** Request constructor
    , purchaseReservedDBInstancesOffering
    -- ** Request lenses
    , prdbiomDBInstanceCount
    , prdbiomReservedDBInstanceId
    , prdbiomReservedDBInstancesOfferingId
    , prdbiomTags

    -- * Response
    , PurchaseReservedDBInstancesOfferingResult
    -- ** Response constructor
    , purchaseReservedDBInstancesOfferingResponse
    -- ** Response lenses
    , prdbiorReservedDBInstance
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data PurchaseReservedDBInstancesOfferingMessage = PurchaseReservedDBInstancesOfferingMessage
    { _prdbiomDBInstanceCount               :: Maybe Int
    , _prdbiomReservedDBInstanceId          :: Maybe Text
    , _prdbiomReservedDBInstancesOfferingId :: Text
    , _prdbiomTags                          :: [Tag]
    } deriving (Eq, Show, Generic)

-- | 'PurchaseReservedDBInstancesOfferingMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prdbiomDBInstanceCount' @::@ 'Maybe' 'Int'
--
-- * 'prdbiomReservedDBInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'prdbiomReservedDBInstancesOfferingId' @::@ 'Text'
--
-- * 'prdbiomTags' @::@ ['Tag']
--
purchaseReservedDBInstancesOffering :: Text -- ^ 'prdbiomReservedDBInstancesOfferingId'
                                    -> PurchaseReservedDBInstancesOfferingMessage
purchaseReservedDBInstancesOffering p1 = PurchaseReservedDBInstancesOfferingMessage
    { _prdbiomReservedDBInstancesOfferingId = p1
    , _prdbiomReservedDBInstanceId          = Nothing
    , _prdbiomDBInstanceCount               = Nothing
    , _prdbiomTags                          = mempty
    }

-- | The number of instances to reserve. Default: 1.
prdbiomDBInstanceCount :: Lens' PurchaseReservedDBInstancesOfferingMessage (Maybe Int)
prdbiomDBInstanceCount =
    lens _prdbiomDBInstanceCount (\s a -> s { _prdbiomDBInstanceCount = a })

-- | Customer-specified identifier to track this reservation. Example:
-- myreservationID.
prdbiomReservedDBInstanceId :: Lens' PurchaseReservedDBInstancesOfferingMessage (Maybe Text)
prdbiomReservedDBInstanceId =
    lens _prdbiomReservedDBInstanceId
        (\s a -> s { _prdbiomReservedDBInstanceId = a })

-- | The ID of the Reserved DB instance offering to purchase. Example:
-- 438012d3-4052-4cc7-b2e3-8d3372e0e706.
prdbiomReservedDBInstancesOfferingId :: Lens' PurchaseReservedDBInstancesOfferingMessage Text
prdbiomReservedDBInstancesOfferingId =
    lens _prdbiomReservedDBInstancesOfferingId
        (\s a -> s { _prdbiomReservedDBInstancesOfferingId = a })

prdbiomTags :: Lens' PurchaseReservedDBInstancesOfferingMessage [Tag]
prdbiomTags = lens _prdbiomTags (\s a -> s { _prdbiomTags = a })

instance ToQuery PurchaseReservedDBInstancesOfferingMessage

instance ToPath PurchaseReservedDBInstancesOfferingMessage where
    toPath = const "/"

newtype PurchaseReservedDBInstancesOfferingResult = PurchaseReservedDBInstancesOfferingResult
    { _prdbiorReservedDBInstance :: Maybe ReservedDBInstance
    } deriving (Eq, Show, Generic)

-- | 'PurchaseReservedDBInstancesOfferingResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prdbiorReservedDBInstance' @::@ 'Maybe' 'ReservedDBInstance'
--
purchaseReservedDBInstancesOfferingResponse :: PurchaseReservedDBInstancesOfferingResult
purchaseReservedDBInstancesOfferingResponse = PurchaseReservedDBInstancesOfferingResult
    { _prdbiorReservedDBInstance = Nothing
    }

prdbiorReservedDBInstance :: Lens' PurchaseReservedDBInstancesOfferingResult (Maybe ReservedDBInstance)
prdbiorReservedDBInstance =
    lens _prdbiorReservedDBInstance
        (\s a -> s { _prdbiorReservedDBInstance = a })

instance FromXML PurchaseReservedDBInstancesOfferingResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PurchaseReservedDBInstancesOfferingResult"

instance AWSRequest PurchaseReservedDBInstancesOfferingMessage where
    type Sv PurchaseReservedDBInstancesOfferingMessage = RDS
    type Rs PurchaseReservedDBInstancesOfferingMessage = PurchaseReservedDBInstancesOfferingResult

    request  = post "PurchaseReservedDBInstancesOffering"
    response = xmlResponse $ \h x -> PurchaseReservedDBInstancesOfferingResult
        <$> x %| "ReservedDBInstance"
