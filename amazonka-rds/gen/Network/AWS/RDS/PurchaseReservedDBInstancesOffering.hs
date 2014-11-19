{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_PurchaseReservedDBInstancesOffering.html>
module Network.AWS.RDS.PurchaseReservedDBInstancesOffering
    (
    -- * Request
      PurchaseReservedDBInstancesOffering
    -- ** Request constructor
    , purchaseReservedDBInstancesOffering
    -- ** Request lenses
    , prdbioDBInstanceCount
    , prdbioReservedDBInstanceId
    , prdbioReservedDBInstancesOfferingId
    , prdbioTags

    -- * Response
    , PurchaseReservedDBInstancesOfferingResponse
    -- ** Response constructor
    , purchaseReservedDBInstancesOfferingResponse
    -- ** Response lenses
    , prdbiorReservedDBInstance
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data PurchaseReservedDBInstancesOffering = PurchaseReservedDBInstancesOffering
    { _prdbioDBInstanceCount               :: Maybe Int
    , _prdbioReservedDBInstanceId          :: Maybe Text
    , _prdbioReservedDBInstancesOfferingId :: Text
    , _prdbioTags                          :: [Tag]
    } deriving (Eq, Show, Generic)

-- | 'PurchaseReservedDBInstancesOffering' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prdbioDBInstanceCount' @::@ 'Maybe' 'Int'
--
-- * 'prdbioReservedDBInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'prdbioReservedDBInstancesOfferingId' @::@ 'Text'
--
-- * 'prdbioTags' @::@ ['Tag']
--
purchaseReservedDBInstancesOffering :: Text -- ^ 'prdbioReservedDBInstancesOfferingId'
                                    -> PurchaseReservedDBInstancesOffering
purchaseReservedDBInstancesOffering p1 = PurchaseReservedDBInstancesOffering
    { _prdbioReservedDBInstancesOfferingId = p1
    , _prdbioReservedDBInstanceId          = Nothing
    , _prdbioDBInstanceCount               = Nothing
    , _prdbioTags                          = mempty
    }

-- | The number of instances to reserve. Default: 1.
prdbioDBInstanceCount :: Lens' PurchaseReservedDBInstancesOffering (Maybe Int)
prdbioDBInstanceCount =
    lens _prdbioDBInstanceCount (\s a -> s { _prdbioDBInstanceCount = a })

-- | Customer-specified identifier to track this reservation. Example:
-- myreservationID.
prdbioReservedDBInstanceId :: Lens' PurchaseReservedDBInstancesOffering (Maybe Text)
prdbioReservedDBInstanceId =
    lens _prdbioReservedDBInstanceId
        (\s a -> s { _prdbioReservedDBInstanceId = a })

-- | The ID of the Reserved DB instance offering to purchase. Example:
-- 438012d3-4052-4cc7-b2e3-8d3372e0e706.
prdbioReservedDBInstancesOfferingId :: Lens' PurchaseReservedDBInstancesOffering Text
prdbioReservedDBInstancesOfferingId =
    lens _prdbioReservedDBInstancesOfferingId
        (\s a -> s { _prdbioReservedDBInstancesOfferingId = a })

prdbioTags :: Lens' PurchaseReservedDBInstancesOffering [Tag]
prdbioTags = lens _prdbioTags (\s a -> s { _prdbioTags = a })

newtype PurchaseReservedDBInstancesOfferingResponse = PurchaseReservedDBInstancesOfferingResponse
    { _prdbiorReservedDBInstance :: Maybe ReservedDBInstance
    } deriving (Eq, Show, Generic)

-- | 'PurchaseReservedDBInstancesOfferingResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prdbiorReservedDBInstance' @::@ 'Maybe' 'ReservedDBInstance'
--
purchaseReservedDBInstancesOfferingResponse :: PurchaseReservedDBInstancesOfferingResponse
purchaseReservedDBInstancesOfferingResponse = PurchaseReservedDBInstancesOfferingResponse
    { _prdbiorReservedDBInstance = Nothing
    }

prdbiorReservedDBInstance :: Lens' PurchaseReservedDBInstancesOfferingResponse (Maybe ReservedDBInstance)
prdbiorReservedDBInstance =
    lens _prdbiorReservedDBInstance
        (\s a -> s { _prdbiorReservedDBInstance = a })

instance ToPath PurchaseReservedDBInstancesOffering where
    toPath = const "/"

instance ToQuery PurchaseReservedDBInstancesOffering

instance ToHeaders PurchaseReservedDBInstancesOffering

instance AWSRequest PurchaseReservedDBInstancesOffering where
    type Sv PurchaseReservedDBInstancesOffering = RDS
    type Rs PurchaseReservedDBInstancesOffering = PurchaseReservedDBInstancesOfferingResponse

    request  = post "PurchaseReservedDBInstancesOffering"
    response = xmlResponse

instance FromXML PurchaseReservedDBInstancesOfferingResponse where
    parseXML = withElement "PurchaseReservedDBInstancesOfferingResult" $ \x ->
            <$> x .@? "ReservedDBInstance"
