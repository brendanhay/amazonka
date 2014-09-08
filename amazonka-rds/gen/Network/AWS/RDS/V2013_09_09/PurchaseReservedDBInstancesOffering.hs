{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.PurchaseReservedDBInstancesOffering
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Purchases a reserved DB instance offering. https://rds.amazonaws.com/
-- ?Action=PurchaseReservedDBInstancesOffering
-- &ReservedDBInstanceId=myreservationID
-- &ReservedDBInstancesOfferingId=438012d3-4052-4cc7-b2e3-8d3372e0e706
-- &DBInstanceCount=1 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-10T18%3A31%3A36.118Z &AWSAccessKeyId= &Signature= Medium
-- Utilization USD mysql 438012d3-4052-4cc7-b2e3-8d3372e0e706 true
-- payment-pending myreservationID 10 2011-12-18T23:24:56.577Z 31536000 123.0
-- 0.123 db.m1.small 7f099901-29cf-11e1-bd06-6fe008f046c3.
module Network.AWS.RDS.V2013_09_09.PurchaseReservedDBInstancesOffering
    (
    -- * Request
      PurchaseReservedDBInstancesOffering
    -- ** Request constructor
    , mkPurchaseReservedDBInstancesOffering
    -- ** Request lenses
    , prdbioReservedDBInstancesOfferingId
    , prdbioReservedDBInstanceId
    , prdbioDBInstanceCount
    , prdbioTags

    -- * Response
    , PurchaseReservedDBInstancesOfferingResponse
    -- ** Response lenses
    , prdbiorReservedDBInstance
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | 
data PurchaseReservedDBInstancesOffering = PurchaseReservedDBInstancesOffering
    { _prdbioReservedDBInstancesOfferingId :: Text
    , _prdbioReservedDBInstanceId :: Maybe Text
    , _prdbioDBInstanceCount :: Maybe Integer
    , _prdbioTags :: [Tag]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PurchaseReservedDBInstancesOffering' request.
mkPurchaseReservedDBInstancesOffering :: Text -- ^ 'prdbioReservedDBInstancesOfferingId'
                                      -> PurchaseReservedDBInstancesOffering
mkPurchaseReservedDBInstancesOffering p1 = PurchaseReservedDBInstancesOffering
    { _prdbioReservedDBInstancesOfferingId = p1
    , _prdbioReservedDBInstanceId = Nothing
    , _prdbioDBInstanceCount = Nothing
    , _prdbioTags = mempty
    }

-- | The ID of the Reserved DB instance offering to purchase. Example:
-- 438012d3-4052-4cc7-b2e3-8d3372e0e706.
prdbioReservedDBInstancesOfferingId :: Lens' PurchaseReservedDBInstancesOffering Text
prdbioReservedDBInstancesOfferingId =
    lens _prdbioReservedDBInstancesOfferingId
         (\s a -> s { _prdbioReservedDBInstancesOfferingId = a })

-- | Customer-specified identifier to track this reservation. Example:
-- myreservationID.
prdbioReservedDBInstanceId :: Lens' PurchaseReservedDBInstancesOffering (Maybe Text)
prdbioReservedDBInstanceId =
    lens _prdbioReservedDBInstanceId
         (\s a -> s { _prdbioReservedDBInstanceId = a })

-- | The number of instances to reserve. Default: 1.
prdbioDBInstanceCount :: Lens' PurchaseReservedDBInstancesOffering (Maybe Integer)
prdbioDBInstanceCount =
    lens _prdbioDBInstanceCount (\s a -> s { _prdbioDBInstanceCount = a })

-- | A list of tags.
prdbioTags :: Lens' PurchaseReservedDBInstancesOffering [Tag]
prdbioTags = lens _prdbioTags (\s a -> s { _prdbioTags = a })

instance ToQuery PurchaseReservedDBInstancesOffering where
    toQuery = genericQuery def

newtype PurchaseReservedDBInstancesOfferingResponse = PurchaseReservedDBInstancesOfferingResponse
    { _prdbiorReservedDBInstance :: Maybe ReservedDBInstance
    } deriving (Show, Generic)

-- | This data type is used as a response element in the
-- DescribeReservedDBInstances and PurchaseReservedDBInstancesOffering
-- actions.
prdbiorReservedDBInstance :: Lens' PurchaseReservedDBInstancesOfferingResponse (Maybe ReservedDBInstance)
prdbiorReservedDBInstance =
    lens _prdbiorReservedDBInstance
         (\s a -> s { _prdbiorReservedDBInstance = a })

instance FromXML PurchaseReservedDBInstancesOfferingResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest PurchaseReservedDBInstancesOffering where
    type Sv PurchaseReservedDBInstancesOffering = RDS
    type Rs PurchaseReservedDBInstancesOffering = PurchaseReservedDBInstancesOfferingResponse

    request = post "PurchaseReservedDBInstancesOffering"
    response _ = xmlResponse
