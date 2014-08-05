{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.RDS.V2013_09_09.PurchaseReservedDBInstancesOffering where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PurchaseReservedDBInstancesOffering' request.
purchaseReservedDBInstancesOffering :: Text -- ^ '_prdbiomReservedDBInstancesOfferingId'
                                    -> PurchaseReservedDBInstancesOffering
purchaseReservedDBInstancesOffering p1 = PurchaseReservedDBInstancesOffering
    { _prdbiomReservedDBInstancesOfferingId = p1
    , _prdbiomDBInstanceCount = Nothing
    , _prdbiomReservedDBInstanceId = Nothing
    , _prdbiomTags = mempty
    }

data PurchaseReservedDBInstancesOffering = PurchaseReservedDBInstancesOffering
    { _prdbiomReservedDBInstancesOfferingId :: Text
      -- ^ The ID of the Reserved DB instance offering to purchase. Example:
      -- 438012d3-4052-4cc7-b2e3-8d3372e0e706.
    , _prdbiomDBInstanceCount :: Maybe Integer
      -- ^ The number of instances to reserve. Default: 1.
    , _prdbiomReservedDBInstanceId :: Maybe Text
      -- ^ Customer-specified identifier to track this reservation. Example:
      -- myreservationID.
    , _prdbiomTags :: [Tag]
      -- ^ A list of tags.
    } deriving (Show, Generic)

makeLenses ''PurchaseReservedDBInstancesOffering

instance ToQuery PurchaseReservedDBInstancesOffering where
    toQuery = genericToQuery def

data PurchaseReservedDBInstancesOfferingResponse = PurchaseReservedDBInstancesOfferingResponse
    { _rdbiwReservedDBInstance :: Maybe ReservedDBInstance
      -- ^ This data type is used as a response element in the
      -- DescribeReservedDBInstances and
      -- PurchaseReservedDBInstancesOffering actions.
    } deriving (Show, Generic)

makeLenses ''PurchaseReservedDBInstancesOfferingResponse

instance AWSRequest PurchaseReservedDBInstancesOffering where
    type Sv PurchaseReservedDBInstancesOffering = RDS
    type Rs PurchaseReservedDBInstancesOffering = PurchaseReservedDBInstancesOfferingResponse

    request = post "PurchaseReservedDBInstancesOffering"
    response _ = cursorResponse $ \hs xml ->
        pure PurchaseReservedDBInstancesOfferingResponse
            <*> xml %|? "ReservedDBInstance"
