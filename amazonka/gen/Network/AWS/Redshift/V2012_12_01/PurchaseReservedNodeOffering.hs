{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.PurchaseReservedNodeOffering
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Allows you to purchase reserved nodes. Amazon Redshift offers a predefined
-- set of reserved node offerings. You can purchase one of the offerings. You
-- can call the DescribeReservedNodeOfferings API to obtain the available
-- reserved node offerings. You can call this API by providing a specific
-- reserved node offering and the number of nodes you want to reserve. For
-- more information about managing parameter groups, go to Purchasing Reserved
-- Nodes in the Amazon Redshift Management Guide.
-- https://redshift.us-east-1.amazonaws.com/
-- ?Action=PurchaseReservedNodeOffering
-- &ReservedNodeOfferingId=3a98bf7d-979a-49cc-b568-18f24315baf0 &NodeCount=2
-- &Version=2012-12-01 &x-amz-algorithm=AWS4-HMAC-SHA256
-- &x-amz-credential=AKIAIOSFODNN7EXAMPLE/20130117/us-east-1/redshift/aws4_request
-- &x-amz-date=20130117T232351Z
-- &x-amz-signedheaders=content-type;host;x-amz-date 2013-01-18T21:42:44.402Z
-- Heavy Utilization 94608000 Hourly 0.21 12452.0 0.0 payment-pending
-- dw1.8xlarge 2 1ba8e2e3-dacf-48d9-841f-cc675182a8a6
-- fcb117cc-61b7-11e2-b6e9-87e586e4ca38.
module Network.AWS.Redshift.V2012_12_01.PurchaseReservedNodeOffering where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'PurchaseReservedNodeOffering' request.
purchaseReservedNodeOffering :: Text -- ^ '_prnomReservedNodeOfferingId'
                             -> PurchaseReservedNodeOffering
purchaseReservedNodeOffering p1 = PurchaseReservedNodeOffering
    { _prnomReservedNodeOfferingId = p1
    , _prnomNodeCount = Nothing
    }

data PurchaseReservedNodeOffering = PurchaseReservedNodeOffering
    { _prnomReservedNodeOfferingId :: Text
      -- ^ The unique identifier of the reserved node offering you want to
      -- purchase.
    , _prnomNodeCount :: Maybe Integer
      -- ^ The number of reserved nodes you want to purchase. Default: 1.
    } deriving (Generic)

makeLenses ''PurchaseReservedNodeOffering

instance ToQuery PurchaseReservedNodeOffering where
    toQuery = genericToQuery def

data PurchaseReservedNodeOfferingResponse = PurchaseReservedNodeOfferingResponse
    { _rnwReservedNode :: Maybe ReservedNode
      -- ^ Describes a reserved node.
    } deriving (Generic)

makeLenses ''PurchaseReservedNodeOfferingResponse

instance FromXML PurchaseReservedNodeOfferingResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest PurchaseReservedNodeOffering where
    type Sv PurchaseReservedNodeOffering = Redshift
    type Rs PurchaseReservedNodeOffering = PurchaseReservedNodeOfferingResponse

    request = post "PurchaseReservedNodeOffering"
    response _ = xmlResponse
