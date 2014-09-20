{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.PurchaseReservedNodeOffering
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
module Network.AWS.Redshift.PurchaseReservedNodeOffering
    (
    -- * Request
      PurchaseReservedNodeOffering
    -- ** Request constructor
    , purchaseReservedNodeOffering
    -- ** Request lenses
    , prnoReservedNodeOfferingId
    , prnoNodeCount

    -- * Response
    , PurchaseReservedNodeOfferingResponse
    -- ** Response constructor
    , purchaseReservedNodeOfferingResponse
    -- ** Response lenses
    , prnorReservedNode
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

-- | 
data PurchaseReservedNodeOffering = PurchaseReservedNodeOffering
    { _prnoReservedNodeOfferingId :: Text
    , _prnoNodeCount :: Maybe Integer
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PurchaseReservedNodeOffering' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedNodeOfferingId ::@ @Text@
--
-- * @NodeCount ::@ @Maybe Integer@
--
purchaseReservedNodeOffering :: Text -- ^ 'prnoReservedNodeOfferingId'
                             -> PurchaseReservedNodeOffering
purchaseReservedNodeOffering p1 = PurchaseReservedNodeOffering
    { _prnoReservedNodeOfferingId = p1
    , _prnoNodeCount = Nothing
    }

-- | The unique identifier of the reserved node offering you want to purchase.
prnoReservedNodeOfferingId :: Lens' PurchaseReservedNodeOffering Text
prnoReservedNodeOfferingId =
    lens _prnoReservedNodeOfferingId
         (\s a -> s { _prnoReservedNodeOfferingId = a })

-- | The number of reserved nodes you want to purchase. Default: 1.
prnoNodeCount :: Lens' PurchaseReservedNodeOffering (Maybe Integer)
prnoNodeCount = lens _prnoNodeCount (\s a -> s { _prnoNodeCount = a })

instance ToQuery PurchaseReservedNodeOffering where
    toQuery = genericQuery def

newtype PurchaseReservedNodeOfferingResponse = PurchaseReservedNodeOfferingResponse
    { _prnorReservedNode :: Maybe ReservedNode
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'PurchaseReservedNodeOfferingResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ReservedNode ::@ @Maybe ReservedNode@
--
purchaseReservedNodeOfferingResponse :: PurchaseReservedNodeOfferingResponse
purchaseReservedNodeOfferingResponse = PurchaseReservedNodeOfferingResponse
    { _prnorReservedNode = Nothing
    }

-- | Describes a reserved node.
prnorReservedNode :: Lens' PurchaseReservedNodeOfferingResponse (Maybe ReservedNode)
prnorReservedNode =
    lens _prnorReservedNode (\s a -> s { _prnorReservedNode = a })

instance FromXML PurchaseReservedNodeOfferingResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest PurchaseReservedNodeOffering where
    type Sv PurchaseReservedNodeOffering = Redshift
    type Rs PurchaseReservedNodeOffering = PurchaseReservedNodeOfferingResponse

    request = post "PurchaseReservedNodeOffering"
    response _ = xmlResponse
