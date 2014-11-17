{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_PurchaseReservedNodeOffering.html>
module Network.AWS.Redshift.PurchaseReservedNodeOffering
    (
    -- * Request
      PurchaseReservedNodeOffering
    -- ** Request constructor
    , purchaseReservedNodeOffering
    -- ** Request lenses
    , prnoNodeCount
    , prnoReservedNodeOfferingId

    -- * Response
    , PurchaseReservedNodeOfferingResponse
    -- ** Response constructor
    , purchaseReservedNodeOfferingResponse
    -- ** Response lenses
    , prnorReservedNode
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

data PurchaseReservedNodeOffering = PurchaseReservedNodeOffering
    { _prnoNodeCount              :: Maybe Int
    , _prnoReservedNodeOfferingId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'PurchaseReservedNodeOffering' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prnoNodeCount' @::@ 'Maybe' 'Int'
--
-- * 'prnoReservedNodeOfferingId' @::@ 'Text'
--
purchaseReservedNodeOffering :: Text -- ^ 'prnoReservedNodeOfferingId'
                             -> PurchaseReservedNodeOffering
purchaseReservedNodeOffering p1 = PurchaseReservedNodeOffering
    { _prnoReservedNodeOfferingId = p1
    , _prnoNodeCount              = Nothing
    }

-- | The number of reserved nodes you want to purchase. Default: 1.
prnoNodeCount :: Lens' PurchaseReservedNodeOffering (Maybe Int)
prnoNodeCount = lens _prnoNodeCount (\s a -> s { _prnoNodeCount = a })

-- | The unique identifier of the reserved node offering you want to purchase.
prnoReservedNodeOfferingId :: Lens' PurchaseReservedNodeOffering Text
prnoReservedNodeOfferingId =
    lens _prnoReservedNodeOfferingId
        (\s a -> s { _prnoReservedNodeOfferingId = a })

newtype PurchaseReservedNodeOfferingResponse = PurchaseReservedNodeOfferingResponse
    { _prnorReservedNode :: Maybe ReservedNode
    } deriving (Eq, Show, Generic)

-- | 'PurchaseReservedNodeOfferingResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prnorReservedNode' @::@ 'Maybe' 'ReservedNode'
--
purchaseReservedNodeOfferingResponse :: PurchaseReservedNodeOfferingResponse
purchaseReservedNodeOfferingResponse = PurchaseReservedNodeOfferingResponse
    { _prnorReservedNode = Nothing
    }

prnorReservedNode :: Lens' PurchaseReservedNodeOfferingResponse (Maybe ReservedNode)
prnorReservedNode =
    lens _prnorReservedNode (\s a -> s { _prnorReservedNode = a })

instance ToPath PurchaseReservedNodeOffering where
    toPath = const "/"

instance ToQuery PurchaseReservedNodeOffering

instance ToHeaders PurchaseReservedNodeOffering

instance AWSRequest PurchaseReservedNodeOffering where
    type Sv PurchaseReservedNodeOffering = Redshift
    type Rs PurchaseReservedNodeOffering = PurchaseReservedNodeOfferingResponse

    request  = post "PurchaseReservedNodeOffering"
    response = xmlResponse

instance FromXML PurchaseReservedNodeOfferingResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PurchaseReservedNodeOfferingResponse"
