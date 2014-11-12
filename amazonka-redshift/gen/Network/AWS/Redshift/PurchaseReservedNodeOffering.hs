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
module Network.AWS.Redshift.PurchaseReservedNodeOffering
    (
    -- * Request
      PurchaseReservedNodeOfferingMessage
    -- ** Request constructor
    , purchaseReservedNodeOffering
    -- ** Request lenses
    , prnomNodeCount
    , prnomReservedNodeOfferingId

    -- * Response
    , PurchaseReservedNodeOfferingResult
    -- ** Response constructor
    , purchaseReservedNodeOfferingResponse
    -- ** Response lenses
    , prnorReservedNode
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data PurchaseReservedNodeOfferingMessage = PurchaseReservedNodeOfferingMessage
    { _prnomNodeCount              :: Maybe Int
    , _prnomReservedNodeOfferingId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'PurchaseReservedNodeOfferingMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prnomNodeCount' @::@ 'Maybe' 'Int'
--
-- * 'prnomReservedNodeOfferingId' @::@ 'Text'
--
purchaseReservedNodeOffering :: Text -- ^ 'prnomReservedNodeOfferingId'
                             -> PurchaseReservedNodeOfferingMessage
purchaseReservedNodeOffering p1 = PurchaseReservedNodeOfferingMessage
    { _prnomReservedNodeOfferingId = p1
    , _prnomNodeCount              = Nothing
    }

-- | The number of reserved nodes you want to purchase. Default: 1.
prnomNodeCount :: Lens' PurchaseReservedNodeOfferingMessage (Maybe Int)
prnomNodeCount = lens _prnomNodeCount (\s a -> s { _prnomNodeCount = a })

-- | The unique identifier of the reserved node offering you want to purchase.
prnomReservedNodeOfferingId :: Lens' PurchaseReservedNodeOfferingMessage Text
prnomReservedNodeOfferingId =
    lens _prnomReservedNodeOfferingId
        (\s a -> s { _prnomReservedNodeOfferingId = a })

instance ToQuery PurchaseReservedNodeOfferingMessage

instance ToPath PurchaseReservedNodeOfferingMessage where
    toPath = const "/"

newtype PurchaseReservedNodeOfferingResult = PurchaseReservedNodeOfferingResult
    { _prnorReservedNode :: Maybe ReservedNode
    } deriving (Eq, Show, Generic)

-- | 'PurchaseReservedNodeOfferingResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'prnorReservedNode' @::@ 'Maybe' 'ReservedNode'
--
purchaseReservedNodeOfferingResponse :: PurchaseReservedNodeOfferingResult
purchaseReservedNodeOfferingResponse = PurchaseReservedNodeOfferingResult
    { _prnorReservedNode = Nothing
    }

prnorReservedNode :: Lens' PurchaseReservedNodeOfferingResult (Maybe ReservedNode)
prnorReservedNode =
    lens _prnorReservedNode (\s a -> s { _prnorReservedNode = a })

instance FromXML PurchaseReservedNodeOfferingResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "PurchaseReservedNodeOfferingResult"

instance AWSRequest PurchaseReservedNodeOfferingMessage where
    type Sv PurchaseReservedNodeOfferingMessage = Redshift
    type Rs PurchaseReservedNodeOfferingMessage = PurchaseReservedNodeOfferingResult

    request  = post "PurchaseReservedNodeOffering"
    response = xmlResponse $ \h x -> PurchaseReservedNodeOfferingResult
        <$> x %| "ReservedNode"
