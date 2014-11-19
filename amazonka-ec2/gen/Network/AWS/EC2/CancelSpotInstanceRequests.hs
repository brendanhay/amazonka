{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CancelSpotInstanceRequests
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Cancels one or more Spot Instance requests. Spot Instances are instances
-- that Amazon EC2 starts on your behalf when the maximum price that you
-- specify exceeds the current Spot Price. Amazon EC2 periodically sets the
-- Spot Price based on available Spot Instance capacity and current Spot
-- Instance requests. For more information about Spot Instances, see Spot
-- Instances in the Amazon Elastic Compute Cloud User Guide. Canceling a Spot
-- Instance request does not terminate running Spot Instances associated with
-- the request.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelSpotInstanceRequests.html>
module Network.AWS.EC2.CancelSpotInstanceRequests
    (
    -- * Request
      CancelSpotInstanceRequests
    -- ** Request constructor
    , cancelSpotInstanceRequests
    -- ** Request lenses
    , csirDryRun
    , csirSpotInstanceRequestIds

    -- * Response
    , CancelSpotInstanceRequestsResponse
    -- ** Response constructor
    , cancelSpotInstanceRequestsResponse
    -- ** Response lenses
    , csirrCancelledSpotInstanceRequests
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CancelSpotInstanceRequests = CancelSpotInstanceRequests
    { _csirDryRun                 :: Maybe Bool
    , _csirSpotInstanceRequestIds :: List "SpotInstanceRequestId" Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CancelSpotInstanceRequests' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csirDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'csirSpotInstanceRequestIds' @::@ ['Text']
--
cancelSpotInstanceRequests :: CancelSpotInstanceRequests
cancelSpotInstanceRequests = CancelSpotInstanceRequests
    { _csirDryRun                 = Nothing
    , _csirSpotInstanceRequestIds = mempty
    }

csirDryRun :: Lens' CancelSpotInstanceRequests (Maybe Bool)
csirDryRun = lens _csirDryRun (\s a -> s { _csirDryRun = a })

-- | One or more Spot Instance request IDs.
csirSpotInstanceRequestIds :: Lens' CancelSpotInstanceRequests [Text]
csirSpotInstanceRequestIds =
    lens _csirSpotInstanceRequestIds
        (\s a -> s { _csirSpotInstanceRequestIds = a })
            . _List

newtype CancelSpotInstanceRequestsResponse = CancelSpotInstanceRequestsResponse
    { _csirrCancelledSpotInstanceRequests :: List "item" CancelledSpotInstanceRequest
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList CancelSpotInstanceRequestsResponse where
    type Item CancelSpotInstanceRequestsResponse = CancelledSpotInstanceRequest

    fromList = CancelSpotInstanceRequestsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _csirrCancelledSpotInstanceRequests

-- | 'CancelSpotInstanceRequestsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csirrCancelledSpotInstanceRequests' @::@ ['CancelledSpotInstanceRequest']
--
cancelSpotInstanceRequestsResponse :: CancelSpotInstanceRequestsResponse
cancelSpotInstanceRequestsResponse = CancelSpotInstanceRequestsResponse
    { _csirrCancelledSpotInstanceRequests = mempty
    }

-- | One or more Spot Instance requests.
csirrCancelledSpotInstanceRequests :: Lens' CancelSpotInstanceRequestsResponse [CancelledSpotInstanceRequest]
csirrCancelledSpotInstanceRequests =
    lens _csirrCancelledSpotInstanceRequests
        (\s a -> s { _csirrCancelledSpotInstanceRequests = a })
            . _List

instance ToPath CancelSpotInstanceRequests where
    toPath = const "/"

instance ToQuery CancelSpotInstanceRequests

instance ToHeaders CancelSpotInstanceRequests

instance AWSRequest CancelSpotInstanceRequests where
    type Sv CancelSpotInstanceRequests = EC2
    type Rs CancelSpotInstanceRequests = CancelSpotInstanceRequestsResponse

    request  = post "CancelSpotInstanceRequests"
    response = xmlResponse

instance FromXML CancelSpotInstanceRequestsResponse where
    parseXML x = CancelSpotInstanceRequestsResponse
        <$> x .@ "spotInstanceRequestSet"
