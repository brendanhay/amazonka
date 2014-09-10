{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2
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
-- the request. Example This example cancels the specified Spot Instance
-- request. https://ec2.amazonaws.com/?Action=CancelSpotInstanceRequests
-- &amp;SpotInstanceRequestId.1=sir-1a2b3c4d &amp;AUTHPARAMS
-- &lt;CancelSpotInstanceRequestsResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;spotInstanceRequestSet&gt; &lt;item&gt;
-- &lt;spotInstanceRequestId&gt;sir-1a2b3c4d&lt;/spotInstanceRequestId&gt;
-- &lt;state&gt;cancelled&lt;/state&gt; &lt;/item&gt;
-- &lt;/spotInstanceRequestSet&gt;
-- &lt;/CancelSpotInstanceRequestsResponse&gt;.
module Network.AWS.EC2
    (
    -- * Request
      CancelSpotInstanceRequests
    -- ** Request constructor
    , mkCancelSpotInstanceRequests
    -- ** Request lenses
    , csirSpotInstanceRequestIds

    -- * Response
    , CancelSpotInstanceRequestsResponse
    -- ** Response constructor
    , mkCancelSpotInstanceRequestsResponse
    -- ** Response lenses
    , csirrCancelledSpotInstanceRequests
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype CancelSpotInstanceRequests = CancelSpotInstanceRequests
    { _csirSpotInstanceRequestIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CancelSpotInstanceRequests' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SpotInstanceRequestIds ::@ @[Text]@
--
mkCancelSpotInstanceRequests :: [Text] -- ^ 'csirSpotInstanceRequestIds'
                             -> CancelSpotInstanceRequests
mkCancelSpotInstanceRequests p1 = CancelSpotInstanceRequests
    { _csirSpotInstanceRequestIds = p1
    }

-- | One or more Spot Instance request IDs.
csirSpotInstanceRequestIds :: Lens' CancelSpotInstanceRequests [Text]
csirSpotInstanceRequestIds =
    lens _csirSpotInstanceRequestIds
         (\s a -> s { _csirSpotInstanceRequestIds = a })

instance ToQuery CancelSpotInstanceRequests where
    toQuery = genericQuery def

newtype CancelSpotInstanceRequestsResponse = CancelSpotInstanceRequestsResponse
    { _csirrCancelledSpotInstanceRequests :: [CancelledSpotInstanceRequest]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CancelSpotInstanceRequestsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CancelledSpotInstanceRequests ::@ @[CancelledSpotInstanceRequest]@
--
mkCancelSpotInstanceRequestsResponse :: CancelSpotInstanceRequestsResponse
mkCancelSpotInstanceRequestsResponse = CancelSpotInstanceRequestsResponse
    { _csirrCancelledSpotInstanceRequests = mempty
    }

-- | One or more Spot Instance requests.
csirrCancelledSpotInstanceRequests :: Lens' CancelSpotInstanceRequestsResponse [CancelledSpotInstanceRequest]
csirrCancelledSpotInstanceRequests =
    lens _csirrCancelledSpotInstanceRequests
         (\s a -> s { _csirrCancelledSpotInstanceRequests = a })

instance FromXML CancelSpotInstanceRequestsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CancelSpotInstanceRequests where
    type Sv CancelSpotInstanceRequests = EC2
    type Rs CancelSpotInstanceRequests = CancelSpotInstanceRequestsResponse

    request = post "CancelSpotInstanceRequests"
    response _ = xmlResponse
