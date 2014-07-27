{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.CancelSpotInstanceRequests
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
module Network.AWS.EC2.V2014_05_01.CancelSpotInstanceRequests where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Region, Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data CancelSpotInstanceRequests = CancelSpotInstanceRequests
    { _csirrSpotInstanceRequestIds :: [Text]
      -- ^ One or more Spot Instance request IDs.
    , _csirrDryRun :: Maybe Bool
      -- ^ 
    } deriving (Generic)

instance ToQuery CancelSpotInstanceRequests where
    toQuery = genericToQuery def

instance AWSRequest CancelSpotInstanceRequests where
    type Sv CancelSpotInstanceRequests = EC2
    type Rs CancelSpotInstanceRequests = CancelSpotInstanceRequestsResponse

    request = post "CancelSpotInstanceRequests"
    response _ = xmlResponse

data CancelSpotInstanceRequestsResponse = CancelSpotInstanceRequestsResponse
    { _csirsCancelledSpotInstanceRequests :: [CancelledSpotInstanceRequest]
      -- ^ One or more Spot Instance requests.
    } deriving (Generic)

instance FromXML CancelSpotInstanceRequestsResponse where
    fromXMLOptions = xmlOptions
