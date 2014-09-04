{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CreateSpotDatafeedSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a datafeed for Spot Instances, enabling you to view Spot Instance
-- usage logs. You can create one data feed per AWS account. For more
-- information, see Spot Instances in the Amazon Elastic Compute Cloud User
-- Guide. Example This example creates a Spot Instance datafeed for the
-- account. https://ec2.amazonaws.com/?Action=CreateSpotDatafeedSubscription
-- &amp;Bucket=my-s3-bucket &amp;AUTHPARAMS
-- &lt;CreateSpotDatafeedSubscriptionResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;spotDatafeedSubscription&gt;
-- &lt;ownerId&gt;123456789012&lt;/ownerId&gt;
-- &lt;bucket&gt;my-s3-bucket&lt;/bucket&gt;
-- &lt;prefix&gt;spotdata_&lt;/prefix&gt; &lt;state&gt;Active&lt;/state&gt;
-- &lt;/spotDatafeedSubscription&gt;
-- &lt;/CreateSpotDatafeedSubscriptionResponse&gt;.
module Network.AWS.EC2.V2014_06_15.CreateSpotDatafeedSubscription
    (
    -- * Request
      CreateSpotDatafeedSubscription
    -- ** Request constructor
    , createSpotDatafeedSubscription
    -- ** Request lenses
    , csdsrBucket
    , csdsrPrefix

    -- * Response
    , CreateSpotDatafeedSubscriptionResponse
    -- ** Response lenses
    , csdssSpotDatafeedSubscription
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateSpotDatafeedSubscription' request.
createSpotDatafeedSubscription :: Text -- ^ 'csdsrBucket'
                               -> CreateSpotDatafeedSubscription
createSpotDatafeedSubscription p1 = CreateSpotDatafeedSubscription
    { _csdsrBucket = p1
    , _csdsrPrefix = Nothing
    }
{-# INLINE createSpotDatafeedSubscription #-}

data CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscription
    { _csdsrBucket :: Text
      -- ^ The Amazon S3 bucket in which to store the Spot Instance
      -- datafeed. Constraints: Must be a valid bucket associated with
      -- your AWS account.
    , _csdsrPrefix :: Maybe Text
      -- ^ A prefix for the datafeed file names.
    } deriving (Show, Generic)

-- | The Amazon S3 bucket in which to store the Spot Instance datafeed.
-- Constraints: Must be a valid bucket associated with your AWS account.
csdsrBucket :: Lens' CreateSpotDatafeedSubscription Text
csdsrBucket f x =
    f (_csdsrBucket x) <&> \y -> x { _csdsrBucket = y }
{-# INLINE csdsrBucket #-}

-- | A prefix for the datafeed file names.
csdsrPrefix :: Lens' CreateSpotDatafeedSubscription (Maybe Text)
csdsrPrefix f x =
    f (_csdsrPrefix x) <&> \y -> x { _csdsrPrefix = y }
{-# INLINE csdsrPrefix #-}

instance ToQuery CreateSpotDatafeedSubscription where
    toQuery = genericQuery def

data CreateSpotDatafeedSubscriptionResponse = CreateSpotDatafeedSubscriptionResponse
    { _csdssSpotDatafeedSubscription :: Maybe SpotDatafeedSubscription
      -- ^ The Spot Instance datafeed subscription.
    } deriving (Show, Generic)

-- | The Spot Instance datafeed subscription.
csdssSpotDatafeedSubscription :: Lens' CreateSpotDatafeedSubscriptionResponse (Maybe SpotDatafeedSubscription)
csdssSpotDatafeedSubscription f x =
    f (_csdssSpotDatafeedSubscription x) <&> \y -> x { _csdssSpotDatafeedSubscription = y }
{-# INLINE csdssSpotDatafeedSubscription #-}

instance FromXML CreateSpotDatafeedSubscriptionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateSpotDatafeedSubscription where
    type Sv CreateSpotDatafeedSubscription = EC2
    type Rs CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscriptionResponse

    request = post "CreateSpotDatafeedSubscription"
    response _ = xmlResponse
