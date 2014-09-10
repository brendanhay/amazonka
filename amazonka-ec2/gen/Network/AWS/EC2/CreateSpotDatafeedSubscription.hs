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
module Network.AWS.EC2
    (
    -- * Request
      CreateSpotDatafeedSubscription
    -- ** Request constructor
    , mkCreateSpotDatafeedSubscription
    -- ** Request lenses
    , csdsBucket
    , csdsPrefix

    -- * Response
    , CreateSpotDatafeedSubscriptionResponse
    -- ** Response constructor
    , mkCreateSpotDatafeedSubscriptionResponse
    -- ** Response lenses
    , csdsrSpotDatafeedSubscription
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscription
    { _csdsBucket :: Text
    , _csdsPrefix :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateSpotDatafeedSubscription' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Bucket ::@ @Text@
--
-- * @Prefix ::@ @Maybe Text@
--
mkCreateSpotDatafeedSubscription :: Text -- ^ 'csdsBucket'
                                 -> CreateSpotDatafeedSubscription
mkCreateSpotDatafeedSubscription p1 = CreateSpotDatafeedSubscription
    { _csdsBucket = p1
    , _csdsPrefix = Nothing
    }

-- | The Amazon S3 bucket in which to store the Spot Instance datafeed.
-- Constraints: Must be a valid bucket associated with your AWS account.
csdsBucket :: Lens' CreateSpotDatafeedSubscription Text
csdsBucket = lens _csdsBucket (\s a -> s { _csdsBucket = a })

-- | A prefix for the datafeed file names.
csdsPrefix :: Lens' CreateSpotDatafeedSubscription (Maybe Text)
csdsPrefix = lens _csdsPrefix (\s a -> s { _csdsPrefix = a })

instance ToQuery CreateSpotDatafeedSubscription where
    toQuery = genericQuery def

newtype CreateSpotDatafeedSubscriptionResponse = CreateSpotDatafeedSubscriptionResponse
    { _csdsrSpotDatafeedSubscription :: Maybe SpotDatafeedSubscription
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateSpotDatafeedSubscriptionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SpotDatafeedSubscription ::@ @Maybe SpotDatafeedSubscription@
--
mkCreateSpotDatafeedSubscriptionResponse :: CreateSpotDatafeedSubscriptionResponse
mkCreateSpotDatafeedSubscriptionResponse = CreateSpotDatafeedSubscriptionResponse
    { _csdsrSpotDatafeedSubscription = Nothing
    }

-- | The Spot Instance datafeed subscription.
csdsrSpotDatafeedSubscription :: Lens' CreateSpotDatafeedSubscriptionResponse (Maybe SpotDatafeedSubscription)
csdsrSpotDatafeedSubscription =
    lens _csdsrSpotDatafeedSubscription
         (\s a -> s { _csdsrSpotDatafeedSubscription = a })

instance FromXML CreateSpotDatafeedSubscriptionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateSpotDatafeedSubscription where
    type Sv CreateSpotDatafeedSubscription = EC2
    type Rs CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscriptionResponse

    request = post "CreateSpotDatafeedSubscription"
    response _ = xmlResponse
