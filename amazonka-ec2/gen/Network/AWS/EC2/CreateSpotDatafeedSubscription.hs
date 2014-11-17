{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateSpotDatafeedSubscription
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
-- Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateSpotDatafeedSubscription.html>
module Network.AWS.EC2.CreateSpotDatafeedSubscription
    (
    -- * Request
      CreateSpotDatafeedSubscription
    -- ** Request constructor
    , createSpotDatafeedSubscription
    -- ** Request lenses
    , csdsBucket
    , csdsDryRun
    , csdsPrefix

    -- * Response
    , CreateSpotDatafeedSubscriptionResponse
    -- ** Response constructor
    , createSpotDatafeedSubscriptionResponse
    -- ** Response lenses
    , csdsrSpotDatafeedSubscription
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscription
    { _csdsBucket :: Text
    , _csdsDryRun :: Maybe Bool
    , _csdsPrefix :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateSpotDatafeedSubscription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csdsBucket' @::@ 'Text'
--
-- * 'csdsDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'csdsPrefix' @::@ 'Maybe' 'Text'
--
createSpotDatafeedSubscription :: Text -- ^ 'csdsBucket'
                               -> CreateSpotDatafeedSubscription
createSpotDatafeedSubscription p1 = CreateSpotDatafeedSubscription
    { _csdsBucket = p1
    , _csdsDryRun = Nothing
    , _csdsPrefix = Nothing
    }

-- | The Amazon S3 bucket in which to store the Spot Instance datafeed.
-- Constraints: Must be a valid bucket associated with your AWS account.
csdsBucket :: Lens' CreateSpotDatafeedSubscription Text
csdsBucket = lens _csdsBucket (\s a -> s { _csdsBucket = a })

csdsDryRun :: Lens' CreateSpotDatafeedSubscription (Maybe Bool)
csdsDryRun = lens _csdsDryRun (\s a -> s { _csdsDryRun = a })

-- | A prefix for the datafeed file names.
csdsPrefix :: Lens' CreateSpotDatafeedSubscription (Maybe Text)
csdsPrefix = lens _csdsPrefix (\s a -> s { _csdsPrefix = a })

newtype CreateSpotDatafeedSubscriptionResponse = CreateSpotDatafeedSubscriptionResponse
    { _csdsrSpotDatafeedSubscription :: Maybe SpotDatafeedSubscription
    } deriving (Eq, Show, Generic)

-- | 'CreateSpotDatafeedSubscriptionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csdsrSpotDatafeedSubscription' @::@ 'Maybe' 'SpotDatafeedSubscription'
--
createSpotDatafeedSubscriptionResponse :: CreateSpotDatafeedSubscriptionResponse
createSpotDatafeedSubscriptionResponse = CreateSpotDatafeedSubscriptionResponse
    { _csdsrSpotDatafeedSubscription = Nothing
    }

-- | The Spot Instance datafeed subscription.
csdsrSpotDatafeedSubscription :: Lens' CreateSpotDatafeedSubscriptionResponse (Maybe SpotDatafeedSubscription)
csdsrSpotDatafeedSubscription =
    lens _csdsrSpotDatafeedSubscription
        (\s a -> s { _csdsrSpotDatafeedSubscription = a })

instance AWSRequest CreateSpotDatafeedSubscription where
    type Sv CreateSpotDatafeedSubscription = EC2
    type Rs CreateSpotDatafeedSubscription = CreateSpotDatafeedSubscriptionResponse

    request  = post "CreateSpotDatafeedSubscription"
    response = xmlResponse

instance FromXML CreateSpotDatafeedSubscriptionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateSpotDatafeedSubscriptionResponse"

instance ToPath CreateSpotDatafeedSubscription where
    toPath = const "/"

instance ToHeaders CreateSpotDatafeedSubscription

instance ToQuery CreateSpotDatafeedSubscription
