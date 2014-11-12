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

-- Module      : Network.AWS.EC2.DescribeSpotDatafeedSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the datafeed for Spot Instances. For more information, see Spot
-- Instances in the Amazon Elastic Compute Cloud User Guide.
module Network.AWS.EC2.DescribeSpotDatafeedSubscription
    (
    -- * Request
      DescribeSpotDatafeedSubscription
    -- ** Request constructor
    , describeSpotDatafeedSubscription
    -- ** Request lenses
    , dsdsDryRun

    -- * Response
    , DescribeSpotDatafeedSubscriptionResult
    -- ** Response constructor
    , describeSpotDatafeedSubscriptionResult
    -- ** Response lenses
    , dsdsrSpotDatafeedSubscription
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

newtype DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription
    { _dsdsDryRun :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeSpotDatafeedSubscription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsdsDryRun' @::@ 'Maybe' 'Bool'
--
describeSpotDatafeedSubscription :: DescribeSpotDatafeedSubscription
describeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription
    { _dsdsDryRun = Nothing
    }

dsdsDryRun :: Lens' DescribeSpotDatafeedSubscription (Maybe Bool)
dsdsDryRun = lens _dsdsDryRun (\s a -> s { _dsdsDryRun = a })

instance ToQuery DescribeSpotDatafeedSubscription

instance ToPath DescribeSpotDatafeedSubscription where
    toPath = const "/"

newtype DescribeSpotDatafeedSubscriptionResult = DescribeSpotDatafeedSubscriptionResult
    { _dsdsrSpotDatafeedSubscription :: Maybe SpotDatafeedSubscription
    } deriving (Eq, Show, Generic)

-- | 'DescribeSpotDatafeedSubscriptionResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsdsrSpotDatafeedSubscription' @::@ 'Maybe' 'SpotDatafeedSubscription'
--
describeSpotDatafeedSubscriptionResult :: DescribeSpotDatafeedSubscriptionResult
describeSpotDatafeedSubscriptionResult = DescribeSpotDatafeedSubscriptionResult
    { _dsdsrSpotDatafeedSubscription = Nothing
    }

-- | The Spot Instance datafeed subscription.
dsdsrSpotDatafeedSubscription :: Lens' DescribeSpotDatafeedSubscriptionResult (Maybe SpotDatafeedSubscription)
dsdsrSpotDatafeedSubscription =
    lens _dsdsrSpotDatafeedSubscription
        (\s a -> s { _dsdsrSpotDatafeedSubscription = a })

instance FromXML DescribeSpotDatafeedSubscriptionResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeSpotDatafeedSubscriptionResult"

instance AWSRequest DescribeSpotDatafeedSubscription where
    type Sv DescribeSpotDatafeedSubscription = EC2
    type Rs DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscriptionResult

    request  = post "DescribeSpotDatafeedSubscription"
    response = xmlResponse $ \h x -> DescribeSpotDatafeedSubscriptionResult
        <$> x %| "spotDatafeedSubscription"
