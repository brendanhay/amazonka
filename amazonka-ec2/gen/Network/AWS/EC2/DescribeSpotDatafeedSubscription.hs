{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
-- Instances in the Amazon Elastic Compute Cloud User Guide. Example This
-- example describes the datafeed for the account.
-- https://ec2.amazonaws.com/?Action=DescribeSpotDatafeedSubscription
-- &amp;AUTHPARAMS 59dbff89-35bd-4eac-99ed-be587EXAMPLE 123456789012
-- my-s3-bucket spotdata_ Active.
module Network.AWS.EC2.DescribeSpotDatafeedSubscription
    (
    -- * Request
      DescribeSpotDatafeedSubscription
    -- ** Request constructor
    , describeSpotDatafeedSubscription
    -- * Response
    , DescribeSpotDatafeedSubscriptionResponse
    -- ** Response constructor
    , describeSpotDatafeedSubscriptionResponse
    -- ** Response lenses
    , dsdsrSpotDatafeedSubscription
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeSpotDatafeedSubscription' request.
describeSpotDatafeedSubscription :: DescribeSpotDatafeedSubscription
describeSpotDatafeedSubscription = DescribeSpotDatafeedSubscription

instance ToQuery DescribeSpotDatafeedSubscription where
    toQuery = genericQuery def

newtype DescribeSpotDatafeedSubscriptionResponse = DescribeSpotDatafeedSubscriptionResponse
    { _dsdsrSpotDatafeedSubscription :: Maybe SpotDatafeedSubscription
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeSpotDatafeedSubscriptionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SpotDatafeedSubscription ::@ @Maybe SpotDatafeedSubscription@
--
describeSpotDatafeedSubscriptionResponse :: DescribeSpotDatafeedSubscriptionResponse
describeSpotDatafeedSubscriptionResponse = DescribeSpotDatafeedSubscriptionResponse
    { _dsdsrSpotDatafeedSubscription = Nothing
    }

-- | The Spot Instance datafeed subscription.
dsdsrSpotDatafeedSubscription :: Lens' DescribeSpotDatafeedSubscriptionResponse (Maybe SpotDatafeedSubscription)
dsdsrSpotDatafeedSubscription =
    lens _dsdsrSpotDatafeedSubscription
         (\s a -> s { _dsdsrSpotDatafeedSubscription = a })

instance FromXML DescribeSpotDatafeedSubscriptionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeSpotDatafeedSubscription where
    type Sv DescribeSpotDatafeedSubscription = EC2
    type Rs DescribeSpotDatafeedSubscription = DescribeSpotDatafeedSubscriptionResponse

    request = post "DescribeSpotDatafeedSubscription"
    response _ = xmlResponse
