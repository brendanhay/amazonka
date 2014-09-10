{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a description of each Auto Scaling instance in the InstanceIds
-- list. If a list is not provided, the service returns the full details of
-- all instances up to a maximum of 50. By default, the service returns a list
-- of 20 items. This action supports pagination by returning a token if there
-- are more pages to retrieve. To get the next page, call this action again
-- with the returned token as the NextToken parameter.
-- https://autoscaling.amazonaws.com/?MaxRecords=20
-- &InstanceIds.member.1=i-78e0d40b &Version=2011-01-01
-- &Action=DescribeAutoScalingInstances &AUTHPARAMS Healthy my-test-asg
-- us-east-1e i-78e0d40b my-test-lc InService
-- df992dc3-b72f-11e2-81e1-750aa6EXAMPLE.
module Network.AWS.AutoScaling
    (
    -- * Request
      DescribeAutoScalingInstances
    -- ** Request constructor
    , mkDescribeAutoScalingInstances
    -- ** Request lenses
    , dasiInstanceIds
    , dasiMaxRecords
    , dasiNextToken

    -- * Response
    , DescribeAutoScalingInstancesResponse
    -- ** Response constructor
    , mkDescribeAutoScalingInstancesResponse
    -- ** Response lenses
    , dasirAutoScalingInstances
    , dasirNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

data DescribeAutoScalingInstances = DescribeAutoScalingInstances
    { _dasiInstanceIds :: [Text]
    , _dasiMaxRecords :: Maybe Integer
    , _dasiNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAutoScalingInstances' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InstanceIds ::@ @[Text]@
--
-- * @MaxRecords ::@ @Maybe Integer@
--
-- * @NextToken ::@ @Maybe Text@
--
mkDescribeAutoScalingInstances :: DescribeAutoScalingInstances
mkDescribeAutoScalingInstances = DescribeAutoScalingInstances
    { _dasiInstanceIds = mempty
    , _dasiMaxRecords = Nothing
    , _dasiNextToken = Nothing
    }

-- | The list of Auto Scaling instances to describe. If this list is omitted,
-- all auto scaling instances are described. The list of requested instances
-- cannot contain more than 50 items. If unknown instances are requested, they
-- are ignored with no error.
dasiInstanceIds :: Lens' DescribeAutoScalingInstances [Text]
dasiInstanceIds = lens _dasiInstanceIds (\s a -> s { _dasiInstanceIds = a })

-- | The maximum number of Auto Scaling instances to be described with each
-- call.
dasiMaxRecords :: Lens' DescribeAutoScalingInstances (Maybe Integer)
dasiMaxRecords = lens _dasiMaxRecords (\s a -> s { _dasiMaxRecords = a })

-- | The token returned by a previous call to indicate that there is more data
-- available.
dasiNextToken :: Lens' DescribeAutoScalingInstances (Maybe Text)
dasiNextToken = lens _dasiNextToken (\s a -> s { _dasiNextToken = a })

instance ToQuery DescribeAutoScalingInstances where
    toQuery = genericQuery def

-- | The AutoScalingInstancesType data type.
data DescribeAutoScalingInstancesResponse = DescribeAutoScalingInstancesResponse
    { _dasirAutoScalingInstances :: [AutoScalingInstanceDetails]
    , _dasirNextToken :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAutoScalingInstancesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AutoScalingInstances ::@ @[AutoScalingInstanceDetails]@
--
-- * @NextToken ::@ @Maybe Text@
--
mkDescribeAutoScalingInstancesResponse :: DescribeAutoScalingInstancesResponse
mkDescribeAutoScalingInstancesResponse = DescribeAutoScalingInstancesResponse
    { _dasirAutoScalingInstances = mempty
    , _dasirNextToken = Nothing
    }

-- | A list of Auto Scaling instances.
dasirAutoScalingInstances :: Lens' DescribeAutoScalingInstancesResponse [AutoScalingInstanceDetails]
dasirAutoScalingInstances =
    lens _dasirAutoScalingInstances
         (\s a -> s { _dasirAutoScalingInstances = a })

-- | A string that marks the start of the next batch of returned results.
dasirNextToken :: Lens' DescribeAutoScalingInstancesResponse (Maybe Text)
dasirNextToken = lens _dasirNextToken (\s a -> s { _dasirNextToken = a })

instance FromXML DescribeAutoScalingInstancesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAutoScalingInstances where
    type Sv DescribeAutoScalingInstances = AutoScaling
    type Rs DescribeAutoScalingInstances = DescribeAutoScalingInstancesResponse

    request = post "DescribeAutoScalingInstances"
    response _ = xmlResponse

instance AWSPager DescribeAutoScalingInstances where
    next rq rs = (\x -> rq & dasiNextToken ?~ x)
        <$> (rs ^. dasirNextToken)
