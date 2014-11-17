{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingInstances
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
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAutoScalingInstances.html>
module Network.AWS.AutoScaling.DescribeAutoScalingInstances
    (
    -- * Request
      DescribeAutoScalingInstances
    -- ** Request constructor
    , describeAutoScalingInstances
    -- ** Request lenses
    , dasiInstanceIds
    , dasiMaxRecords
    , dasiNextToken

    -- * Response
    , DescribeAutoScalingInstancesResponse
    -- ** Response constructor
    , describeAutoScalingInstancesResponse
    -- ** Response lenses
    , dasirAutoScalingInstances
    , dasirNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data DescribeAutoScalingInstances = DescribeAutoScalingInstances
    { _dasiInstanceIds :: [Text]
    , _dasiMaxRecords  :: Maybe Int
    , _dasiNextToken   :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeAutoScalingInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasiInstanceIds' @::@ ['Text']
--
-- * 'dasiMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dasiNextToken' @::@ 'Maybe' 'Text'
--
describeAutoScalingInstances :: DescribeAutoScalingInstances
describeAutoScalingInstances = DescribeAutoScalingInstances
    { _dasiInstanceIds = mempty
    , _dasiMaxRecords  = Nothing
    , _dasiNextToken   = Nothing
    }

-- | The list of Auto Scaling instances to describe. If this list is omitted,
-- all auto scaling instances are described. The list of requested instances
-- cannot contain more than 50 items. If unknown instances are requested,
-- they are ignored with no error.
dasiInstanceIds :: Lens' DescribeAutoScalingInstances [Text]
dasiInstanceIds = lens _dasiInstanceIds (\s a -> s { _dasiInstanceIds = a })

-- | The maximum number of Auto Scaling instances to be described with each
-- call.
dasiMaxRecords :: Lens' DescribeAutoScalingInstances (Maybe Int)
dasiMaxRecords = lens _dasiMaxRecords (\s a -> s { _dasiMaxRecords = a })

-- | The token returned by a previous call to indicate that there is more data
-- available.
dasiNextToken :: Lens' DescribeAutoScalingInstances (Maybe Text)
dasiNextToken = lens _dasiNextToken (\s a -> s { _dasiNextToken = a })

data DescribeAutoScalingInstancesResponse = DescribeAutoScalingInstancesResponse
    { _dasirAutoScalingInstances :: [AutoScalingInstanceDetails]
    , _dasirNextToken            :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeAutoScalingInstancesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasirAutoScalingInstances' @::@ ['AutoScalingInstanceDetails']
--
-- * 'dasirNextToken' @::@ 'Maybe' 'Text'
--
describeAutoScalingInstancesResponse :: DescribeAutoScalingInstancesResponse
describeAutoScalingInstancesResponse = DescribeAutoScalingInstancesResponse
    { _dasirAutoScalingInstances = mempty
    , _dasirNextToken            = Nothing
    }

-- | A list of Auto Scaling instances.
dasirAutoScalingInstances :: Lens' DescribeAutoScalingInstancesResponse [AutoScalingInstanceDetails]
dasirAutoScalingInstances =
    lens _dasirAutoScalingInstances
        (\s a -> s { _dasirAutoScalingInstances = a })

-- | A string that marks the start of the next batch of returned results.
dasirNextToken :: Lens' DescribeAutoScalingInstancesResponse (Maybe Text)
dasirNextToken = lens _dasirNextToken (\s a -> s { _dasirNextToken = a })

instance ToPath DescribeAutoScalingInstances where
    toPath = const "/"

instance ToQuery DescribeAutoScalingInstances

instance ToHeaders DescribeAutoScalingInstances

instance AWSRequest DescribeAutoScalingInstances where
    type Sv DescribeAutoScalingInstances = AutoScaling
    type Rs DescribeAutoScalingInstances = DescribeAutoScalingInstancesResponse

    request  = post "DescribeAutoScalingInstances"
    response = xmlResponse

instance FromXML DescribeAutoScalingInstancesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeAutoScalingInstancesResponse"
