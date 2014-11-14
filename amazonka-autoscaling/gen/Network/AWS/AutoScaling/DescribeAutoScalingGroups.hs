{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a full description of each Auto Scaling group in the given list.
-- This includes all Amazon EC2 instances that are members of the group. If a
-- list of names is not provided, the service returns the full details of all
-- Auto Scaling groups. This action supports pagination by returning a token
-- if there are more pages to retrieve. To get the next page, call this action
-- again with the returned token as the NextToken parameter.
module Network.AWS.AutoScaling.DescribeAutoScalingGroups
    (
    -- * Request
      DescribeAutoScalingGroups
    -- ** Request constructor
    , describeAutoScalingGroups
    -- ** Request lenses
    , dasgAutoScalingGroupNames
    , dasgMaxRecords
    , dasgNextToken

    -- * Response
    , DescribeAutoScalingGroupsResponse
    -- ** Response constructor
    , describeAutoScalingGroupsResponse
    -- ** Response lenses
    , dasgrAutoScalingGroups
    , dasgrNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data DescribeAutoScalingGroups = DescribeAutoScalingGroups
    { _dasgAutoScalingGroupNames :: [Text]
    , _dasgMaxRecords            :: Maybe Int
    , _dasgNextToken             :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeAutoScalingGroups' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasgAutoScalingGroupNames' @::@ ['Text']
--
-- * 'dasgMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dasgNextToken' @::@ 'Maybe' 'Text'
--
describeAutoScalingGroups :: DescribeAutoScalingGroups
describeAutoScalingGroups = DescribeAutoScalingGroups
    { _dasgAutoScalingGroupNames = mempty
    , _dasgNextToken             = Nothing
    , _dasgMaxRecords            = Nothing
    }

-- | A list of Auto Scaling group names.
dasgAutoScalingGroupNames :: Lens' DescribeAutoScalingGroups [Text]
dasgAutoScalingGroupNames =
    lens _dasgAutoScalingGroupNames
        (\s a -> s { _dasgAutoScalingGroupNames = a })

-- | The maximum number of records to return.
dasgMaxRecords :: Lens' DescribeAutoScalingGroups (Maybe Int)
dasgMaxRecords = lens _dasgMaxRecords (\s a -> s { _dasgMaxRecords = a })

-- | A string that marks the start of the next batch of returned results.
dasgNextToken :: Lens' DescribeAutoScalingGroups (Maybe Text)
dasgNextToken = lens _dasgNextToken (\s a -> s { _dasgNextToken = a })

instance ToQuery DescribeAutoScalingGroups

instance ToPath DescribeAutoScalingGroups where
    toPath = const "/"

data DescribeAutoScalingGroupsResponse = DescribeAutoScalingGroupsResponse
    { _dasgrAutoScalingGroups :: [AutoScalingGroup]
    , _dasgrNextToken         :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'DescribeAutoScalingGroupsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasgrAutoScalingGroups' @::@ ['AutoScalingGroup']
--
-- * 'dasgrNextToken' @::@ 'Maybe' 'Text'
--
describeAutoScalingGroupsResponse :: DescribeAutoScalingGroupsResponse
describeAutoScalingGroupsResponse = DescribeAutoScalingGroupsResponse
    { _dasgrAutoScalingGroups = mempty
    , _dasgrNextToken         = Nothing
    }

-- | A list of Auto Scaling groups.
dasgrAutoScalingGroups :: Lens' DescribeAutoScalingGroupsResponse [AutoScalingGroup]
dasgrAutoScalingGroups =
    lens _dasgrAutoScalingGroups (\s a -> s { _dasgrAutoScalingGroups = a })

-- | A string that marks the start of the next batch of returned results.
dasgrNextToken :: Lens' DescribeAutoScalingGroupsResponse (Maybe Text)
dasgrNextToken = lens _dasgrNextToken (\s a -> s { _dasgrNextToken = a })

instance AWSRequest DescribeAutoScalingGroups where
    type Sv DescribeAutoScalingGroups = AutoScaling
    type Rs DescribeAutoScalingGroups = DescribeAutoScalingGroupsResponse

    request  = post "DescribeAutoScalingGroups"
    response = xmlResponse $ \h x -> DescribeAutoScalingGroupsResponse
        <$> x %| "AutoScalingGroups"
        <*> x %| "NextToken"
