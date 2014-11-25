{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingGroups
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes one or more Auto Scaling groups. If a list of names is not
-- provided, the call describes all Auto Scaling groups.
--
-- You can specify a maximum number of items to be returned with a single call.
-- If there are more items to return, the call returns a token. To get the next
-- set of items, repeat the call with the returned token in the 'NextToken'
-- parameter.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAutoScalingGroups.html>
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
    { _dasgAutoScalingGroupNames :: List "AutoScalingGroupNames" Text
    , _dasgMaxRecords            :: Maybe Int
    , _dasgNextToken             :: Maybe Text
    } deriving (Eq, Ord, Show)

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

-- | The group names.
dasgAutoScalingGroupNames :: Lens' DescribeAutoScalingGroups [Text]
dasgAutoScalingGroupNames =
    lens _dasgAutoScalingGroupNames
        (\s a -> s { _dasgAutoScalingGroupNames = a })
            . _List

-- | The maximum number of items to return with this call.
dasgMaxRecords :: Lens' DescribeAutoScalingGroups (Maybe Int)
dasgMaxRecords = lens _dasgMaxRecords (\s a -> s { _dasgMaxRecords = a })

-- | The token for the next set of items to return. (You received this token from
-- a previous call.)
dasgNextToken :: Lens' DescribeAutoScalingGroups (Maybe Text)
dasgNextToken = lens _dasgNextToken (\s a -> s { _dasgNextToken = a })

data DescribeAutoScalingGroupsResponse = DescribeAutoScalingGroupsResponse
    { _dasgrAutoScalingGroups :: List "AutoScalingGroups" AutoScalingGroup
    , _dasgrNextToken         :: Maybe Text
    } deriving (Eq, Show)

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

-- | The groups.
dasgrAutoScalingGroups :: Lens' DescribeAutoScalingGroupsResponse [AutoScalingGroup]
dasgrAutoScalingGroups =
    lens _dasgrAutoScalingGroups (\s a -> s { _dasgrAutoScalingGroups = a })
        . _List

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dasgrNextToken :: Lens' DescribeAutoScalingGroupsResponse (Maybe Text)
dasgrNextToken = lens _dasgrNextToken (\s a -> s { _dasgrNextToken = a })

instance ToPath DescribeAutoScalingGroups where
    toPath = const "/"

instance ToQuery DescribeAutoScalingGroups where
    toQuery DescribeAutoScalingGroups{..} = mconcat
        [ "AutoScalingGroupNames" =? _dasgAutoScalingGroupNames
        , "MaxRecords"            =? _dasgMaxRecords
        , "NextToken"             =? _dasgNextToken
        ]

instance ToHeaders DescribeAutoScalingGroups

instance AWSRequest DescribeAutoScalingGroups where
    type Sv DescribeAutoScalingGroups = AutoScaling
    type Rs DescribeAutoScalingGroups = DescribeAutoScalingGroupsResponse

    request  = post "DescribeAutoScalingGroups"
    response = xmlResponse

instance FromXML DescribeAutoScalingGroupsResponse where
    parseXML = withElement "DescribeAutoScalingGroupsResult" $ \x -> DescribeAutoScalingGroupsResponse
        <$> x .@  "AutoScalingGroups"
        <*> x .@? "NextToken"

instance AWSPager DescribeAutoScalingGroups where
    page rq rs
        | stop (rq ^. dasgNextToken) = Nothing
        | otherwise = (\x -> rq & dasgNextToken ?~ x)
            <$> (rs ^. dasgrNextToken)
