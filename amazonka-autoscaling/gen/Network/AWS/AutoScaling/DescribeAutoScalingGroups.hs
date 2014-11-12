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
      AutoScalingGroupNamesType
    -- ** Request constructor
    , autoScalingGroupNamesType
    -- ** Request lenses
    , asgntAutoScalingGroupNames
    , asgntMaxRecords
    , asgntNextToken

    -- * Response
    , AutoScalingGroupsType
    -- ** Response constructor
    , autoScalingGroupsType
    -- ** Response lenses
    , asgtAutoScalingGroups
    , asgtNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data AutoScalingGroupNamesType = AutoScalingGroupNamesType
    { _asgntAutoScalingGroupNames :: [Text]
    , _asgntMaxRecords            :: Maybe Int
    , _asgntNextToken             :: Maybe Text
    } (Eq, Ord, Show, Generic)

-- | 'AutoScalingGroupNamesType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asgntAutoScalingGroupNames' @::@ ['Text']
--
-- * 'asgntMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'asgntNextToken' @::@ 'Maybe' 'Text'
--
autoScalingGroupNamesType :: AutoScalingGroupNamesType
autoScalingGroupNamesType = AutoScalingGroupNamesType
    { _asgntAutoScalingGroupNames = mempty
    , _asgntNextToken             = Nothing
    , _asgntMaxRecords            = Nothing
    }

-- | A list of Auto Scaling group names.
asgntAutoScalingGroupNames :: Lens' AutoScalingGroupNamesType [Text]
asgntAutoScalingGroupNames =
    lens _asgntAutoScalingGroupNames
        (\s a -> s { _asgntAutoScalingGroupNames = a })

-- | The maximum number of records to return.
asgntMaxRecords :: Lens' AutoScalingGroupNamesType (Maybe Int)
asgntMaxRecords = lens _asgntMaxRecords (\s a -> s { _asgntMaxRecords = a })

-- | A string that marks the start of the next batch of returned results.
asgntNextToken :: Lens' AutoScalingGroupNamesType (Maybe Text)
asgntNextToken = lens _asgntNextToken (\s a -> s { _asgntNextToken = a })
instance ToQuery AutoScalingGroupNamesType

instance ToPath AutoScalingGroupNamesType where
    toPath = const "/"

data AutoScalingGroupsType = AutoScalingGroupsType
    { _asgtAutoScalingGroups :: [AutoScalingGroup]
    , _asgtNextToken         :: Maybe Text
    } (Eq, Show, Generic)

-- | 'AutoScalingGroupsType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asgtAutoScalingGroups' @::@ ['AutoScalingGroup']
--
-- * 'asgtNextToken' @::@ 'Maybe' 'Text'
--
autoScalingGroupsType :: AutoScalingGroupsType
autoScalingGroupsType = AutoScalingGroupsType
    { _asgtAutoScalingGroups = mempty
    , _asgtNextToken         = Nothing
    }

-- | A list of Auto Scaling groups.
asgtAutoScalingGroups :: Lens' AutoScalingGroupsType [AutoScalingGroup]
asgtAutoScalingGroups =
    lens _asgtAutoScalingGroups (\s a -> s { _asgtAutoScalingGroups = a })

-- | A string that marks the start of the next batch of returned results.
asgtNextToken :: Lens' AutoScalingGroupsType (Maybe Text)
asgtNextToken = lens _asgtNextToken (\s a -> s { _asgtNextToken = a })

instance FromXML AutoScalingGroupsType where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "AutoScalingGroupsType"

instance AWSRequest AutoScalingGroupNamesType where
    type Sv AutoScalingGroupNamesType = AutoScaling
    type Rs AutoScalingGroupNamesType = AutoScalingGroupsType

    request  = post "DescribeAutoScalingGroups"
    response = xmlResponse $ \h x -> AutoScalingGroupsType
        <$> x %| "AutoScalingGroups"
        <*> x %| "NextToken"
