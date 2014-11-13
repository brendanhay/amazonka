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

-- Module      : Network.AWS.AutoScaling.DescribeNotificationConfigurations
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of notification actions associated with Auto Scaling groups
-- for specified events.
module Network.AWS.AutoScaling.DescribeNotificationConfigurations
    (
    -- * Request
      DescribeNotificationConfigurations
    -- ** Request constructor
    , describeNotificationConfigurations
    -- ** Request lenses
    , dncAutoScalingGroupNames
    , dncMaxRecords
    , dncNextToken

    -- * Response
    , DescribeNotificationConfigurationsResponse
    -- ** Response constructor
    , describeNotificationConfigurationsResponse
    -- ** Response lenses
    , dncrNextToken
    , dncrNotificationConfigurations
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data DescribeNotificationConfigurations = DescribeNotificationConfigurations
    { _dncAutoScalingGroupNames :: [Text]
    , _dncMaxRecords            :: Maybe Int
    , _dncNextToken             :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeNotificationConfigurations' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dncAutoScalingGroupNames' @::@ ['Text']
--
-- * 'dncMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dncNextToken' @::@ 'Maybe' 'Text'
--
describeNotificationConfigurations :: DescribeNotificationConfigurations
describeNotificationConfigurations = DescribeNotificationConfigurations
    { _dncAutoScalingGroupNames = mempty
    , _dncNextToken             = Nothing
    , _dncMaxRecords            = Nothing
    }

-- | The name of the Auto Scaling group.
dncAutoScalingGroupNames :: Lens' DescribeNotificationConfigurations [Text]
dncAutoScalingGroupNames =
    lens _dncAutoScalingGroupNames
        (\s a -> s { _dncAutoScalingGroupNames = a })

-- | Maximum number of records to be returned.
dncMaxRecords :: Lens' DescribeNotificationConfigurations (Maybe Int)
dncMaxRecords = lens _dncMaxRecords (\s a -> s { _dncMaxRecords = a })

-- | A string that is used to mark the start of the next batch of returned
-- results for pagination.
dncNextToken :: Lens' DescribeNotificationConfigurations (Maybe Text)
dncNextToken = lens _dncNextToken (\s a -> s { _dncNextToken = a })

instance ToQuery DescribeNotificationConfigurations

instance ToPath DescribeNotificationConfigurations where
    toPath = const "/"

data DescribeNotificationConfigurationsResponse = DescribeNotificationConfigurationsResponse
    { _dncrNextToken                  :: Maybe Text
    , _dncrNotificationConfigurations :: [NotificationConfiguration]
    } deriving (Eq, Show, Generic)

-- | 'DescribeNotificationConfigurationsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dncrNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dncrNotificationConfigurations' @::@ ['NotificationConfiguration']
--
describeNotificationConfigurationsResponse :: DescribeNotificationConfigurationsResponse
describeNotificationConfigurationsResponse = DescribeNotificationConfigurationsResponse
    { _dncrNotificationConfigurations = mempty
    , _dncrNextToken                  = Nothing
    }

-- | A string that is used to mark the start of the next batch of returned
-- results for pagination.
dncrNextToken :: Lens' DescribeNotificationConfigurationsResponse (Maybe Text)
dncrNextToken = lens _dncrNextToken (\s a -> s { _dncrNextToken = a })

-- | The list of notification configurations.
dncrNotificationConfigurations :: Lens' DescribeNotificationConfigurationsResponse [NotificationConfiguration]
dncrNotificationConfigurations =
    lens _dncrNotificationConfigurations
        (\s a -> s { _dncrNotificationConfigurations = a })

instance AWSRequest DescribeNotificationConfigurations where
    type Sv DescribeNotificationConfigurations = AutoScaling
    type Rs DescribeNotificationConfigurations = DescribeNotificationConfigurationsResponse

    request  = post "DescribeNotificationConfigurations"
    response = xmlResponse $ \h x -> DescribeNotificationConfigurationsResponse
        <$> x %| "NextToken"
        <*> x %| "NotificationConfigurations"
