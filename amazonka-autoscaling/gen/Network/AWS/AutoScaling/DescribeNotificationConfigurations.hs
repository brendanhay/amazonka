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
      DescribeNotificationConfigurationsType
    -- ** Request constructor
    , describeNotificationConfigurationsType
    -- ** Request lenses
    , dnctAutoScalingGroupNames
    , dnctMaxRecords
    , dnctNextToken

    -- * Response
    , DescribeNotificationConfigurationsAnswer
    -- ** Response constructor
    , describeNotificationConfigurationsAnswer
    -- ** Response lenses
    , dncaNextToken
    , dncaNotificationConfigurations
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data DescribeNotificationConfigurationsType = DescribeNotificationConfigurationsType
    { _dnctAutoScalingGroupNames :: [Text]
    , _dnctMaxRecords            :: Maybe Int
    , _dnctNextToken             :: Maybe Text
    } (Eq, Ord, Show, Generic)

-- | 'DescribeNotificationConfigurationsType' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dnctAutoScalingGroupNames' @::@ ['Text']
--
-- * 'dnctMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dnctNextToken' @::@ 'Maybe' 'Text'
--
describeNotificationConfigurationsType :: DescribeNotificationConfigurationsType
describeNotificationConfigurationsType = DescribeNotificationConfigurationsType
    { _dnctAutoScalingGroupNames = mempty
    , _dnctNextToken             = Nothing
    , _dnctMaxRecords            = Nothing
    }

-- | The name of the Auto Scaling group.
dnctAutoScalingGroupNames :: Lens' DescribeNotificationConfigurationsType [Text]
dnctAutoScalingGroupNames =
    lens _dnctAutoScalingGroupNames
        (\s a -> s { _dnctAutoScalingGroupNames = a })

-- | Maximum number of records to be returned.
dnctMaxRecords :: Lens' DescribeNotificationConfigurationsType (Maybe Int)
dnctMaxRecords = lens _dnctMaxRecords (\s a -> s { _dnctMaxRecords = a })

-- | A string that is used to mark the start of the next batch of returned
-- results for pagination.
dnctNextToken :: Lens' DescribeNotificationConfigurationsType (Maybe Text)
dnctNextToken = lens _dnctNextToken (\s a -> s { _dnctNextToken = a })
instance ToQuery DescribeNotificationConfigurationsType

instance ToPath DescribeNotificationConfigurationsType where
    toPath = const "/"

data DescribeNotificationConfigurationsAnswer = DescribeNotificationConfigurationsAnswer
    { _dncaNextToken                  :: Maybe Text
    , _dncaNotificationConfigurations :: [NotificationConfiguration]
    } (Eq, Show, Generic)

-- | 'DescribeNotificationConfigurationsAnswer' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dncaNextToken' @::@ 'Maybe' 'Text'
--
-- * 'dncaNotificationConfigurations' @::@ ['NotificationConfiguration']
--
describeNotificationConfigurationsAnswer :: DescribeNotificationConfigurationsAnswer
describeNotificationConfigurationsAnswer = DescribeNotificationConfigurationsAnswer
    { _dncaNotificationConfigurations = mempty
    , _dncaNextToken                  = Nothing
    }

-- | A string that is used to mark the start of the next batch of returned
-- results for pagination.
dncaNextToken :: Lens' DescribeNotificationConfigurationsAnswer (Maybe Text)
dncaNextToken = lens _dncaNextToken (\s a -> s { _dncaNextToken = a })

-- | The list of notification configurations.
dncaNotificationConfigurations :: Lens' DescribeNotificationConfigurationsAnswer [NotificationConfiguration]
dncaNotificationConfigurations =
    lens _dncaNotificationConfigurations
        (\s a -> s { _dncaNotificationConfigurations = a })

instance FromXML DescribeNotificationConfigurationsAnswer where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeNotificationConfigurationsAnswer"

instance AWSRequest DescribeNotificationConfigurationsType where
    type Sv DescribeNotificationConfigurationsType = AutoScaling
    type Rs DescribeNotificationConfigurationsType = DescribeNotificationConfigurationsAnswer

    request  = post "DescribeNotificationConfigurations"
    response = xmlResponse $ \h x -> DescribeNotificationConfigurationsAnswer
        <$> x %| "NextToken"
        <*> x %| "NotificationConfigurations"
