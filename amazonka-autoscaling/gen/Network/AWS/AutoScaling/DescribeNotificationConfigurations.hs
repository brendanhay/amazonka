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

-- Module      : Network.AWS.AutoScaling.DescribeNotificationConfigurations
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the notification actions associated with the specified Auto
-- Scaling group.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeNotificationConfigurations.html>
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
    { _dncAutoScalingGroupNames :: List "AutoScalingGroupNames" Text
    , _dncMaxRecords            :: Maybe Int
    , _dncNextToken             :: Maybe Text
    } deriving (Eq, Ord, Show)

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

-- | The name of the group.
dncAutoScalingGroupNames :: Lens' DescribeNotificationConfigurations [Text]
dncAutoScalingGroupNames =
    lens _dncAutoScalingGroupNames
        (\s a -> s { _dncAutoScalingGroupNames = a })
            . _List

-- | The maximum number of items to return with this call.
dncMaxRecords :: Lens' DescribeNotificationConfigurations (Maybe Int)
dncMaxRecords = lens _dncMaxRecords (\s a -> s { _dncMaxRecords = a })

-- | The token for the next set of items to return. (You received this token
-- from a previous call.).
dncNextToken :: Lens' DescribeNotificationConfigurations (Maybe Text)
dncNextToken = lens _dncNextToken (\s a -> s { _dncNextToken = a })

data DescribeNotificationConfigurationsResponse = DescribeNotificationConfigurationsResponse
    { _dncrNextToken                  :: Maybe Text
    , _dncrNotificationConfigurations :: List "NotificationConfigurations" NotificationConfiguration
    } deriving (Eq, Show)

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

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dncrNextToken :: Lens' DescribeNotificationConfigurationsResponse (Maybe Text)
dncrNextToken = lens _dncrNextToken (\s a -> s { _dncrNextToken = a })

-- | The notification configurations.
dncrNotificationConfigurations :: Lens' DescribeNotificationConfigurationsResponse [NotificationConfiguration]
dncrNotificationConfigurations =
    lens _dncrNotificationConfigurations
        (\s a -> s { _dncrNotificationConfigurations = a })
            . _List

instance ToPath DescribeNotificationConfigurations where
    toPath = const "/"

instance ToQuery DescribeNotificationConfigurations where
    toQuery DescribeNotificationConfigurations{..} = mconcat
        [ "AutoScalingGroupNames" =? _dncAutoScalingGroupNames
        , "MaxRecords"            =? _dncMaxRecords
        , "NextToken"             =? _dncNextToken
        ]

instance ToHeaders DescribeNotificationConfigurations

instance AWSRequest DescribeNotificationConfigurations where
    type Sv DescribeNotificationConfigurations = AutoScaling
    type Rs DescribeNotificationConfigurations = DescribeNotificationConfigurationsResponse

    request  = post "DescribeNotificationConfigurations"
    response = xmlResponse

instance FromXML DescribeNotificationConfigurationsResponse where
    parseXML = withElement "DescribeNotificationConfigurationsResult" $ \x -> DescribeNotificationConfigurationsResponse
        <$> x .@? "NextToken"
        <*> x .@  "NotificationConfigurations"

instance AWSPager DescribeNotificationConfigurations where
    page rq rs
        | stop (rq ^. dncNextToken) = Nothing
        | otherwise = (\x -> rq & dncNextToken ?~ x)
            <$> (rs ^. dncrNextToken)
