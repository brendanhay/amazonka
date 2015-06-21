{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.DescribeNotificationConfigurations
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

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
    , dncNextToken
    , dncMaxRecords

    -- * Response
    , DescribeNotificationConfigurationsResponse
    -- ** Response constructor
    , describeNotificationConfigurationsResponse
    -- ** Response lenses
    , dncrNextToken
    , dncrNotificationConfigurations
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.Pagers
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeNotificationConfigurations' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dncAutoScalingGroupNames'
--
-- * 'dncNextToken'
--
-- * 'dncMaxRecords'
data DescribeNotificationConfigurations = DescribeNotificationConfigurations'{_dncAutoScalingGroupNames :: Maybe [Text], _dncNextToken :: Maybe Text, _dncMaxRecords :: Maybe Int} deriving (Eq, Read, Show)

-- | 'DescribeNotificationConfigurations' smart constructor.
describeNotificationConfigurations :: DescribeNotificationConfigurations
describeNotificationConfigurations = DescribeNotificationConfigurations'{_dncAutoScalingGroupNames = Nothing, _dncNextToken = Nothing, _dncMaxRecords = Nothing};

-- | The name of the group.
dncAutoScalingGroupNames :: Lens' DescribeNotificationConfigurations [Text]
dncAutoScalingGroupNames = lens _dncAutoScalingGroupNames (\ s a -> s{_dncAutoScalingGroupNames = a}) . _Default;

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
dncNextToken :: Lens' DescribeNotificationConfigurations (Maybe Text)
dncNextToken = lens _dncNextToken (\ s a -> s{_dncNextToken = a});

-- | The maximum number of items to return with this call.
dncMaxRecords :: Lens' DescribeNotificationConfigurations (Maybe Int)
dncMaxRecords = lens _dncMaxRecords (\ s a -> s{_dncMaxRecords = a});

instance AWSPager DescribeNotificationConfigurations
         where
        page rq rs
          | stop (rs ^. dncrNextToken) = Nothing
          | otherwise =
            rq & dncNextToken ?~ rs ^. dncrNextToken

instance AWSRequest
         DescribeNotificationConfigurations where
        type Sv DescribeNotificationConfigurations =
             AutoScaling
        type Rs DescribeNotificationConfigurations =
             DescribeNotificationConfigurationsResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeNotificationConfigurationsResult"
              (\ s h x ->
                 DescribeNotificationConfigurationsResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "NotificationConfigurations" .!@ mempty >>=
                        parseXMLList "member"))

instance ToHeaders DescribeNotificationConfigurations
         where
        toHeaders = const mempty

instance ToPath DescribeNotificationConfigurations
         where
        toPath = const "/"

instance ToQuery DescribeNotificationConfigurations
         where
        toQuery DescribeNotificationConfigurations'{..}
          = mconcat
              ["Action" =:
                 ("DescribeNotificationConfigurations" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "AutoScalingGroupNames" =:
                 toQuery
                   (toQueryList "member" <$> _dncAutoScalingGroupNames),
               "NextToken" =: _dncNextToken,
               "MaxRecords" =: _dncMaxRecords]

-- | /See:/ 'describeNotificationConfigurationsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dncrNextToken'
--
-- * 'dncrNotificationConfigurations'
data DescribeNotificationConfigurationsResponse = DescribeNotificationConfigurationsResponse'{_dncrNextToken :: Maybe Text, _dncrNotificationConfigurations :: [NotificationConfiguration]} deriving (Eq, Read, Show)

-- | 'DescribeNotificationConfigurationsResponse' smart constructor.
describeNotificationConfigurationsResponse :: DescribeNotificationConfigurationsResponse
describeNotificationConfigurationsResponse = DescribeNotificationConfigurationsResponse'{_dncrNextToken = Nothing, _dncrNotificationConfigurations = mempty};

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dncrNextToken :: Lens' DescribeNotificationConfigurationsResponse (Maybe Text)
dncrNextToken = lens _dncrNextToken (\ s a -> s{_dncrNextToken = a});

-- | The notification configurations.
dncrNotificationConfigurations :: Lens' DescribeNotificationConfigurationsResponse [NotificationConfiguration]
dncrNotificationConfigurations = lens _dncrNotificationConfigurations (\ s a -> s{_dncrNotificationConfigurations = a});
