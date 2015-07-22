{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeNotificationConfigurations
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the notification actions associated with the specified Auto
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
    , dncrqAutoScalingGroupNames
    , dncrqNextToken
    , dncrqMaxRecords

    -- * Response
    , DescribeNotificationConfigurationsResponse
    -- ** Response constructor
    , describeNotificationConfigurationsResponse
    -- ** Response lenses
    , dncrsNextToken
    , dncrsStatus
    , dncrsNotificationConfigurations
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeNotificationConfigurations' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dncrqAutoScalingGroupNames'
--
-- * 'dncrqNextToken'
--
-- * 'dncrqMaxRecords'
data DescribeNotificationConfigurations = DescribeNotificationConfigurations'
    { _dncrqAutoScalingGroupNames :: !(Maybe [Text])
    , _dncrqNextToken             :: !(Maybe Text)
    , _dncrqMaxRecords            :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeNotificationConfigurations' smart constructor.
describeNotificationConfigurations :: DescribeNotificationConfigurations
describeNotificationConfigurations =
    DescribeNotificationConfigurations'
    { _dncrqAutoScalingGroupNames = Nothing
    , _dncrqNextToken = Nothing
    , _dncrqMaxRecords = Nothing
    }

-- | The name of the group.
dncrqAutoScalingGroupNames :: Lens' DescribeNotificationConfigurations [Text]
dncrqAutoScalingGroupNames = lens _dncrqAutoScalingGroupNames (\ s a -> s{_dncrqAutoScalingGroupNames = a}) . _Default;

-- | The token for the next set of items to return. (You received this token
-- from a previous call.)
dncrqNextToken :: Lens' DescribeNotificationConfigurations (Maybe Text)
dncrqNextToken = lens _dncrqNextToken (\ s a -> s{_dncrqNextToken = a});

-- | The maximum number of items to return with this call.
dncrqMaxRecords :: Lens' DescribeNotificationConfigurations (Maybe Int)
dncrqMaxRecords = lens _dncrqMaxRecords (\ s a -> s{_dncrqMaxRecords = a});

instance AWSPager DescribeNotificationConfigurations
         where
        page rq rs
          | stop (rs ^. dncrsNextToken) = Nothing
          | stop (rs ^. dncrsNotificationConfigurations) =
            Nothing
          | otherwise =
            Just $ rq & dncrqNextToken .~ rs ^. dncrsNextToken

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
                   (x .@? "NextToken") <*> (pure (fromEnum s)) <*>
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
                   (toQueryList "member" <$>
                      _dncrqAutoScalingGroupNames),
               "NextToken" =: _dncrqNextToken,
               "MaxRecords" =: _dncrqMaxRecords]

-- | /See:/ 'describeNotificationConfigurationsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dncrsNextToken'
--
-- * 'dncrsStatus'
--
-- * 'dncrsNotificationConfigurations'
data DescribeNotificationConfigurationsResponse = DescribeNotificationConfigurationsResponse'
    { _dncrsNextToken                  :: !(Maybe Text)
    , _dncrsStatus                     :: !Int
    , _dncrsNotificationConfigurations :: ![NotificationConfiguration]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeNotificationConfigurationsResponse' smart constructor.
describeNotificationConfigurationsResponse :: Int -> DescribeNotificationConfigurationsResponse
describeNotificationConfigurationsResponse pStatus =
    DescribeNotificationConfigurationsResponse'
    { _dncrsNextToken = Nothing
    , _dncrsStatus = pStatus
    , _dncrsNotificationConfigurations = mempty
    }

-- | The token to use when requesting the next set of items. If there are no
-- additional items to return, the string is empty.
dncrsNextToken :: Lens' DescribeNotificationConfigurationsResponse (Maybe Text)
dncrsNextToken = lens _dncrsNextToken (\ s a -> s{_dncrsNextToken = a});

-- | FIXME: Undocumented member.
dncrsStatus :: Lens' DescribeNotificationConfigurationsResponse Int
dncrsStatus = lens _dncrsStatus (\ s a -> s{_dncrsStatus = a});

-- | The notification configurations.
dncrsNotificationConfigurations :: Lens' DescribeNotificationConfigurationsResponse [NotificationConfiguration]
dncrsNotificationConfigurations = lens _dncrsNotificationConfigurations (\ s a -> s{_dncrsNotificationConfigurations = a});
