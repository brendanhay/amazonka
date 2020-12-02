{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeNotificationConfigurations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the notification actions associated with the specified Auto Scaling group.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AutoScaling.DescribeNotificationConfigurations
    (
    -- * Creating a Request
      describeNotificationConfigurations
    , DescribeNotificationConfigurations
    -- * Request Lenses
    , dncAutoScalingGroupNames
    , dncNextToken
    , dncMaxRecords

    -- * Destructuring the Response
    , describeNotificationConfigurationsResponse
    , DescribeNotificationConfigurationsResponse
    -- * Response Lenses
    , dncrsNextToken
    , dncrsResponseStatus
    , dncrsNotificationConfigurations
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeNotificationConfigurations' smart constructor.
data DescribeNotificationConfigurations = DescribeNotificationConfigurations'
  { _dncAutoScalingGroupNames :: !(Maybe [Text])
  , _dncNextToken             :: !(Maybe Text)
  , _dncMaxRecords            :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNotificationConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dncAutoScalingGroupNames' - The name of the Auto Scaling group.
--
-- * 'dncNextToken' - The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'dncMaxRecords' - The maximum number of items to return with this call. The default value is 50 and the maximum value is 100.
describeNotificationConfigurations
    :: DescribeNotificationConfigurations
describeNotificationConfigurations =
  DescribeNotificationConfigurations'
    { _dncAutoScalingGroupNames = Nothing
    , _dncNextToken = Nothing
    , _dncMaxRecords = Nothing
    }


-- | The name of the Auto Scaling group.
dncAutoScalingGroupNames :: Lens' DescribeNotificationConfigurations [Text]
dncAutoScalingGroupNames = lens _dncAutoScalingGroupNames (\ s a -> s{_dncAutoScalingGroupNames = a}) . _Default . _Coerce

-- | The token for the next set of items to return. (You received this token from a previous call.)
dncNextToken :: Lens' DescribeNotificationConfigurations (Maybe Text)
dncNextToken = lens _dncNextToken (\ s a -> s{_dncNextToken = a})

-- | The maximum number of items to return with this call. The default value is 50 and the maximum value is 100.
dncMaxRecords :: Lens' DescribeNotificationConfigurations (Maybe Int)
dncMaxRecords = lens _dncMaxRecords (\ s a -> s{_dncMaxRecords = a})

instance AWSPager DescribeNotificationConfigurations
         where
        page rq rs
          | stop (rs ^. dncrsNextToken) = Nothing
          | stop (rs ^. dncrsNotificationConfigurations) =
            Nothing
          | otherwise =
            Just $ rq & dncNextToken .~ rs ^. dncrsNextToken

instance AWSRequest
           DescribeNotificationConfigurations
         where
        type Rs DescribeNotificationConfigurations =
             DescribeNotificationConfigurationsResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper
              "DescribeNotificationConfigurationsResult"
              (\ s h x ->
                 DescribeNotificationConfigurationsResponse' <$>
                   (x .@? "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .@? "NotificationConfigurations" .!@ mempty >>=
                        parseXMLList "member"))

instance Hashable DescribeNotificationConfigurations
         where

instance NFData DescribeNotificationConfigurations
         where

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
data DescribeNotificationConfigurationsResponse = DescribeNotificationConfigurationsResponse'
  { _dncrsNextToken                  :: !(Maybe Text)
  , _dncrsResponseStatus             :: !Int
  , _dncrsNotificationConfigurations :: ![NotificationConfiguration]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeNotificationConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dncrsNextToken' - The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- * 'dncrsResponseStatus' - -- | The response status code.
--
-- * 'dncrsNotificationConfigurations' - The notification configurations.
describeNotificationConfigurationsResponse
    :: Int -- ^ 'dncrsResponseStatus'
    -> DescribeNotificationConfigurationsResponse
describeNotificationConfigurationsResponse pResponseStatus_ =
  DescribeNotificationConfigurationsResponse'
    { _dncrsNextToken = Nothing
    , _dncrsResponseStatus = pResponseStatus_
    , _dncrsNotificationConfigurations = mempty
    }


-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
dncrsNextToken :: Lens' DescribeNotificationConfigurationsResponse (Maybe Text)
dncrsNextToken = lens _dncrsNextToken (\ s a -> s{_dncrsNextToken = a})

-- | -- | The response status code.
dncrsResponseStatus :: Lens' DescribeNotificationConfigurationsResponse Int
dncrsResponseStatus = lens _dncrsResponseStatus (\ s a -> s{_dncrsResponseStatus = a})

-- | The notification configurations.
dncrsNotificationConfigurations :: Lens' DescribeNotificationConfigurationsResponse [NotificationConfiguration]
dncrsNotificationConfigurations = lens _dncrsNotificationConfigurations (\ s a -> s{_dncrsNotificationConfigurations = a}) . _Coerce

instance NFData
           DescribeNotificationConfigurationsResponse
         where
