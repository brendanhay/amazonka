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
-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the notification types that are supported by Auto Scaling.
module Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes
    (
    -- * Creating a Request
      describeAutoScalingNotificationTypes
    , DescribeAutoScalingNotificationTypes

    -- * Destructuring the Response
    , describeAutoScalingNotificationTypesResponse
    , DescribeAutoScalingNotificationTypesResponse
    -- * Response Lenses
    , dasntrsAutoScalingNotificationTypes
    , dasntrsResponseStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.AutoScaling.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeAutoScalingNotificationTypes' smart constructor.
data DescribeAutoScalingNotificationTypes =
    DescribeAutoScalingNotificationTypes'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeAutoScalingNotificationTypes' with the minimum fields required to make a request.
--
describeAutoScalingNotificationTypes
    :: DescribeAutoScalingNotificationTypes
describeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypes'

instance AWSRequest
         DescribeAutoScalingNotificationTypes where
        type Rs DescribeAutoScalingNotificationTypes =
             DescribeAutoScalingNotificationTypesResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper
              "DescribeAutoScalingNotificationTypesResult"
              (\ s h x ->
                 DescribeAutoScalingNotificationTypesResponse' <$>
                   (x .@? "AutoScalingNotificationTypes" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable
         DescribeAutoScalingNotificationTypes

instance ToHeaders
         DescribeAutoScalingNotificationTypes where
        toHeaders = const mempty

instance ToPath DescribeAutoScalingNotificationTypes
         where
        toPath = const "/"

instance ToQuery DescribeAutoScalingNotificationTypes
         where
        toQuery
          = const
              (mconcat
                 ["Action" =:
                    ("DescribeAutoScalingNotificationTypes" ::
                       ByteString),
                  "Version" =: ("2011-01-01" :: ByteString)])

-- | /See:/ 'describeAutoScalingNotificationTypesResponse' smart constructor.
data DescribeAutoScalingNotificationTypesResponse = DescribeAutoScalingNotificationTypesResponse'
    { _dasntrsAutoScalingNotificationTypes :: !(Maybe [Text])
    , _dasntrsResponseStatus               :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeAutoScalingNotificationTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasntrsAutoScalingNotificationTypes'
--
-- * 'dasntrsResponseStatus'
describeAutoScalingNotificationTypesResponse
    :: Int -- ^ 'dasntrsResponseStatus'
    -> DescribeAutoScalingNotificationTypesResponse
describeAutoScalingNotificationTypesResponse pResponseStatus_ =
    DescribeAutoScalingNotificationTypesResponse'
    { _dasntrsAutoScalingNotificationTypes = Nothing
    , _dasntrsResponseStatus = pResponseStatus_
    }

-- | One or more of the following notification types:
--
-- -   'autoscaling:EC2_INSTANCE_LAUNCH'
--
-- -   'autoscaling:EC2_INSTANCE_LAUNCH_ERROR'
--
-- -   'autoscaling:EC2_INSTANCE_TERMINATE'
--
-- -   'autoscaling:EC2_INSTANCE_TERMINATE_ERROR'
--
-- -   'autoscaling:TEST_NOTIFICATION'
--
dasntrsAutoScalingNotificationTypes :: Lens' DescribeAutoScalingNotificationTypesResponse [Text]
dasntrsAutoScalingNotificationTypes = lens _dasntrsAutoScalingNotificationTypes (\ s a -> s{_dasntrsAutoScalingNotificationTypes = a}) . _Default . _Coerce;

-- | The response status code.
dasntrsResponseStatus :: Lens' DescribeAutoScalingNotificationTypesResponse Int
dasntrsResponseStatus = lens _dasntrsResponseStatus (\ s a -> s{_dasntrsResponseStatus = a});
