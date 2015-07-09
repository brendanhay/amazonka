{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes the notification types that are supported by Auto Scaling.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DescribeAutoScalingNotificationTypes.html>
module Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes
    (
    -- * Request
      DescribeAutoScalingNotificationTypes
    -- ** Request constructor
    , describeAutoScalingNotificationTypes

    -- * Response
    , DescribeAutoScalingNotificationTypesResponse
    -- ** Response constructor
    , describeAutoScalingNotificationTypesResponse
    -- ** Response lenses
    , dasntrAutoScalingNotificationTypes
    , dasntrStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeAutoScalingNotificationTypes' smart constructor.
data DescribeAutoScalingNotificationTypes =
    DescribeAutoScalingNotificationTypes'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAutoScalingNotificationTypes' smart constructor.
describeAutoScalingNotificationTypes :: DescribeAutoScalingNotificationTypes
describeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypes'

instance AWSRequest
         DescribeAutoScalingNotificationTypes where
        type Sv DescribeAutoScalingNotificationTypes =
             AutoScaling
        type Rs DescribeAutoScalingNotificationTypes =
             DescribeAutoScalingNotificationTypesResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeAutoScalingNotificationTypesResult"
              (\ s h x ->
                 DescribeAutoScalingNotificationTypesResponse' <$>
                   (x .@? "AutoScalingNotificationTypes" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dasntrAutoScalingNotificationTypes'
--
-- * 'dasntrStatus'
data DescribeAutoScalingNotificationTypesResponse = DescribeAutoScalingNotificationTypesResponse'
    { _dasntrAutoScalingNotificationTypes :: !(Maybe [Text])
    , _dasntrStatus                       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeAutoScalingNotificationTypesResponse' smart constructor.
describeAutoScalingNotificationTypesResponse :: Int -> DescribeAutoScalingNotificationTypesResponse
describeAutoScalingNotificationTypesResponse pStatus =
    DescribeAutoScalingNotificationTypesResponse'
    { _dasntrAutoScalingNotificationTypes = Nothing
    , _dasntrStatus = pStatus
    }

-- | One or more of the following notification types:
--
-- -   @autoscaling:EC2_INSTANCE_LAUNCH@
--
-- -   @autoscaling:EC2_INSTANCE_LAUNCH_ERROR@
--
-- -   @autoscaling:EC2_INSTANCE_TERMINATE@
--
-- -   @autoscaling:EC2_INSTANCE_TERMINATE_ERROR@
--
-- -   @autoscaling:TEST_NOTIFICATION@
--
dasntrAutoScalingNotificationTypes :: Lens' DescribeAutoScalingNotificationTypesResponse [Text]
dasntrAutoScalingNotificationTypes = lens _dasntrAutoScalingNotificationTypes (\ s a -> s{_dasntrAutoScalingNotificationTypes = a}) . _Default;

-- | FIXME: Undocumented member.
dasntrStatus :: Lens' DescribeAutoScalingNotificationTypesResponse Int
dasntrStatus = lens _dasntrStatus (\ s a -> s{_dasntrStatus = a});
