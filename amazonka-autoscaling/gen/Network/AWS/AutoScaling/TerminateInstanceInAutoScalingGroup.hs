{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
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

-- | Terminates the specified instance and optionally adjusts the desired
-- group size.
--
-- This call simply makes a termination request. The instances is not
-- terminated immediately.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_TerminateInstanceInAutoScalingGroup.html>
module Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
    (
    -- * Request
      TerminateInstanceInAutoScalingGroup
    -- ** Request constructor
    , terminateInstanceInAutoScalingGroup
    -- ** Request lenses
    , tiiasgInstanceId
    , tiiasgShouldDecrementDesiredCapacity

    -- * Response
    , TerminateInstanceInAutoScalingGroupResponse
    -- ** Response constructor
    , terminateInstanceInAutoScalingGroupResponse
    -- ** Response lenses
    , tiiasgrActivity
    , tiiasgrStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'terminateInstanceInAutoScalingGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tiiasgInstanceId'
--
-- * 'tiiasgShouldDecrementDesiredCapacity'
data TerminateInstanceInAutoScalingGroup = TerminateInstanceInAutoScalingGroup'
    { _tiiasgInstanceId                     :: Text
    , _tiiasgShouldDecrementDesiredCapacity :: !Bool
    } deriving (Eq,Read,Show)

-- | 'TerminateInstanceInAutoScalingGroup' smart constructor.
terminateInstanceInAutoScalingGroup :: Text -> Bool -> TerminateInstanceInAutoScalingGroup
terminateInstanceInAutoScalingGroup pInstanceId pShouldDecrementDesiredCapacity =
    TerminateInstanceInAutoScalingGroup'
    { _tiiasgInstanceId = pInstanceId
    , _tiiasgShouldDecrementDesiredCapacity = pShouldDecrementDesiredCapacity
    }

-- | The ID of the EC2 instance.
tiiasgInstanceId :: Lens' TerminateInstanceInAutoScalingGroup Text
tiiasgInstanceId = lens _tiiasgInstanceId (\ s a -> s{_tiiasgInstanceId = a});

-- | If @true@, terminating this instance also decrements the size of the
-- Auto Scaling group.
tiiasgShouldDecrementDesiredCapacity :: Lens' TerminateInstanceInAutoScalingGroup Bool
tiiasgShouldDecrementDesiredCapacity = lens _tiiasgShouldDecrementDesiredCapacity (\ s a -> s{_tiiasgShouldDecrementDesiredCapacity = a});

instance AWSRequest
         TerminateInstanceInAutoScalingGroup where
        type Sv TerminateInstanceInAutoScalingGroup =
             AutoScaling
        type Rs TerminateInstanceInAutoScalingGroup =
             TerminateInstanceInAutoScalingGroupResponse
        request = post
        response
          = receiveXMLWrapper
              "TerminateInstanceInAutoScalingGroupResult"
              (\ s h x ->
                 TerminateInstanceInAutoScalingGroupResponse' <$>
                   (x .@? "Activity") <*> (pure (fromEnum s)))

instance ToHeaders
         TerminateInstanceInAutoScalingGroup where
        toHeaders = const mempty

instance ToPath TerminateInstanceInAutoScalingGroup
         where
        toPath = const "/"

instance ToQuery TerminateInstanceInAutoScalingGroup
         where
        toQuery TerminateInstanceInAutoScalingGroup'{..}
          = mconcat
              ["Action" =:
                 ("TerminateInstanceInAutoScalingGroup" ::
                    ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "InstanceId" =: _tiiasgInstanceId,
               "ShouldDecrementDesiredCapacity" =:
                 _tiiasgShouldDecrementDesiredCapacity]

-- | /See:/ 'terminateInstanceInAutoScalingGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'tiiasgrActivity'
--
-- * 'tiiasgrStatus'
data TerminateInstanceInAutoScalingGroupResponse = TerminateInstanceInAutoScalingGroupResponse'
    { _tiiasgrActivity :: Maybe Activity
    , _tiiasgrStatus   :: !Int
    } deriving (Eq,Read,Show)

-- | 'TerminateInstanceInAutoScalingGroupResponse' smart constructor.
terminateInstanceInAutoScalingGroupResponse :: Int -> TerminateInstanceInAutoScalingGroupResponse
terminateInstanceInAutoScalingGroupResponse pStatus =
    TerminateInstanceInAutoScalingGroupResponse'
    { _tiiasgrActivity = Nothing
    , _tiiasgrStatus = pStatus
    }

-- | A scaling activity.
tiiasgrActivity :: Lens' TerminateInstanceInAutoScalingGroupResponse (Maybe Activity)
tiiasgrActivity = lens _tiiasgrActivity (\ s a -> s{_tiiasgrActivity = a});

-- | FIXME: Undocumented member.
tiiasgrStatus :: Lens' TerminateInstanceInAutoScalingGroupResponse Int
tiiasgrStatus = lens _tiiasgrStatus (\ s a -> s{_tiiasgrStatus = a});
