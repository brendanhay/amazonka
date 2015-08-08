{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Terminates the specified instance and optionally adjusts the desired
-- group size.
--
-- This call simply makes a termination request. The instances is not
-- terminated immediately.
--
-- /See:/ <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_TerminateInstanceInAutoScalingGroup.html AWS API Reference> for TerminateInstanceInAutoScalingGroup.
module Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
    (
    -- * Creating a Request
      TerminateInstanceInAutoScalingGroup
    , terminateInstanceInAutoScalingGroup
    -- * Request Lenses
    , tiiasgInstanceId
    , tiiasgShouldDecrementDesiredCapacity

    -- * Destructuring the Response
    , TerminateInstanceInAutoScalingGroupResponse
    , terminateInstanceInAutoScalingGroupResponse
    -- * Response Lenses
    , tiiasgrsActivity
    , tiiasgrsStatus
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
    { _tiiasgInstanceId                     :: !Text
    , _tiiasgShouldDecrementDesiredCapacity :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TerminateInstanceInAutoScalingGroup' smart constructor.
terminateInstanceInAutoScalingGroup :: Text -> Bool -> TerminateInstanceInAutoScalingGroup
terminateInstanceInAutoScalingGroup pInstanceId_ pShouldDecrementDesiredCapacity_ =
    TerminateInstanceInAutoScalingGroup'
    { _tiiasgInstanceId = pInstanceId_
    , _tiiasgShouldDecrementDesiredCapacity = pShouldDecrementDesiredCapacity_
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
        request = postQuery
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
-- * 'tiiasgrsActivity'
--
-- * 'tiiasgrsStatus'
data TerminateInstanceInAutoScalingGroupResponse = TerminateInstanceInAutoScalingGroupResponse'
    { _tiiasgrsActivity :: !(Maybe Activity)
    , _tiiasgrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'TerminateInstanceInAutoScalingGroupResponse' smart constructor.
terminateInstanceInAutoScalingGroupResponse :: Int -> TerminateInstanceInAutoScalingGroupResponse
terminateInstanceInAutoScalingGroupResponse pStatus_ =
    TerminateInstanceInAutoScalingGroupResponse'
    { _tiiasgrsActivity = Nothing
    , _tiiasgrsStatus = pStatus_
    }

-- | A scaling activity.
tiiasgrsActivity :: Lens' TerminateInstanceInAutoScalingGroupResponse (Maybe Activity)
tiiasgrsActivity = lens _tiiasgrsActivity (\ s a -> s{_tiiasgrsActivity = a});

-- | Undocumented member.
tiiasgrsStatus :: Lens' TerminateInstanceInAutoScalingGroupResponse Int
tiiasgrsStatus = lens _tiiasgrsStatus (\ s a -> s{_tiiasgrsStatus = a});
