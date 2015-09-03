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
-- Module      : Network.AWS.AutoScaling.TerminateInstanceInAutoScalingGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
      terminateInstanceInAutoScalingGroup
    , TerminateInstanceInAutoScalingGroup
    -- * Request Lenses
    , tiiasgInstanceId
    , tiiasgShouldDecrementDesiredCapacity

    -- * Destructuring the Response
    , terminateInstanceInAutoScalingGroupResponse
    , TerminateInstanceInAutoScalingGroupResponse
    -- * Response Lenses
    , tiiasgrsActivity
    , tiiasgrsResponseStatus
    ) where

import           Network.AWS.AutoScaling.Types
import           Network.AWS.AutoScaling.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'terminateInstanceInAutoScalingGroup' smart constructor.
data TerminateInstanceInAutoScalingGroup = TerminateInstanceInAutoScalingGroup'
    { _tiiasgInstanceId                     :: !Text
    , _tiiasgShouldDecrementDesiredCapacity :: !Bool
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TerminateInstanceInAutoScalingGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiiasgInstanceId'
--
-- * 'tiiasgShouldDecrementDesiredCapacity'
terminateInstanceInAutoScalingGroup
    :: Text -- ^ 'tiiasgInstanceId'
    -> Bool -- ^ 'tiiasgShouldDecrementDesiredCapacity'
    -> TerminateInstanceInAutoScalingGroup
terminateInstanceInAutoScalingGroup pInstanceId_ pShouldDecrementDesiredCapacity_ =
    TerminateInstanceInAutoScalingGroup'
    { _tiiasgInstanceId = pInstanceId_
    , _tiiasgShouldDecrementDesiredCapacity = pShouldDecrementDesiredCapacity_
    }

-- | The ID of the EC2 instance.
tiiasgInstanceId :: Lens' TerminateInstanceInAutoScalingGroup Text
tiiasgInstanceId = lens _tiiasgInstanceId (\ s a -> s{_tiiasgInstanceId = a});

-- | If 'true', terminating this instance also decrements the size of the
-- Auto Scaling group.
tiiasgShouldDecrementDesiredCapacity :: Lens' TerminateInstanceInAutoScalingGroup Bool
tiiasgShouldDecrementDesiredCapacity = lens _tiiasgShouldDecrementDesiredCapacity (\ s a -> s{_tiiasgShouldDecrementDesiredCapacity = a});

instance AWSRequest
         TerminateInstanceInAutoScalingGroup where
        type Rs TerminateInstanceInAutoScalingGroup =
             TerminateInstanceInAutoScalingGroupResponse
        request = postQuery autoScaling
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
data TerminateInstanceInAutoScalingGroupResponse = TerminateInstanceInAutoScalingGroupResponse'
    { _tiiasgrsActivity       :: !(Maybe Activity)
    , _tiiasgrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'TerminateInstanceInAutoScalingGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tiiasgrsActivity'
--
-- * 'tiiasgrsResponseStatus'
terminateInstanceInAutoScalingGroupResponse
    :: Int -- ^ 'tiiasgrsResponseStatus'
    -> TerminateInstanceInAutoScalingGroupResponse
terminateInstanceInAutoScalingGroupResponse pResponseStatus_ =
    TerminateInstanceInAutoScalingGroupResponse'
    { _tiiasgrsActivity = Nothing
    , _tiiasgrsResponseStatus = pResponseStatus_
    }

-- | A scaling activity.
tiiasgrsActivity :: Lens' TerminateInstanceInAutoScalingGroupResponse (Maybe Activity)
tiiasgrsActivity = lens _tiiasgrsActivity (\ s a -> s{_tiiasgrsActivity = a});

-- | The response status code.
tiiasgrsResponseStatus :: Lens' TerminateInstanceInAutoScalingGroupResponse Int
tiiasgrsResponseStatus = lens _tiiasgrsResponseStatus (\ s a -> s{_tiiasgrsResponseStatus = a});
