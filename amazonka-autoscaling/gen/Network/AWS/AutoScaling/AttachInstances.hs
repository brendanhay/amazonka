{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.AutoScaling.AttachInstances
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

-- | Attaches one or more EC2 instances to the specified Auto Scaling group.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AutoScaling/latest/DeveloperGuide/attach-instance-asg.html Attach Amazon EC2 Instances to Your Existing Auto Scaling Group>
-- in the /Auto Scaling Developer Guide/.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_AttachInstances.html>
module Network.AWS.AutoScaling.AttachInstances
    (
    -- * Request
      AttachInstances
    -- ** Request constructor
    , attachInstances
    -- ** Request lenses
    , aiInstanceIds
    , aiAutoScalingGroupName

    -- * Response
    , AttachInstancesResponse
    -- ** Response constructor
    , attachInstancesResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.AutoScaling.Types

-- | /See:/ 'attachInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aiInstanceIds'
--
-- * 'aiAutoScalingGroupName'
data AttachInstances = AttachInstances'{_aiInstanceIds :: Maybe [Text], _aiAutoScalingGroupName :: Text} deriving (Eq, Read, Show)

-- | 'AttachInstances' smart constructor.
attachInstances :: Text -> AttachInstances
attachInstances pAutoScalingGroupName = AttachInstances'{_aiInstanceIds = Nothing, _aiAutoScalingGroupName = pAutoScalingGroupName};

-- | One or more EC2 instance IDs. You must specify at least one ID.
aiInstanceIds :: Lens' AttachInstances (Maybe [Text])
aiInstanceIds = lens _aiInstanceIds (\ s a -> s{_aiInstanceIds = a});

-- | The name of the group.
aiAutoScalingGroupName :: Lens' AttachInstances Text
aiAutoScalingGroupName = lens _aiAutoScalingGroupName (\ s a -> s{_aiAutoScalingGroupName = a});

instance AWSRequest AttachInstances where
        type Sv AttachInstances = AutoScaling
        type Rs AttachInstances = AttachInstancesResponse
        request = post
        response = receiveNull AttachInstancesResponse'

instance ToHeaders AttachInstances where
        toHeaders = const mempty

instance ToPath AttachInstances where
        toPath = const "/"

instance ToQuery AttachInstances where
        toQuery AttachInstances'{..}
          = mconcat
              ["Action" =: ("AttachInstances" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "InstanceIds" =: "member" =: _aiInstanceIds,
               "AutoScalingGroupName" =: _aiAutoScalingGroupName]

-- | /See:/ 'attachInstancesResponse' smart constructor.
data AttachInstancesResponse = AttachInstancesResponse' deriving (Eq, Read, Show)

-- | 'AttachInstancesResponse' smart constructor.
attachInstancesResponse :: AttachInstancesResponse
attachInstancesResponse = AttachInstancesResponse';
