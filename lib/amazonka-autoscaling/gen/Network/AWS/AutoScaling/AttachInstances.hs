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
-- Module      : Network.AWS.AutoScaling.AttachInstances
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches one or more EC2 instances to the specified Auto Scaling group.
--
--
-- When you attach instances, Auto Scaling increases the desired capacity of the group by the number of instances being attached. If the number of instances being attached plus the desired capacity of the group exceeds the maximum size of the group, the operation fails.
--
-- If there is a Classic Load Balancer attached to your Auto Scaling group, the instances are also registered with the load balancer. If there are target groups attached to your Auto Scaling group, the instances are also registered with the target groups.
--
-- For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/attach-instance-asg.html Attach EC2 Instances to Your Auto Scaling Group> in the /Auto Scaling User Guide/ .
--
module Network.AWS.AutoScaling.AttachInstances
    (
    -- * Creating a Request
      attachInstances
    , AttachInstances
    -- * Request Lenses
    , aiInstanceIds
    , aiAutoScalingGroupName

    -- * Destructuring the Response
    , attachInstancesResponse
    , AttachInstancesResponse
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'attachInstances' smart constructor.
data AttachInstances = AttachInstances'
  { _aiInstanceIds          :: !(Maybe [Text])
  , _aiAutoScalingGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiInstanceIds' - The IDs of the instances. You can specify up to 20 instances.
--
-- * 'aiAutoScalingGroupName' - The name of the Auto Scaling group.
attachInstances
    :: Text -- ^ 'aiAutoScalingGroupName'
    -> AttachInstances
attachInstances pAutoScalingGroupName_ =
  AttachInstances'
    {_aiInstanceIds = Nothing, _aiAutoScalingGroupName = pAutoScalingGroupName_}


-- | The IDs of the instances. You can specify up to 20 instances.
aiInstanceIds :: Lens' AttachInstances [Text]
aiInstanceIds = lens _aiInstanceIds (\ s a -> s{_aiInstanceIds = a}) . _Default . _Coerce

-- | The name of the Auto Scaling group.
aiAutoScalingGroupName :: Lens' AttachInstances Text
aiAutoScalingGroupName = lens _aiAutoScalingGroupName (\ s a -> s{_aiAutoScalingGroupName = a})

instance AWSRequest AttachInstances where
        type Rs AttachInstances = AttachInstancesResponse
        request = postQuery autoScaling
        response = receiveNull AttachInstancesResponse'

instance Hashable AttachInstances where

instance NFData AttachInstances where

instance ToHeaders AttachInstances where
        toHeaders = const mempty

instance ToPath AttachInstances where
        toPath = const "/"

instance ToQuery AttachInstances where
        toQuery AttachInstances'{..}
          = mconcat
              ["Action" =: ("AttachInstances" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "InstanceIds" =:
                 toQuery (toQueryList "member" <$> _aiInstanceIds),
               "AutoScalingGroupName" =: _aiAutoScalingGroupName]

-- | /See:/ 'attachInstancesResponse' smart constructor.
data AttachInstancesResponse =
  AttachInstancesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachInstancesResponse' with the minimum fields required to make a request.
--
attachInstancesResponse
    :: AttachInstancesResponse
attachInstancesResponse = AttachInstancesResponse'


instance NFData AttachInstancesResponse where
