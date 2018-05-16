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
-- Module      : Network.AWS.AutoScaling.AttachLoadBalancerTargetGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches one or more target groups to the specified Auto Scaling group.
--
--
-- To describe the target groups for an Auto Scaling group, use 'DescribeLoadBalancerTargetGroups' . To detach the target group from the Auto Scaling group, use 'DetachLoadBalancerTargetGroups' .
--
-- For more information, see <http://docs.aws.amazon.com/autoscaling/latest/userguide/attach-load-balancer-asg.html Attach a Load Balancer to Your Auto Scaling Group> in the /Auto Scaling User Guide/ .
--
module Network.AWS.AutoScaling.AttachLoadBalancerTargetGroups
    (
    -- * Creating a Request
      attachLoadBalancerTargetGroups
    , AttachLoadBalancerTargetGroups
    -- * Request Lenses
    , albtgAutoScalingGroupName
    , albtgTargetGroupARNs

    -- * Destructuring the Response
    , attachLoadBalancerTargetGroupsResponse
    , AttachLoadBalancerTargetGroupsResponse
    -- * Response Lenses
    , albtgrsResponseStatus
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'attachLoadBalancerTargetGroups' smart constructor.
data AttachLoadBalancerTargetGroups = AttachLoadBalancerTargetGroups'
  { _albtgAutoScalingGroupName :: !Text
  , _albtgTargetGroupARNs      :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachLoadBalancerTargetGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'albtgAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'albtgTargetGroupARNs' - The Amazon Resource Names (ARN) of the target groups. You can specify up to 10 target groups.
attachLoadBalancerTargetGroups
    :: Text -- ^ 'albtgAutoScalingGroupName'
    -> AttachLoadBalancerTargetGroups
attachLoadBalancerTargetGroups pAutoScalingGroupName_ =
  AttachLoadBalancerTargetGroups'
    { _albtgAutoScalingGroupName = pAutoScalingGroupName_
    , _albtgTargetGroupARNs = mempty
    }


-- | The name of the Auto Scaling group.
albtgAutoScalingGroupName :: Lens' AttachLoadBalancerTargetGroups Text
albtgAutoScalingGroupName = lens _albtgAutoScalingGroupName (\ s a -> s{_albtgAutoScalingGroupName = a})

-- | The Amazon Resource Names (ARN) of the target groups. You can specify up to 10 target groups.
albtgTargetGroupARNs :: Lens' AttachLoadBalancerTargetGroups [Text]
albtgTargetGroupARNs = lens _albtgTargetGroupARNs (\ s a -> s{_albtgTargetGroupARNs = a}) . _Coerce

instance AWSRequest AttachLoadBalancerTargetGroups
         where
        type Rs AttachLoadBalancerTargetGroups =
             AttachLoadBalancerTargetGroupsResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper
              "AttachLoadBalancerTargetGroupsResult"
              (\ s h x ->
                 AttachLoadBalancerTargetGroupsResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AttachLoadBalancerTargetGroups
         where

instance NFData AttachLoadBalancerTargetGroups where

instance ToHeaders AttachLoadBalancerTargetGroups
         where
        toHeaders = const mempty

instance ToPath AttachLoadBalancerTargetGroups where
        toPath = const "/"

instance ToQuery AttachLoadBalancerTargetGroups where
        toQuery AttachLoadBalancerTargetGroups'{..}
          = mconcat
              ["Action" =:
                 ("AttachLoadBalancerTargetGroups" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "AutoScalingGroupName" =: _albtgAutoScalingGroupName,
               "TargetGroupARNs" =:
                 toQueryList "member" _albtgTargetGroupARNs]

-- | /See:/ 'attachLoadBalancerTargetGroupsResponse' smart constructor.
newtype AttachLoadBalancerTargetGroupsResponse = AttachLoadBalancerTargetGroupsResponse'
  { _albtgrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AttachLoadBalancerTargetGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'albtgrsResponseStatus' - -- | The response status code.
attachLoadBalancerTargetGroupsResponse
    :: Int -- ^ 'albtgrsResponseStatus'
    -> AttachLoadBalancerTargetGroupsResponse
attachLoadBalancerTargetGroupsResponse pResponseStatus_ =
  AttachLoadBalancerTargetGroupsResponse'
    {_albtgrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
albtgrsResponseStatus :: Lens' AttachLoadBalancerTargetGroupsResponse Int
albtgrsResponseStatus = lens _albtgrsResponseStatus (\ s a -> s{_albtgrsResponseStatus = a})

instance NFData
           AttachLoadBalancerTargetGroupsResponse
         where
