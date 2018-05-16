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
-- Module      : Network.AWS.AutoScaling.DetachLoadBalancerTargetGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches one or more target groups from the specified Auto Scaling group.
--
--
module Network.AWS.AutoScaling.DetachLoadBalancerTargetGroups
    (
    -- * Creating a Request
      detachLoadBalancerTargetGroups
    , DetachLoadBalancerTargetGroups
    -- * Request Lenses
    , dlbtgAutoScalingGroupName
    , dlbtgTargetGroupARNs

    -- * Destructuring the Response
    , detachLoadBalancerTargetGroupsResponse
    , DetachLoadBalancerTargetGroupsResponse
    -- * Response Lenses
    , dlbtgrsResponseStatus
    ) where

import Network.AWS.AutoScaling.Types
import Network.AWS.AutoScaling.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'detachLoadBalancerTargetGroups' smart constructor.
data DetachLoadBalancerTargetGroups = DetachLoadBalancerTargetGroups'
  { _dlbtgAutoScalingGroupName :: !Text
  , _dlbtgTargetGroupARNs      :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachLoadBalancerTargetGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbtgAutoScalingGroupName' - The name of the Auto Scaling group.
--
-- * 'dlbtgTargetGroupARNs' - The Amazon Resource Names (ARN) of the target groups. You can specify up to 10 target groups.
detachLoadBalancerTargetGroups
    :: Text -- ^ 'dlbtgAutoScalingGroupName'
    -> DetachLoadBalancerTargetGroups
detachLoadBalancerTargetGroups pAutoScalingGroupName_ =
  DetachLoadBalancerTargetGroups'
    { _dlbtgAutoScalingGroupName = pAutoScalingGroupName_
    , _dlbtgTargetGroupARNs = mempty
    }


-- | The name of the Auto Scaling group.
dlbtgAutoScalingGroupName :: Lens' DetachLoadBalancerTargetGroups Text
dlbtgAutoScalingGroupName = lens _dlbtgAutoScalingGroupName (\ s a -> s{_dlbtgAutoScalingGroupName = a})

-- | The Amazon Resource Names (ARN) of the target groups. You can specify up to 10 target groups.
dlbtgTargetGroupARNs :: Lens' DetachLoadBalancerTargetGroups [Text]
dlbtgTargetGroupARNs = lens _dlbtgTargetGroupARNs (\ s a -> s{_dlbtgTargetGroupARNs = a}) . _Coerce

instance AWSRequest DetachLoadBalancerTargetGroups
         where
        type Rs DetachLoadBalancerTargetGroups =
             DetachLoadBalancerTargetGroupsResponse
        request = postQuery autoScaling
        response
          = receiveXMLWrapper
              "DetachLoadBalancerTargetGroupsResult"
              (\ s h x ->
                 DetachLoadBalancerTargetGroupsResponse' <$>
                   (pure (fromEnum s)))

instance Hashable DetachLoadBalancerTargetGroups
         where

instance NFData DetachLoadBalancerTargetGroups where

instance ToHeaders DetachLoadBalancerTargetGroups
         where
        toHeaders = const mempty

instance ToPath DetachLoadBalancerTargetGroups where
        toPath = const "/"

instance ToQuery DetachLoadBalancerTargetGroups where
        toQuery DetachLoadBalancerTargetGroups'{..}
          = mconcat
              ["Action" =:
                 ("DetachLoadBalancerTargetGroups" :: ByteString),
               "Version" =: ("2011-01-01" :: ByteString),
               "AutoScalingGroupName" =: _dlbtgAutoScalingGroupName,
               "TargetGroupARNs" =:
                 toQueryList "member" _dlbtgTargetGroupARNs]

-- | /See:/ 'detachLoadBalancerTargetGroupsResponse' smart constructor.
newtype DetachLoadBalancerTargetGroupsResponse = DetachLoadBalancerTargetGroupsResponse'
  { _dlbtgrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DetachLoadBalancerTargetGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlbtgrsResponseStatus' - -- | The response status code.
detachLoadBalancerTargetGroupsResponse
    :: Int -- ^ 'dlbtgrsResponseStatus'
    -> DetachLoadBalancerTargetGroupsResponse
detachLoadBalancerTargetGroupsResponse pResponseStatus_ =
  DetachLoadBalancerTargetGroupsResponse'
    {_dlbtgrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
dlbtgrsResponseStatus :: Lens' DetachLoadBalancerTargetGroupsResponse Int
dlbtgrsResponseStatus = lens _dlbtgrsResponseStatus (\ s a -> s{_dlbtgrsResponseStatus = a})

instance NFData
           DetachLoadBalancerTargetGroupsResponse
         where
