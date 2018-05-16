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
-- Module      : Network.AWS.EMR.RemoveAutoScalingPolicy
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an automatic scaling policy from a specified instance group within an EMR cluster.
--
--
module Network.AWS.EMR.RemoveAutoScalingPolicy
    (
    -- * Creating a Request
      removeAutoScalingPolicy
    , RemoveAutoScalingPolicy
    -- * Request Lenses
    , raspClusterId
    , raspInstanceGroupId

    -- * Destructuring the Response
    , removeAutoScalingPolicyResponse
    , RemoveAutoScalingPolicyResponse
    -- * Response Lenses
    , rasprsResponseStatus
    ) where

import Network.AWS.EMR.Types
import Network.AWS.EMR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeAutoScalingPolicy' smart constructor.
data RemoveAutoScalingPolicy = RemoveAutoScalingPolicy'
  { _raspClusterId       :: !Text
  , _raspInstanceGroupId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveAutoScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'raspClusterId' - Specifies the ID of a cluster. The instance group to which the automatic scaling policy is applied is within this cluster.
--
-- * 'raspInstanceGroupId' - Specifies the ID of the instance group to which the scaling policy is applied.
removeAutoScalingPolicy
    :: Text -- ^ 'raspClusterId'
    -> Text -- ^ 'raspInstanceGroupId'
    -> RemoveAutoScalingPolicy
removeAutoScalingPolicy pClusterId_ pInstanceGroupId_ =
  RemoveAutoScalingPolicy'
    {_raspClusterId = pClusterId_, _raspInstanceGroupId = pInstanceGroupId_}


-- | Specifies the ID of a cluster. The instance group to which the automatic scaling policy is applied is within this cluster.
raspClusterId :: Lens' RemoveAutoScalingPolicy Text
raspClusterId = lens _raspClusterId (\ s a -> s{_raspClusterId = a})

-- | Specifies the ID of the instance group to which the scaling policy is applied.
raspInstanceGroupId :: Lens' RemoveAutoScalingPolicy Text
raspInstanceGroupId = lens _raspInstanceGroupId (\ s a -> s{_raspInstanceGroupId = a})

instance AWSRequest RemoveAutoScalingPolicy where
        type Rs RemoveAutoScalingPolicy =
             RemoveAutoScalingPolicyResponse
        request = postJSON emr
        response
          = receiveEmpty
              (\ s h x ->
                 RemoveAutoScalingPolicyResponse' <$>
                   (pure (fromEnum s)))

instance Hashable RemoveAutoScalingPolicy where

instance NFData RemoveAutoScalingPolicy where

instance ToHeaders RemoveAutoScalingPolicy where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.RemoveAutoScalingPolicy" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RemoveAutoScalingPolicy where
        toJSON RemoveAutoScalingPolicy'{..}
          = object
              (catMaybes
                 [Just ("ClusterId" .= _raspClusterId),
                  Just ("InstanceGroupId" .= _raspInstanceGroupId)])

instance ToPath RemoveAutoScalingPolicy where
        toPath = const "/"

instance ToQuery RemoveAutoScalingPolicy where
        toQuery = const mempty

-- | /See:/ 'removeAutoScalingPolicyResponse' smart constructor.
newtype RemoveAutoScalingPolicyResponse = RemoveAutoScalingPolicyResponse'
  { _rasprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveAutoScalingPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rasprsResponseStatus' - -- | The response status code.
removeAutoScalingPolicyResponse
    :: Int -- ^ 'rasprsResponseStatus'
    -> RemoveAutoScalingPolicyResponse
removeAutoScalingPolicyResponse pResponseStatus_ =
  RemoveAutoScalingPolicyResponse' {_rasprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
rasprsResponseStatus :: Lens' RemoveAutoScalingPolicyResponse Int
rasprsResponseStatus = lens _rasprsResponseStatus (\ s a -> s{_rasprsResponseStatus = a})

instance NFData RemoveAutoScalingPolicyResponse where
