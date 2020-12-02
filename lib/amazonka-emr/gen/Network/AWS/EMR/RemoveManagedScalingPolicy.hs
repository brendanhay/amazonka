{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.RemoveManagedScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a managed scaling policy from a specified EMR cluster.
module Network.AWS.EMR.RemoveManagedScalingPolicy
  ( -- * Creating a Request
    removeManagedScalingPolicy,
    RemoveManagedScalingPolicy,

    -- * Request Lenses
    rmspClusterId,

    -- * Destructuring the Response
    removeManagedScalingPolicyResponse,
    RemoveManagedScalingPolicyResponse,

    -- * Response Lenses
    rmsprsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'removeManagedScalingPolicy' smart constructor.
newtype RemoveManagedScalingPolicy = RemoveManagedScalingPolicy'
  { _rmspClusterId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemoveManagedScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmspClusterId' - Specifies the ID of the cluster from which the managed scaling policy will be removed.
removeManagedScalingPolicy ::
  -- | 'rmspClusterId'
  Text ->
  RemoveManagedScalingPolicy
removeManagedScalingPolicy pClusterId_ =
  RemoveManagedScalingPolicy' {_rmspClusterId = pClusterId_}

-- | Specifies the ID of the cluster from which the managed scaling policy will be removed.
rmspClusterId :: Lens' RemoveManagedScalingPolicy Text
rmspClusterId = lens _rmspClusterId (\s a -> s {_rmspClusterId = a})

instance AWSRequest RemoveManagedScalingPolicy where
  type
    Rs RemoveManagedScalingPolicy =
      RemoveManagedScalingPolicyResponse
  request = postJSON emr
  response =
    receiveEmpty
      ( \s h x ->
          RemoveManagedScalingPolicyResponse' <$> (pure (fromEnum s))
      )

instance Hashable RemoveManagedScalingPolicy

instance NFData RemoveManagedScalingPolicy

instance ToHeaders RemoveManagedScalingPolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("ElasticMapReduce.RemoveManagedScalingPolicy" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON RemoveManagedScalingPolicy where
  toJSON RemoveManagedScalingPolicy' {..} =
    object (catMaybes [Just ("ClusterId" .= _rmspClusterId)])

instance ToPath RemoveManagedScalingPolicy where
  toPath = const "/"

instance ToQuery RemoveManagedScalingPolicy where
  toQuery = const mempty

-- | /See:/ 'removeManagedScalingPolicyResponse' smart constructor.
newtype RemoveManagedScalingPolicyResponse = RemoveManagedScalingPolicyResponse'
  { _rmsprsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemoveManagedScalingPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmsprsResponseStatus' - -- | The response status code.
removeManagedScalingPolicyResponse ::
  -- | 'rmsprsResponseStatus'
  Int ->
  RemoveManagedScalingPolicyResponse
removeManagedScalingPolicyResponse pResponseStatus_ =
  RemoveManagedScalingPolicyResponse'
    { _rmsprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
rmsprsResponseStatus :: Lens' RemoveManagedScalingPolicyResponse Int
rmsprsResponseStatus = lens _rmsprsResponseStatus (\s a -> s {_rmsprsResponseStatus = a})

instance NFData RemoveManagedScalingPolicyResponse
