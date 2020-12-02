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
-- Module      : Network.AWS.EMR.PutManagedScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a managed scaling policy for an Amazon EMR cluster. The managed scaling policy defines the limits for resources, such as EC2 instances that can be added or terminated from a cluster. The policy only applies to the core and task nodes. The master node cannot be scaled after initial configuration.
module Network.AWS.EMR.PutManagedScalingPolicy
  ( -- * Creating a Request
    putManagedScalingPolicy,
    PutManagedScalingPolicy,

    -- * Request Lenses
    pmspClusterId,
    pmspManagedScalingPolicy,

    -- * Destructuring the Response
    putManagedScalingPolicyResponse,
    PutManagedScalingPolicyResponse,

    -- * Response Lenses
    pmsprsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putManagedScalingPolicy' smart constructor.
data PutManagedScalingPolicy = PutManagedScalingPolicy'
  { _pmspClusterId ::
      !Text,
    _pmspManagedScalingPolicy ::
      !ManagedScalingPolicy
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutManagedScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmspClusterId' - Specifies the ID of an EMR cluster where the managed scaling policy is attached.
--
-- * 'pmspManagedScalingPolicy' - Specifies the constraints for the managed scaling policy.
putManagedScalingPolicy ::
  -- | 'pmspClusterId'
  Text ->
  -- | 'pmspManagedScalingPolicy'
  ManagedScalingPolicy ->
  PutManagedScalingPolicy
putManagedScalingPolicy pClusterId_ pManagedScalingPolicy_ =
  PutManagedScalingPolicy'
    { _pmspClusterId = pClusterId_,
      _pmspManagedScalingPolicy = pManagedScalingPolicy_
    }

-- | Specifies the ID of an EMR cluster where the managed scaling policy is attached.
pmspClusterId :: Lens' PutManagedScalingPolicy Text
pmspClusterId = lens _pmspClusterId (\s a -> s {_pmspClusterId = a})

-- | Specifies the constraints for the managed scaling policy.
pmspManagedScalingPolicy :: Lens' PutManagedScalingPolicy ManagedScalingPolicy
pmspManagedScalingPolicy = lens _pmspManagedScalingPolicy (\s a -> s {_pmspManagedScalingPolicy = a})

instance AWSRequest PutManagedScalingPolicy where
  type Rs PutManagedScalingPolicy = PutManagedScalingPolicyResponse
  request = postJSON emr
  response =
    receiveEmpty
      ( \s h x ->
          PutManagedScalingPolicyResponse' <$> (pure (fromEnum s))
      )

instance Hashable PutManagedScalingPolicy

instance NFData PutManagedScalingPolicy

instance ToHeaders PutManagedScalingPolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("ElasticMapReduce.PutManagedScalingPolicy" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutManagedScalingPolicy where
  toJSON PutManagedScalingPolicy' {..} =
    object
      ( catMaybes
          [ Just ("ClusterId" .= _pmspClusterId),
            Just ("ManagedScalingPolicy" .= _pmspManagedScalingPolicy)
          ]
      )

instance ToPath PutManagedScalingPolicy where
  toPath = const "/"

instance ToQuery PutManagedScalingPolicy where
  toQuery = const mempty

-- | /See:/ 'putManagedScalingPolicyResponse' smart constructor.
newtype PutManagedScalingPolicyResponse = PutManagedScalingPolicyResponse'
  { _pmsprsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutManagedScalingPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmsprsResponseStatus' - -- | The response status code.
putManagedScalingPolicyResponse ::
  -- | 'pmsprsResponseStatus'
  Int ->
  PutManagedScalingPolicyResponse
putManagedScalingPolicyResponse pResponseStatus_ =
  PutManagedScalingPolicyResponse'
    { _pmsprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
pmsprsResponseStatus :: Lens' PutManagedScalingPolicyResponse Int
pmsprsResponseStatus = lens _pmsprsResponseStatus (\s a -> s {_pmsprsResponseStatus = a})

instance NFData PutManagedScalingPolicyResponse
