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
-- Module      : Network.AWS.EMR.GetManagedScalingPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Fetches the attached managed scaling policy for an Amazon EMR cluster.
module Network.AWS.EMR.GetManagedScalingPolicy
  ( -- * Creating a Request
    getManagedScalingPolicy,
    GetManagedScalingPolicy,

    -- * Request Lenses
    gmspClusterId,

    -- * Destructuring the Response
    getManagedScalingPolicyResponse,
    GetManagedScalingPolicyResponse,

    -- * Response Lenses
    gmsprsManagedScalingPolicy,
    gmsprsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getManagedScalingPolicy' smart constructor.
newtype GetManagedScalingPolicy = GetManagedScalingPolicy'
  { _gmspClusterId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetManagedScalingPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmspClusterId' - Specifies the ID of the cluster for which the managed scaling policy will be fetched.
getManagedScalingPolicy ::
  -- | 'gmspClusterId'
  Text ->
  GetManagedScalingPolicy
getManagedScalingPolicy pClusterId_ =
  GetManagedScalingPolicy' {_gmspClusterId = pClusterId_}

-- | Specifies the ID of the cluster for which the managed scaling policy will be fetched.
gmspClusterId :: Lens' GetManagedScalingPolicy Text
gmspClusterId = lens _gmspClusterId (\s a -> s {_gmspClusterId = a})

instance AWSRequest GetManagedScalingPolicy where
  type Rs GetManagedScalingPolicy = GetManagedScalingPolicyResponse
  request = postJSON emr
  response =
    receiveJSON
      ( \s h x ->
          GetManagedScalingPolicyResponse'
            <$> (x .?> "ManagedScalingPolicy") <*> (pure (fromEnum s))
      )

instance Hashable GetManagedScalingPolicy

instance NFData GetManagedScalingPolicy

instance ToHeaders GetManagedScalingPolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("ElasticMapReduce.GetManagedScalingPolicy" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetManagedScalingPolicy where
  toJSON GetManagedScalingPolicy' {..} =
    object (catMaybes [Just ("ClusterId" .= _gmspClusterId)])

instance ToPath GetManagedScalingPolicy where
  toPath = const "/"

instance ToQuery GetManagedScalingPolicy where
  toQuery = const mempty

-- | /See:/ 'getManagedScalingPolicyResponse' smart constructor.
data GetManagedScalingPolicyResponse = GetManagedScalingPolicyResponse'
  { _gmsprsManagedScalingPolicy ::
      !( Maybe
           ManagedScalingPolicy
       ),
    _gmsprsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetManagedScalingPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmsprsManagedScalingPolicy' - Specifies the managed scaling policy that is attached to an Amazon EMR cluster.
--
-- * 'gmsprsResponseStatus' - -- | The response status code.
getManagedScalingPolicyResponse ::
  -- | 'gmsprsResponseStatus'
  Int ->
  GetManagedScalingPolicyResponse
getManagedScalingPolicyResponse pResponseStatus_ =
  GetManagedScalingPolicyResponse'
    { _gmsprsManagedScalingPolicy =
        Nothing,
      _gmsprsResponseStatus = pResponseStatus_
    }

-- | Specifies the managed scaling policy that is attached to an Amazon EMR cluster.
gmsprsManagedScalingPolicy :: Lens' GetManagedScalingPolicyResponse (Maybe ManagedScalingPolicy)
gmsprsManagedScalingPolicy = lens _gmsprsManagedScalingPolicy (\s a -> s {_gmsprsManagedScalingPolicy = a})

-- | -- | The response status code.
gmsprsResponseStatus :: Lens' GetManagedScalingPolicyResponse Int
gmsprsResponseStatus = lens _gmsprsResponseStatus (\s a -> s {_gmsprsResponseStatus = a})

instance NFData GetManagedScalingPolicyResponse
