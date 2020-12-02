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
-- Module      : Network.AWS.CodeBuild.GetResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a resource policy that is identified by its resource ARN.
module Network.AWS.CodeBuild.GetResourcePolicy
  ( -- * Creating a Request
    getResourcePolicy,
    GetResourcePolicy,

    -- * Request Lenses
    grpResourceARN,

    -- * Destructuring the Response
    getResourcePolicyResponse,
    GetResourcePolicyResponse,

    -- * Response Lenses
    grprsPolicy,
    grprsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getResourcePolicy' smart constructor.
newtype GetResourcePolicy = GetResourcePolicy'
  { _grpResourceARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetResourcePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grpResourceARN' - The ARN of the resource that is associated with the resource policy.
getResourcePolicy ::
  -- | 'grpResourceARN'
  Text ->
  GetResourcePolicy
getResourcePolicy pResourceARN_ =
  GetResourcePolicy' {_grpResourceARN = pResourceARN_}

-- | The ARN of the resource that is associated with the resource policy.
grpResourceARN :: Lens' GetResourcePolicy Text
grpResourceARN = lens _grpResourceARN (\s a -> s {_grpResourceARN = a})

instance AWSRequest GetResourcePolicy where
  type Rs GetResourcePolicy = GetResourcePolicyResponse
  request = postJSON codeBuild
  response =
    receiveJSON
      ( \s h x ->
          GetResourcePolicyResponse'
            <$> (x .?> "policy") <*> (pure (fromEnum s))
      )

instance Hashable GetResourcePolicy

instance NFData GetResourcePolicy

instance ToHeaders GetResourcePolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeBuild_20161006.GetResourcePolicy" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetResourcePolicy where
  toJSON GetResourcePolicy' {..} =
    object (catMaybes [Just ("resourceArn" .= _grpResourceARN)])

instance ToPath GetResourcePolicy where
  toPath = const "/"

instance ToQuery GetResourcePolicy where
  toQuery = const mempty

-- | /See:/ 'getResourcePolicyResponse' smart constructor.
data GetResourcePolicyResponse = GetResourcePolicyResponse'
  { _grprsPolicy ::
      !(Maybe Text),
    _grprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetResourcePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grprsPolicy' - The resource policy for the resource identified by the input ARN parameter.
--
-- * 'grprsResponseStatus' - -- | The response status code.
getResourcePolicyResponse ::
  -- | 'grprsResponseStatus'
  Int ->
  GetResourcePolicyResponse
getResourcePolicyResponse pResponseStatus_ =
  GetResourcePolicyResponse'
    { _grprsPolicy = Nothing,
      _grprsResponseStatus = pResponseStatus_
    }

-- | The resource policy for the resource identified by the input ARN parameter.
grprsPolicy :: Lens' GetResourcePolicyResponse (Maybe Text)
grprsPolicy = lens _grprsPolicy (\s a -> s {_grprsPolicy = a})

-- | -- | The response status code.
grprsResponseStatus :: Lens' GetResourcePolicyResponse Int
grprsResponseStatus = lens _grprsResponseStatus (\s a -> s {_grprsResponseStatus = a})

instance NFData GetResourcePolicyResponse
