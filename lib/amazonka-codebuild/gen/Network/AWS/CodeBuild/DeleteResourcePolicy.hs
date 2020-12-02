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
-- Module      : Network.AWS.CodeBuild.DeleteResourcePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a resource policy that is identified by its resource ARN.
module Network.AWS.CodeBuild.DeleteResourcePolicy
  ( -- * Creating a Request
    deleteResourcePolicy,
    DeleteResourcePolicy,

    -- * Request Lenses
    drpResourceARN,

    -- * Destructuring the Response
    deleteResourcePolicyResponse,
    DeleteResourcePolicyResponse,

    -- * Response Lenses
    drprsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteResourcePolicy' smart constructor.
newtype DeleteResourcePolicy = DeleteResourcePolicy'
  { _drpResourceARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteResourcePolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drpResourceARN' - The ARN of the resource that is associated with the resource policy.
deleteResourcePolicy ::
  -- | 'drpResourceARN'
  Text ->
  DeleteResourcePolicy
deleteResourcePolicy pResourceARN_ =
  DeleteResourcePolicy' {_drpResourceARN = pResourceARN_}

-- | The ARN of the resource that is associated with the resource policy.
drpResourceARN :: Lens' DeleteResourcePolicy Text
drpResourceARN = lens _drpResourceARN (\s a -> s {_drpResourceARN = a})

instance AWSRequest DeleteResourcePolicy where
  type Rs DeleteResourcePolicy = DeleteResourcePolicyResponse
  request = postJSON codeBuild
  response =
    receiveEmpty
      (\s h x -> DeleteResourcePolicyResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteResourcePolicy

instance NFData DeleteResourcePolicy

instance ToHeaders DeleteResourcePolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeBuild_20161006.DeleteResourcePolicy" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteResourcePolicy where
  toJSON DeleteResourcePolicy' {..} =
    object (catMaybes [Just ("resourceArn" .= _drpResourceARN)])

instance ToPath DeleteResourcePolicy where
  toPath = const "/"

instance ToQuery DeleteResourcePolicy where
  toQuery = const mempty

-- | /See:/ 'deleteResourcePolicyResponse' smart constructor.
newtype DeleteResourcePolicyResponse = DeleteResourcePolicyResponse'
  { _drprsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteResourcePolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drprsResponseStatus' - -- | The response status code.
deleteResourcePolicyResponse ::
  -- | 'drprsResponseStatus'
  Int ->
  DeleteResourcePolicyResponse
deleteResourcePolicyResponse pResponseStatus_ =
  DeleteResourcePolicyResponse'
    { _drprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
drprsResponseStatus :: Lens' DeleteResourcePolicyResponse Int
drprsResponseStatus = lens _drprsResponseStatus (\s a -> s {_drprsResponseStatus = a})

instance NFData DeleteResourcePolicyResponse
