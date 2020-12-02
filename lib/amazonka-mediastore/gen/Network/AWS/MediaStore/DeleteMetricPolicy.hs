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
-- Module      : Network.AWS.MediaStore.DeleteMetricPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the metric policy that is associated with the specified container. If there is no metric policy associated with the container, MediaStore doesn't send metrics to CloudWatch.
module Network.AWS.MediaStore.DeleteMetricPolicy
  ( -- * Creating a Request
    deleteMetricPolicy,
    DeleteMetricPolicy,

    -- * Request Lenses
    dmpContainerName,

    -- * Destructuring the Response
    deleteMetricPolicyResponse,
    DeleteMetricPolicyResponse,

    -- * Response Lenses
    dmprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.MediaStore.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteMetricPolicy' smart constructor.
newtype DeleteMetricPolicy = DeleteMetricPolicy'
  { _dmpContainerName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteMetricPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmpContainerName' - The name of the container that is associated with the metric policy that you want to delete.
deleteMetricPolicy ::
  -- | 'dmpContainerName'
  Text ->
  DeleteMetricPolicy
deleteMetricPolicy pContainerName_ =
  DeleteMetricPolicy' {_dmpContainerName = pContainerName_}

-- | The name of the container that is associated with the metric policy that you want to delete.
dmpContainerName :: Lens' DeleteMetricPolicy Text
dmpContainerName = lens _dmpContainerName (\s a -> s {_dmpContainerName = a})

instance AWSRequest DeleteMetricPolicy where
  type Rs DeleteMetricPolicy = DeleteMetricPolicyResponse
  request = postJSON mediaStore
  response =
    receiveEmpty
      (\s h x -> DeleteMetricPolicyResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteMetricPolicy

instance NFData DeleteMetricPolicy

instance ToHeaders DeleteMetricPolicy where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("MediaStore_20170901.DeleteMetricPolicy" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteMetricPolicy where
  toJSON DeleteMetricPolicy' {..} =
    object (catMaybes [Just ("ContainerName" .= _dmpContainerName)])

instance ToPath DeleteMetricPolicy where
  toPath = const "/"

instance ToQuery DeleteMetricPolicy where
  toQuery = const mempty

-- | /See:/ 'deleteMetricPolicyResponse' smart constructor.
newtype DeleteMetricPolicyResponse = DeleteMetricPolicyResponse'
  { _dmprsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteMetricPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmprsResponseStatus' - -- | The response status code.
deleteMetricPolicyResponse ::
  -- | 'dmprsResponseStatus'
  Int ->
  DeleteMetricPolicyResponse
deleteMetricPolicyResponse pResponseStatus_ =
  DeleteMetricPolicyResponse'
    { _dmprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dmprsResponseStatus :: Lens' DeleteMetricPolicyResponse Int
dmprsResponseStatus = lens _dmprsResponseStatus (\s a -> s {_dmprsResponseStatus = a})

instance NFData DeleteMetricPolicyResponse
