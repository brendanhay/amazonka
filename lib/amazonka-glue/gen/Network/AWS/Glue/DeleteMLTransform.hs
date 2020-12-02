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
-- Module      : Network.AWS.Glue.DeleteMLTransform
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Glue machine learning transform. Machine learning transforms are a special type of transform that use machine learning to learn the details of the transformation to be performed by learning from examples provided by humans. These transformations are then saved by AWS Glue. If you no longer need a transform, you can delete it by calling @DeleteMLTransforms@ . However, any AWS Glue jobs that still reference the deleted transform will no longer succeed.
module Network.AWS.Glue.DeleteMLTransform
  ( -- * Creating a Request
    deleteMLTransform,
    DeleteMLTransform,

    -- * Request Lenses
    dmltTransformId,

    -- * Destructuring the Response
    deleteMLTransformResponse,
    DeleteMLTransformResponse,

    -- * Response Lenses
    dmltrsTransformId,
    dmltrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteMLTransform' smart constructor.
newtype DeleteMLTransform = DeleteMLTransform'
  { _dmltTransformId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteMLTransform' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmltTransformId' - The unique identifier of the transform to delete.
deleteMLTransform ::
  -- | 'dmltTransformId'
  Text ->
  DeleteMLTransform
deleteMLTransform pTransformId_ =
  DeleteMLTransform' {_dmltTransformId = pTransformId_}

-- | The unique identifier of the transform to delete.
dmltTransformId :: Lens' DeleteMLTransform Text
dmltTransformId = lens _dmltTransformId (\s a -> s {_dmltTransformId = a})

instance AWSRequest DeleteMLTransform where
  type Rs DeleteMLTransform = DeleteMLTransformResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          DeleteMLTransformResponse'
            <$> (x .?> "TransformId") <*> (pure (fromEnum s))
      )

instance Hashable DeleteMLTransform

instance NFData DeleteMLTransform

instance ToHeaders DeleteMLTransform where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.DeleteMLTransform" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteMLTransform where
  toJSON DeleteMLTransform' {..} =
    object (catMaybes [Just ("TransformId" .= _dmltTransformId)])

instance ToPath DeleteMLTransform where
  toPath = const "/"

instance ToQuery DeleteMLTransform where
  toQuery = const mempty

-- | /See:/ 'deleteMLTransformResponse' smart constructor.
data DeleteMLTransformResponse = DeleteMLTransformResponse'
  { _dmltrsTransformId ::
      !(Maybe Text),
    _dmltrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteMLTransformResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmltrsTransformId' - The unique identifier of the transform that was deleted.
--
-- * 'dmltrsResponseStatus' - -- | The response status code.
deleteMLTransformResponse ::
  -- | 'dmltrsResponseStatus'
  Int ->
  DeleteMLTransformResponse
deleteMLTransformResponse pResponseStatus_ =
  DeleteMLTransformResponse'
    { _dmltrsTransformId = Nothing,
      _dmltrsResponseStatus = pResponseStatus_
    }

-- | The unique identifier of the transform that was deleted.
dmltrsTransformId :: Lens' DeleteMLTransformResponse (Maybe Text)
dmltrsTransformId = lens _dmltrsTransformId (\s a -> s {_dmltrsTransformId = a})

-- | -- | The response status code.
dmltrsResponseStatus :: Lens' DeleteMLTransformResponse Int
dmltrsResponseStatus = lens _dmltrsResponseStatus (\s a -> s {_dmltrsResponseStatus = a})

instance NFData DeleteMLTransformResponse
