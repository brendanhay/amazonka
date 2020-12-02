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
-- Module      : Network.AWS.Rekognition.DeleteProjectVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Rekognition Custom Labels model.
--
--
-- You can't delete a model if it is running or if it is training. To check the status of a model, use the @Status@ field returned from 'DescribeProjectVersions' . To stop a running model call 'StopProjectVersion' . If the model is training, wait until it finishes.
--
-- This operation requires permissions to perform the @rekognition:DeleteProjectVersion@ action.
module Network.AWS.Rekognition.DeleteProjectVersion
  ( -- * Creating a Request
    deleteProjectVersion,
    DeleteProjectVersion,

    -- * Request Lenses
    dpvProjectVersionARN,

    -- * Destructuring the Response
    deleteProjectVersionResponse,
    DeleteProjectVersionResponse,

    -- * Response Lenses
    delrsStatus,
    delrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteProjectVersion' smart constructor.
newtype DeleteProjectVersion = DeleteProjectVersion'
  { _dpvProjectVersionARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteProjectVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpvProjectVersionARN' - The Amazon Resource Name (ARN) of the model version that you want to delete.
deleteProjectVersion ::
  -- | 'dpvProjectVersionARN'
  Text ->
  DeleteProjectVersion
deleteProjectVersion pProjectVersionARN_ =
  DeleteProjectVersion'
    { _dpvProjectVersionARN =
        pProjectVersionARN_
    }

-- | The Amazon Resource Name (ARN) of the model version that you want to delete.
dpvProjectVersionARN :: Lens' DeleteProjectVersion Text
dpvProjectVersionARN = lens _dpvProjectVersionARN (\s a -> s {_dpvProjectVersionARN = a})

instance AWSRequest DeleteProjectVersion where
  type Rs DeleteProjectVersion = DeleteProjectVersionResponse
  request = postJSON rekognition
  response =
    receiveJSON
      ( \s h x ->
          DeleteProjectVersionResponse'
            <$> (x .?> "Status") <*> (pure (fromEnum s))
      )

instance Hashable DeleteProjectVersion

instance NFData DeleteProjectVersion

instance ToHeaders DeleteProjectVersion where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("RekognitionService.DeleteProjectVersion" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteProjectVersion where
  toJSON DeleteProjectVersion' {..} =
    object
      (catMaybes [Just ("ProjectVersionArn" .= _dpvProjectVersionARN)])

instance ToPath DeleteProjectVersion where
  toPath = const "/"

instance ToQuery DeleteProjectVersion where
  toQuery = const mempty

-- | /See:/ 'deleteProjectVersionResponse' smart constructor.
data DeleteProjectVersionResponse = DeleteProjectVersionResponse'
  { _delrsStatus ::
      !(Maybe ProjectVersionStatus),
    _delrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteProjectVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delrsStatus' - The status of the deletion operation.
--
-- * 'delrsResponseStatus' - -- | The response status code.
deleteProjectVersionResponse ::
  -- | 'delrsResponseStatus'
  Int ->
  DeleteProjectVersionResponse
deleteProjectVersionResponse pResponseStatus_ =
  DeleteProjectVersionResponse'
    { _delrsStatus = Nothing,
      _delrsResponseStatus = pResponseStatus_
    }

-- | The status of the deletion operation.
delrsStatus :: Lens' DeleteProjectVersionResponse (Maybe ProjectVersionStatus)
delrsStatus = lens _delrsStatus (\s a -> s {_delrsStatus = a})

-- | -- | The response status code.
delrsResponseStatus :: Lens' DeleteProjectVersionResponse Int
delrsResponseStatus = lens _delrsResponseStatus (\s a -> s {_delrsResponseStatus = a})

instance NFData DeleteProjectVersionResponse
