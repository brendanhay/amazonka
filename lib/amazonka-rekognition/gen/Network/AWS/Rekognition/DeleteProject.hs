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
-- Module      : Network.AWS.Rekognition.DeleteProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon Rekognition Custom Labels project. To delete a project you must first delete all models associated with the project. To delete a model, see 'DeleteProjectVersion' .
--
--
-- This operation requires permissions to perform the @rekognition:DeleteProject@ action.
module Network.AWS.Rekognition.DeleteProject
  ( -- * Creating a Request
    deleteProject,
    DeleteProject,

    -- * Request Lenses
    dpProjectARN,

    -- * Destructuring the Response
    deleteProjectResponse,
    DeleteProjectResponse,

    -- * Response Lenses
    dprsStatus,
    dprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteProject' smart constructor.
newtype DeleteProject = DeleteProject' {_dpProjectARN :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpProjectARN' - The Amazon Resource Name (ARN) of the project that you want to delete.
deleteProject ::
  -- | 'dpProjectARN'
  Text ->
  DeleteProject
deleteProject pProjectARN_ =
  DeleteProject' {_dpProjectARN = pProjectARN_}

-- | The Amazon Resource Name (ARN) of the project that you want to delete.
dpProjectARN :: Lens' DeleteProject Text
dpProjectARN = lens _dpProjectARN (\s a -> s {_dpProjectARN = a})

instance AWSRequest DeleteProject where
  type Rs DeleteProject = DeleteProjectResponse
  request = postJSON rekognition
  response =
    receiveJSON
      ( \s h x ->
          DeleteProjectResponse'
            <$> (x .?> "Status") <*> (pure (fromEnum s))
      )

instance Hashable DeleteProject

instance NFData DeleteProject

instance ToHeaders DeleteProject where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("RekognitionService.DeleteProject" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteProject where
  toJSON DeleteProject' {..} =
    object (catMaybes [Just ("ProjectArn" .= _dpProjectARN)])

instance ToPath DeleteProject where
  toPath = const "/"

instance ToQuery DeleteProject where
  toQuery = const mempty

-- | /See:/ 'deleteProjectResponse' smart constructor.
data DeleteProjectResponse = DeleteProjectResponse'
  { _dprsStatus ::
      !(Maybe ProjectStatus),
    _dprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprsStatus' - The current status of the delete project operation.
--
-- * 'dprsResponseStatus' - -- | The response status code.
deleteProjectResponse ::
  -- | 'dprsResponseStatus'
  Int ->
  DeleteProjectResponse
deleteProjectResponse pResponseStatus_ =
  DeleteProjectResponse'
    { _dprsStatus = Nothing,
      _dprsResponseStatus = pResponseStatus_
    }

-- | The current status of the delete project operation.
dprsStatus :: Lens' DeleteProjectResponse (Maybe ProjectStatus)
dprsStatus = lens _dprsStatus (\s a -> s {_dprsStatus = a})

-- | -- | The response status code.
dprsResponseStatus :: Lens' DeleteProjectResponse Int
dprsResponseStatus = lens _dprsResponseStatus (\s a -> s {_dprsResponseStatus = a})

instance NFData DeleteProjectResponse
