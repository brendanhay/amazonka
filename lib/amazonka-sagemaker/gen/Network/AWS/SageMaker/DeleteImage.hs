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
-- Module      : Network.AWS.SageMaker.DeleteImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a SageMaker image and all versions of the image. The container images aren't deleted.
module Network.AWS.SageMaker.DeleteImage
  ( -- * Creating a Request
    deleteImage,
    DeleteImage,

    -- * Request Lenses
    delImageName,

    -- * Destructuring the Response
    deleteImageResponse,
    DeleteImageResponse,

    -- * Response Lenses
    dirsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'deleteImage' smart constructor.
newtype DeleteImage = DeleteImage' {_delImageName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delImageName' - The name of the image to delete.
deleteImage ::
  -- | 'delImageName'
  Text ->
  DeleteImage
deleteImage pImageName_ = DeleteImage' {_delImageName = pImageName_}

-- | The name of the image to delete.
delImageName :: Lens' DeleteImage Text
delImageName = lens _delImageName (\s a -> s {_delImageName = a})

instance AWSRequest DeleteImage where
  type Rs DeleteImage = DeleteImageResponse
  request = postJSON sageMaker
  response =
    receiveEmpty
      (\s h x -> DeleteImageResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteImage

instance NFData DeleteImage

instance ToHeaders DeleteImage where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.DeleteImage" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteImage where
  toJSON DeleteImage' {..} =
    object (catMaybes [Just ("ImageName" .= _delImageName)])

instance ToPath DeleteImage where
  toPath = const "/"

instance ToQuery DeleteImage where
  toQuery = const mempty

-- | /See:/ 'deleteImageResponse' smart constructor.
newtype DeleteImageResponse = DeleteImageResponse'
  { _dirsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsResponseStatus' - -- | The response status code.
deleteImageResponse ::
  -- | 'dirsResponseStatus'
  Int ->
  DeleteImageResponse
deleteImageResponse pResponseStatus_ =
  DeleteImageResponse' {_dirsResponseStatus = pResponseStatus_}

-- | -- | The response status code.
dirsResponseStatus :: Lens' DeleteImageResponse Int
dirsResponseStatus = lens _dirsResponseStatus (\s a -> s {_dirsResponseStatus = a})

instance NFData DeleteImageResponse
