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
-- Module      : Network.AWS.Lightsail.DeleteContainerImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a container image that is registered to your Amazon Lightsail container service.
module Network.AWS.Lightsail.DeleteContainerImage
  ( -- * Creating a Request
    deleteContainerImage,
    DeleteContainerImage,

    -- * Request Lenses
    dciServiceName,
    dciImage,

    -- * Destructuring the Response
    deleteContainerImageResponse,
    DeleteContainerImageResponse,

    -- * Response Lenses
    dcirsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteContainerImage' smart constructor.
data DeleteContainerImage = DeleteContainerImage'
  { _dciServiceName ::
      !Text,
    _dciImage :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteContainerImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dciServiceName' - The name of the container service for which to delete a registered container image.
--
-- * 'dciImage' - The name of the container image to delete from the container service. Use the @GetContainerImages@ action to get the name of the container images that are registered to a container service.
deleteContainerImage ::
  -- | 'dciServiceName'
  Text ->
  -- | 'dciImage'
  Text ->
  DeleteContainerImage
deleteContainerImage pServiceName_ pImage_ =
  DeleteContainerImage'
    { _dciServiceName = pServiceName_,
      _dciImage = pImage_
    }

-- | The name of the container service for which to delete a registered container image.
dciServiceName :: Lens' DeleteContainerImage Text
dciServiceName = lens _dciServiceName (\s a -> s {_dciServiceName = a})

-- | The name of the container image to delete from the container service. Use the @GetContainerImages@ action to get the name of the container images that are registered to a container service.
dciImage :: Lens' DeleteContainerImage Text
dciImage = lens _dciImage (\s a -> s {_dciImage = a})

instance AWSRequest DeleteContainerImage where
  type Rs DeleteContainerImage = DeleteContainerImageResponse
  request = postJSON lightsail
  response =
    receiveEmpty
      (\s h x -> DeleteContainerImageResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteContainerImage

instance NFData DeleteContainerImage

instance ToHeaders DeleteContainerImage where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.DeleteContainerImage" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteContainerImage where
  toJSON DeleteContainerImage' {..} =
    object
      ( catMaybes
          [ Just ("serviceName" .= _dciServiceName),
            Just ("image" .= _dciImage)
          ]
      )

instance ToPath DeleteContainerImage where
  toPath = const "/"

instance ToQuery DeleteContainerImage where
  toQuery = const mempty

-- | /See:/ 'deleteContainerImageResponse' smart constructor.
newtype DeleteContainerImageResponse = DeleteContainerImageResponse'
  { _dcirsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteContainerImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcirsResponseStatus' - -- | The response status code.
deleteContainerImageResponse ::
  -- | 'dcirsResponseStatus'
  Int ->
  DeleteContainerImageResponse
deleteContainerImageResponse pResponseStatus_ =
  DeleteContainerImageResponse'
    { _dcirsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dcirsResponseStatus :: Lens' DeleteContainerImageResponse Int
dcirsResponseStatus = lens _dcirsResponseStatus (\s a -> s {_dcirsResponseStatus = a})

instance NFData DeleteContainerImageResponse
