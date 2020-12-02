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
-- Module      : Network.AWS.SageMaker.DeleteImageVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a version of a SageMaker image. The container image the version represents isn't deleted.
module Network.AWS.SageMaker.DeleteImageVersion
  ( -- * Creating a Request
    deleteImageVersion,
    DeleteImageVersion,

    -- * Request Lenses
    divImageName,
    divVersion,

    -- * Destructuring the Response
    deleteImageVersionResponse,
    DeleteImageVersionResponse,

    -- * Response Lenses
    divrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'deleteImageVersion' smart constructor.
data DeleteImageVersion = DeleteImageVersion'
  { _divImageName ::
      !Text,
    _divVersion :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteImageVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'divImageName' - The name of the image.
--
-- * 'divVersion' - The version to delete.
deleteImageVersion ::
  -- | 'divImageName'
  Text ->
  -- | 'divVersion'
  Natural ->
  DeleteImageVersion
deleteImageVersion pImageName_ pVersion_ =
  DeleteImageVersion'
    { _divImageName = pImageName_,
      _divVersion = _Nat # pVersion_
    }

-- | The name of the image.
divImageName :: Lens' DeleteImageVersion Text
divImageName = lens _divImageName (\s a -> s {_divImageName = a})

-- | The version to delete.
divVersion :: Lens' DeleteImageVersion Natural
divVersion = lens _divVersion (\s a -> s {_divVersion = a}) . _Nat

instance AWSRequest DeleteImageVersion where
  type Rs DeleteImageVersion = DeleteImageVersionResponse
  request = postJSON sageMaker
  response =
    receiveEmpty
      (\s h x -> DeleteImageVersionResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteImageVersion

instance NFData DeleteImageVersion

instance ToHeaders DeleteImageVersion where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.DeleteImageVersion" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteImageVersion where
  toJSON DeleteImageVersion' {..} =
    object
      ( catMaybes
          [ Just ("ImageName" .= _divImageName),
            Just ("Version" .= _divVersion)
          ]
      )

instance ToPath DeleteImageVersion where
  toPath = const "/"

instance ToQuery DeleteImageVersion where
  toQuery = const mempty

-- | /See:/ 'deleteImageVersionResponse' smart constructor.
newtype DeleteImageVersionResponse = DeleteImageVersionResponse'
  { _divrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteImageVersionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'divrsResponseStatus' - -- | The response status code.
deleteImageVersionResponse ::
  -- | 'divrsResponseStatus'
  Int ->
  DeleteImageVersionResponse
deleteImageVersionResponse pResponseStatus_ =
  DeleteImageVersionResponse'
    { _divrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
divrsResponseStatus :: Lens' DeleteImageVersionResponse Int
divrsResponseStatus = lens _divrsResponseStatus (\s a -> s {_divrsResponseStatus = a})

instance NFData DeleteImageVersionResponse
