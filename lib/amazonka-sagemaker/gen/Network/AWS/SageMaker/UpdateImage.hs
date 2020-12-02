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
-- Module      : Network.AWS.SageMaker.UpdateImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the properties of a SageMaker image. To change the image's tags, use the 'AddTags' and 'DeleteTags' APIs.
module Network.AWS.SageMaker.UpdateImage
  ( -- * Creating a Request
    updateImage,
    UpdateImage,

    -- * Request Lenses
    uiDeleteProperties,
    uiDisplayName,
    uiDescription,
    uiRoleARN,
    uiImageName,

    -- * Destructuring the Response
    updateImageResponse,
    UpdateImageResponse,

    -- * Response Lenses
    uirsImageARN,
    uirsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'updateImage' smart constructor.
data UpdateImage = UpdateImage'
  { _uiDeleteProperties ::
      !(Maybe [Text]),
    _uiDisplayName :: !(Maybe Text),
    _uiDescription :: !(Maybe Text),
    _uiRoleARN :: !(Maybe Text),
    _uiImageName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiDeleteProperties' - A list of properties to delete. Only the @Description@ and @DisplayName@ properties can be deleted.
--
-- * 'uiDisplayName' - The new display name for the image.
--
-- * 'uiDescription' - The new description for the image.
--
-- * 'uiRoleARN' - The new Amazon Resource Name (ARN) for the IAM role that enables Amazon SageMaker to perform tasks on your behalf.
--
-- * 'uiImageName' - The name of the image to update.
updateImage ::
  -- | 'uiImageName'
  Text ->
  UpdateImage
updateImage pImageName_ =
  UpdateImage'
    { _uiDeleteProperties = Nothing,
      _uiDisplayName = Nothing,
      _uiDescription = Nothing,
      _uiRoleARN = Nothing,
      _uiImageName = pImageName_
    }

-- | A list of properties to delete. Only the @Description@ and @DisplayName@ properties can be deleted.
uiDeleteProperties :: Lens' UpdateImage [Text]
uiDeleteProperties = lens _uiDeleteProperties (\s a -> s {_uiDeleteProperties = a}) . _Default . _Coerce

-- | The new display name for the image.
uiDisplayName :: Lens' UpdateImage (Maybe Text)
uiDisplayName = lens _uiDisplayName (\s a -> s {_uiDisplayName = a})

-- | The new description for the image.
uiDescription :: Lens' UpdateImage (Maybe Text)
uiDescription = lens _uiDescription (\s a -> s {_uiDescription = a})

-- | The new Amazon Resource Name (ARN) for the IAM role that enables Amazon SageMaker to perform tasks on your behalf.
uiRoleARN :: Lens' UpdateImage (Maybe Text)
uiRoleARN = lens _uiRoleARN (\s a -> s {_uiRoleARN = a})

-- | The name of the image to update.
uiImageName :: Lens' UpdateImage Text
uiImageName = lens _uiImageName (\s a -> s {_uiImageName = a})

instance AWSRequest UpdateImage where
  type Rs UpdateImage = UpdateImageResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          UpdateImageResponse'
            <$> (x .?> "ImageArn") <*> (pure (fromEnum s))
      )

instance Hashable UpdateImage

instance NFData UpdateImage

instance ToHeaders UpdateImage where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("SageMaker.UpdateImage" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateImage where
  toJSON UpdateImage' {..} =
    object
      ( catMaybes
          [ ("DeleteProperties" .=) <$> _uiDeleteProperties,
            ("DisplayName" .=) <$> _uiDisplayName,
            ("Description" .=) <$> _uiDescription,
            ("RoleArn" .=) <$> _uiRoleARN,
            Just ("ImageName" .= _uiImageName)
          ]
      )

instance ToPath UpdateImage where
  toPath = const "/"

instance ToQuery UpdateImage where
  toQuery = const mempty

-- | /See:/ 'updateImageResponse' smart constructor.
data UpdateImageResponse = UpdateImageResponse'
  { _uirsImageARN ::
      !(Maybe Text),
    _uirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uirsImageARN' - The Amazon Resource Name (ARN) of the image.
--
-- * 'uirsResponseStatus' - -- | The response status code.
updateImageResponse ::
  -- | 'uirsResponseStatus'
  Int ->
  UpdateImageResponse
updateImageResponse pResponseStatus_ =
  UpdateImageResponse'
    { _uirsImageARN = Nothing,
      _uirsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the image.
uirsImageARN :: Lens' UpdateImageResponse (Maybe Text)
uirsImageARN = lens _uirsImageARN (\s a -> s {_uirsImageARN = a})

-- | -- | The response status code.
uirsResponseStatus :: Lens' UpdateImageResponse Int
uirsResponseStatus = lens _uirsResponseStatus (\s a -> s {_uirsResponseStatus = a})

instance NFData UpdateImageResponse
