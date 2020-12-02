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
-- Module      : Network.AWS.WorkSpaces.CopyWorkspaceImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified image from the specified Region to the current Region. For more information about copying images, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/copy-custom-image.html Copy a Custom WorkSpaces Image> .
--
--
-- /Important:/ Before copying a shared image, be sure to verify that it has been shared from the correct AWS account. To determine if an image has been shared and to see the AWS account ID that owns an image, use the <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceImages.html DescribeWorkSpaceImages> and <https://docs.aws.amazon.com/workspaces/latest/api/API_DescribeWorkspaceImagePermissions.html DescribeWorkspaceImagePermissions> API operations.
module Network.AWS.WorkSpaces.CopyWorkspaceImage
  ( -- * Creating a Request
    copyWorkspaceImage,
    CopyWorkspaceImage,

    -- * Request Lenses
    cwiDescription,
    cwiTags,
    cwiName,
    cwiSourceImageId,
    cwiSourceRegion,

    -- * Destructuring the Response
    copyWorkspaceImageResponse,
    CopyWorkspaceImageResponse,

    -- * Response Lenses
    cwirsImageId,
    cwirsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'copyWorkspaceImage' smart constructor.
data CopyWorkspaceImage = CopyWorkspaceImage'
  { _cwiDescription ::
      !(Maybe Text),
    _cwiTags :: !(Maybe [Tag]),
    _cwiName :: !Text,
    _cwiSourceImageId :: !Text,
    _cwiSourceRegion :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CopyWorkspaceImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwiDescription' - A description of the image.
--
-- * 'cwiTags' - The tags for the image.
--
-- * 'cwiName' - The name of the image.
--
-- * 'cwiSourceImageId' - The identifier of the source image.
--
-- * 'cwiSourceRegion' - The identifier of the source Region.
copyWorkspaceImage ::
  -- | 'cwiName'
  Text ->
  -- | 'cwiSourceImageId'
  Text ->
  -- | 'cwiSourceRegion'
  Text ->
  CopyWorkspaceImage
copyWorkspaceImage pName_ pSourceImageId_ pSourceRegion_ =
  CopyWorkspaceImage'
    { _cwiDescription = Nothing,
      _cwiTags = Nothing,
      _cwiName = pName_,
      _cwiSourceImageId = pSourceImageId_,
      _cwiSourceRegion = pSourceRegion_
    }

-- | A description of the image.
cwiDescription :: Lens' CopyWorkspaceImage (Maybe Text)
cwiDescription = lens _cwiDescription (\s a -> s {_cwiDescription = a})

-- | The tags for the image.
cwiTags :: Lens' CopyWorkspaceImage [Tag]
cwiTags = lens _cwiTags (\s a -> s {_cwiTags = a}) . _Default . _Coerce

-- | The name of the image.
cwiName :: Lens' CopyWorkspaceImage Text
cwiName = lens _cwiName (\s a -> s {_cwiName = a})

-- | The identifier of the source image.
cwiSourceImageId :: Lens' CopyWorkspaceImage Text
cwiSourceImageId = lens _cwiSourceImageId (\s a -> s {_cwiSourceImageId = a})

-- | The identifier of the source Region.
cwiSourceRegion :: Lens' CopyWorkspaceImage Text
cwiSourceRegion = lens _cwiSourceRegion (\s a -> s {_cwiSourceRegion = a})

instance AWSRequest CopyWorkspaceImage where
  type Rs CopyWorkspaceImage = CopyWorkspaceImageResponse
  request = postJSON workSpaces
  response =
    receiveJSON
      ( \s h x ->
          CopyWorkspaceImageResponse'
            <$> (x .?> "ImageId") <*> (pure (fromEnum s))
      )

instance Hashable CopyWorkspaceImage

instance NFData CopyWorkspaceImage

instance ToHeaders CopyWorkspaceImage where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkspacesService.CopyWorkspaceImage" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CopyWorkspaceImage where
  toJSON CopyWorkspaceImage' {..} =
    object
      ( catMaybes
          [ ("Description" .=) <$> _cwiDescription,
            ("Tags" .=) <$> _cwiTags,
            Just ("Name" .= _cwiName),
            Just ("SourceImageId" .= _cwiSourceImageId),
            Just ("SourceRegion" .= _cwiSourceRegion)
          ]
      )

instance ToPath CopyWorkspaceImage where
  toPath = const "/"

instance ToQuery CopyWorkspaceImage where
  toQuery = const mempty

-- | /See:/ 'copyWorkspaceImageResponse' smart constructor.
data CopyWorkspaceImageResponse = CopyWorkspaceImageResponse'
  { _cwirsImageId ::
      !(Maybe Text),
    _cwirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CopyWorkspaceImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwirsImageId' - The identifier of the image.
--
-- * 'cwirsResponseStatus' - -- | The response status code.
copyWorkspaceImageResponse ::
  -- | 'cwirsResponseStatus'
  Int ->
  CopyWorkspaceImageResponse
copyWorkspaceImageResponse pResponseStatus_ =
  CopyWorkspaceImageResponse'
    { _cwirsImageId = Nothing,
      _cwirsResponseStatus = pResponseStatus_
    }

-- | The identifier of the image.
cwirsImageId :: Lens' CopyWorkspaceImageResponse (Maybe Text)
cwirsImageId = lens _cwirsImageId (\s a -> s {_cwirsImageId = a})

-- | -- | The response status code.
cwirsResponseStatus :: Lens' CopyWorkspaceImageResponse Int
cwirsResponseStatus = lens _cwirsResponseStatus (\s a -> s {_cwirsResponseStatus = a})

instance NFData CopyWorkspaceImageResponse
