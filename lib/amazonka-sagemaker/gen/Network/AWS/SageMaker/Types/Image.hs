{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.Image
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Image where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ImageStatus

-- | A SageMaker image. A SageMaker image represents a set of container images that are derived from a common base container image. Each of these container images is represented by a SageMaker @ImageVersion@ .
--
--
--
-- /See:/ 'image' smart constructor.
data Image = Image'
  { _iFailureReason :: !(Maybe Text),
    _iDisplayName :: !(Maybe Text),
    _iDescription :: !(Maybe Text),
    _iCreationTime :: !POSIX,
    _iImageARN :: !Text,
    _iImageName :: !Text,
    _iImageStatus :: !ImageStatus,
    _iLastModifiedTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Image' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iFailureReason' - When a create, update, or delete operation fails, the reason for the failure.
--
-- * 'iDisplayName' - The name of the image as displayed.
--
-- * 'iDescription' - The description of the image.
--
-- * 'iCreationTime' - When the image was created.
--
-- * 'iImageARN' - The Amazon Resource Name (ARN) of the image.
--
-- * 'iImageName' - The name of the image.
--
-- * 'iImageStatus' - The status of the image.
--
-- * 'iLastModifiedTime' - When the image was last modified.
image ::
  -- | 'iCreationTime'
  UTCTime ->
  -- | 'iImageARN'
  Text ->
  -- | 'iImageName'
  Text ->
  -- | 'iImageStatus'
  ImageStatus ->
  -- | 'iLastModifiedTime'
  UTCTime ->
  Image
image
  pCreationTime_
  pImageARN_
  pImageName_
  pImageStatus_
  pLastModifiedTime_ =
    Image'
      { _iFailureReason = Nothing,
        _iDisplayName = Nothing,
        _iDescription = Nothing,
        _iCreationTime = _Time # pCreationTime_,
        _iImageARN = pImageARN_,
        _iImageName = pImageName_,
        _iImageStatus = pImageStatus_,
        _iLastModifiedTime = _Time # pLastModifiedTime_
      }

-- | When a create, update, or delete operation fails, the reason for the failure.
iFailureReason :: Lens' Image (Maybe Text)
iFailureReason = lens _iFailureReason (\s a -> s {_iFailureReason = a})

-- | The name of the image as displayed.
iDisplayName :: Lens' Image (Maybe Text)
iDisplayName = lens _iDisplayName (\s a -> s {_iDisplayName = a})

-- | The description of the image.
iDescription :: Lens' Image (Maybe Text)
iDescription = lens _iDescription (\s a -> s {_iDescription = a})

-- | When the image was created.
iCreationTime :: Lens' Image UTCTime
iCreationTime = lens _iCreationTime (\s a -> s {_iCreationTime = a}) . _Time

-- | The Amazon Resource Name (ARN) of the image.
iImageARN :: Lens' Image Text
iImageARN = lens _iImageARN (\s a -> s {_iImageARN = a})

-- | The name of the image.
iImageName :: Lens' Image Text
iImageName = lens _iImageName (\s a -> s {_iImageName = a})

-- | The status of the image.
iImageStatus :: Lens' Image ImageStatus
iImageStatus = lens _iImageStatus (\s a -> s {_iImageStatus = a})

-- | When the image was last modified.
iLastModifiedTime :: Lens' Image UTCTime
iLastModifiedTime = lens _iLastModifiedTime (\s a -> s {_iLastModifiedTime = a}) . _Time

instance FromJSON Image where
  parseJSON =
    withObject
      "Image"
      ( \x ->
          Image'
            <$> (x .:? "FailureReason")
            <*> (x .:? "DisplayName")
            <*> (x .:? "Description")
            <*> (x .: "CreationTime")
            <*> (x .: "ImageArn")
            <*> (x .: "ImageName")
            <*> (x .: "ImageStatus")
            <*> (x .: "LastModifiedTime")
      )

instance Hashable Image

instance NFData Image
