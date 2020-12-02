{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.Image
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.Image where

import Network.AWS.AppStream.Types.Application
import Network.AWS.AppStream.Types.ImagePermissions
import Network.AWS.AppStream.Types.ImageState
import Network.AWS.AppStream.Types.ImageStateChangeReason
import Network.AWS.AppStream.Types.PlatformType
import Network.AWS.AppStream.Types.VisibilityType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an image.
--
--
--
-- /See:/ 'image' smart constructor.
data Image = Image'
  { _iState :: !(Maybe ImageState),
    _iImagePermissions :: !(Maybe ImagePermissions),
    _iPlatform :: !(Maybe PlatformType),
    _iPublicBaseImageReleasedDate :: !(Maybe POSIX),
    _iStateChangeReason :: !(Maybe ImageStateChangeReason),
    _iARN :: !(Maybe Text),
    _iCreatedTime :: !(Maybe POSIX),
    _iImageBuilderSupported :: !(Maybe Bool),
    _iVisibility :: !(Maybe VisibilityType),
    _iImageBuilderName :: !(Maybe Text),
    _iBaseImageARN :: !(Maybe Text),
    _iDisplayName :: !(Maybe Text),
    _iDescription :: !(Maybe Text),
    _iAppstreamAgentVersion :: !(Maybe Text),
    _iApplications :: !(Maybe [Application]),
    _iName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Image' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iState' - The image starts in the @PENDING@ state. If image creation succeeds, the state is @AVAILABLE@ . If image creation fails, the state is @FAILED@ .
--
-- * 'iImagePermissions' - The permissions to provide to the destination AWS account for the specified image.
--
-- * 'iPlatform' - The operating system platform of the image.
--
-- * 'iPublicBaseImageReleasedDate' - The release date of the public base image. For private images, this date is the release date of the base image from which the image was created.
--
-- * 'iStateChangeReason' - The reason why the last state change occurred.
--
-- * 'iARN' - The ARN of the image.
--
-- * 'iCreatedTime' - The time the image was created.
--
-- * 'iImageBuilderSupported' - Indicates whether an image builder can be launched from this image.
--
-- * 'iVisibility' - Indicates whether the image is public or private.
--
-- * 'iImageBuilderName' - The name of the image builder that was used to create the private image. If the image is shared, this value is null.
--
-- * 'iBaseImageARN' - The ARN of the image from which this image was created.
--
-- * 'iDisplayName' - The image name to display.
--
-- * 'iDescription' - The description to display.
--
-- * 'iAppstreamAgentVersion' - The version of the AppStream 2.0 agent to use for instances that are launched from this image.
--
-- * 'iApplications' - The applications associated with the image.
--
-- * 'iName' - The name of the image.
image ::
  -- | 'iName'
  Text ->
  Image
image pName_ =
  Image'
    { _iState = Nothing,
      _iImagePermissions = Nothing,
      _iPlatform = Nothing,
      _iPublicBaseImageReleasedDate = Nothing,
      _iStateChangeReason = Nothing,
      _iARN = Nothing,
      _iCreatedTime = Nothing,
      _iImageBuilderSupported = Nothing,
      _iVisibility = Nothing,
      _iImageBuilderName = Nothing,
      _iBaseImageARN = Nothing,
      _iDisplayName = Nothing,
      _iDescription = Nothing,
      _iAppstreamAgentVersion = Nothing,
      _iApplications = Nothing,
      _iName = pName_
    }

-- | The image starts in the @PENDING@ state. If image creation succeeds, the state is @AVAILABLE@ . If image creation fails, the state is @FAILED@ .
iState :: Lens' Image (Maybe ImageState)
iState = lens _iState (\s a -> s {_iState = a})

-- | The permissions to provide to the destination AWS account for the specified image.
iImagePermissions :: Lens' Image (Maybe ImagePermissions)
iImagePermissions = lens _iImagePermissions (\s a -> s {_iImagePermissions = a})

-- | The operating system platform of the image.
iPlatform :: Lens' Image (Maybe PlatformType)
iPlatform = lens _iPlatform (\s a -> s {_iPlatform = a})

-- | The release date of the public base image. For private images, this date is the release date of the base image from which the image was created.
iPublicBaseImageReleasedDate :: Lens' Image (Maybe UTCTime)
iPublicBaseImageReleasedDate = lens _iPublicBaseImageReleasedDate (\s a -> s {_iPublicBaseImageReleasedDate = a}) . mapping _Time

-- | The reason why the last state change occurred.
iStateChangeReason :: Lens' Image (Maybe ImageStateChangeReason)
iStateChangeReason = lens _iStateChangeReason (\s a -> s {_iStateChangeReason = a})

-- | The ARN of the image.
iARN :: Lens' Image (Maybe Text)
iARN = lens _iARN (\s a -> s {_iARN = a})

-- | The time the image was created.
iCreatedTime :: Lens' Image (Maybe UTCTime)
iCreatedTime = lens _iCreatedTime (\s a -> s {_iCreatedTime = a}) . mapping _Time

-- | Indicates whether an image builder can be launched from this image.
iImageBuilderSupported :: Lens' Image (Maybe Bool)
iImageBuilderSupported = lens _iImageBuilderSupported (\s a -> s {_iImageBuilderSupported = a})

-- | Indicates whether the image is public or private.
iVisibility :: Lens' Image (Maybe VisibilityType)
iVisibility = lens _iVisibility (\s a -> s {_iVisibility = a})

-- | The name of the image builder that was used to create the private image. If the image is shared, this value is null.
iImageBuilderName :: Lens' Image (Maybe Text)
iImageBuilderName = lens _iImageBuilderName (\s a -> s {_iImageBuilderName = a})

-- | The ARN of the image from which this image was created.
iBaseImageARN :: Lens' Image (Maybe Text)
iBaseImageARN = lens _iBaseImageARN (\s a -> s {_iBaseImageARN = a})

-- | The image name to display.
iDisplayName :: Lens' Image (Maybe Text)
iDisplayName = lens _iDisplayName (\s a -> s {_iDisplayName = a})

-- | The description to display.
iDescription :: Lens' Image (Maybe Text)
iDescription = lens _iDescription (\s a -> s {_iDescription = a})

-- | The version of the AppStream 2.0 agent to use for instances that are launched from this image.
iAppstreamAgentVersion :: Lens' Image (Maybe Text)
iAppstreamAgentVersion = lens _iAppstreamAgentVersion (\s a -> s {_iAppstreamAgentVersion = a})

-- | The applications associated with the image.
iApplications :: Lens' Image [Application]
iApplications = lens _iApplications (\s a -> s {_iApplications = a}) . _Default . _Coerce

-- | The name of the image.
iName :: Lens' Image Text
iName = lens _iName (\s a -> s {_iName = a})

instance FromJSON Image where
  parseJSON =
    withObject
      "Image"
      ( \x ->
          Image'
            <$> (x .:? "State")
            <*> (x .:? "ImagePermissions")
            <*> (x .:? "Platform")
            <*> (x .:? "PublicBaseImageReleasedDate")
            <*> (x .:? "StateChangeReason")
            <*> (x .:? "Arn")
            <*> (x .:? "CreatedTime")
            <*> (x .:? "ImageBuilderSupported")
            <*> (x .:? "Visibility")
            <*> (x .:? "ImageBuilderName")
            <*> (x .:? "BaseImageArn")
            <*> (x .:? "DisplayName")
            <*> (x .:? "Description")
            <*> (x .:? "AppstreamAgentVersion")
            <*> (x .:? "Applications" .!= mempty)
            <*> (x .: "Name")
      )

instance Hashable Image

instance NFData Image
