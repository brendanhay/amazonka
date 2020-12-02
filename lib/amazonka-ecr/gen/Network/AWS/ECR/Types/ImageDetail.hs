{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.ImageDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.ImageDetail where

import Network.AWS.ECR.Types.ImageScanFindingsSummary
import Network.AWS.ECR.Types.ImageScanStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that describes an image returned by a 'DescribeImages' operation.
--
--
--
-- /See:/ 'imageDetail' smart constructor.
data ImageDetail = ImageDetail'
  { _idRegistryId :: !(Maybe Text),
    _idImageTags :: !(Maybe [Text]),
    _idImageScanStatus :: !(Maybe ImageScanStatus),
    _idImageManifestMediaType :: !(Maybe Text),
    _idImageSizeInBytes :: !(Maybe Integer),
    _idImageDigest :: !(Maybe Text),
    _idImageScanFindingsSummary :: !(Maybe ImageScanFindingsSummary),
    _idArtifactMediaType :: !(Maybe Text),
    _idImagePushedAt :: !(Maybe POSIX),
    _idRepositoryName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ImageDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'idRegistryId' - The AWS account ID associated with the registry to which this image belongs.
--
-- * 'idImageTags' - The list of tags associated with this image.
--
-- * 'idImageScanStatus' - The current state of the scan.
--
-- * 'idImageManifestMediaType' - The media type of the image manifest.
--
-- * 'idImageSizeInBytes' - The size, in bytes, of the image in the repository. If the image is a manifest list, this will be the max size of all manifests in the list.
--
-- * 'idImageDigest' - The @sha256@ digest of the image manifest.
--
-- * 'idImageScanFindingsSummary' - A summary of the last completed image scan.
--
-- * 'idArtifactMediaType' - The artifact media type of the image.
--
-- * 'idImagePushedAt' - The date and time, expressed in standard JavaScript date format, at which the current image was pushed to the repository.
--
-- * 'idRepositoryName' - The name of the repository to which this image belongs.
imageDetail ::
  ImageDetail
imageDetail =
  ImageDetail'
    { _idRegistryId = Nothing,
      _idImageTags = Nothing,
      _idImageScanStatus = Nothing,
      _idImageManifestMediaType = Nothing,
      _idImageSizeInBytes = Nothing,
      _idImageDigest = Nothing,
      _idImageScanFindingsSummary = Nothing,
      _idArtifactMediaType = Nothing,
      _idImagePushedAt = Nothing,
      _idRepositoryName = Nothing
    }

-- | The AWS account ID associated with the registry to which this image belongs.
idRegistryId :: Lens' ImageDetail (Maybe Text)
idRegistryId = lens _idRegistryId (\s a -> s {_idRegistryId = a})

-- | The list of tags associated with this image.
idImageTags :: Lens' ImageDetail [Text]
idImageTags = lens _idImageTags (\s a -> s {_idImageTags = a}) . _Default . _Coerce

-- | The current state of the scan.
idImageScanStatus :: Lens' ImageDetail (Maybe ImageScanStatus)
idImageScanStatus = lens _idImageScanStatus (\s a -> s {_idImageScanStatus = a})

-- | The media type of the image manifest.
idImageManifestMediaType :: Lens' ImageDetail (Maybe Text)
idImageManifestMediaType = lens _idImageManifestMediaType (\s a -> s {_idImageManifestMediaType = a})

-- | The size, in bytes, of the image in the repository. If the image is a manifest list, this will be the max size of all manifests in the list.
idImageSizeInBytes :: Lens' ImageDetail (Maybe Integer)
idImageSizeInBytes = lens _idImageSizeInBytes (\s a -> s {_idImageSizeInBytes = a})

-- | The @sha256@ digest of the image manifest.
idImageDigest :: Lens' ImageDetail (Maybe Text)
idImageDigest = lens _idImageDigest (\s a -> s {_idImageDigest = a})

-- | A summary of the last completed image scan.
idImageScanFindingsSummary :: Lens' ImageDetail (Maybe ImageScanFindingsSummary)
idImageScanFindingsSummary = lens _idImageScanFindingsSummary (\s a -> s {_idImageScanFindingsSummary = a})

-- | The artifact media type of the image.
idArtifactMediaType :: Lens' ImageDetail (Maybe Text)
idArtifactMediaType = lens _idArtifactMediaType (\s a -> s {_idArtifactMediaType = a})

-- | The date and time, expressed in standard JavaScript date format, at which the current image was pushed to the repository.
idImagePushedAt :: Lens' ImageDetail (Maybe UTCTime)
idImagePushedAt = lens _idImagePushedAt (\s a -> s {_idImagePushedAt = a}) . mapping _Time

-- | The name of the repository to which this image belongs.
idRepositoryName :: Lens' ImageDetail (Maybe Text)
idRepositoryName = lens _idRepositoryName (\s a -> s {_idRepositoryName = a})

instance FromJSON ImageDetail where
  parseJSON =
    withObject
      "ImageDetail"
      ( \x ->
          ImageDetail'
            <$> (x .:? "registryId")
            <*> (x .:? "imageTags" .!= mempty)
            <*> (x .:? "imageScanStatus")
            <*> (x .:? "imageManifestMediaType")
            <*> (x .:? "imageSizeInBytes")
            <*> (x .:? "imageDigest")
            <*> (x .:? "imageScanFindingsSummary")
            <*> (x .:? "artifactMediaType")
            <*> (x .:? "imagePushedAt")
            <*> (x .:? "repositoryName")
      )

instance Hashable ImageDetail

instance NFData ImageDetail
