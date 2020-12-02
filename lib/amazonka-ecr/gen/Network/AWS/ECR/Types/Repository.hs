{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.Repository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.Repository where

import Network.AWS.ECR.Types.EncryptionConfiguration
import Network.AWS.ECR.Types.ImageScanningConfiguration
import Network.AWS.ECR.Types.ImageTagMutability
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing a repository.
--
--
--
-- /See:/ 'repository' smart constructor.
data Repository = Repository'
  { _rRepositoryARN :: !(Maybe Text),
    _rCreatedAt :: !(Maybe POSIX),
    _rRegistryId :: !(Maybe Text),
    _rImageScanningConfiguration ::
      !(Maybe ImageScanningConfiguration),
    _rRepositoryURI :: !(Maybe Text),
    _rEncryptionConfiguration :: !(Maybe EncryptionConfiguration),
    _rRepositoryName :: !(Maybe Text),
    _rImageTagMutability :: !(Maybe ImageTagMutability)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Repository' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rRepositoryARN' - The Amazon Resource Name (ARN) that identifies the repository. The ARN contains the @arn:aws:ecr@ namespace, followed by the region of the repository, AWS account ID of the repository owner, repository namespace, and repository name. For example, @arn:aws:ecr:region:012345678910:repository/test@ .
--
-- * 'rCreatedAt' - The date and time, in JavaScript date format, when the repository was created.
--
-- * 'rRegistryId' - The AWS account ID associated with the registry that contains the repository.
--
-- * 'rImageScanningConfiguration' - Undocumented member.
--
-- * 'rRepositoryURI' - The URI for the repository. You can use this URI for container image @push@ and @pull@ operations.
--
-- * 'rEncryptionConfiguration' - The encryption configuration for the repository. This determines how the contents of your repository are encrypted at rest.
--
-- * 'rRepositoryName' - The name of the repository.
--
-- * 'rImageTagMutability' - The tag mutability setting for the repository.
repository ::
  Repository
repository =
  Repository'
    { _rRepositoryARN = Nothing,
      _rCreatedAt = Nothing,
      _rRegistryId = Nothing,
      _rImageScanningConfiguration = Nothing,
      _rRepositoryURI = Nothing,
      _rEncryptionConfiguration = Nothing,
      _rRepositoryName = Nothing,
      _rImageTagMutability = Nothing
    }

-- | The Amazon Resource Name (ARN) that identifies the repository. The ARN contains the @arn:aws:ecr@ namespace, followed by the region of the repository, AWS account ID of the repository owner, repository namespace, and repository name. For example, @arn:aws:ecr:region:012345678910:repository/test@ .
rRepositoryARN :: Lens' Repository (Maybe Text)
rRepositoryARN = lens _rRepositoryARN (\s a -> s {_rRepositoryARN = a})

-- | The date and time, in JavaScript date format, when the repository was created.
rCreatedAt :: Lens' Repository (Maybe UTCTime)
rCreatedAt = lens _rCreatedAt (\s a -> s {_rCreatedAt = a}) . mapping _Time

-- | The AWS account ID associated with the registry that contains the repository.
rRegistryId :: Lens' Repository (Maybe Text)
rRegistryId = lens _rRegistryId (\s a -> s {_rRegistryId = a})

-- | Undocumented member.
rImageScanningConfiguration :: Lens' Repository (Maybe ImageScanningConfiguration)
rImageScanningConfiguration = lens _rImageScanningConfiguration (\s a -> s {_rImageScanningConfiguration = a})

-- | The URI for the repository. You can use this URI for container image @push@ and @pull@ operations.
rRepositoryURI :: Lens' Repository (Maybe Text)
rRepositoryURI = lens _rRepositoryURI (\s a -> s {_rRepositoryURI = a})

-- | The encryption configuration for the repository. This determines how the contents of your repository are encrypted at rest.
rEncryptionConfiguration :: Lens' Repository (Maybe EncryptionConfiguration)
rEncryptionConfiguration = lens _rEncryptionConfiguration (\s a -> s {_rEncryptionConfiguration = a})

-- | The name of the repository.
rRepositoryName :: Lens' Repository (Maybe Text)
rRepositoryName = lens _rRepositoryName (\s a -> s {_rRepositoryName = a})

-- | The tag mutability setting for the repository.
rImageTagMutability :: Lens' Repository (Maybe ImageTagMutability)
rImageTagMutability = lens _rImageTagMutability (\s a -> s {_rImageTagMutability = a})

instance FromJSON Repository where
  parseJSON =
    withObject
      "Repository"
      ( \x ->
          Repository'
            <$> (x .:? "repositoryArn")
            <*> (x .:? "createdAt")
            <*> (x .:? "registryId")
            <*> (x .:? "imageScanningConfiguration")
            <*> (x .:? "repositoryUri")
            <*> (x .:? "encryptionConfiguration")
            <*> (x .:? "repositoryName")
            <*> (x .:? "imageTagMutability")
      )

instance Hashable Repository

instance NFData Repository
