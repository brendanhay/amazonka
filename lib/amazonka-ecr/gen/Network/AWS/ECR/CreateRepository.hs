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
-- Module      : Network.AWS.ECR.CreateRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a repository. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/Repositories.html Amazon ECR Repositories> in the /Amazon Elastic Container Registry User Guide/ .
module Network.AWS.ECR.CreateRepository
  ( -- * Creating a Request
    createRepository,
    CreateRepository,

    -- * Request Lenses
    crImageScanningConfiguration,
    crEncryptionConfiguration,
    crImageTagMutability,
    crTags,
    crRepositoryName,

    -- * Destructuring the Response
    createRepositoryResponse,
    CreateRepositoryResponse,

    -- * Response Lenses
    crrsRepository,
    crrsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createRepository' smart constructor.
data CreateRepository = CreateRepository'
  { _crImageScanningConfiguration ::
      !(Maybe ImageScanningConfiguration),
    _crEncryptionConfiguration ::
      !(Maybe EncryptionConfiguration),
    _crImageTagMutability :: !(Maybe ImageTagMutability),
    _crTags :: !(Maybe [Tag]),
    _crRepositoryName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateRepository' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crImageScanningConfiguration' - The image scanning configuration for the repository. This determines whether images are scanned for known vulnerabilities after being pushed to the repository.
--
-- * 'crEncryptionConfiguration' - The encryption configuration for the repository. This determines how the contents of your repository are encrypted at rest.
--
-- * 'crImageTagMutability' - The tag mutability setting for the repository. If this parameter is omitted, the default setting of @MUTABLE@ will be used which will allow image tags to be overwritten. If @IMMUTABLE@ is specified, all image tags within the repository will be immutable which will prevent them from being overwritten.
--
-- * 'crTags' - The metadata that you apply to the repository to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
--
-- * 'crRepositoryName' - The name to use for the repository. The repository name may be specified on its own (such as @nginx-web-app@ ) or it can be prepended with a namespace to group the repository into a category (such as @project-a/nginx-web-app@ ).
createRepository ::
  -- | 'crRepositoryName'
  Text ->
  CreateRepository
createRepository pRepositoryName_ =
  CreateRepository'
    { _crImageScanningConfiguration = Nothing,
      _crEncryptionConfiguration = Nothing,
      _crImageTagMutability = Nothing,
      _crTags = Nothing,
      _crRepositoryName = pRepositoryName_
    }

-- | The image scanning configuration for the repository. This determines whether images are scanned for known vulnerabilities after being pushed to the repository.
crImageScanningConfiguration :: Lens' CreateRepository (Maybe ImageScanningConfiguration)
crImageScanningConfiguration = lens _crImageScanningConfiguration (\s a -> s {_crImageScanningConfiguration = a})

-- | The encryption configuration for the repository. This determines how the contents of your repository are encrypted at rest.
crEncryptionConfiguration :: Lens' CreateRepository (Maybe EncryptionConfiguration)
crEncryptionConfiguration = lens _crEncryptionConfiguration (\s a -> s {_crEncryptionConfiguration = a})

-- | The tag mutability setting for the repository. If this parameter is omitted, the default setting of @MUTABLE@ will be used which will allow image tags to be overwritten. If @IMMUTABLE@ is specified, all image tags within the repository will be immutable which will prevent them from being overwritten.
crImageTagMutability :: Lens' CreateRepository (Maybe ImageTagMutability)
crImageTagMutability = lens _crImageTagMutability (\s a -> s {_crImageTagMutability = a})

-- | The metadata that you apply to the repository to help you categorize and organize them. Each tag consists of a key and an optional value, both of which you define. Tag keys can have a maximum character length of 128 characters, and tag values can have a maximum length of 256 characters.
crTags :: Lens' CreateRepository [Tag]
crTags = lens _crTags (\s a -> s {_crTags = a}) . _Default . _Coerce

-- | The name to use for the repository. The repository name may be specified on its own (such as @nginx-web-app@ ) or it can be prepended with a namespace to group the repository into a category (such as @project-a/nginx-web-app@ ).
crRepositoryName :: Lens' CreateRepository Text
crRepositoryName = lens _crRepositoryName (\s a -> s {_crRepositoryName = a})

instance AWSRequest CreateRepository where
  type Rs CreateRepository = CreateRepositoryResponse
  request = postJSON ecr
  response =
    receiveJSON
      ( \s h x ->
          CreateRepositoryResponse'
            <$> (x .?> "repository") <*> (pure (fromEnum s))
      )

instance Hashable CreateRepository

instance NFData CreateRepository

instance ToHeaders CreateRepository where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AmazonEC2ContainerRegistry_V20150921.CreateRepository" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateRepository where
  toJSON CreateRepository' {..} =
    object
      ( catMaybes
          [ ("imageScanningConfiguration" .=)
              <$> _crImageScanningConfiguration,
            ("encryptionConfiguration" .=) <$> _crEncryptionConfiguration,
            ("imageTagMutability" .=) <$> _crImageTagMutability,
            ("tags" .=) <$> _crTags,
            Just ("repositoryName" .= _crRepositoryName)
          ]
      )

instance ToPath CreateRepository where
  toPath = const "/"

instance ToQuery CreateRepository where
  toQuery = const mempty

-- | /See:/ 'createRepositoryResponse' smart constructor.
data CreateRepositoryResponse = CreateRepositoryResponse'
  { _crrsRepository ::
      !(Maybe Repository),
    _crrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateRepositoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crrsRepository' - The repository that was created.
--
-- * 'crrsResponseStatus' - -- | The response status code.
createRepositoryResponse ::
  -- | 'crrsResponseStatus'
  Int ->
  CreateRepositoryResponse
createRepositoryResponse pResponseStatus_ =
  CreateRepositoryResponse'
    { _crrsRepository = Nothing,
      _crrsResponseStatus = pResponseStatus_
    }

-- | The repository that was created.
crrsRepository :: Lens' CreateRepositoryResponse (Maybe Repository)
crrsRepository = lens _crrsRepository (\s a -> s {_crrsRepository = a})

-- | -- | The response status code.
crrsResponseStatus :: Lens' CreateRepositoryResponse Int
crrsResponseStatus = lens _crrsResponseStatus (\s a -> s {_crrsResponseStatus = a})

instance NFData CreateRepositoryResponse
