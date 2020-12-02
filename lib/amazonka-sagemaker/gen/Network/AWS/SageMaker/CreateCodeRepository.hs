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
-- Module      : Network.AWS.SageMaker.CreateCodeRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Git repository as a resource in your Amazon SageMaker account. You can associate the repository with notebook instances so that you can use Git source control for the notebooks you create. The Git repository is a resource in your Amazon SageMaker account, so it can be associated with more than one notebook instance, and it persists independently from the lifecycle of any notebook instances it is associated with.
--
--
-- The repository can be hosted either in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository.
module Network.AWS.SageMaker.CreateCodeRepository
  ( -- * Creating a Request
    createCodeRepository,
    CreateCodeRepository,

    -- * Request Lenses
    ccrCodeRepositoryName,
    ccrGitConfig,

    -- * Destructuring the Response
    createCodeRepositoryResponse,
    CreateCodeRepositoryResponse,

    -- * Response Lenses
    ccrrsResponseStatus,
    ccrrsCodeRepositoryARN,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'createCodeRepository' smart constructor.
data CreateCodeRepository = CreateCodeRepository'
  { _ccrCodeRepositoryName ::
      !Text,
    _ccrGitConfig :: !GitConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCodeRepository' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrCodeRepositoryName' - The name of the Git repository. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
--
-- * 'ccrGitConfig' - Specifies details about the repository, including the URL where the repository is located, the default branch, and credentials to use to access the repository.
createCodeRepository ::
  -- | 'ccrCodeRepositoryName'
  Text ->
  -- | 'ccrGitConfig'
  GitConfig ->
  CreateCodeRepository
createCodeRepository pCodeRepositoryName_ pGitConfig_ =
  CreateCodeRepository'
    { _ccrCodeRepositoryName =
        pCodeRepositoryName_,
      _ccrGitConfig = pGitConfig_
    }

-- | The name of the Git repository. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
ccrCodeRepositoryName :: Lens' CreateCodeRepository Text
ccrCodeRepositoryName = lens _ccrCodeRepositoryName (\s a -> s {_ccrCodeRepositoryName = a})

-- | Specifies details about the repository, including the URL where the repository is located, the default branch, and credentials to use to access the repository.
ccrGitConfig :: Lens' CreateCodeRepository GitConfig
ccrGitConfig = lens _ccrGitConfig (\s a -> s {_ccrGitConfig = a})

instance AWSRequest CreateCodeRepository where
  type Rs CreateCodeRepository = CreateCodeRepositoryResponse
  request = postJSON sageMaker
  response =
    receiveJSON
      ( \s h x ->
          CreateCodeRepositoryResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "CodeRepositoryArn")
      )

instance Hashable CreateCodeRepository

instance NFData CreateCodeRepository

instance ToHeaders CreateCodeRepository where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("SageMaker.CreateCodeRepository" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateCodeRepository where
  toJSON CreateCodeRepository' {..} =
    object
      ( catMaybes
          [ Just ("CodeRepositoryName" .= _ccrCodeRepositoryName),
            Just ("GitConfig" .= _ccrGitConfig)
          ]
      )

instance ToPath CreateCodeRepository where
  toPath = const "/"

instance ToQuery CreateCodeRepository where
  toQuery = const mempty

-- | /See:/ 'createCodeRepositoryResponse' smart constructor.
data CreateCodeRepositoryResponse = CreateCodeRepositoryResponse'
  { _ccrrsResponseStatus ::
      !Int,
    _ccrrsCodeRepositoryARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateCodeRepositoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccrrsResponseStatus' - -- | The response status code.
--
-- * 'ccrrsCodeRepositoryARN' - The Amazon Resource Name (ARN) of the new repository.
createCodeRepositoryResponse ::
  -- | 'ccrrsResponseStatus'
  Int ->
  -- | 'ccrrsCodeRepositoryARN'
  Text ->
  CreateCodeRepositoryResponse
createCodeRepositoryResponse pResponseStatus_ pCodeRepositoryARN_ =
  CreateCodeRepositoryResponse'
    { _ccrrsResponseStatus =
        pResponseStatus_,
      _ccrrsCodeRepositoryARN = pCodeRepositoryARN_
    }

-- | -- | The response status code.
ccrrsResponseStatus :: Lens' CreateCodeRepositoryResponse Int
ccrrsResponseStatus = lens _ccrrsResponseStatus (\s a -> s {_ccrrsResponseStatus = a})

-- | The Amazon Resource Name (ARN) of the new repository.
ccrrsCodeRepositoryARN :: Lens' CreateCodeRepositoryResponse Text
ccrrsCodeRepositoryARN = lens _ccrrsCodeRepositoryARN (\s a -> s {_ccrrsCodeRepositoryARN = a})

instance NFData CreateCodeRepositoryResponse
