{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- The repository can be hosted either in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository.
module Network.AWS.SageMaker.CreateCodeRepository
  ( -- * Creating a request
    CreateCodeRepository (..),
    mkCreateCodeRepository,

    -- ** Request lenses
    ccrCodeRepositoryName,
    ccrGitConfig,

    -- * Destructuring the response
    CreateCodeRepositoryResponse (..),
    mkCreateCodeRepositoryResponse,

    -- ** Response lenses
    ccrrsResponseStatus,
    ccrrsCodeRepositoryARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateCodeRepository' smart constructor.
data CreateCodeRepository = CreateCodeRepository'
  { codeRepositoryName ::
      Lude.Text,
    gitConfig :: GitConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCodeRepository' with the minimum fields required to make a request.
--
-- * 'codeRepositoryName' - The name of the Git repository. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
-- * 'gitConfig' - Specifies details about the repository, including the URL where the repository is located, the default branch, and credentials to use to access the repository.
mkCreateCodeRepository ::
  -- | 'codeRepositoryName'
  Lude.Text ->
  -- | 'gitConfig'
  GitConfig ->
  CreateCodeRepository
mkCreateCodeRepository pCodeRepositoryName_ pGitConfig_ =
  CreateCodeRepository'
    { codeRepositoryName = pCodeRepositoryName_,
      gitConfig = pGitConfig_
    }

-- | The name of the Git repository. The name must have 1 to 63 characters. Valid characters are a-z, A-Z, 0-9, and - (hyphen).
--
-- /Note:/ Consider using 'codeRepositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrCodeRepositoryName :: Lens.Lens' CreateCodeRepository Lude.Text
ccrCodeRepositoryName = Lens.lens (codeRepositoryName :: CreateCodeRepository -> Lude.Text) (\s a -> s {codeRepositoryName = a} :: CreateCodeRepository)
{-# DEPRECATED ccrCodeRepositoryName "Use generic-lens or generic-optics with 'codeRepositoryName' instead." #-}

-- | Specifies details about the repository, including the URL where the repository is located, the default branch, and credentials to use to access the repository.
--
-- /Note:/ Consider using 'gitConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrGitConfig :: Lens.Lens' CreateCodeRepository GitConfig
ccrGitConfig = Lens.lens (gitConfig :: CreateCodeRepository -> GitConfig) (\s a -> s {gitConfig = a} :: CreateCodeRepository)
{-# DEPRECATED ccrGitConfig "Use generic-lens or generic-optics with 'gitConfig' instead." #-}

instance Lude.AWSRequest CreateCodeRepository where
  type Rs CreateCodeRepository = CreateCodeRepositoryResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCodeRepositoryResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "CodeRepositoryArn")
      )

instance Lude.ToHeaders CreateCodeRepository where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateCodeRepository" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateCodeRepository where
  toJSON CreateCodeRepository' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("CodeRepositoryName" Lude..= codeRepositoryName),
            Lude.Just ("GitConfig" Lude..= gitConfig)
          ]
      )

instance Lude.ToPath CreateCodeRepository where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateCodeRepository where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateCodeRepositoryResponse' smart constructor.
data CreateCodeRepositoryResponse = CreateCodeRepositoryResponse'
  { responseStatus ::
      Lude.Int,
    codeRepositoryARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCodeRepositoryResponse' with the minimum fields required to make a request.
--
-- * 'codeRepositoryARN' - The Amazon Resource Name (ARN) of the new repository.
-- * 'responseStatus' - The response status code.
mkCreateCodeRepositoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'codeRepositoryARN'
  Lude.Text ->
  CreateCodeRepositoryResponse
mkCreateCodeRepositoryResponse pResponseStatus_ pCodeRepositoryARN_ =
  CreateCodeRepositoryResponse'
    { responseStatus = pResponseStatus_,
      codeRepositoryARN = pCodeRepositoryARN_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsResponseStatus :: Lens.Lens' CreateCodeRepositoryResponse Lude.Int
ccrrsResponseStatus = Lens.lens (responseStatus :: CreateCodeRepositoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCodeRepositoryResponse)
{-# DEPRECATED ccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the new repository.
--
-- /Note:/ Consider using 'codeRepositoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrrsCodeRepositoryARN :: Lens.Lens' CreateCodeRepositoryResponse Lude.Text
ccrrsCodeRepositoryARN = Lens.lens (codeRepositoryARN :: CreateCodeRepositoryResponse -> Lude.Text) (\s a -> s {codeRepositoryARN = a} :: CreateCodeRepositoryResponse)
{-# DEPRECATED ccrrsCodeRepositoryARN "Use generic-lens or generic-optics with 'codeRepositoryARN' instead." #-}
