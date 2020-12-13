{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.UpdateCodeRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified Git repository with the specified values.
module Network.AWS.SageMaker.UpdateCodeRepository
  ( -- * Creating a request
    UpdateCodeRepository (..),
    mkUpdateCodeRepository,

    -- ** Request lenses
    ucrCodeRepositoryName,
    ucrGitConfig,

    -- * Destructuring the response
    UpdateCodeRepositoryResponse (..),
    mkUpdateCodeRepositoryResponse,

    -- ** Response lenses
    ucrrsCodeRepositoryARN,
    ucrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkUpdateCodeRepository' smart constructor.
data UpdateCodeRepository = UpdateCodeRepository'
  { -- | The name of the Git repository to update.
    codeRepositoryName :: Lude.Text,
    -- | The configuration of the git repository, including the URL and the Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the repository. The secret must have a staging label of @AWSCURRENT@ and must be in the following format:
    --
    -- @{"username": /UserName/ , "password": /Password/ }@
    gitConfig :: Lude.Maybe GitConfigForUpdate
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCodeRepository' with the minimum fields required to make a request.
--
-- * 'codeRepositoryName' - The name of the Git repository to update.
-- * 'gitConfig' - The configuration of the git repository, including the URL and the Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the repository. The secret must have a staging label of @AWSCURRENT@ and must be in the following format:
--
-- @{"username": /UserName/ , "password": /Password/ }@
mkUpdateCodeRepository ::
  -- | 'codeRepositoryName'
  Lude.Text ->
  UpdateCodeRepository
mkUpdateCodeRepository pCodeRepositoryName_ =
  UpdateCodeRepository'
    { codeRepositoryName = pCodeRepositoryName_,
      gitConfig = Lude.Nothing
    }

-- | The name of the Git repository to update.
--
-- /Note:/ Consider using 'codeRepositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrCodeRepositoryName :: Lens.Lens' UpdateCodeRepository Lude.Text
ucrCodeRepositoryName = Lens.lens (codeRepositoryName :: UpdateCodeRepository -> Lude.Text) (\s a -> s {codeRepositoryName = a} :: UpdateCodeRepository)
{-# DEPRECATED ucrCodeRepositoryName "Use generic-lens or generic-optics with 'codeRepositoryName' instead." #-}

-- | The configuration of the git repository, including the URL and the Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the repository. The secret must have a staging label of @AWSCURRENT@ and must be in the following format:
--
-- @{"username": /UserName/ , "password": /Password/ }@
--
-- /Note:/ Consider using 'gitConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrGitConfig :: Lens.Lens' UpdateCodeRepository (Lude.Maybe GitConfigForUpdate)
ucrGitConfig = Lens.lens (gitConfig :: UpdateCodeRepository -> Lude.Maybe GitConfigForUpdate) (\s a -> s {gitConfig = a} :: UpdateCodeRepository)
{-# DEPRECATED ucrGitConfig "Use generic-lens or generic-optics with 'gitConfig' instead." #-}

instance Lude.AWSRequest UpdateCodeRepository where
  type Rs UpdateCodeRepository = UpdateCodeRepositoryResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateCodeRepositoryResponse'
            Lude.<$> (x Lude..:> "CodeRepositoryArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateCodeRepository where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.UpdateCodeRepository" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateCodeRepository where
  toJSON UpdateCodeRepository' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("CodeRepositoryName" Lude..= codeRepositoryName),
            ("GitConfig" Lude..=) Lude.<$> gitConfig
          ]
      )

instance Lude.ToPath UpdateCodeRepository where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateCodeRepository where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateCodeRepositoryResponse' smart constructor.
data UpdateCodeRepositoryResponse = UpdateCodeRepositoryResponse'
  { -- | The ARN of the Git repository.
    codeRepositoryARN :: Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateCodeRepositoryResponse' with the minimum fields required to make a request.
--
-- * 'codeRepositoryARN' - The ARN of the Git repository.
-- * 'responseStatus' - The response status code.
mkUpdateCodeRepositoryResponse ::
  -- | 'codeRepositoryARN'
  Lude.Text ->
  -- | 'responseStatus'
  Lude.Int ->
  UpdateCodeRepositoryResponse
mkUpdateCodeRepositoryResponse pCodeRepositoryARN_ pResponseStatus_ =
  UpdateCodeRepositoryResponse'
    { codeRepositoryARN =
        pCodeRepositoryARN_,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the Git repository.
--
-- /Note:/ Consider using 'codeRepositoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsCodeRepositoryARN :: Lens.Lens' UpdateCodeRepositoryResponse Lude.Text
ucrrsCodeRepositoryARN = Lens.lens (codeRepositoryARN :: UpdateCodeRepositoryResponse -> Lude.Text) (\s a -> s {codeRepositoryARN = a} :: UpdateCodeRepositoryResponse)
{-# DEPRECATED ucrrsCodeRepositoryARN "Use generic-lens or generic-optics with 'codeRepositoryARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucrrsResponseStatus :: Lens.Lens' UpdateCodeRepositoryResponse Lude.Int
ucrrsResponseStatus = Lens.lens (responseStatus :: UpdateCodeRepositoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateCodeRepositoryResponse)
{-# DEPRECATED ucrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
