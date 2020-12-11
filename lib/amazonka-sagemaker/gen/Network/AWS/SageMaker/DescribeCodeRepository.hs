{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DescribeCodeRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about the specified Git repository.
module Network.AWS.SageMaker.DescribeCodeRepository
  ( -- * Creating a request
    DescribeCodeRepository (..),
    mkDescribeCodeRepository,

    -- ** Request lenses
    dcrCodeRepositoryName,

    -- * Destructuring the response
    DescribeCodeRepositoryResponse (..),
    mkDescribeCodeRepositoryResponse,

    -- ** Response lenses
    dcrrsGitConfig,
    dcrrsResponseStatus,
    dcrrsCodeRepositoryName,
    dcrrsCodeRepositoryARN,
    dcrrsCreationTime,
    dcrrsLastModifiedTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDescribeCodeRepository' smart constructor.
newtype DescribeCodeRepository = DescribeCodeRepository'
  { codeRepositoryName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCodeRepository' with the minimum fields required to make a request.
--
-- * 'codeRepositoryName' - The name of the Git repository to describe.
mkDescribeCodeRepository ::
  -- | 'codeRepositoryName'
  Lude.Text ->
  DescribeCodeRepository
mkDescribeCodeRepository pCodeRepositoryName_ =
  DescribeCodeRepository'
    { codeRepositoryName =
        pCodeRepositoryName_
    }

-- | The name of the Git repository to describe.
--
-- /Note:/ Consider using 'codeRepositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrCodeRepositoryName :: Lens.Lens' DescribeCodeRepository Lude.Text
dcrCodeRepositoryName = Lens.lens (codeRepositoryName :: DescribeCodeRepository -> Lude.Text) (\s a -> s {codeRepositoryName = a} :: DescribeCodeRepository)
{-# DEPRECATED dcrCodeRepositoryName "Use generic-lens or generic-optics with 'codeRepositoryName' instead." #-}

instance Lude.AWSRequest DescribeCodeRepository where
  type Rs DescribeCodeRepository = DescribeCodeRepositoryResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCodeRepositoryResponse'
            Lude.<$> (x Lude..?> "GitConfig")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "CodeRepositoryName")
            Lude.<*> (x Lude..:> "CodeRepositoryArn")
            Lude.<*> (x Lude..:> "CreationTime")
            Lude.<*> (x Lude..:> "LastModifiedTime")
      )

instance Lude.ToHeaders DescribeCodeRepository where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DescribeCodeRepository" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeCodeRepository where
  toJSON DescribeCodeRepository' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("CodeRepositoryName" Lude..= codeRepositoryName)]
      )

instance Lude.ToPath DescribeCodeRepository where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCodeRepository where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeCodeRepositoryResponse' smart constructor.
data DescribeCodeRepositoryResponse = DescribeCodeRepositoryResponse'
  { gitConfig ::
      Lude.Maybe GitConfig,
    responseStatus :: Lude.Int,
    codeRepositoryName ::
      Lude.Text,
    codeRepositoryARN ::
      Lude.Text,
    creationTime ::
      Lude.Timestamp,
    lastModifiedTime ::
      Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCodeRepositoryResponse' with the minimum fields required to make a request.
--
-- * 'codeRepositoryARN' - The Amazon Resource Name (ARN) of the Git repository.
-- * 'codeRepositoryName' - The name of the Git repository.
-- * 'creationTime' - The date and time that the repository was created.
-- * 'gitConfig' - Configuration details about the repository, including the URL where the repository is located, the default branch, and the Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the repository.
-- * 'lastModifiedTime' - The date and time that the repository was last changed.
-- * 'responseStatus' - The response status code.
mkDescribeCodeRepositoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'codeRepositoryName'
  Lude.Text ->
  -- | 'codeRepositoryARN'
  Lude.Text ->
  -- | 'creationTime'
  Lude.Timestamp ->
  -- | 'lastModifiedTime'
  Lude.Timestamp ->
  DescribeCodeRepositoryResponse
mkDescribeCodeRepositoryResponse
  pResponseStatus_
  pCodeRepositoryName_
  pCodeRepositoryARN_
  pCreationTime_
  pLastModifiedTime_ =
    DescribeCodeRepositoryResponse'
      { gitConfig = Lude.Nothing,
        responseStatus = pResponseStatus_,
        codeRepositoryName = pCodeRepositoryName_,
        codeRepositoryARN = pCodeRepositoryARN_,
        creationTime = pCreationTime_,
        lastModifiedTime = pLastModifiedTime_
      }

-- | Configuration details about the repository, including the URL where the repository is located, the default branch, and the Amazon Resource Name (ARN) of the AWS Secrets Manager secret that contains the credentials used to access the repository.
--
-- /Note:/ Consider using 'gitConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsGitConfig :: Lens.Lens' DescribeCodeRepositoryResponse (Lude.Maybe GitConfig)
dcrrsGitConfig = Lens.lens (gitConfig :: DescribeCodeRepositoryResponse -> Lude.Maybe GitConfig) (\s a -> s {gitConfig = a} :: DescribeCodeRepositoryResponse)
{-# DEPRECATED dcrrsGitConfig "Use generic-lens or generic-optics with 'gitConfig' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DescribeCodeRepositoryResponse Lude.Int
dcrrsResponseStatus = Lens.lens (responseStatus :: DescribeCodeRepositoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCodeRepositoryResponse)
{-# DEPRECATED dcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The name of the Git repository.
--
-- /Note:/ Consider using 'codeRepositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsCodeRepositoryName :: Lens.Lens' DescribeCodeRepositoryResponse Lude.Text
dcrrsCodeRepositoryName = Lens.lens (codeRepositoryName :: DescribeCodeRepositoryResponse -> Lude.Text) (\s a -> s {codeRepositoryName = a} :: DescribeCodeRepositoryResponse)
{-# DEPRECATED dcrrsCodeRepositoryName "Use generic-lens or generic-optics with 'codeRepositoryName' instead." #-}

-- | The Amazon Resource Name (ARN) of the Git repository.
--
-- /Note:/ Consider using 'codeRepositoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsCodeRepositoryARN :: Lens.Lens' DescribeCodeRepositoryResponse Lude.Text
dcrrsCodeRepositoryARN = Lens.lens (codeRepositoryARN :: DescribeCodeRepositoryResponse -> Lude.Text) (\s a -> s {codeRepositoryARN = a} :: DescribeCodeRepositoryResponse)
{-# DEPRECATED dcrrsCodeRepositoryARN "Use generic-lens or generic-optics with 'codeRepositoryARN' instead." #-}

-- | The date and time that the repository was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsCreationTime :: Lens.Lens' DescribeCodeRepositoryResponse Lude.Timestamp
dcrrsCreationTime = Lens.lens (creationTime :: DescribeCodeRepositoryResponse -> Lude.Timestamp) (\s a -> s {creationTime = a} :: DescribeCodeRepositoryResponse)
{-# DEPRECATED dcrrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The date and time that the repository was last changed.
--
-- /Note:/ Consider using 'lastModifiedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsLastModifiedTime :: Lens.Lens' DescribeCodeRepositoryResponse Lude.Timestamp
dcrrsLastModifiedTime = Lens.lens (lastModifiedTime :: DescribeCodeRepositoryResponse -> Lude.Timestamp) (\s a -> s {lastModifiedTime = a} :: DescribeCodeRepositoryResponse)
{-# DEPRECATED dcrrsLastModifiedTime "Use generic-lens or generic-optics with 'lastModifiedTime' instead." #-}
