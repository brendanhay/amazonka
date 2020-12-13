{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.GetRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a repository.
module Network.AWS.CodeCommit.GetRepository
  ( -- * Creating a request
    GetRepository (..),
    mkGetRepository,

    -- ** Request lenses
    grRepositoryName,

    -- * Destructuring the response
    GetRepositoryResponse (..),
    mkGetRepositoryResponse,

    -- ** Response lenses
    grrsRepositoryMetadata,
    grrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a get repository operation.
--
-- /See:/ 'mkGetRepository' smart constructor.
newtype GetRepository = GetRepository'
  { -- | The name of the repository to get information about.
    repositoryName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRepository' with the minimum fields required to make a request.
--
-- * 'repositoryName' - The name of the repository to get information about.
mkGetRepository ::
  -- | 'repositoryName'
  Lude.Text ->
  GetRepository
mkGetRepository pRepositoryName_ =
  GetRepository' {repositoryName = pRepositoryName_}

-- | The name of the repository to get information about.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grRepositoryName :: Lens.Lens' GetRepository Lude.Text
grRepositoryName = Lens.lens (repositoryName :: GetRepository -> Lude.Text) (\s a -> s {repositoryName = a} :: GetRepository)
{-# DEPRECATED grRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest GetRepository where
  type Rs GetRepository = GetRepositoryResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRepositoryResponse'
            Lude.<$> (x Lude..?> "repositoryMetadata")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRepository where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.GetRepository" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRepository where
  toJSON GetRepository' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("repositoryName" Lude..= repositoryName)]
      )

instance Lude.ToPath GetRepository where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRepository where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a get repository operation.
--
-- /See:/ 'mkGetRepositoryResponse' smart constructor.
data GetRepositoryResponse = GetRepositoryResponse'
  { -- | Information about the repository.
    repositoryMetadata :: Lude.Maybe RepositoryMetadata,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRepositoryResponse' with the minimum fields required to make a request.
--
-- * 'repositoryMetadata' - Information about the repository.
-- * 'responseStatus' - The response status code.
mkGetRepositoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRepositoryResponse
mkGetRepositoryResponse pResponseStatus_ =
  GetRepositoryResponse'
    { repositoryMetadata = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the repository.
--
-- /Note:/ Consider using 'repositoryMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsRepositoryMetadata :: Lens.Lens' GetRepositoryResponse (Lude.Maybe RepositoryMetadata)
grrsRepositoryMetadata = Lens.lens (repositoryMetadata :: GetRepositoryResponse -> Lude.Maybe RepositoryMetadata) (\s a -> s {repositoryMetadata = a} :: GetRepositoryResponse)
{-# DEPRECATED grrsRepositoryMetadata "Use generic-lens or generic-optics with 'repositoryMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrsResponseStatus :: Lens.Lens' GetRepositoryResponse Lude.Int
grrsResponseStatus = Lens.lens (responseStatus :: GetRepositoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRepositoryResponse)
{-# DEPRECATED grrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
