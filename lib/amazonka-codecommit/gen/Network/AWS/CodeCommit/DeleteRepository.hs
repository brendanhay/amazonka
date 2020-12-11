{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.DeleteRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a repository. If a specified repository was already deleted, a null repository ID is returned.
--
-- /Important:/ Deleting a repository also deletes all associated objects and metadata. After a repository is deleted, all future push calls to the deleted repository fail.
module Network.AWS.CodeCommit.DeleteRepository
  ( -- * Creating a request
    DeleteRepository (..),
    mkDeleteRepository,

    -- ** Request lenses
    drRepositoryName,

    -- * Destructuring the response
    DeleteRepositoryResponse (..),
    mkDeleteRepositoryResponse,

    -- ** Response lenses
    drrsRepositoryId,
    drrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a delete repository operation.
--
-- /See:/ 'mkDeleteRepository' smart constructor.
newtype DeleteRepository = DeleteRepository'
  { repositoryName ::
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

-- | Creates a value of 'DeleteRepository' with the minimum fields required to make a request.
--
-- * 'repositoryName' - The name of the repository to delete.
mkDeleteRepository ::
  -- | 'repositoryName'
  Lude.Text ->
  DeleteRepository
mkDeleteRepository pRepositoryName_ =
  DeleteRepository' {repositoryName = pRepositoryName_}

-- | The name of the repository to delete.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drRepositoryName :: Lens.Lens' DeleteRepository Lude.Text
drRepositoryName = Lens.lens (repositoryName :: DeleteRepository -> Lude.Text) (\s a -> s {repositoryName = a} :: DeleteRepository)
{-# DEPRECATED drRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest DeleteRepository where
  type Rs DeleteRepository = DeleteRepositoryResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteRepositoryResponse'
            Lude.<$> (x Lude..?> "repositoryId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteRepository where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.DeleteRepository" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteRepository where
  toJSON DeleteRepository' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("repositoryName" Lude..= repositoryName)]
      )

instance Lude.ToPath DeleteRepository where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteRepository where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a delete repository operation.
--
-- /See:/ 'mkDeleteRepositoryResponse' smart constructor.
data DeleteRepositoryResponse = DeleteRepositoryResponse'
  { repositoryId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRepositoryResponse' with the minimum fields required to make a request.
--
-- * 'repositoryId' - The ID of the repository that was deleted.
-- * 'responseStatus' - The response status code.
mkDeleteRepositoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteRepositoryResponse
mkDeleteRepositoryResponse pResponseStatus_ =
  DeleteRepositoryResponse'
    { repositoryId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the repository that was deleted.
--
-- /Note:/ Consider using 'repositoryId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsRepositoryId :: Lens.Lens' DeleteRepositoryResponse (Lude.Maybe Lude.Text)
drrsRepositoryId = Lens.lens (repositoryId :: DeleteRepositoryResponse -> Lude.Maybe Lude.Text) (\s a -> s {repositoryId = a} :: DeleteRepositoryResponse)
{-# DEPRECATED drrsRepositoryId "Use generic-lens or generic-optics with 'repositoryId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drrsResponseStatus :: Lens.Lens' DeleteRepositoryResponse Lude.Int
drrsResponseStatus = Lens.lens (responseStatus :: DeleteRepositoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteRepositoryResponse)
{-# DEPRECATED drrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
