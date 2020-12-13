{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.CreateRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new, empty repository.
module Network.AWS.CodeCommit.CreateRepository
  ( -- * Creating a request
    CreateRepository (..),
    mkCreateRepository,

    -- ** Request lenses
    crRepositoryDescription,
    crRepositoryName,
    crTags,

    -- * Destructuring the response
    CreateRepositoryResponse (..),
    mkCreateRepositoryResponse,

    -- ** Response lenses
    crrsRepositoryMetadata,
    crrsResponseStatus,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a create repository operation.
--
-- /See:/ 'mkCreateRepository' smart constructor.
data CreateRepository = CreateRepository'
  { -- | A comment or description about the new repository.
    repositoryDescription :: Lude.Maybe Lude.Text,
    -- | The name of the new repository to be created.
    repositoryName :: Lude.Text,
    -- | One or more tag key-value pairs to use when tagging this repository.
    tags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRepository' with the minimum fields required to make a request.
--
-- * 'repositoryDescription' - A comment or description about the new repository.
-- * 'repositoryName' - The name of the new repository to be created.
-- * 'tags' - One or more tag key-value pairs to use when tagging this repository.
mkCreateRepository ::
  -- | 'repositoryName'
  Lude.Text ->
  CreateRepository
mkCreateRepository pRepositoryName_ =
  CreateRepository'
    { repositoryDescription = Lude.Nothing,
      repositoryName = pRepositoryName_,
      tags = Lude.Nothing
    }

-- | A comment or description about the new repository.
--
-- /Note:/ Consider using 'repositoryDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRepositoryDescription :: Lens.Lens' CreateRepository (Lude.Maybe Lude.Text)
crRepositoryDescription = Lens.lens (repositoryDescription :: CreateRepository -> Lude.Maybe Lude.Text) (\s a -> s {repositoryDescription = a} :: CreateRepository)
{-# DEPRECATED crRepositoryDescription "Use generic-lens or generic-optics with 'repositoryDescription' instead." #-}

-- | The name of the new repository to be created.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crRepositoryName :: Lens.Lens' CreateRepository Lude.Text
crRepositoryName = Lens.lens (repositoryName :: CreateRepository -> Lude.Text) (\s a -> s {repositoryName = a} :: CreateRepository)
{-# DEPRECATED crRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

-- | One or more tag key-value pairs to use when tagging this repository.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crTags :: Lens.Lens' CreateRepository (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
crTags = Lens.lens (tags :: CreateRepository -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {tags = a} :: CreateRepository)
{-# DEPRECATED crTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateRepository where
  type Rs CreateRepository = CreateRepositoryResponse
  request = Req.postJSON codeCommitService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateRepositoryResponse'
            Lude.<$> (x Lude..?> "repositoryMetadata")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateRepository where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.CreateRepository" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateRepository where
  toJSON CreateRepository' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("repositoryDescription" Lude..=) Lude.<$> repositoryDescription,
            Lude.Just ("repositoryName" Lude..= repositoryName),
            ("tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateRepository where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateRepository where
  toQuery = Lude.const Lude.mempty

-- | Represents the output of a create repository operation.
--
-- /See:/ 'mkCreateRepositoryResponse' smart constructor.
data CreateRepositoryResponse = CreateRepositoryResponse'
  { -- | Information about the newly created repository.
    repositoryMetadata :: Lude.Maybe RepositoryMetadata,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateRepositoryResponse' with the minimum fields required to make a request.
--
-- * 'repositoryMetadata' - Information about the newly created repository.
-- * 'responseStatus' - The response status code.
mkCreateRepositoryResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateRepositoryResponse
mkCreateRepositoryResponse pResponseStatus_ =
  CreateRepositoryResponse'
    { repositoryMetadata = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the newly created repository.
--
-- /Note:/ Consider using 'repositoryMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsRepositoryMetadata :: Lens.Lens' CreateRepositoryResponse (Lude.Maybe RepositoryMetadata)
crrsRepositoryMetadata = Lens.lens (repositoryMetadata :: CreateRepositoryResponse -> Lude.Maybe RepositoryMetadata) (\s a -> s {repositoryMetadata = a} :: CreateRepositoryResponse)
{-# DEPRECATED crrsRepositoryMetadata "Use generic-lens or generic-optics with 'repositoryMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsResponseStatus :: Lens.Lens' CreateRepositoryResponse Lude.Int
crrsResponseStatus = Lens.lens (responseStatus :: CreateRepositoryResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateRepositoryResponse)
{-# DEPRECATED crrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
