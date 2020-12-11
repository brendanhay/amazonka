{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdateRepositoryDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets or changes the comment or description for a repository.
module Network.AWS.CodeCommit.UpdateRepositoryDescription
  ( -- * Creating a request
    UpdateRepositoryDescription (..),
    mkUpdateRepositoryDescription,

    -- ** Request lenses
    urdRepositoryDescription,
    urdRepositoryName,

    -- * Destructuring the response
    UpdateRepositoryDescriptionResponse (..),
    mkUpdateRepositoryDescriptionResponse,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of an update repository description operation.
--
-- /See:/ 'mkUpdateRepositoryDescription' smart constructor.
data UpdateRepositoryDescription = UpdateRepositoryDescription'
  { repositoryDescription ::
      Lude.Maybe Lude.Text,
    repositoryName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRepositoryDescription' with the minimum fields required to make a request.
--
-- * 'repositoryDescription' - The new comment or description for the specified repository. Repository descriptions are limited to 1,000 characters.
-- * 'repositoryName' - The name of the repository to set or change the comment or description for.
mkUpdateRepositoryDescription ::
  -- | 'repositoryName'
  Lude.Text ->
  UpdateRepositoryDescription
mkUpdateRepositoryDescription pRepositoryName_ =
  UpdateRepositoryDescription'
    { repositoryDescription =
        Lude.Nothing,
      repositoryName = pRepositoryName_
    }

-- | The new comment or description for the specified repository. Repository descriptions are limited to 1,000 characters.
--
-- /Note:/ Consider using 'repositoryDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdRepositoryDescription :: Lens.Lens' UpdateRepositoryDescription (Lude.Maybe Lude.Text)
urdRepositoryDescription = Lens.lens (repositoryDescription :: UpdateRepositoryDescription -> Lude.Maybe Lude.Text) (\s a -> s {repositoryDescription = a} :: UpdateRepositoryDescription)
{-# DEPRECATED urdRepositoryDescription "Use generic-lens or generic-optics with 'repositoryDescription' instead." #-}

-- | The name of the repository to set or change the comment or description for.
--
-- /Note:/ Consider using 'repositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urdRepositoryName :: Lens.Lens' UpdateRepositoryDescription Lude.Text
urdRepositoryName = Lens.lens (repositoryName :: UpdateRepositoryDescription -> Lude.Text) (\s a -> s {repositoryName = a} :: UpdateRepositoryDescription)
{-# DEPRECATED urdRepositoryName "Use generic-lens or generic-optics with 'repositoryName' instead." #-}

instance Lude.AWSRequest UpdateRepositoryDescription where
  type
    Rs UpdateRepositoryDescription =
      UpdateRepositoryDescriptionResponse
  request = Req.postJSON codeCommitService
  response = Res.receiveNull UpdateRepositoryDescriptionResponse'

instance Lude.ToHeaders UpdateRepositoryDescription where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeCommit_20150413.UpdateRepositoryDescription" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateRepositoryDescription where
  toJSON UpdateRepositoryDescription' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("repositoryDescription" Lude..=) Lude.<$> repositoryDescription,
            Lude.Just ("repositoryName" Lude..= repositoryName)
          ]
      )

instance Lude.ToPath UpdateRepositoryDescription where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateRepositoryDescription where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateRepositoryDescriptionResponse' smart constructor.
data UpdateRepositoryDescriptionResponse = UpdateRepositoryDescriptionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRepositoryDescriptionResponse' with the minimum fields required to make a request.
mkUpdateRepositoryDescriptionResponse ::
  UpdateRepositoryDescriptionResponse
mkUpdateRepositoryDescriptionResponse =
  UpdateRepositoryDescriptionResponse'
