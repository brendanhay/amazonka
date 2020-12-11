{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.UpdateRepositoryName
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Renames a repository. The repository name must be unique across the calling AWS account. Repository names are limited to 100 alphanumeric, dash, and underscore characters, and cannot include certain characters. The suffix .git is prohibited. For more information about the limits on repository names, see <https://docs.aws.amazon.com/codecommit/latest/userguide/limits.html Limits> in the AWS CodeCommit User Guide.
module Network.AWS.CodeCommit.UpdateRepositoryName
  ( -- * Creating a request
    UpdateRepositoryName (..),
    mkUpdateRepositoryName,

    -- ** Request lenses
    urnOldName,
    urnNewName,

    -- * Destructuring the response
    UpdateRepositoryNameResponse (..),
    mkUpdateRepositoryNameResponse,
  )
where

import Network.AWS.CodeCommit.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of an update repository description operation.
--
-- /See:/ 'mkUpdateRepositoryName' smart constructor.
data UpdateRepositoryName = UpdateRepositoryName'
  { oldName ::
      Lude.Text,
    newName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRepositoryName' with the minimum fields required to make a request.
--
-- * 'newName' - The new name for the repository.
-- * 'oldName' - The current name of the repository.
mkUpdateRepositoryName ::
  -- | 'oldName'
  Lude.Text ->
  -- | 'newName'
  Lude.Text ->
  UpdateRepositoryName
mkUpdateRepositoryName pOldName_ pNewName_ =
  UpdateRepositoryName' {oldName = pOldName_, newName = pNewName_}

-- | The current name of the repository.
--
-- /Note:/ Consider using 'oldName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urnOldName :: Lens.Lens' UpdateRepositoryName Lude.Text
urnOldName = Lens.lens (oldName :: UpdateRepositoryName -> Lude.Text) (\s a -> s {oldName = a} :: UpdateRepositoryName)
{-# DEPRECATED urnOldName "Use generic-lens or generic-optics with 'oldName' instead." #-}

-- | The new name for the repository.
--
-- /Note:/ Consider using 'newName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
urnNewName :: Lens.Lens' UpdateRepositoryName Lude.Text
urnNewName = Lens.lens (newName :: UpdateRepositoryName -> Lude.Text) (\s a -> s {newName = a} :: UpdateRepositoryName)
{-# DEPRECATED urnNewName "Use generic-lens or generic-optics with 'newName' instead." #-}

instance Lude.AWSRequest UpdateRepositoryName where
  type Rs UpdateRepositoryName = UpdateRepositoryNameResponse
  request = Req.postJSON codeCommitService
  response = Res.receiveNull UpdateRepositoryNameResponse'

instance Lude.ToHeaders UpdateRepositoryName where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeCommit_20150413.UpdateRepositoryName" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateRepositoryName where
  toJSON UpdateRepositoryName' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("oldName" Lude..= oldName),
            Lude.Just ("newName" Lude..= newName)
          ]
      )

instance Lude.ToPath UpdateRepositoryName where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateRepositoryName where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateRepositoryNameResponse' smart constructor.
data UpdateRepositoryNameResponse = UpdateRepositoryNameResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateRepositoryNameResponse' with the minimum fields required to make a request.
mkUpdateRepositoryNameResponse ::
  UpdateRepositoryNameResponse
mkUpdateRepositoryNameResponse = UpdateRepositoryNameResponse'
