{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteCodeRepository
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Git repository from your account.
module Network.AWS.SageMaker.DeleteCodeRepository
  ( -- * Creating a request
    DeleteCodeRepository (..),
    mkDeleteCodeRepository,

    -- ** Request lenses
    dCodeRepositoryName,

    -- * Destructuring the response
    DeleteCodeRepositoryResponse (..),
    mkDeleteCodeRepositoryResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDeleteCodeRepository' smart constructor.
newtype DeleteCodeRepository = DeleteCodeRepository'
  { -- | The name of the Git repository to delete.
    codeRepositoryName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCodeRepository' with the minimum fields required to make a request.
--
-- * 'codeRepositoryName' - The name of the Git repository to delete.
mkDeleteCodeRepository ::
  -- | 'codeRepositoryName'
  Lude.Text ->
  DeleteCodeRepository
mkDeleteCodeRepository pCodeRepositoryName_ =
  DeleteCodeRepository' {codeRepositoryName = pCodeRepositoryName_}

-- | The name of the Git repository to delete.
--
-- /Note:/ Consider using 'codeRepositoryName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCodeRepositoryName :: Lens.Lens' DeleteCodeRepository Lude.Text
dCodeRepositoryName = Lens.lens (codeRepositoryName :: DeleteCodeRepository -> Lude.Text) (\s a -> s {codeRepositoryName = a} :: DeleteCodeRepository)
{-# DEPRECATED dCodeRepositoryName "Use generic-lens or generic-optics with 'codeRepositoryName' instead." #-}

instance Lude.AWSRequest DeleteCodeRepository where
  type Rs DeleteCodeRepository = DeleteCodeRepositoryResponse
  request = Req.postJSON sageMakerService
  response = Res.receiveNull DeleteCodeRepositoryResponse'

instance Lude.ToHeaders DeleteCodeRepository where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DeleteCodeRepository" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteCodeRepository where
  toJSON DeleteCodeRepository' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("CodeRepositoryName" Lude..= codeRepositoryName)]
      )

instance Lude.ToPath DeleteCodeRepository where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCodeRepository where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteCodeRepositoryResponse' smart constructor.
data DeleteCodeRepositoryResponse = DeleteCodeRepositoryResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCodeRepositoryResponse' with the minimum fields required to make a request.
mkDeleteCodeRepositoryResponse ::
  DeleteCodeRepositoryResponse
mkDeleteCodeRepositoryResponse = DeleteCodeRepositoryResponse'
