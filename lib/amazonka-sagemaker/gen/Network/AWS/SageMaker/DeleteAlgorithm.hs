{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.DeleteAlgorithm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified algorithm from your account.
module Network.AWS.SageMaker.DeleteAlgorithm
  ( -- * Creating a request
    DeleteAlgorithm (..),
    mkDeleteAlgorithm,

    -- ** Request lenses
    daAlgorithmName,

    -- * Destructuring the response
    DeleteAlgorithmResponse (..),
    mkDeleteAlgorithmResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkDeleteAlgorithm' smart constructor.
newtype DeleteAlgorithm = DeleteAlgorithm'
  { algorithmName ::
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

-- | Creates a value of 'DeleteAlgorithm' with the minimum fields required to make a request.
--
-- * 'algorithmName' - The name of the algorithm to delete.
mkDeleteAlgorithm ::
  -- | 'algorithmName'
  Lude.Text ->
  DeleteAlgorithm
mkDeleteAlgorithm pAlgorithmName_ =
  DeleteAlgorithm' {algorithmName = pAlgorithmName_}

-- | The name of the algorithm to delete.
--
-- /Note:/ Consider using 'algorithmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAlgorithmName :: Lens.Lens' DeleteAlgorithm Lude.Text
daAlgorithmName = Lens.lens (algorithmName :: DeleteAlgorithm -> Lude.Text) (\s a -> s {algorithmName = a} :: DeleteAlgorithm)
{-# DEPRECATED daAlgorithmName "Use generic-lens or generic-optics with 'algorithmName' instead." #-}

instance Lude.AWSRequest DeleteAlgorithm where
  type Rs DeleteAlgorithm = DeleteAlgorithmResponse
  request = Req.postJSON sageMakerService
  response = Res.receiveNull DeleteAlgorithmResponse'

instance Lude.ToHeaders DeleteAlgorithm where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.DeleteAlgorithm" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteAlgorithm where
  toJSON DeleteAlgorithm' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("AlgorithmName" Lude..= algorithmName)]
      )

instance Lude.ToPath DeleteAlgorithm where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAlgorithm where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAlgorithmResponse' smart constructor.
data DeleteAlgorithmResponse = DeleteAlgorithmResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAlgorithmResponse' with the minimum fields required to make a request.
mkDeleteAlgorithmResponse ::
  DeleteAlgorithmResponse
mkDeleteAlgorithmResponse = DeleteAlgorithmResponse'
