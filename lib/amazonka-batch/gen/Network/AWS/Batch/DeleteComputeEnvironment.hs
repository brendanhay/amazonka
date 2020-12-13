{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.DeleteComputeEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an AWS Batch compute environment.
--
-- Before you can delete a compute environment, you must set its state to @DISABLED@ with the 'UpdateComputeEnvironment' API operation and disassociate it from any job queues with the 'UpdateJobQueue' API operation.
module Network.AWS.Batch.DeleteComputeEnvironment
  ( -- * Creating a request
    DeleteComputeEnvironment (..),
    mkDeleteComputeEnvironment,

    -- ** Request lenses
    dceComputeEnvironment,

    -- * Destructuring the response
    DeleteComputeEnvironmentResponse (..),
    mkDeleteComputeEnvironmentResponse,

    -- ** Response lenses
    dcersResponseStatus,
  )
where

import Network.AWS.Batch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteComputeEnvironment' smart constructor.
newtype DeleteComputeEnvironment = DeleteComputeEnvironment'
  { -- | The name or Amazon Resource Name (ARN) of the compute environment to delete.
    computeEnvironment :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteComputeEnvironment' with the minimum fields required to make a request.
--
-- * 'computeEnvironment' - The name or Amazon Resource Name (ARN) of the compute environment to delete.
mkDeleteComputeEnvironment ::
  -- | 'computeEnvironment'
  Lude.Text ->
  DeleteComputeEnvironment
mkDeleteComputeEnvironment pComputeEnvironment_ =
  DeleteComputeEnvironment'
    { computeEnvironment =
        pComputeEnvironment_
    }

-- | The name or Amazon Resource Name (ARN) of the compute environment to delete.
--
-- /Note:/ Consider using 'computeEnvironment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceComputeEnvironment :: Lens.Lens' DeleteComputeEnvironment Lude.Text
dceComputeEnvironment = Lens.lens (computeEnvironment :: DeleteComputeEnvironment -> Lude.Text) (\s a -> s {computeEnvironment = a} :: DeleteComputeEnvironment)
{-# DEPRECATED dceComputeEnvironment "Use generic-lens or generic-optics with 'computeEnvironment' instead." #-}

instance Lude.AWSRequest DeleteComputeEnvironment where
  type Rs DeleteComputeEnvironment = DeleteComputeEnvironmentResponse
  request = Req.postJSON batchService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteComputeEnvironmentResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteComputeEnvironment where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteComputeEnvironment where
  toJSON DeleteComputeEnvironment' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("computeEnvironment" Lude..= computeEnvironment)]
      )

instance Lude.ToPath DeleteComputeEnvironment where
  toPath = Lude.const "/v1/deletecomputeenvironment"

instance Lude.ToQuery DeleteComputeEnvironment where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteComputeEnvironmentResponse' smart constructor.
newtype DeleteComputeEnvironmentResponse = DeleteComputeEnvironmentResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteComputeEnvironmentResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteComputeEnvironmentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteComputeEnvironmentResponse
mkDeleteComputeEnvironmentResponse pResponseStatus_ =
  DeleteComputeEnvironmentResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcersResponseStatus :: Lens.Lens' DeleteComputeEnvironmentResponse Lude.Int
dcersResponseStatus = Lens.lens (responseStatus :: DeleteComputeEnvironmentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteComputeEnvironmentResponse)
{-# DEPRECATED dcersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
