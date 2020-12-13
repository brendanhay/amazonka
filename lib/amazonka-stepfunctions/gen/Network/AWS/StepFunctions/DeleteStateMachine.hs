{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.DeleteStateMachine
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a state machine. This is an asynchronous operation: It sets the state machine's status to @DELETING@ and begins the deletion process.
module Network.AWS.StepFunctions.DeleteStateMachine
  ( -- * Creating a request
    DeleteStateMachine (..),
    mkDeleteStateMachine,

    -- ** Request lenses
    dsmStateMachineARN,

    -- * Destructuring the response
    DeleteStateMachineResponse (..),
    mkDeleteStateMachineResponse,

    -- ** Response lenses
    dsmrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StepFunctions.Types

-- | /See:/ 'mkDeleteStateMachine' smart constructor.
newtype DeleteStateMachine = DeleteStateMachine'
  { -- | The Amazon Resource Name (ARN) of the state machine to delete.
    stateMachineARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteStateMachine' with the minimum fields required to make a request.
--
-- * 'stateMachineARN' - The Amazon Resource Name (ARN) of the state machine to delete.
mkDeleteStateMachine ::
  -- | 'stateMachineARN'
  Lude.Text ->
  DeleteStateMachine
mkDeleteStateMachine pStateMachineARN_ =
  DeleteStateMachine' {stateMachineARN = pStateMachineARN_}

-- | The Amazon Resource Name (ARN) of the state machine to delete.
--
-- /Note:/ Consider using 'stateMachineARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmStateMachineARN :: Lens.Lens' DeleteStateMachine Lude.Text
dsmStateMachineARN = Lens.lens (stateMachineARN :: DeleteStateMachine -> Lude.Text) (\s a -> s {stateMachineARN = a} :: DeleteStateMachine)
{-# DEPRECATED dsmStateMachineARN "Use generic-lens or generic-optics with 'stateMachineARN' instead." #-}

instance Lude.AWSRequest DeleteStateMachine where
  type Rs DeleteStateMachine = DeleteStateMachineResponse
  request = Req.postJSON stepFunctionsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteStateMachineResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteStateMachine where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSStepFunctions.DeleteStateMachine" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteStateMachine where
  toJSON DeleteStateMachine' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("stateMachineArn" Lude..= stateMachineARN)]
      )

instance Lude.ToPath DeleteStateMachine where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteStateMachine where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteStateMachineResponse' smart constructor.
newtype DeleteStateMachineResponse = DeleteStateMachineResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteStateMachineResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteStateMachineResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteStateMachineResponse
mkDeleteStateMachineResponse pResponseStatus_ =
  DeleteStateMachineResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmrsResponseStatus :: Lens.Lens' DeleteStateMachineResponse Lude.Int
dsmrsResponseStatus = Lens.lens (responseStatus :: DeleteStateMachineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteStateMachineResponse)
{-# DEPRECATED dsmrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
