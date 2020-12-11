{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.CancelCommand
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attempts to cancel the command specified by the Command ID. There is no guarantee that the command will be terminated and the underlying process stopped.
module Network.AWS.SSM.CancelCommand
  ( -- * Creating a request
    CancelCommand (..),
    mkCancelCommand,

    -- ** Request lenses
    ccInstanceIds,
    ccCommandId,

    -- * Destructuring the response
    CancelCommandResponse (..),
    mkCancelCommandResponse,

    -- ** Response lenses
    ccrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- |
--
-- /See:/ 'mkCancelCommand' smart constructor.
data CancelCommand = CancelCommand'
  { instanceIds ::
      Lude.Maybe [Lude.Text],
    commandId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelCommand' with the minimum fields required to make a request.
--
-- * 'commandId' - The ID of the command you want to cancel.
-- * 'instanceIds' - (Optional) A list of instance IDs on which you want to cancel the command. If not provided, the command is canceled on every instance on which it was requested.
mkCancelCommand ::
  -- | 'commandId'
  Lude.Text ->
  CancelCommand
mkCancelCommand pCommandId_ =
  CancelCommand'
    { instanceIds = Lude.Nothing,
      commandId = pCommandId_
    }

-- | (Optional) A list of instance IDs on which you want to cancel the command. If not provided, the command is canceled on every instance on which it was requested.
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccInstanceIds :: Lens.Lens' CancelCommand (Lude.Maybe [Lude.Text])
ccInstanceIds = Lens.lens (instanceIds :: CancelCommand -> Lude.Maybe [Lude.Text]) (\s a -> s {instanceIds = a} :: CancelCommand)
{-# DEPRECATED ccInstanceIds "Use generic-lens or generic-optics with 'instanceIds' instead." #-}

-- | The ID of the command you want to cancel.
--
-- /Note:/ Consider using 'commandId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCommandId :: Lens.Lens' CancelCommand Lude.Text
ccCommandId = Lens.lens (commandId :: CancelCommand -> Lude.Text) (\s a -> s {commandId = a} :: CancelCommand)
{-# DEPRECATED ccCommandId "Use generic-lens or generic-optics with 'commandId' instead." #-}

instance Lude.AWSRequest CancelCommand where
  type Rs CancelCommand = CancelCommandResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CancelCommandResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelCommand where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.CancelCommand" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CancelCommand where
  toJSON CancelCommand' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstanceIds" Lude..=) Lude.<$> instanceIds,
            Lude.Just ("CommandId" Lude..= commandId)
          ]
      )

instance Lude.ToPath CancelCommand where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelCommand where
  toQuery = Lude.const Lude.mempty

-- | Whether or not the command was successfully canceled. There is no guarantee that a request can be canceled.
--
-- /See:/ 'mkCancelCommandResponse' smart constructor.
newtype CancelCommandResponse = CancelCommandResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelCommandResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCancelCommandResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelCommandResponse
mkCancelCommandResponse pResponseStatus_ =
  CancelCommandResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccrsResponseStatus :: Lens.Lens' CancelCommandResponse Lude.Int
ccrsResponseStatus = Lens.lens (responseStatus :: CancelCommandResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelCommandResponse)
{-# DEPRECATED ccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
