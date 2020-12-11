{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.CancelReplay
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified replay.
module Network.AWS.CloudWatchEvents.CancelReplay
  ( -- * Creating a request
    CancelReplay (..),
    mkCancelReplay,

    -- ** Request lenses
    crReplayName,

    -- * Destructuring the response
    CancelReplayResponse (..),
    mkCancelReplayResponse,

    -- ** Response lenses
    crrsState,
    crrsReplayARN,
    crrsStateReason,
    crrsResponseStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCancelReplay' smart constructor.
newtype CancelReplay = CancelReplay' {replayName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelReplay' with the minimum fields required to make a request.
--
-- * 'replayName' - The name of the replay to cancel.
mkCancelReplay ::
  -- | 'replayName'
  Lude.Text ->
  CancelReplay
mkCancelReplay pReplayName_ =
  CancelReplay' {replayName = pReplayName_}

-- | The name of the replay to cancel.
--
-- /Note:/ Consider using 'replayName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crReplayName :: Lens.Lens' CancelReplay Lude.Text
crReplayName = Lens.lens (replayName :: CancelReplay -> Lude.Text) (\s a -> s {replayName = a} :: CancelReplay)
{-# DEPRECATED crReplayName "Use generic-lens or generic-optics with 'replayName' instead." #-}

instance Lude.AWSRequest CancelReplay where
  type Rs CancelReplay = CancelReplayResponse
  request = Req.postJSON cloudWatchEventsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CancelReplayResponse'
            Lude.<$> (x Lude..?> "State")
            Lude.<*> (x Lude..?> "ReplayArn")
            Lude.<*> (x Lude..?> "StateReason")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CancelReplay where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.CancelReplay" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CancelReplay where
  toJSON CancelReplay' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("ReplayName" Lude..= replayName)])

instance Lude.ToPath CancelReplay where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelReplay where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCancelReplayResponse' smart constructor.
data CancelReplayResponse = CancelReplayResponse'
  { state ::
      Lude.Maybe ReplayState,
    replayARN :: Lude.Maybe Lude.Text,
    stateReason :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CancelReplayResponse' with the minimum fields required to make a request.
--
-- * 'replayARN' - The ARN of the replay to cancel.
-- * 'responseStatus' - The response status code.
-- * 'state' - The current state of the replay.
-- * 'stateReason' - The reason that the replay is in the current state.
mkCancelReplayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CancelReplayResponse
mkCancelReplayResponse pResponseStatus_ =
  CancelReplayResponse'
    { state = Lude.Nothing,
      replayARN = Lude.Nothing,
      stateReason = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current state of the replay.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsState :: Lens.Lens' CancelReplayResponse (Lude.Maybe ReplayState)
crrsState = Lens.lens (state :: CancelReplayResponse -> Lude.Maybe ReplayState) (\s a -> s {state = a} :: CancelReplayResponse)
{-# DEPRECATED crrsState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The ARN of the replay to cancel.
--
-- /Note:/ Consider using 'replayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsReplayARN :: Lens.Lens' CancelReplayResponse (Lude.Maybe Lude.Text)
crrsReplayARN = Lens.lens (replayARN :: CancelReplayResponse -> Lude.Maybe Lude.Text) (\s a -> s {replayARN = a} :: CancelReplayResponse)
{-# DEPRECATED crrsReplayARN "Use generic-lens or generic-optics with 'replayARN' instead." #-}

-- | The reason that the replay is in the current state.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsStateReason :: Lens.Lens' CancelReplayResponse (Lude.Maybe Lude.Text)
crrsStateReason = Lens.lens (stateReason :: CancelReplayResponse -> Lude.Maybe Lude.Text) (\s a -> s {stateReason = a} :: CancelReplayResponse)
{-# DEPRECATED crrsStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crrsResponseStatus :: Lens.Lens' CancelReplayResponse Lude.Int
crrsResponseStatus = Lens.lens (responseStatus :: CancelReplayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CancelReplayResponse)
{-# DEPRECATED crrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
