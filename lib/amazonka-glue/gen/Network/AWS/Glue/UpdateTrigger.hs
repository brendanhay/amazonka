{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a trigger definition.
module Network.AWS.Glue.UpdateTrigger
  ( -- * Creating a request
    UpdateTrigger (..),
    mkUpdateTrigger,

    -- ** Request lenses
    utName,
    utTriggerUpdate,

    -- * Destructuring the response
    UpdateTriggerResponse (..),
    mkUpdateTriggerResponse,

    -- ** Response lenses
    updrsTrigger,
    updrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateTrigger' smart constructor.
data UpdateTrigger = UpdateTrigger'
  { name :: Lude.Text,
    triggerUpdate :: TriggerUpdate
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateTrigger' with the minimum fields required to make a request.
--
-- * 'name' - The name of the trigger to update.
-- * 'triggerUpdate' - The new values with which to update the trigger.
mkUpdateTrigger ::
  -- | 'name'
  Lude.Text ->
  -- | 'triggerUpdate'
  TriggerUpdate ->
  UpdateTrigger
mkUpdateTrigger pName_ pTriggerUpdate_ =
  UpdateTrigger' {name = pName_, triggerUpdate = pTriggerUpdate_}

-- | The name of the trigger to update.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utName :: Lens.Lens' UpdateTrigger Lude.Text
utName = Lens.lens (name :: UpdateTrigger -> Lude.Text) (\s a -> s {name = a} :: UpdateTrigger)
{-# DEPRECATED utName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The new values with which to update the trigger.
--
-- /Note:/ Consider using 'triggerUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utTriggerUpdate :: Lens.Lens' UpdateTrigger TriggerUpdate
utTriggerUpdate = Lens.lens (triggerUpdate :: UpdateTrigger -> TriggerUpdate) (\s a -> s {triggerUpdate = a} :: UpdateTrigger)
{-# DEPRECATED utTriggerUpdate "Use generic-lens or generic-optics with 'triggerUpdate' instead." #-}

instance Lude.AWSRequest UpdateTrigger where
  type Rs UpdateTrigger = UpdateTriggerResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateTriggerResponse'
            Lude.<$> (x Lude..?> "Trigger") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateTrigger where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.UpdateTrigger" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateTrigger where
  toJSON UpdateTrigger' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            Lude.Just ("TriggerUpdate" Lude..= triggerUpdate)
          ]
      )

instance Lude.ToPath UpdateTrigger where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateTrigger where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateTriggerResponse' smart constructor.
data UpdateTriggerResponse = UpdateTriggerResponse'
  { trigger ::
      Lude.Maybe Trigger,
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

-- | Creates a value of 'UpdateTriggerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'trigger' - The resulting trigger definition.
mkUpdateTriggerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateTriggerResponse
mkUpdateTriggerResponse pResponseStatus_ =
  UpdateTriggerResponse'
    { trigger = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The resulting trigger definition.
--
-- /Note:/ Consider using 'trigger' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updrsTrigger :: Lens.Lens' UpdateTriggerResponse (Lude.Maybe Trigger)
updrsTrigger = Lens.lens (trigger :: UpdateTriggerResponse -> Lude.Maybe Trigger) (\s a -> s {trigger = a} :: UpdateTriggerResponse)
{-# DEPRECATED updrsTrigger "Use generic-lens or generic-optics with 'trigger' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updrsResponseStatus :: Lens.Lens' UpdateTriggerResponse Lude.Int
updrsResponseStatus = Lens.lens (responseStatus :: UpdateTriggerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateTriggerResponse)
{-# DEPRECATED updrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
