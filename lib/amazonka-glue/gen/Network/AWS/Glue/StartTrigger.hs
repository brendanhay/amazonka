{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StartTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an existing trigger. See <https://docs.aws.amazon.com/glue/latest/dg/trigger-job.html Triggering Jobs> for information about how different types of trigger are started.
module Network.AWS.Glue.StartTrigger
  ( -- * Creating a request
    StartTrigger (..),
    mkStartTrigger,

    -- ** Request lenses
    stName,

    -- * Destructuring the response
    StartTriggerResponse (..),
    mkStartTriggerResponse,

    -- ** Response lenses
    stfrsName,
    stfrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartTrigger' smart constructor.
newtype StartTrigger = StartTrigger'
  { -- | The name of the trigger to start.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartTrigger' with the minimum fields required to make a request.
--
-- * 'name' - The name of the trigger to start.
mkStartTrigger ::
  -- | 'name'
  Lude.Text ->
  StartTrigger
mkStartTrigger pName_ = StartTrigger' {name = pName_}

-- | The name of the trigger to start.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stName :: Lens.Lens' StartTrigger Lude.Text
stName = Lens.lens (name :: StartTrigger -> Lude.Text) (\s a -> s {name = a} :: StartTrigger)
{-# DEPRECATED stName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest StartTrigger where
  type Rs StartTrigger = StartTriggerResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartTriggerResponse'
            Lude.<$> (x Lude..?> "Name") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartTrigger where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.StartTrigger" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartTrigger where
  toJSON StartTrigger' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath StartTrigger where
  toPath = Lude.const "/"

instance Lude.ToQuery StartTrigger where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartTriggerResponse' smart constructor.
data StartTriggerResponse = StartTriggerResponse'
  { -- | The name of the trigger that was started.
    name :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartTriggerResponse' with the minimum fields required to make a request.
--
-- * 'name' - The name of the trigger that was started.
-- * 'responseStatus' - The response status code.
mkStartTriggerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartTriggerResponse
mkStartTriggerResponse pResponseStatus_ =
  StartTriggerResponse'
    { name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the trigger that was started.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stfrsName :: Lens.Lens' StartTriggerResponse (Lude.Maybe Lude.Text)
stfrsName = Lens.lens (name :: StartTriggerResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: StartTriggerResponse)
{-# DEPRECATED stfrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stfrsResponseStatus :: Lens.Lens' StartTriggerResponse Lude.Int
stfrsResponseStatus = Lens.lens (responseStatus :: StartTriggerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartTriggerResponse)
{-# DEPRECATED stfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
