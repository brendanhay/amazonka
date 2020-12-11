{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetTrigger
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the definition of a trigger.
module Network.AWS.Glue.GetTrigger
  ( -- * Creating a request
    GetTrigger (..),
    mkGetTrigger,

    -- ** Request lenses
    gtName,

    -- * Destructuring the response
    GetTriggerResponse (..),
    mkGetTriggerResponse,

    -- ** Response lenses
    getrsTrigger,
    getrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTrigger' smart constructor.
newtype GetTrigger = GetTrigger' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTrigger' with the minimum fields required to make a request.
--
-- * 'name' - The name of the trigger to retrieve.
mkGetTrigger ::
  -- | 'name'
  Lude.Text ->
  GetTrigger
mkGetTrigger pName_ = GetTrigger' {name = pName_}

-- | The name of the trigger to retrieve.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtName :: Lens.Lens' GetTrigger Lude.Text
gtName = Lens.lens (name :: GetTrigger -> Lude.Text) (\s a -> s {name = a} :: GetTrigger)
{-# DEPRECATED gtName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetTrigger where
  type Rs GetTrigger = GetTriggerResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTriggerResponse'
            Lude.<$> (x Lude..?> "Trigger") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTrigger where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target" Lude.=# ("AWSGlue.GetTrigger" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetTrigger where
  toJSON GetTrigger' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath GetTrigger where
  toPath = Lude.const "/"

instance Lude.ToQuery GetTrigger where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTriggerResponse' smart constructor.
data GetTriggerResponse = GetTriggerResponse'
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

-- | Creates a value of 'GetTriggerResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'trigger' - The requested trigger definition.
mkGetTriggerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTriggerResponse
mkGetTriggerResponse pResponseStatus_ =
  GetTriggerResponse'
    { trigger = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The requested trigger definition.
--
-- /Note:/ Consider using 'trigger' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsTrigger :: Lens.Lens' GetTriggerResponse (Lude.Maybe Trigger)
getrsTrigger = Lens.lens (trigger :: GetTriggerResponse -> Lude.Maybe Trigger) (\s a -> s {trigger = a} :: GetTriggerResponse)
{-# DEPRECATED getrsTrigger "Use generic-lens or generic-optics with 'trigger' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
getrsResponseStatus :: Lens.Lens' GetTriggerResponse Lude.Int
getrsResponseStatus = Lens.lens (responseStatus :: GetTriggerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTriggerResponse)
{-# DEPRECATED getrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
