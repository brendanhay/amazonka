{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DeleteEventBus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified custom event bus or partner event bus. All rules associated with this event bus need to be deleted. You can't delete your account's default event bus.
module Network.AWS.CloudWatchEvents.DeleteEventBus
  ( -- * Creating a request
    DeleteEventBus (..),
    mkDeleteEventBus,

    -- ** Request lenses
    debsName,

    -- * Destructuring the response
    DeleteEventBusResponse (..),
    mkDeleteEventBusResponse,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteEventBus' smart constructor.
newtype DeleteEventBus = DeleteEventBus'
  { -- | The name of the event bus to delete.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEventBus' with the minimum fields required to make a request.
--
-- * 'name' - The name of the event bus to delete.
mkDeleteEventBus ::
  -- | 'name'
  Lude.Text ->
  DeleteEventBus
mkDeleteEventBus pName_ = DeleteEventBus' {name = pName_}

-- | The name of the event bus to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
debsName :: Lens.Lens' DeleteEventBus Lude.Text
debsName = Lens.lens (name :: DeleteEventBus -> Lude.Text) (\s a -> s {name = a} :: DeleteEventBus)
{-# DEPRECATED debsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteEventBus where
  type Rs DeleteEventBus = DeleteEventBusResponse
  request = Req.postJSON cloudWatchEventsService
  response = Res.receiveNull DeleteEventBusResponse'

instance Lude.ToHeaders DeleteEventBus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.DeleteEventBus" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteEventBus where
  toJSON DeleteEventBus' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DeleteEventBus where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteEventBus where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteEventBusResponse' smart constructor.
data DeleteEventBusResponse = DeleteEventBusResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteEventBusResponse' with the minimum fields required to make a request.
mkDeleteEventBusResponse ::
  DeleteEventBusResponse
mkDeleteEventBusResponse = DeleteEventBusResponse'
