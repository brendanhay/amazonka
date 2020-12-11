{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteGameSessionQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a game session queue. Once a queue is successfully deleted, unfulfilled 'StartGameSessionPlacement' requests that reference the queue will fail. To delete a queue, specify the queue name.
--
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/queues-intro.html Using Multi-Region Queues>
-- __Related operations__
--
--     * 'CreateGameSessionQueue'
--
--
--     * 'DescribeGameSessionQueues'
--
--
--     * 'UpdateGameSessionQueue'
--
--
--     * 'DeleteGameSessionQueue'
module Network.AWS.GameLift.DeleteGameSessionQueue
  ( -- * Creating a request
    DeleteGameSessionQueue (..),
    mkDeleteGameSessionQueue,

    -- ** Request lenses
    dgsqName,

    -- * Destructuring the response
    DeleteGameSessionQueueResponse (..),
    mkDeleteGameSessionQueueResponse,

    -- ** Response lenses
    dgsqrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDeleteGameSessionQueue' smart constructor.
newtype DeleteGameSessionQueue = DeleteGameSessionQueue'
  { name ::
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

-- | Creates a value of 'DeleteGameSessionQueue' with the minimum fields required to make a request.
--
-- * 'name' - A descriptive label that is associated with game session queue. Queue names must be unique within each Region. You can use either the queue ID or ARN value.
mkDeleteGameSessionQueue ::
  -- | 'name'
  Lude.Text ->
  DeleteGameSessionQueue
mkDeleteGameSessionQueue pName_ =
  DeleteGameSessionQueue' {name = pName_}

-- | A descriptive label that is associated with game session queue. Queue names must be unique within each Region. You can use either the queue ID or ARN value.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsqName :: Lens.Lens' DeleteGameSessionQueue Lude.Text
dgsqName = Lens.lens (name :: DeleteGameSessionQueue -> Lude.Text) (\s a -> s {name = a} :: DeleteGameSessionQueue)
{-# DEPRECATED dgsqName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteGameSessionQueue where
  type Rs DeleteGameSessionQueue = DeleteGameSessionQueueResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteGameSessionQueueResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteGameSessionQueue where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DeleteGameSessionQueue" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteGameSessionQueue where
  toJSON DeleteGameSessionQueue' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DeleteGameSessionQueue where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteGameSessionQueue where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteGameSessionQueueResponse' smart constructor.
newtype DeleteGameSessionQueueResponse = DeleteGameSessionQueueResponse'
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

-- | Creates a value of 'DeleteGameSessionQueueResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteGameSessionQueueResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteGameSessionQueueResponse
mkDeleteGameSessionQueueResponse pResponseStatus_ =
  DeleteGameSessionQueueResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsqrsResponseStatus :: Lens.Lens' DeleteGameSessionQueueResponse Lude.Int
dgsqrsResponseStatus = Lens.lens (responseStatus :: DeleteGameSessionQueueResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteGameSessionQueueResponse)
{-# DEPRECATED dgsqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
