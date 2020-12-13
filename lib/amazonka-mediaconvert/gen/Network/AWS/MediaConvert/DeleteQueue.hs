{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.DeleteQueue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently delete a queue you have created.
module Network.AWS.MediaConvert.DeleteQueue
  ( -- * Creating a request
    DeleteQueue (..),
    mkDeleteQueue,

    -- ** Request lenses
    dqName,

    -- * Destructuring the response
    DeleteQueueResponse (..),
    mkDeleteQueueResponse,

    -- ** Response lenses
    dqrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteQueue' smart constructor.
newtype DeleteQueue = DeleteQueue'
  { -- | The name of the queue that you want to delete.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteQueue' with the minimum fields required to make a request.
--
-- * 'name' - The name of the queue that you want to delete.
mkDeleteQueue ::
  -- | 'name'
  Lude.Text ->
  DeleteQueue
mkDeleteQueue pName_ = DeleteQueue' {name = pName_}

-- | The name of the queue that you want to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqName :: Lens.Lens' DeleteQueue Lude.Text
dqName = Lens.lens (name :: DeleteQueue -> Lude.Text) (\s a -> s {name = a} :: DeleteQueue)
{-# DEPRECATED dqName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteQueue where
  type Rs DeleteQueue = DeleteQueueResponse
  request = Req.delete mediaConvertService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteQueueResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteQueue where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteQueue where
  toPath DeleteQueue' {..} =
    Lude.mconcat ["/2017-08-29/queues/", Lude.toBS name]

instance Lude.ToQuery DeleteQueue where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteQueueResponse' smart constructor.
newtype DeleteQueueResponse = DeleteQueueResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteQueueResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteQueueResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteQueueResponse
mkDeleteQueueResponse pResponseStatus_ =
  DeleteQueueResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dqrsResponseStatus :: Lens.Lens' DeleteQueueResponse Lude.Int
dqrsResponseStatus = Lens.lens (responseStatus :: DeleteQueueResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteQueueResponse)
{-# DEPRECATED dqrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
