{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.StopThingRegistrationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a bulk thing provisioning task.
module Network.AWS.IoT.StopThingRegistrationTask
  ( -- * Creating a request
    StopThingRegistrationTask (..),
    mkStopThingRegistrationTask,

    -- ** Request lenses
    strtTaskId,

    -- * Destructuring the response
    StopThingRegistrationTaskResponse (..),
    mkStopThingRegistrationTaskResponse,

    -- ** Response lenses
    srsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopThingRegistrationTask' smart constructor.
newtype StopThingRegistrationTask = StopThingRegistrationTask'
  { taskId ::
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

-- | Creates a value of 'StopThingRegistrationTask' with the minimum fields required to make a request.
--
-- * 'taskId' - The bulk thing provisioning task ID.
mkStopThingRegistrationTask ::
  -- | 'taskId'
  Lude.Text ->
  StopThingRegistrationTask
mkStopThingRegistrationTask pTaskId_ =
  StopThingRegistrationTask' {taskId = pTaskId_}

-- | The bulk thing provisioning task ID.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
strtTaskId :: Lens.Lens' StopThingRegistrationTask Lude.Text
strtTaskId = Lens.lens (taskId :: StopThingRegistrationTask -> Lude.Text) (\s a -> s {taskId = a} :: StopThingRegistrationTask)
{-# DEPRECATED strtTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

instance Lude.AWSRequest StopThingRegistrationTask where
  type
    Rs StopThingRegistrationTask =
      StopThingRegistrationTaskResponse
  request = Req.putJSON ioTService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StopThingRegistrationTaskResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopThingRegistrationTask where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON StopThingRegistrationTask where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath StopThingRegistrationTask where
  toPath StopThingRegistrationTask' {..} =
    Lude.mconcat
      ["/thing-registration-tasks/", Lude.toBS taskId, "/cancel"]

instance Lude.ToQuery StopThingRegistrationTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopThingRegistrationTaskResponse' smart constructor.
newtype StopThingRegistrationTaskResponse = StopThingRegistrationTaskResponse'
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

-- | Creates a value of 'StopThingRegistrationTaskResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStopThingRegistrationTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopThingRegistrationTaskResponse
mkStopThingRegistrationTaskResponse pResponseStatus_ =
  StopThingRegistrationTaskResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopThingRegistrationTaskResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StopThingRegistrationTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopThingRegistrationTaskResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
