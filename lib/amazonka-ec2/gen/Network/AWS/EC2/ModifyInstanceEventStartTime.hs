{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ModifyInstanceEventStartTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the start time for a scheduled Amazon EC2 instance event.
module Network.AWS.EC2.ModifyInstanceEventStartTime
  ( -- * Creating a request
    ModifyInstanceEventStartTime (..),
    mkModifyInstanceEventStartTime,

    -- ** Request lenses
    miestDryRun,
    miestInstanceId,
    miestInstanceEventId,
    miestNotBefore,

    -- * Destructuring the response
    ModifyInstanceEventStartTimeResponse (..),
    mkModifyInstanceEventStartTimeResponse,

    -- ** Response lenses
    miestrsEvent,
    miestrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkModifyInstanceEventStartTime' smart constructor.
data ModifyInstanceEventStartTime = ModifyInstanceEventStartTime'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    instanceId :: Lude.Text,
    instanceEventId :: Lude.Text,
    notBefore :: Lude.DateTime
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyInstanceEventStartTime' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'instanceEventId' - The ID of the event whose date and time you are modifying.
-- * 'instanceId' - The ID of the instance with the scheduled event.
-- * 'notBefore' - The new date and time when the event will take place.
mkModifyInstanceEventStartTime ::
  -- | 'instanceId'
  Lude.Text ->
  -- | 'instanceEventId'
  Lude.Text ->
  -- | 'notBefore'
  Lude.DateTime ->
  ModifyInstanceEventStartTime
mkModifyInstanceEventStartTime
  pInstanceId_
  pInstanceEventId_
  pNotBefore_ =
    ModifyInstanceEventStartTime'
      { dryRun = Lude.Nothing,
        instanceId = pInstanceId_,
        instanceEventId = pInstanceEventId_,
        notBefore = pNotBefore_
      }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miestDryRun :: Lens.Lens' ModifyInstanceEventStartTime (Lude.Maybe Lude.Bool)
miestDryRun = Lens.lens (dryRun :: ModifyInstanceEventStartTime -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: ModifyInstanceEventStartTime)
{-# DEPRECATED miestDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the instance with the scheduled event.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miestInstanceId :: Lens.Lens' ModifyInstanceEventStartTime Lude.Text
miestInstanceId = Lens.lens (instanceId :: ModifyInstanceEventStartTime -> Lude.Text) (\s a -> s {instanceId = a} :: ModifyInstanceEventStartTime)
{-# DEPRECATED miestInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The ID of the event whose date and time you are modifying.
--
-- /Note:/ Consider using 'instanceEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miestInstanceEventId :: Lens.Lens' ModifyInstanceEventStartTime Lude.Text
miestInstanceEventId = Lens.lens (instanceEventId :: ModifyInstanceEventStartTime -> Lude.Text) (\s a -> s {instanceEventId = a} :: ModifyInstanceEventStartTime)
{-# DEPRECATED miestInstanceEventId "Use generic-lens or generic-optics with 'instanceEventId' instead." #-}

-- | The new date and time when the event will take place.
--
-- /Note:/ Consider using 'notBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miestNotBefore :: Lens.Lens' ModifyInstanceEventStartTime Lude.DateTime
miestNotBefore = Lens.lens (notBefore :: ModifyInstanceEventStartTime -> Lude.DateTime) (\s a -> s {notBefore = a} :: ModifyInstanceEventStartTime)
{-# DEPRECATED miestNotBefore "Use generic-lens or generic-optics with 'notBefore' instead." #-}

instance Lude.AWSRequest ModifyInstanceEventStartTime where
  type
    Rs ModifyInstanceEventStartTime =
      ModifyInstanceEventStartTimeResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          ModifyInstanceEventStartTimeResponse'
            Lude.<$> (x Lude..@? "event") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyInstanceEventStartTime where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifyInstanceEventStartTime where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyInstanceEventStartTime where
  toQuery ModifyInstanceEventStartTime' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifyInstanceEventStartTime" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "InstanceId" Lude.=: instanceId,
        "InstanceEventId" Lude.=: instanceEventId,
        "NotBefore" Lude.=: notBefore
      ]

-- | /See:/ 'mkModifyInstanceEventStartTimeResponse' smart constructor.
data ModifyInstanceEventStartTimeResponse = ModifyInstanceEventStartTimeResponse'
  { event ::
      Lude.Maybe
        InstanceStatusEvent,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyInstanceEventStartTimeResponse' with the minimum fields required to make a request.
--
-- * 'event' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkModifyInstanceEventStartTimeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyInstanceEventStartTimeResponse
mkModifyInstanceEventStartTimeResponse pResponseStatus_ =
  ModifyInstanceEventStartTimeResponse'
    { event = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'event' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miestrsEvent :: Lens.Lens' ModifyInstanceEventStartTimeResponse (Lude.Maybe InstanceStatusEvent)
miestrsEvent = Lens.lens (event :: ModifyInstanceEventStartTimeResponse -> Lude.Maybe InstanceStatusEvent) (\s a -> s {event = a} :: ModifyInstanceEventStartTimeResponse)
{-# DEPRECATED miestrsEvent "Use generic-lens or generic-optics with 'event' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
miestrsResponseStatus :: Lens.Lens' ModifyInstanceEventStartTimeResponse Lude.Int
miestrsResponseStatus = Lens.lens (responseStatus :: ModifyInstanceEventStartTimeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyInstanceEventStartTimeResponse)
{-# DEPRECATED miestrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
