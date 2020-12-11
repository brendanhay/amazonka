{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTrafficMirrorSession
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Traffic Mirror session.
module Network.AWS.EC2.DeleteTrafficMirrorSession
  ( -- * Creating a request
    DeleteTrafficMirrorSession (..),
    mkDeleteTrafficMirrorSession,

    -- ** Request lenses
    dtmstDryRun,
    dtmstTrafficMirrorSessionId,

    -- * Destructuring the response
    DeleteTrafficMirrorSessionResponse (..),
    mkDeleteTrafficMirrorSessionResponse,

    -- ** Response lenses
    dtmstrsTrafficMirrorSessionId,
    dtmstrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTrafficMirrorSession' smart constructor.
data DeleteTrafficMirrorSession = DeleteTrafficMirrorSession'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    trafficMirrorSessionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTrafficMirrorSession' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'trafficMirrorSessionId' - The ID of the Traffic Mirror session.
mkDeleteTrafficMirrorSession ::
  -- | 'trafficMirrorSessionId'
  Lude.Text ->
  DeleteTrafficMirrorSession
mkDeleteTrafficMirrorSession pTrafficMirrorSessionId_ =
  DeleteTrafficMirrorSession'
    { dryRun = Lude.Nothing,
      trafficMirrorSessionId = pTrafficMirrorSessionId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmstDryRun :: Lens.Lens' DeleteTrafficMirrorSession (Lude.Maybe Lude.Bool)
dtmstDryRun = Lens.lens (dryRun :: DeleteTrafficMirrorSession -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteTrafficMirrorSession)
{-# DEPRECATED dtmstDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the Traffic Mirror session.
--
-- /Note:/ Consider using 'trafficMirrorSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmstTrafficMirrorSessionId :: Lens.Lens' DeleteTrafficMirrorSession Lude.Text
dtmstTrafficMirrorSessionId = Lens.lens (trafficMirrorSessionId :: DeleteTrafficMirrorSession -> Lude.Text) (\s a -> s {trafficMirrorSessionId = a} :: DeleteTrafficMirrorSession)
{-# DEPRECATED dtmstTrafficMirrorSessionId "Use generic-lens or generic-optics with 'trafficMirrorSessionId' instead." #-}

instance Lude.AWSRequest DeleteTrafficMirrorSession where
  type
    Rs DeleteTrafficMirrorSession =
      DeleteTrafficMirrorSessionResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteTrafficMirrorSessionResponse'
            Lude.<$> (x Lude..@? "trafficMirrorSessionId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTrafficMirrorSession where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteTrafficMirrorSession where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTrafficMirrorSession where
  toQuery DeleteTrafficMirrorSession' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteTrafficMirrorSession" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "TrafficMirrorSessionId" Lude.=: trafficMirrorSessionId
      ]

-- | /See:/ 'mkDeleteTrafficMirrorSessionResponse' smart constructor.
data DeleteTrafficMirrorSessionResponse = DeleteTrafficMirrorSessionResponse'
  { trafficMirrorSessionId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DeleteTrafficMirrorSessionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'trafficMirrorSessionId' - The ID of the deleted Traffic Mirror session.
mkDeleteTrafficMirrorSessionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTrafficMirrorSessionResponse
mkDeleteTrafficMirrorSessionResponse pResponseStatus_ =
  DeleteTrafficMirrorSessionResponse'
    { trafficMirrorSessionId =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the deleted Traffic Mirror session.
--
-- /Note:/ Consider using 'trafficMirrorSessionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmstrsTrafficMirrorSessionId :: Lens.Lens' DeleteTrafficMirrorSessionResponse (Lude.Maybe Lude.Text)
dtmstrsTrafficMirrorSessionId = Lens.lens (trafficMirrorSessionId :: DeleteTrafficMirrorSessionResponse -> Lude.Maybe Lude.Text) (\s a -> s {trafficMirrorSessionId = a} :: DeleteTrafficMirrorSessionResponse)
{-# DEPRECATED dtmstrsTrafficMirrorSessionId "Use generic-lens or generic-optics with 'trafficMirrorSessionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmstrsResponseStatus :: Lens.Lens' DeleteTrafficMirrorSessionResponse Lude.Int
dtmstrsResponseStatus = Lens.lens (responseStatus :: DeleteTrafficMirrorSessionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTrafficMirrorSessionResponse)
{-# DEPRECATED dtmstrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
