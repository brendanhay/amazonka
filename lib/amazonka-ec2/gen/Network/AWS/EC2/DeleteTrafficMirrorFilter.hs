{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTrafficMirrorFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Traffic Mirror filter.
--
-- You cannot delete a Traffic Mirror filter that is in use by a Traffic Mirror session.
module Network.AWS.EC2.DeleteTrafficMirrorFilter
  ( -- * Creating a request
    DeleteTrafficMirrorFilter (..),
    mkDeleteTrafficMirrorFilter,

    -- ** Request lenses
    dtmftDryRun,
    dtmftTrafficMirrorFilterId,

    -- * Destructuring the response
    DeleteTrafficMirrorFilterResponse (..),
    mkDeleteTrafficMirrorFilterResponse,

    -- ** Response lenses
    dtmfrsTrafficMirrorFilterId,
    dtmfrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTrafficMirrorFilter' smart constructor.
data DeleteTrafficMirrorFilter = DeleteTrafficMirrorFilter'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    trafficMirrorFilterId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTrafficMirrorFilter' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'trafficMirrorFilterId' - The ID of the Traffic Mirror filter.
mkDeleteTrafficMirrorFilter ::
  -- | 'trafficMirrorFilterId'
  Lude.Text ->
  DeleteTrafficMirrorFilter
mkDeleteTrafficMirrorFilter pTrafficMirrorFilterId_ =
  DeleteTrafficMirrorFilter'
    { dryRun = Lude.Nothing,
      trafficMirrorFilterId = pTrafficMirrorFilterId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmftDryRun :: Lens.Lens' DeleteTrafficMirrorFilter (Lude.Maybe Lude.Bool)
dtmftDryRun = Lens.lens (dryRun :: DeleteTrafficMirrorFilter -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteTrafficMirrorFilter)
{-# DEPRECATED dtmftDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmftTrafficMirrorFilterId :: Lens.Lens' DeleteTrafficMirrorFilter Lude.Text
dtmftTrafficMirrorFilterId = Lens.lens (trafficMirrorFilterId :: DeleteTrafficMirrorFilter -> Lude.Text) (\s a -> s {trafficMirrorFilterId = a} :: DeleteTrafficMirrorFilter)
{-# DEPRECATED dtmftTrafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead." #-}

instance Lude.AWSRequest DeleteTrafficMirrorFilter where
  type
    Rs DeleteTrafficMirrorFilter =
      DeleteTrafficMirrorFilterResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteTrafficMirrorFilterResponse'
            Lude.<$> (x Lude..@? "trafficMirrorFilterId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTrafficMirrorFilter where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteTrafficMirrorFilter where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTrafficMirrorFilter where
  toQuery DeleteTrafficMirrorFilter' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteTrafficMirrorFilter" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "TrafficMirrorFilterId" Lude.=: trafficMirrorFilterId
      ]

-- | /See:/ 'mkDeleteTrafficMirrorFilterResponse' smart constructor.
data DeleteTrafficMirrorFilterResponse = DeleteTrafficMirrorFilterResponse'
  { trafficMirrorFilterId ::
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

-- | Creates a value of 'DeleteTrafficMirrorFilterResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'trafficMirrorFilterId' - The ID of the Traffic Mirror filter.
mkDeleteTrafficMirrorFilterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTrafficMirrorFilterResponse
mkDeleteTrafficMirrorFilterResponse pResponseStatus_ =
  DeleteTrafficMirrorFilterResponse'
    { trafficMirrorFilterId =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the Traffic Mirror filter.
--
-- /Note:/ Consider using 'trafficMirrorFilterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfrsTrafficMirrorFilterId :: Lens.Lens' DeleteTrafficMirrorFilterResponse (Lude.Maybe Lude.Text)
dtmfrsTrafficMirrorFilterId = Lens.lens (trafficMirrorFilterId :: DeleteTrafficMirrorFilterResponse -> Lude.Maybe Lude.Text) (\s a -> s {trafficMirrorFilterId = a} :: DeleteTrafficMirrorFilterResponse)
{-# DEPRECATED dtmfrsTrafficMirrorFilterId "Use generic-lens or generic-optics with 'trafficMirrorFilterId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfrsResponseStatus :: Lens.Lens' DeleteTrafficMirrorFilterResponse Lude.Int
dtmfrsResponseStatus = Lens.lens (responseStatus :: DeleteTrafficMirrorFilterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTrafficMirrorFilterResponse)
{-# DEPRECATED dtmfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
