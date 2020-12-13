{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTrafficMirrorFilterRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Traffic Mirror rule.
module Network.AWS.EC2.DeleteTrafficMirrorFilterRule
  ( -- * Creating a request
    DeleteTrafficMirrorFilterRule (..),
    mkDeleteTrafficMirrorFilterRule,

    -- ** Request lenses
    dtmfrTrafficMirrorFilterRuleId,
    dtmfrDryRun,

    -- * Destructuring the response
    DeleteTrafficMirrorFilterRuleResponse (..),
    mkDeleteTrafficMirrorFilterRuleResponse,

    -- ** Response lenses
    dtmfrrsTrafficMirrorFilterRuleId,
    dtmfrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTrafficMirrorFilterRule' smart constructor.
data DeleteTrafficMirrorFilterRule = DeleteTrafficMirrorFilterRule'
  { -- | The ID of the Traffic Mirror rule.
    trafficMirrorFilterRuleId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTrafficMirrorFilterRule' with the minimum fields required to make a request.
--
-- * 'trafficMirrorFilterRuleId' - The ID of the Traffic Mirror rule.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDeleteTrafficMirrorFilterRule ::
  -- | 'trafficMirrorFilterRuleId'
  Lude.Text ->
  DeleteTrafficMirrorFilterRule
mkDeleteTrafficMirrorFilterRule pTrafficMirrorFilterRuleId_ =
  DeleteTrafficMirrorFilterRule'
    { trafficMirrorFilterRuleId =
        pTrafficMirrorFilterRuleId_,
      dryRun = Lude.Nothing
    }

-- | The ID of the Traffic Mirror rule.
--
-- /Note:/ Consider using 'trafficMirrorFilterRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfrTrafficMirrorFilterRuleId :: Lens.Lens' DeleteTrafficMirrorFilterRule Lude.Text
dtmfrTrafficMirrorFilterRuleId = Lens.lens (trafficMirrorFilterRuleId :: DeleteTrafficMirrorFilterRule -> Lude.Text) (\s a -> s {trafficMirrorFilterRuleId = a} :: DeleteTrafficMirrorFilterRule)
{-# DEPRECATED dtmfrTrafficMirrorFilterRuleId "Use generic-lens or generic-optics with 'trafficMirrorFilterRuleId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfrDryRun :: Lens.Lens' DeleteTrafficMirrorFilterRule (Lude.Maybe Lude.Bool)
dtmfrDryRun = Lens.lens (dryRun :: DeleteTrafficMirrorFilterRule -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteTrafficMirrorFilterRule)
{-# DEPRECATED dtmfrDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DeleteTrafficMirrorFilterRule where
  type
    Rs DeleteTrafficMirrorFilterRule =
      DeleteTrafficMirrorFilterRuleResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteTrafficMirrorFilterRuleResponse'
            Lude.<$> (x Lude..@? "trafficMirrorFilterRuleId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTrafficMirrorFilterRule where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteTrafficMirrorFilterRule where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTrafficMirrorFilterRule where
  toQuery DeleteTrafficMirrorFilterRule' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteTrafficMirrorFilterRule" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "TrafficMirrorFilterRuleId" Lude.=: trafficMirrorFilterRuleId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDeleteTrafficMirrorFilterRuleResponse' smart constructor.
data DeleteTrafficMirrorFilterRuleResponse = DeleteTrafficMirrorFilterRuleResponse'
  { -- | The ID of the deleted Traffic Mirror rule.
    trafficMirrorFilterRuleId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTrafficMirrorFilterRuleResponse' with the minimum fields required to make a request.
--
-- * 'trafficMirrorFilterRuleId' - The ID of the deleted Traffic Mirror rule.
-- * 'responseStatus' - The response status code.
mkDeleteTrafficMirrorFilterRuleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTrafficMirrorFilterRuleResponse
mkDeleteTrafficMirrorFilterRuleResponse pResponseStatus_ =
  DeleteTrafficMirrorFilterRuleResponse'
    { trafficMirrorFilterRuleId =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the deleted Traffic Mirror rule.
--
-- /Note:/ Consider using 'trafficMirrorFilterRuleId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfrrsTrafficMirrorFilterRuleId :: Lens.Lens' DeleteTrafficMirrorFilterRuleResponse (Lude.Maybe Lude.Text)
dtmfrrsTrafficMirrorFilterRuleId = Lens.lens (trafficMirrorFilterRuleId :: DeleteTrafficMirrorFilterRuleResponse -> Lude.Maybe Lude.Text) (\s a -> s {trafficMirrorFilterRuleId = a} :: DeleteTrafficMirrorFilterRuleResponse)
{-# DEPRECATED dtmfrrsTrafficMirrorFilterRuleId "Use generic-lens or generic-optics with 'trafficMirrorFilterRuleId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtmfrrsResponseStatus :: Lens.Lens' DeleteTrafficMirrorFilterRuleResponse Lude.Int
dtmfrrsResponseStatus = Lens.lens (responseStatus :: DeleteTrafficMirrorFilterRuleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTrafficMirrorFilterRuleResponse)
{-# DEPRECATED dtmfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
