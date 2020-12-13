{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DeregisterTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters the specified targets from the specified target group. After the targets are deregistered, they no longer receive traffic from the load balancer.
module Network.AWS.ELBv2.DeregisterTargets
  ( -- * Creating a request
    DeregisterTargets (..),
    mkDeregisterTargets,

    -- ** Request lenses
    dtTargetGroupARN,
    dtTargets,

    -- * Destructuring the response
    DeregisterTargetsResponse (..),
    mkDeregisterTargetsResponse,

    -- ** Response lenses
    dtrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeregisterTargets' smart constructor.
data DeregisterTargets = DeregisterTargets'
  { -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupARN :: Lude.Text,
    -- | The targets. If you specified a port override when you registered a target, you must specify both the target ID and the port when you deregister it.
    targets :: [TargetDescription]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterTargets' with the minimum fields required to make a request.
--
-- * 'targetGroupARN' - The Amazon Resource Name (ARN) of the target group.
-- * 'targets' - The targets. If you specified a port override when you registered a target, you must specify both the target ID and the port when you deregister it.
mkDeregisterTargets ::
  -- | 'targetGroupARN'
  Lude.Text ->
  DeregisterTargets
mkDeregisterTargets pTargetGroupARN_ =
  DeregisterTargets'
    { targetGroupARN = pTargetGroupARN_,
      targets = Lude.mempty
    }

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'targetGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTargetGroupARN :: Lens.Lens' DeregisterTargets Lude.Text
dtTargetGroupARN = Lens.lens (targetGroupARN :: DeregisterTargets -> Lude.Text) (\s a -> s {targetGroupARN = a} :: DeregisterTargets)
{-# DEPRECATED dtTargetGroupARN "Use generic-lens or generic-optics with 'targetGroupARN' instead." #-}

-- | The targets. If you specified a port override when you registered a target, you must specify both the target ID and the port when you deregister it.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTargets :: Lens.Lens' DeregisterTargets [TargetDescription]
dtTargets = Lens.lens (targets :: DeregisterTargets -> [TargetDescription]) (\s a -> s {targets = a} :: DeregisterTargets)
{-# DEPRECATED dtTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

instance Lude.AWSRequest DeregisterTargets where
  type Rs DeregisterTargets = DeregisterTargetsResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "DeregisterTargetsResult"
      ( \s h x ->
          DeregisterTargetsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeregisterTargets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeregisterTargets where
  toPath = Lude.const "/"

instance Lude.ToQuery DeregisterTargets where
  toQuery DeregisterTargets' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeregisterTargets" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "TargetGroupArn" Lude.=: targetGroupARN,
        "Targets" Lude.=: Lude.toQueryList "member" targets
      ]

-- | /See:/ 'mkDeregisterTargetsResponse' smart constructor.
newtype DeregisterTargetsResponse = DeregisterTargetsResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeregisterTargetsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeregisterTargetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeregisterTargetsResponse
mkDeregisterTargetsResponse pResponseStatus_ =
  DeregisterTargetsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResponseStatus :: Lens.Lens' DeregisterTargetsResponse Lude.Int
dtrsResponseStatus = Lens.lens (responseStatus :: DeregisterTargetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeregisterTargetsResponse)
{-# DEPRECATED dtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
