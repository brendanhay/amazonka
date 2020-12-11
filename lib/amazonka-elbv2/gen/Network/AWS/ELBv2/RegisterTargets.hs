{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.RegisterTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers the specified targets with the specified target group.
--
-- If the target is an EC2 instance, it must be in the @running@ state when you register it.
-- By default, the load balancer routes requests to registered targets using the protocol and port for the target group. Alternatively, you can override the port for a target when you register it. You can register each EC2 instance or IP address with the same target group multiple times using different ports.
-- With a Network Load Balancer, you cannot register instances by instance ID if they have the following instance types: C1, CC1, CC2, CG1, CG2, CR1, CS1, G1, G2, HI1, HS1, M1, M2, M3, and T1. You can register instances of these types by IP address.
module Network.AWS.ELBv2.RegisterTargets
  ( -- * Creating a request
    RegisterTargets (..),
    mkRegisterTargets,

    -- ** Request lenses
    rtTargetGroupARN,
    rtTargets,

    -- * Destructuring the response
    RegisterTargetsResponse (..),
    mkRegisterTargetsResponse,

    -- ** Response lenses
    rrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRegisterTargets' smart constructor.
data RegisterTargets = RegisterTargets'
  { targetGroupARN ::
      Lude.Text,
    targets :: [TargetDescription]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RegisterTargets' with the minimum fields required to make a request.
--
-- * 'targetGroupARN' - The Amazon Resource Name (ARN) of the target group.
-- * 'targets' - The targets.
mkRegisterTargets ::
  -- | 'targetGroupARN'
  Lude.Text ->
  RegisterTargets
mkRegisterTargets pTargetGroupARN_ =
  RegisterTargets'
    { targetGroupARN = pTargetGroupARN_,
      targets = Lude.mempty
    }

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'targetGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTargetGroupARN :: Lens.Lens' RegisterTargets Lude.Text
rtTargetGroupARN = Lens.lens (targetGroupARN :: RegisterTargets -> Lude.Text) (\s a -> s {targetGroupARN = a} :: RegisterTargets)
{-# DEPRECATED rtTargetGroupARN "Use generic-lens or generic-optics with 'targetGroupARN' instead." #-}

-- | The targets.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTargets :: Lens.Lens' RegisterTargets [TargetDescription]
rtTargets = Lens.lens (targets :: RegisterTargets -> [TargetDescription]) (\s a -> s {targets = a} :: RegisterTargets)
{-# DEPRECATED rtTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

instance Lude.AWSRequest RegisterTargets where
  type Rs RegisterTargets = RegisterTargetsResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "RegisterTargetsResult"
      ( \s h x ->
          RegisterTargetsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RegisterTargets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RegisterTargets where
  toPath = Lude.const "/"

instance Lude.ToQuery RegisterTargets where
  toQuery RegisterTargets' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RegisterTargets" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "TargetGroupArn" Lude.=: targetGroupARN,
        "Targets" Lude.=: Lude.toQueryList "member" targets
      ]

-- | /See:/ 'mkRegisterTargetsResponse' smart constructor.
newtype RegisterTargetsResponse = RegisterTargetsResponse'
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

-- | Creates a value of 'RegisterTargetsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRegisterTargetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RegisterTargetsResponse
mkRegisterTargetsResponse pResponseStatus_ =
  RegisterTargetsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrsResponseStatus :: Lens.Lens' RegisterTargetsResponse Lude.Int
rrsResponseStatus = Lens.lens (responseStatus :: RegisterTargetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RegisterTargetsResponse)
{-# DEPRECATED rrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
