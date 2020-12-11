{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeLifecycleHooks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the lifecycle hooks for the specified Auto Scaling group.
module Network.AWS.AutoScaling.DescribeLifecycleHooks
  ( -- * Creating a request
    DescribeLifecycleHooks (..),
    mkDescribeLifecycleHooks,

    -- ** Request lenses
    dlhLifecycleHookNames,
    dlhAutoScalingGroupName,

    -- * Destructuring the response
    DescribeLifecycleHooksResponse (..),
    mkDescribeLifecycleHooksResponse,

    -- ** Response lenses
    dlhrsLifecycleHooks,
    dlhrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLifecycleHooks' smart constructor.
data DescribeLifecycleHooks = DescribeLifecycleHooks'
  { lifecycleHookNames ::
      Lude.Maybe [Lude.Text],
    autoScalingGroupName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLifecycleHooks' with the minimum fields required to make a request.
--
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
-- * 'lifecycleHookNames' - The names of one or more lifecycle hooks. If you omit this parameter, all lifecycle hooks are described.
mkDescribeLifecycleHooks ::
  -- | 'autoScalingGroupName'
  Lude.Text ->
  DescribeLifecycleHooks
mkDescribeLifecycleHooks pAutoScalingGroupName_ =
  DescribeLifecycleHooks'
    { lifecycleHookNames = Lude.Nothing,
      autoScalingGroupName = pAutoScalingGroupName_
    }

-- | The names of one or more lifecycle hooks. If you omit this parameter, all lifecycle hooks are described.
--
-- /Note:/ Consider using 'lifecycleHookNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlhLifecycleHookNames :: Lens.Lens' DescribeLifecycleHooks (Lude.Maybe [Lude.Text])
dlhLifecycleHookNames = Lens.lens (lifecycleHookNames :: DescribeLifecycleHooks -> Lude.Maybe [Lude.Text]) (\s a -> s {lifecycleHookNames = a} :: DescribeLifecycleHooks)
{-# DEPRECATED dlhLifecycleHookNames "Use generic-lens or generic-optics with 'lifecycleHookNames' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlhAutoScalingGroupName :: Lens.Lens' DescribeLifecycleHooks Lude.Text
dlhAutoScalingGroupName = Lens.lens (autoScalingGroupName :: DescribeLifecycleHooks -> Lude.Text) (\s a -> s {autoScalingGroupName = a} :: DescribeLifecycleHooks)
{-# DEPRECATED dlhAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

instance Lude.AWSRequest DescribeLifecycleHooks where
  type Rs DescribeLifecycleHooks = DescribeLifecycleHooksResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DescribeLifecycleHooksResult"
      ( \s h x ->
          DescribeLifecycleHooksResponse'
            Lude.<$> ( x Lude..@? "LifecycleHooks" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLifecycleHooks where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeLifecycleHooks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLifecycleHooks where
  toQuery DescribeLifecycleHooks' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeLifecycleHooks" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "LifecycleHookNames"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "member" Lude.<$> lifecycleHookNames),
        "AutoScalingGroupName" Lude.=: autoScalingGroupName
      ]

-- | /See:/ 'mkDescribeLifecycleHooksResponse' smart constructor.
data DescribeLifecycleHooksResponse = DescribeLifecycleHooksResponse'
  { lifecycleHooks ::
      Lude.Maybe [LifecycleHook],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLifecycleHooksResponse' with the minimum fields required to make a request.
--
-- * 'lifecycleHooks' - The lifecycle hooks for the specified group.
-- * 'responseStatus' - The response status code.
mkDescribeLifecycleHooksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLifecycleHooksResponse
mkDescribeLifecycleHooksResponse pResponseStatus_ =
  DescribeLifecycleHooksResponse'
    { lifecycleHooks = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The lifecycle hooks for the specified group.
--
-- /Note:/ Consider using 'lifecycleHooks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlhrsLifecycleHooks :: Lens.Lens' DescribeLifecycleHooksResponse (Lude.Maybe [LifecycleHook])
dlhrsLifecycleHooks = Lens.lens (lifecycleHooks :: DescribeLifecycleHooksResponse -> Lude.Maybe [LifecycleHook]) (\s a -> s {lifecycleHooks = a} :: DescribeLifecycleHooksResponse)
{-# DEPRECATED dlhrsLifecycleHooks "Use generic-lens or generic-optics with 'lifecycleHooks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlhrsResponseStatus :: Lens.Lens' DescribeLifecycleHooksResponse Lude.Int
dlhrsResponseStatus = Lens.lens (responseStatus :: DescribeLifecycleHooksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLifecycleHooksResponse)
{-# DEPRECATED dlhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
