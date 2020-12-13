{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeLifecycleHookTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available types of lifecycle hooks.
--
-- The following hook types are supported:
--
--     * autoscaling:EC2_INSTANCE_LAUNCHING
--
--
--     * autoscaling:EC2_INSTANCE_TERMINATING
module Network.AWS.AutoScaling.DescribeLifecycleHookTypes
  ( -- * Creating a request
    DescribeLifecycleHookTypes (..),
    mkDescribeLifecycleHookTypes,

    -- * Destructuring the response
    DescribeLifecycleHookTypesResponse (..),
    mkDescribeLifecycleHookTypesResponse,

    -- ** Response lenses
    dlhtrsLifecycleHookTypes,
    dlhtrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLifecycleHookTypes' smart constructor.
data DescribeLifecycleHookTypes = DescribeLifecycleHookTypes'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLifecycleHookTypes' with the minimum fields required to make a request.
mkDescribeLifecycleHookTypes ::
  DescribeLifecycleHookTypes
mkDescribeLifecycleHookTypes = DescribeLifecycleHookTypes'

instance Lude.AWSRequest DescribeLifecycleHookTypes where
  type
    Rs DescribeLifecycleHookTypes =
      DescribeLifecycleHookTypesResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DescribeLifecycleHookTypesResult"
      ( \s h x ->
          DescribeLifecycleHookTypesResponse'
            Lude.<$> ( x Lude..@? "LifecycleHookTypes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeLifecycleHookTypes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeLifecycleHookTypes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeLifecycleHookTypes where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action"
              Lude.=: ("DescribeLifecycleHookTypes" :: Lude.ByteString),
            "Version" Lude.=: ("2011-01-01" :: Lude.ByteString)
          ]
      )

-- | /See:/ 'mkDescribeLifecycleHookTypesResponse' smart constructor.
data DescribeLifecycleHookTypesResponse = DescribeLifecycleHookTypesResponse'
  { -- | The lifecycle hook types.
    lifecycleHookTypes :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLifecycleHookTypesResponse' with the minimum fields required to make a request.
--
-- * 'lifecycleHookTypes' - The lifecycle hook types.
-- * 'responseStatus' - The response status code.
mkDescribeLifecycleHookTypesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeLifecycleHookTypesResponse
mkDescribeLifecycleHookTypesResponse pResponseStatus_ =
  DescribeLifecycleHookTypesResponse'
    { lifecycleHookTypes =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The lifecycle hook types.
--
-- /Note:/ Consider using 'lifecycleHookTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlhtrsLifecycleHookTypes :: Lens.Lens' DescribeLifecycleHookTypesResponse (Lude.Maybe [Lude.Text])
dlhtrsLifecycleHookTypes = Lens.lens (lifecycleHookTypes :: DescribeLifecycleHookTypesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {lifecycleHookTypes = a} :: DescribeLifecycleHookTypesResponse)
{-# DEPRECATED dlhtrsLifecycleHookTypes "Use generic-lens or generic-optics with 'lifecycleHookTypes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlhtrsResponseStatus :: Lens.Lens' DescribeLifecycleHookTypesResponse Lude.Int
dlhtrsResponseStatus = Lens.lens (responseStatus :: DescribeLifecycleHookTypesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeLifecycleHookTypesResponse)
{-# DEPRECATED dlhtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
