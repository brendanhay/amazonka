{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the notification types that are supported by Amazon EC2 Auto Scaling.
module Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes
  ( -- * Creating a request
    DescribeAutoScalingNotificationTypes (..),
    mkDescribeAutoScalingNotificationTypes,

    -- * Destructuring the response
    DescribeAutoScalingNotificationTypesResponse (..),
    mkDescribeAutoScalingNotificationTypesResponse,

    -- ** Response lenses
    dasntrsAutoScalingNotificationTypes,
    dasntrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAutoScalingNotificationTypes' smart constructor.
data DescribeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypes'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAutoScalingNotificationTypes' with the minimum fields required to make a request.
mkDescribeAutoScalingNotificationTypes ::
  DescribeAutoScalingNotificationTypes
mkDescribeAutoScalingNotificationTypes =
  DescribeAutoScalingNotificationTypes'

instance Lude.AWSRequest DescribeAutoScalingNotificationTypes where
  type
    Rs DescribeAutoScalingNotificationTypes =
      DescribeAutoScalingNotificationTypesResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DescribeAutoScalingNotificationTypesResult"
      ( \s h x ->
          DescribeAutoScalingNotificationTypesResponse'
            Lude.<$> ( x Lude..@? "AutoScalingNotificationTypes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAutoScalingNotificationTypes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAutoScalingNotificationTypes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAutoScalingNotificationTypes where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action"
              Lude.=: ("DescribeAutoScalingNotificationTypes" :: Lude.ByteString),
            "Version" Lude.=: ("2011-01-01" :: Lude.ByteString)
          ]
      )

-- | /See:/ 'mkDescribeAutoScalingNotificationTypesResponse' smart constructor.
data DescribeAutoScalingNotificationTypesResponse = DescribeAutoScalingNotificationTypesResponse'
  { -- | The notification types.
    autoScalingNotificationTypes :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAutoScalingNotificationTypesResponse' with the minimum fields required to make a request.
--
-- * 'autoScalingNotificationTypes' - The notification types.
-- * 'responseStatus' - The response status code.
mkDescribeAutoScalingNotificationTypesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAutoScalingNotificationTypesResponse
mkDescribeAutoScalingNotificationTypesResponse pResponseStatus_ =
  DescribeAutoScalingNotificationTypesResponse'
    { autoScalingNotificationTypes =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The notification types.
--
-- /Note:/ Consider using 'autoScalingNotificationTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasntrsAutoScalingNotificationTypes :: Lens.Lens' DescribeAutoScalingNotificationTypesResponse (Lude.Maybe [Lude.Text])
dasntrsAutoScalingNotificationTypes = Lens.lens (autoScalingNotificationTypes :: DescribeAutoScalingNotificationTypesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {autoScalingNotificationTypes = a} :: DescribeAutoScalingNotificationTypesResponse)
{-# DEPRECATED dasntrsAutoScalingNotificationTypes "Use generic-lens or generic-optics with 'autoScalingNotificationTypes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasntrsResponseStatus :: Lens.Lens' DescribeAutoScalingNotificationTypesResponse Lude.Int
dasntrsResponseStatus = Lens.lens (responseStatus :: DescribeAutoScalingNotificationTypesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAutoScalingNotificationTypesResponse)
{-# DEPRECATED dasntrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
