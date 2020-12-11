{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeAdjustmentTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available adjustment types for Amazon EC2 Auto Scaling scaling policies. These settings apply to step scaling policies and simple scaling policies; they do not apply to target tracking scaling policies.
--
-- The following adjustment types are supported:
--
--     * ChangeInCapacity
--
--
--     * ExactCapacity
--
--
--     * PercentChangeInCapacity
module Network.AWS.AutoScaling.DescribeAdjustmentTypes
  ( -- * Creating a request
    DescribeAdjustmentTypes (..),
    mkDescribeAdjustmentTypes,

    -- * Destructuring the response
    DescribeAdjustmentTypesResponse (..),
    mkDescribeAdjustmentTypesResponse,

    -- ** Response lenses
    datrsAdjustmentTypes,
    datrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeAdjustmentTypes' smart constructor.
data DescribeAdjustmentTypes = DescribeAdjustmentTypes'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeAdjustmentTypes' with the minimum fields required to make a request.
mkDescribeAdjustmentTypes ::
  DescribeAdjustmentTypes
mkDescribeAdjustmentTypes = DescribeAdjustmentTypes'

instance Lude.AWSRequest DescribeAdjustmentTypes where
  type Rs DescribeAdjustmentTypes = DescribeAdjustmentTypesResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DescribeAdjustmentTypesResult"
      ( \s h x ->
          DescribeAdjustmentTypesResponse'
            Lude.<$> ( x Lude..@? "AdjustmentTypes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeAdjustmentTypes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeAdjustmentTypes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeAdjustmentTypes where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action" Lude.=: ("DescribeAdjustmentTypes" :: Lude.ByteString),
            "Version" Lude.=: ("2011-01-01" :: Lude.ByteString)
          ]
      )

-- | /See:/ 'mkDescribeAdjustmentTypesResponse' smart constructor.
data DescribeAdjustmentTypesResponse = DescribeAdjustmentTypesResponse'
  { adjustmentTypes ::
      Lude.Maybe [AdjustmentType],
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

-- | Creates a value of 'DescribeAdjustmentTypesResponse' with the minimum fields required to make a request.
--
-- * 'adjustmentTypes' - The policy adjustment types.
-- * 'responseStatus' - The response status code.
mkDescribeAdjustmentTypesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeAdjustmentTypesResponse
mkDescribeAdjustmentTypesResponse pResponseStatus_ =
  DescribeAdjustmentTypesResponse'
    { adjustmentTypes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The policy adjustment types.
--
-- /Note:/ Consider using 'adjustmentTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrsAdjustmentTypes :: Lens.Lens' DescribeAdjustmentTypesResponse (Lude.Maybe [AdjustmentType])
datrsAdjustmentTypes = Lens.lens (adjustmentTypes :: DescribeAdjustmentTypesResponse -> Lude.Maybe [AdjustmentType]) (\s a -> s {adjustmentTypes = a} :: DescribeAdjustmentTypesResponse)
{-# DEPRECATED datrsAdjustmentTypes "Use generic-lens or generic-optics with 'adjustmentTypes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
datrsResponseStatus :: Lens.Lens' DescribeAdjustmentTypesResponse Lude.Int
datrsResponseStatus = Lens.lens (responseStatus :: DescribeAdjustmentTypesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeAdjustmentTypesResponse)
{-# DEPRECATED datrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
