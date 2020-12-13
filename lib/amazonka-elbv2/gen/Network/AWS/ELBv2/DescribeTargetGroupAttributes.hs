{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DescribeTargetGroupAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the attributes for the specified target group.
--
-- For more information, see the following:
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/load-balancer-target-groups.html#target-group-attributes Target group attributes> in the /Application Load Balancers Guide/
--
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/load-balancer-target-groups.html#target-group-attributes Target group attributes> in the /Network Load Balancers Guide/
--
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/gateway/target-groups.html#target-group-attributes Target group attributes> in the /Gateway Load Balancers Guide/
module Network.AWS.ELBv2.DescribeTargetGroupAttributes
  ( -- * Creating a request
    DescribeTargetGroupAttributes (..),
    mkDescribeTargetGroupAttributes,

    -- ** Request lenses
    dtgaTargetGroupARN,

    -- * Destructuring the response
    DescribeTargetGroupAttributesResponse (..),
    mkDescribeTargetGroupAttributesResponse,

    -- ** Response lenses
    dtgarsAttributes,
    dtgarsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTargetGroupAttributes' smart constructor.
newtype DescribeTargetGroupAttributes = DescribeTargetGroupAttributes'
  { -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTargetGroupAttributes' with the minimum fields required to make a request.
--
-- * 'targetGroupARN' - The Amazon Resource Name (ARN) of the target group.
mkDescribeTargetGroupAttributes ::
  -- | 'targetGroupARN'
  Lude.Text ->
  DescribeTargetGroupAttributes
mkDescribeTargetGroupAttributes pTargetGroupARN_ =
  DescribeTargetGroupAttributes' {targetGroupARN = pTargetGroupARN_}

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'targetGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgaTargetGroupARN :: Lens.Lens' DescribeTargetGroupAttributes Lude.Text
dtgaTargetGroupARN = Lens.lens (targetGroupARN :: DescribeTargetGroupAttributes -> Lude.Text) (\s a -> s {targetGroupARN = a} :: DescribeTargetGroupAttributes)
{-# DEPRECATED dtgaTargetGroupARN "Use generic-lens or generic-optics with 'targetGroupARN' instead." #-}

instance Lude.AWSRequest DescribeTargetGroupAttributes where
  type
    Rs DescribeTargetGroupAttributes =
      DescribeTargetGroupAttributesResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "DescribeTargetGroupAttributesResult"
      ( \s h x ->
          DescribeTargetGroupAttributesResponse'
            Lude.<$> ( x Lude..@? "Attributes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTargetGroupAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeTargetGroupAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTargetGroupAttributes where
  toQuery DescribeTargetGroupAttributes' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DescribeTargetGroupAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "TargetGroupArn" Lude.=: targetGroupARN
      ]

-- | /See:/ 'mkDescribeTargetGroupAttributesResponse' smart constructor.
data DescribeTargetGroupAttributesResponse = DescribeTargetGroupAttributesResponse'
  { -- | Information about the target group attributes
    attributes :: Lude.Maybe [TargetGroupAttribute],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTargetGroupAttributesResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - Information about the target group attributes
-- * 'responseStatus' - The response status code.
mkDescribeTargetGroupAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTargetGroupAttributesResponse
mkDescribeTargetGroupAttributesResponse pResponseStatus_ =
  DescribeTargetGroupAttributesResponse'
    { attributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the target group attributes
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgarsAttributes :: Lens.Lens' DescribeTargetGroupAttributesResponse (Lude.Maybe [TargetGroupAttribute])
dtgarsAttributes = Lens.lens (attributes :: DescribeTargetGroupAttributesResponse -> Lude.Maybe [TargetGroupAttribute]) (\s a -> s {attributes = a} :: DescribeTargetGroupAttributesResponse)
{-# DEPRECATED dtgarsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgarsResponseStatus :: Lens.Lens' DescribeTargetGroupAttributesResponse Lude.Int
dtgarsResponseStatus = Lens.lens (responseStatus :: DescribeTargetGroupAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTargetGroupAttributesResponse)
{-# DEPRECATED dtgarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
