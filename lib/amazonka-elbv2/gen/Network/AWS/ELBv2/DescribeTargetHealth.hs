{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.DescribeTargetHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the health of the specified targets or all of your targets.
module Network.AWS.ELBv2.DescribeTargetHealth
  ( -- * Creating a request
    DescribeTargetHealth (..),
    mkDescribeTargetHealth,

    -- ** Request lenses
    dthTargetGroupARN,
    dthTargets,

    -- * Destructuring the response
    DescribeTargetHealthResponse (..),
    mkDescribeTargetHealthResponse,

    -- ** Response lenses
    dthrsTargetHealthDescriptions,
    dthrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTargetHealth' smart constructor.
data DescribeTargetHealth = DescribeTargetHealth'
  { -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupARN :: Lude.Text,
    -- | The targets.
    targets :: Lude.Maybe [TargetDescription]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTargetHealth' with the minimum fields required to make a request.
--
-- * 'targetGroupARN' - The Amazon Resource Name (ARN) of the target group.
-- * 'targets' - The targets.
mkDescribeTargetHealth ::
  -- | 'targetGroupARN'
  Lude.Text ->
  DescribeTargetHealth
mkDescribeTargetHealth pTargetGroupARN_ =
  DescribeTargetHealth'
    { targetGroupARN = pTargetGroupARN_,
      targets = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the target group.
--
-- /Note:/ Consider using 'targetGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dthTargetGroupARN :: Lens.Lens' DescribeTargetHealth Lude.Text
dthTargetGroupARN = Lens.lens (targetGroupARN :: DescribeTargetHealth -> Lude.Text) (\s a -> s {targetGroupARN = a} :: DescribeTargetHealth)
{-# DEPRECATED dthTargetGroupARN "Use generic-lens or generic-optics with 'targetGroupARN' instead." #-}

-- | The targets.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dthTargets :: Lens.Lens' DescribeTargetHealth (Lude.Maybe [TargetDescription])
dthTargets = Lens.lens (targets :: DescribeTargetHealth -> Lude.Maybe [TargetDescription]) (\s a -> s {targets = a} :: DescribeTargetHealth)
{-# DEPRECATED dthTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

instance Lude.AWSRequest DescribeTargetHealth where
  type Rs DescribeTargetHealth = DescribeTargetHealthResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "DescribeTargetHealthResult"
      ( \s h x ->
          DescribeTargetHealthResponse'
            Lude.<$> ( x Lude..@? "TargetHealthDescriptions" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTargetHealth where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeTargetHealth where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTargetHealth where
  toQuery DescribeTargetHealth' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DescribeTargetHealth" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "TargetGroupArn" Lude.=: targetGroupARN,
        "Targets"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> targets)
      ]

-- | /See:/ 'mkDescribeTargetHealthResponse' smart constructor.
data DescribeTargetHealthResponse = DescribeTargetHealthResponse'
  { -- | Information about the health of the targets.
    targetHealthDescriptions :: Lude.Maybe [TargetHealthDescription],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTargetHealthResponse' with the minimum fields required to make a request.
--
-- * 'targetHealthDescriptions' - Information about the health of the targets.
-- * 'responseStatus' - The response status code.
mkDescribeTargetHealthResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTargetHealthResponse
mkDescribeTargetHealthResponse pResponseStatus_ =
  DescribeTargetHealthResponse'
    { targetHealthDescriptions =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the health of the targets.
--
-- /Note:/ Consider using 'targetHealthDescriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dthrsTargetHealthDescriptions :: Lens.Lens' DescribeTargetHealthResponse (Lude.Maybe [TargetHealthDescription])
dthrsTargetHealthDescriptions = Lens.lens (targetHealthDescriptions :: DescribeTargetHealthResponse -> Lude.Maybe [TargetHealthDescription]) (\s a -> s {targetHealthDescriptions = a} :: DescribeTargetHealthResponse)
{-# DEPRECATED dthrsTargetHealthDescriptions "Use generic-lens or generic-optics with 'targetHealthDescriptions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dthrsResponseStatus :: Lens.Lens' DescribeTargetHealthResponse Lude.Int
dthrsResponseStatus = Lens.lens (responseStatus :: DescribeTargetHealthResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTargetHealthResponse)
{-# DEPRECATED dthrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
