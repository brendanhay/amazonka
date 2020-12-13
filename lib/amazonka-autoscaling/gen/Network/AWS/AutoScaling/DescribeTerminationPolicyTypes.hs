{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the termination policies supported by Amazon EC2 Auto Scaling.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html Controlling which Auto Scaling instances terminate during scale in> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.DescribeTerminationPolicyTypes
  ( -- * Creating a request
    DescribeTerminationPolicyTypes (..),
    mkDescribeTerminationPolicyTypes,

    -- * Destructuring the response
    DescribeTerminationPolicyTypesResponse (..),
    mkDescribeTerminationPolicyTypesResponse,

    -- ** Response lenses
    dtptrsTerminationPolicyTypes,
    dtptrsResponseStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTerminationPolicyTypes' smart constructor.
data DescribeTerminationPolicyTypes = DescribeTerminationPolicyTypes'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTerminationPolicyTypes' with the minimum fields required to make a request.
mkDescribeTerminationPolicyTypes ::
  DescribeTerminationPolicyTypes
mkDescribeTerminationPolicyTypes = DescribeTerminationPolicyTypes'

instance Lude.AWSRequest DescribeTerminationPolicyTypes where
  type
    Rs DescribeTerminationPolicyTypes =
      DescribeTerminationPolicyTypesResponse
  request = Req.postQuery autoScalingService
  response =
    Res.receiveXMLWrapper
      "DescribeTerminationPolicyTypesResult"
      ( \s h x ->
          DescribeTerminationPolicyTypesResponse'
            Lude.<$> ( x Lude..@? "TerminationPolicyTypes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTerminationPolicyTypes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeTerminationPolicyTypes where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTerminationPolicyTypes where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action"
              Lude.=: ("DescribeTerminationPolicyTypes" :: Lude.ByteString),
            "Version" Lude.=: ("2011-01-01" :: Lude.ByteString)
          ]
      )

-- | /See:/ 'mkDescribeTerminationPolicyTypesResponse' smart constructor.
data DescribeTerminationPolicyTypesResponse = DescribeTerminationPolicyTypesResponse'
  { -- | The termination policies supported by Amazon EC2 Auto Scaling: @OldestInstance@ , @OldestLaunchConfiguration@ , @NewestInstance@ , @ClosestToNextInstanceHour@ , @Default@ , @OldestLaunchTemplate@ , and @AllocationStrategy@ .
    terminationPolicyTypes :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTerminationPolicyTypesResponse' with the minimum fields required to make a request.
--
-- * 'terminationPolicyTypes' - The termination policies supported by Amazon EC2 Auto Scaling: @OldestInstance@ , @OldestLaunchConfiguration@ , @NewestInstance@ , @ClosestToNextInstanceHour@ , @Default@ , @OldestLaunchTemplate@ , and @AllocationStrategy@ .
-- * 'responseStatus' - The response status code.
mkDescribeTerminationPolicyTypesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTerminationPolicyTypesResponse
mkDescribeTerminationPolicyTypesResponse pResponseStatus_ =
  DescribeTerminationPolicyTypesResponse'
    { terminationPolicyTypes =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The termination policies supported by Amazon EC2 Auto Scaling: @OldestInstance@ , @OldestLaunchConfiguration@ , @NewestInstance@ , @ClosestToNextInstanceHour@ , @Default@ , @OldestLaunchTemplate@ , and @AllocationStrategy@ .
--
-- /Note:/ Consider using 'terminationPolicyTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtptrsTerminationPolicyTypes :: Lens.Lens' DescribeTerminationPolicyTypesResponse (Lude.Maybe [Lude.Text])
dtptrsTerminationPolicyTypes = Lens.lens (terminationPolicyTypes :: DescribeTerminationPolicyTypesResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {terminationPolicyTypes = a} :: DescribeTerminationPolicyTypesResponse)
{-# DEPRECATED dtptrsTerminationPolicyTypes "Use generic-lens or generic-optics with 'terminationPolicyTypes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtptrsResponseStatus :: Lens.Lens' DescribeTerminationPolicyTypesResponse Lude.Int
dtptrsResponseStatus = Lens.lens (responseStatus :: DescribeTerminationPolicyTypesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTerminationPolicyTypesResponse)
{-# DEPRECATED dtptrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
