{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DeletePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified scaling policy.
--
-- Deleting either a step scaling policy or a simple scaling policy deletes the underlying alarm action, but does not delete the alarm, even if it no longer has an associated action.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/deleting-scaling-policy.html Deleting a scaling policy> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.DeletePolicy
  ( -- * Creating a request
    DeletePolicy (..),
    mkDeletePolicy,

    -- ** Request lenses
    dpfPolicyName,
    dpfAutoScalingGroupName,

    -- * Destructuring the response
    DeletePolicyResponse (..),
    mkDeletePolicyResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeletePolicy' smart constructor.
data DeletePolicy = DeletePolicy'
  { -- | The name or Amazon Resource Name (ARN) of the policy.
    policyName :: Lude.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePolicy' with the minimum fields required to make a request.
--
-- * 'policyName' - The name or Amazon Resource Name (ARN) of the policy.
-- * 'autoScalingGroupName' - The name of the Auto Scaling group.
mkDeletePolicy ::
  -- | 'policyName'
  Lude.Text ->
  DeletePolicy
mkDeletePolicy pPolicyName_ =
  DeletePolicy'
    { policyName = pPolicyName_,
      autoScalingGroupName = Lude.Nothing
    }

-- | The name or Amazon Resource Name (ARN) of the policy.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfPolicyName :: Lens.Lens' DeletePolicy Lude.Text
dpfPolicyName = Lens.lens (policyName :: DeletePolicy -> Lude.Text) (\s a -> s {policyName = a} :: DeletePolicy)
{-# DEPRECATED dpfPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpfAutoScalingGroupName :: Lens.Lens' DeletePolicy (Lude.Maybe Lude.Text)
dpfAutoScalingGroupName = Lens.lens (autoScalingGroupName :: DeletePolicy -> Lude.Maybe Lude.Text) (\s a -> s {autoScalingGroupName = a} :: DeletePolicy)
{-# DEPRECATED dpfAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

instance Lude.AWSRequest DeletePolicy where
  type Rs DeletePolicy = DeletePolicyResponse
  request = Req.postQuery autoScalingService
  response = Res.receiveNull DeletePolicyResponse'

instance Lude.ToHeaders DeletePolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeletePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DeletePolicy where
  toQuery DeletePolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeletePolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2011-01-01" :: Lude.ByteString),
        "PolicyName" Lude.=: policyName,
        "AutoScalingGroupName" Lude.=: autoScalingGroupName
      ]

-- | /See:/ 'mkDeletePolicyResponse' smart constructor.
data DeletePolicyResponse = DeletePolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePolicyResponse' with the minimum fields required to make a request.
mkDeletePolicyResponse ::
  DeletePolicyResponse
mkDeletePolicyResponse = DeletePolicyResponse'
