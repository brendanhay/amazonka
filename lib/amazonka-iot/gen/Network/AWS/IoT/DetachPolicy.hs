{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DetachPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a policy from the specified target.
module Network.AWS.IoT.DetachPolicy
  ( -- * Creating a request
    DetachPolicy (..),
    mkDetachPolicy,

    -- ** Request lenses
    dPolicyName,
    dTarget,

    -- * Destructuring the response
    DetachPolicyResponse (..),
    mkDetachPolicyResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetachPolicy' smart constructor.
data DetachPolicy = DetachPolicy'
  { -- | The policy to detach.
    policyName :: Lude.Text,
    -- | The target from which the policy will be detached.
    target :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachPolicy' with the minimum fields required to make a request.
--
-- * 'policyName' - The policy to detach.
-- * 'target' - The target from which the policy will be detached.
mkDetachPolicy ::
  -- | 'policyName'
  Lude.Text ->
  -- | 'target'
  Lude.Text ->
  DetachPolicy
mkDetachPolicy pPolicyName_ pTarget_ =
  DetachPolicy' {policyName = pPolicyName_, target = pTarget_}

-- | The policy to detach.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dPolicyName :: Lens.Lens' DetachPolicy Lude.Text
dPolicyName = Lens.lens (policyName :: DetachPolicy -> Lude.Text) (\s a -> s {policyName = a} :: DetachPolicy)
{-# DEPRECATED dPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The target from which the policy will be detached.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dTarget :: Lens.Lens' DetachPolicy Lude.Text
dTarget = Lens.lens (target :: DetachPolicy -> Lude.Text) (\s a -> s {target = a} :: DetachPolicy)
{-# DEPRECATED dTarget "Use generic-lens or generic-optics with 'target' instead." #-}

instance Lude.AWSRequest DetachPolicy where
  type Rs DetachPolicy = DetachPolicyResponse
  request = Req.postJSON ioTService
  response = Res.receiveNull DetachPolicyResponse'

instance Lude.ToHeaders DetachPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON DetachPolicy where
  toJSON DetachPolicy' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("target" Lude..= target)])

instance Lude.ToPath DetachPolicy where
  toPath DetachPolicy' {..} =
    Lude.mconcat ["/target-policies/", Lude.toBS policyName]

instance Lude.ToQuery DetachPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetachPolicyResponse' smart constructor.
data DetachPolicyResponse = DetachPolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachPolicyResponse' with the minimum fields required to make a request.
mkDetachPolicyResponse ::
  DetachPolicyResponse
mkDetachPolicyResponse = DetachPolicyResponse'
