{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.AttachPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a policy to the specified target.
module Network.AWS.IoT.AttachPolicy
  ( -- * Creating a request
    AttachPolicy (..),
    mkAttachPolicy,

    -- ** Request lenses
    apPolicyName,
    apTarget,

    -- * Destructuring the response
    AttachPolicyResponse (..),
    mkAttachPolicyResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAttachPolicy' smart constructor.
data AttachPolicy = AttachPolicy'
  { -- | The name of the policy to attach.
    policyName :: Lude.Text,
    -- | The <https://docs.aws.amazon.com/iot/latest/developerguide/security-iam.html identity> to which the policy is attached.
    target :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachPolicy' with the minimum fields required to make a request.
--
-- * 'policyName' - The name of the policy to attach.
-- * 'target' - The <https://docs.aws.amazon.com/iot/latest/developerguide/security-iam.html identity> to which the policy is attached.
mkAttachPolicy ::
  -- | 'policyName'
  Lude.Text ->
  -- | 'target'
  Lude.Text ->
  AttachPolicy
mkAttachPolicy pPolicyName_ pTarget_ =
  AttachPolicy' {policyName = pPolicyName_, target = pTarget_}

-- | The name of the policy to attach.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apPolicyName :: Lens.Lens' AttachPolicy Lude.Text
apPolicyName = Lens.lens (policyName :: AttachPolicy -> Lude.Text) (\s a -> s {policyName = a} :: AttachPolicy)
{-# DEPRECATED apPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The <https://docs.aws.amazon.com/iot/latest/developerguide/security-iam.html identity> to which the policy is attached.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apTarget :: Lens.Lens' AttachPolicy Lude.Text
apTarget = Lens.lens (target :: AttachPolicy -> Lude.Text) (\s a -> s {target = a} :: AttachPolicy)
{-# DEPRECATED apTarget "Use generic-lens or generic-optics with 'target' instead." #-}

instance Lude.AWSRequest AttachPolicy where
  type Rs AttachPolicy = AttachPolicyResponse
  request = Req.putJSON ioTService
  response = Res.receiveNull AttachPolicyResponse'

instance Lude.ToHeaders AttachPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON AttachPolicy where
  toJSON AttachPolicy' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("target" Lude..= target)])

instance Lude.ToPath AttachPolicy where
  toPath AttachPolicy' {..} =
    Lude.mconcat ["/target-policies/", Lude.toBS policyName]

instance Lude.ToQuery AttachPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAttachPolicyResponse' smart constructor.
data AttachPolicyResponse = AttachPolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachPolicyResponse' with the minimum fields required to make a request.
mkAttachPolicyResponse ::
  AttachPolicyResponse
mkAttachPolicyResponse = AttachPolicyResponse'
