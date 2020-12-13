{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeletePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified policy.
--
-- A policy cannot be deleted if it has non-default versions or it is attached to any certificate.
-- To delete a policy, use the DeletePolicyVersion API to delete all non-default versions of the policy; use the DetachPrincipalPolicy API to detach the policy from any certificate; and then use the DeletePolicy API to delete the policy.
-- When a policy is deleted using DeletePolicy, its default version is deleted with it.
module Network.AWS.IoT.DeletePolicy
  ( -- * Creating a request
    DeletePolicy (..),
    mkDeletePolicy,

    -- ** Request lenses
    dpPolicyName,

    -- * Destructuring the response
    DeletePolicyResponse (..),
    mkDeletePolicyResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the DeletePolicy operation.
--
-- /See:/ 'mkDeletePolicy' smart constructor.
newtype DeletePolicy = DeletePolicy'
  { -- | The name of the policy to delete.
    policyName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePolicy' with the minimum fields required to make a request.
--
-- * 'policyName' - The name of the policy to delete.
mkDeletePolicy ::
  -- | 'policyName'
  Lude.Text ->
  DeletePolicy
mkDeletePolicy pPolicyName_ =
  DeletePolicy' {policyName = pPolicyName_}

-- | The name of the policy to delete.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPolicyName :: Lens.Lens' DeletePolicy Lude.Text
dpPolicyName = Lens.lens (policyName :: DeletePolicy -> Lude.Text) (\s a -> s {policyName = a} :: DeletePolicy)
{-# DEPRECATED dpPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

instance Lude.AWSRequest DeletePolicy where
  type Rs DeletePolicy = DeletePolicyResponse
  request = Req.delete ioTService
  response = Res.receiveNull DeletePolicyResponse'

instance Lude.ToHeaders DeletePolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeletePolicy where
  toPath DeletePolicy' {..} =
    Lude.mconcat ["/policies/", Lude.toBS policyName]

instance Lude.ToQuery DeletePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeletePolicyResponse' smart constructor.
data DeletePolicyResponse = DeletePolicyResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePolicyResponse' with the minimum fields required to make a request.
mkDeletePolicyResponse ::
  DeletePolicyResponse
mkDeletePolicyResponse = DeletePolicyResponse'
