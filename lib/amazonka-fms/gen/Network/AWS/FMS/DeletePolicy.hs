{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.DeletePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes an AWS Firewall Manager policy.
module Network.AWS.FMS.DeletePolicy
  ( -- * Creating a request
    DeletePolicy (..),
    mkDeletePolicy,

    -- ** Request lenses
    dpPolicyId,
    dpDeleteAllPolicyResources,

    -- * Destructuring the response
    DeletePolicyResponse (..),
    mkDeletePolicyResponse,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeletePolicy' smart constructor.
data DeletePolicy = DeletePolicy'
  { -- | The ID of the policy that you want to delete. You can retrieve this ID from @PutPolicy@ and @ListPolicies@ .
    policyId :: Lude.Text,
    -- | If @True@ , the request performs cleanup according to the policy type.
    --
    -- For AWS WAF and Shield Advanced policies, the cleanup does the following:
    --
    --     * Deletes rule groups created by AWS Firewall Manager
    --
    --
    --     * Removes web ACLs from in-scope resources
    --
    --
    --     * Deletes web ACLs that contain no rules or rule groups
    --
    --
    -- For security group policies, the cleanup does the following for each security group in the policy:
    --
    --     * Disassociates the security group from in-scope resources
    --
    --
    --     * Deletes the security group if it was created through Firewall Manager and if it's no longer associated with any resources through another policy
    --
    --
    -- After the cleanup, in-scope resources are no longer protected by web ACLs in this policy. Protection of out-of-scope resources remains unchanged. Scope is determined by tags that you create and accounts that you associate with the policy. When creating the policy, if you specify that only resources in specific accounts or with specific tags are in scope of the policy, those accounts and resources are handled by the policy. All others are out of scope. If you don't specify tags or accounts, all resources are in scope.
    deleteAllPolicyResources :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePolicy' with the minimum fields required to make a request.
--
-- * 'policyId' - The ID of the policy that you want to delete. You can retrieve this ID from @PutPolicy@ and @ListPolicies@ .
-- * 'deleteAllPolicyResources' - If @True@ , the request performs cleanup according to the policy type.
--
-- For AWS WAF and Shield Advanced policies, the cleanup does the following:
--
--     * Deletes rule groups created by AWS Firewall Manager
--
--
--     * Removes web ACLs from in-scope resources
--
--
--     * Deletes web ACLs that contain no rules or rule groups
--
--
-- For security group policies, the cleanup does the following for each security group in the policy:
--
--     * Disassociates the security group from in-scope resources
--
--
--     * Deletes the security group if it was created through Firewall Manager and if it's no longer associated with any resources through another policy
--
--
-- After the cleanup, in-scope resources are no longer protected by web ACLs in this policy. Protection of out-of-scope resources remains unchanged. Scope is determined by tags that you create and accounts that you associate with the policy. When creating the policy, if you specify that only resources in specific accounts or with specific tags are in scope of the policy, those accounts and resources are handled by the policy. All others are out of scope. If you don't specify tags or accounts, all resources are in scope.
mkDeletePolicy ::
  -- | 'policyId'
  Lude.Text ->
  DeletePolicy
mkDeletePolicy pPolicyId_ =
  DeletePolicy'
    { policyId = pPolicyId_,
      deleteAllPolicyResources = Lude.Nothing
    }

-- | The ID of the policy that you want to delete. You can retrieve this ID from @PutPolicy@ and @ListPolicies@ .
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPolicyId :: Lens.Lens' DeletePolicy Lude.Text
dpPolicyId = Lens.lens (policyId :: DeletePolicy -> Lude.Text) (\s a -> s {policyId = a} :: DeletePolicy)
{-# DEPRECATED dpPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

-- | If @True@ , the request performs cleanup according to the policy type.
--
-- For AWS WAF and Shield Advanced policies, the cleanup does the following:
--
--     * Deletes rule groups created by AWS Firewall Manager
--
--
--     * Removes web ACLs from in-scope resources
--
--
--     * Deletes web ACLs that contain no rules or rule groups
--
--
-- For security group policies, the cleanup does the following for each security group in the policy:
--
--     * Disassociates the security group from in-scope resources
--
--
--     * Deletes the security group if it was created through Firewall Manager and if it's no longer associated with any resources through another policy
--
--
-- After the cleanup, in-scope resources are no longer protected by web ACLs in this policy. Protection of out-of-scope resources remains unchanged. Scope is determined by tags that you create and accounts that you associate with the policy. When creating the policy, if you specify that only resources in specific accounts or with specific tags are in scope of the policy, those accounts and resources are handled by the policy. All others are out of scope. If you don't specify tags or accounts, all resources are in scope.
--
-- /Note:/ Consider using 'deleteAllPolicyResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDeleteAllPolicyResources :: Lens.Lens' DeletePolicy (Lude.Maybe Lude.Bool)
dpDeleteAllPolicyResources = Lens.lens (deleteAllPolicyResources :: DeletePolicy -> Lude.Maybe Lude.Bool) (\s a -> s {deleteAllPolicyResources = a} :: DeletePolicy)
{-# DEPRECATED dpDeleteAllPolicyResources "Use generic-lens or generic-optics with 'deleteAllPolicyResources' instead." #-}

instance Lude.AWSRequest DeletePolicy where
  type Rs DeletePolicy = DeletePolicyResponse
  request = Req.postJSON fmsService
  response = Res.receiveNull DeletePolicyResponse'

instance Lude.ToHeaders DeletePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.DeletePolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeletePolicy where
  toJSON DeletePolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PolicyId" Lude..= policyId),
            ("DeleteAllPolicyResources" Lude..=)
              Lude.<$> deleteAllPolicyResources
          ]
      )

instance Lude.ToPath DeletePolicy where
  toPath = Lude.const "/"

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
