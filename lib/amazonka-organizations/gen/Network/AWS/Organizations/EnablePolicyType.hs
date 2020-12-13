{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.EnablePolicyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a policy type in a root. After you enable a policy type in a root, you can attach policies of that type to the root, any organizational unit (OU), or account in that root. You can undo this by using the 'DisablePolicyType' operation.
--
-- This is an asynchronous request that AWS performs in the background. AWS recommends that you first use 'ListRoots' to see the status of policy types for a specified root, and then use this operation.
-- This operation can be called only from the organization's management account.
-- You can enable a policy type in a root only if that policy type is available in the organization. To view the status of available policy types in the organization, use 'DescribeOrganization' .
module Network.AWS.Organizations.EnablePolicyType
  ( -- * Creating a request
    EnablePolicyType (..),
    mkEnablePolicyType,

    -- ** Request lenses
    eptPolicyType,
    eptRootId,

    -- * Destructuring the response
    EnablePolicyTypeResponse (..),
    mkEnablePolicyTypeResponse,

    -- ** Response lenses
    eptrsRoot,
    eptrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnablePolicyType' smart constructor.
data EnablePolicyType = EnablePolicyType'
  { -- | The policy type that you want to enable. You can specify one of the following values:
    --
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
    --
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
    --
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
    --
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
    policyType :: PolicyType,
    -- | The unique identifier (ID) of the root in which you want to enable a policy type. You can get the ID from the 'ListRoots' operation.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lowercase letters or digits.
    rootId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnablePolicyType' with the minimum fields required to make a request.
--
-- * 'policyType' - The policy type that you want to enable. You can specify one of the following values:
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
--
--
-- * 'rootId' - The unique identifier (ID) of the root in which you want to enable a policy type. You can get the ID from the 'ListRoots' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lowercase letters or digits.
mkEnablePolicyType ::
  -- | 'policyType'
  PolicyType ->
  -- | 'rootId'
  Lude.Text ->
  EnablePolicyType
mkEnablePolicyType pPolicyType_ pRootId_ =
  EnablePolicyType' {policyType = pPolicyType_, rootId = pRootId_}

-- | The policy type that you want to enable. You can specify one of the following values:
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_scp.html SERVICE_CONTROL_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
--
--
--
-- /Note:/ Consider using 'policyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eptPolicyType :: Lens.Lens' EnablePolicyType PolicyType
eptPolicyType = Lens.lens (policyType :: EnablePolicyType -> PolicyType) (\s a -> s {policyType = a} :: EnablePolicyType)
{-# DEPRECATED eptPolicyType "Use generic-lens or generic-optics with 'policyType' instead." #-}

-- | The unique identifier (ID) of the root in which you want to enable a policy type. You can get the ID from the 'ListRoots' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'rootId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eptRootId :: Lens.Lens' EnablePolicyType Lude.Text
eptRootId = Lens.lens (rootId :: EnablePolicyType -> Lude.Text) (\s a -> s {rootId = a} :: EnablePolicyType)
{-# DEPRECATED eptRootId "Use generic-lens or generic-optics with 'rootId' instead." #-}

instance Lude.AWSRequest EnablePolicyType where
  type Rs EnablePolicyType = EnablePolicyTypeResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          EnablePolicyTypeResponse'
            Lude.<$> (x Lude..?> "Root") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EnablePolicyType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSOrganizationsV20161128.EnablePolicyType" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON EnablePolicyType where
  toJSON EnablePolicyType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PolicyType" Lude..= policyType),
            Lude.Just ("RootId" Lude..= rootId)
          ]
      )

instance Lude.ToPath EnablePolicyType where
  toPath = Lude.const "/"

instance Lude.ToQuery EnablePolicyType where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkEnablePolicyTypeResponse' smart constructor.
data EnablePolicyTypeResponse = EnablePolicyTypeResponse'
  { -- | A structure that shows the root with the updated list of enabled policy types.
    root :: Lude.Maybe Root,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnablePolicyTypeResponse' with the minimum fields required to make a request.
--
-- * 'root' - A structure that shows the root with the updated list of enabled policy types.
-- * 'responseStatus' - The response status code.
mkEnablePolicyTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  EnablePolicyTypeResponse
mkEnablePolicyTypeResponse pResponseStatus_ =
  EnablePolicyTypeResponse'
    { root = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure that shows the root with the updated list of enabled policy types.
--
-- /Note:/ Consider using 'root' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eptrsRoot :: Lens.Lens' EnablePolicyTypeResponse (Lude.Maybe Root)
eptrsRoot = Lens.lens (root :: EnablePolicyTypeResponse -> Lude.Maybe Root) (\s a -> s {root = a} :: EnablePolicyTypeResponse)
{-# DEPRECATED eptrsRoot "Use generic-lens or generic-optics with 'root' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eptrsResponseStatus :: Lens.Lens' EnablePolicyTypeResponse Lude.Int
eptrsResponseStatus = Lens.lens (responseStatus :: EnablePolicyTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EnablePolicyTypeResponse)
{-# DEPRECATED eptrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
