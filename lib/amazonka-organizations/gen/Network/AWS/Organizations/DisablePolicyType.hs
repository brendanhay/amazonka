{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DisablePolicyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables an organizational policy type in a root. A policy of a certain type can be attached to entities in a root only if that type is enabled in the root. After you perform this operation, you no longer can attach policies of the specified type to that root or to any organizational unit (OU) or account in that root. You can undo this by using the 'EnablePolicyType' operation.
--
-- This is an asynchronous request that AWS performs in the background. If you disable a policy type for a root, it still appears enabled for the organization if <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_org_support-all-features.html all features> are enabled for the organization. AWS recommends that you first use 'ListRoots' to see the status of policy types for a specified root, and then use this operation.
-- This operation can be called only from the organization's management account.
-- To view the status of available policy types in the organization, use 'DescribeOrganization' .
module Network.AWS.Organizations.DisablePolicyType
  ( -- * Creating a request
    DisablePolicyType (..),
    mkDisablePolicyType,

    -- ** Request lenses
    dptPolicyType,
    dptRootId,

    -- * Destructuring the response
    DisablePolicyTypeResponse (..),
    mkDisablePolicyTypeResponse,

    -- ** Response lenses
    dptrsRoot,
    dptrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisablePolicyType' smart constructor.
data DisablePolicyType = DisablePolicyType'
  { -- | The policy type that you want to disable in this root. You can specify one of the following values:
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
    -- | The unique identifier (ID) of the root in which you want to disable a policy type. You can get the ID from the 'ListRoots' operation.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lowercase letters or digits.
    rootId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisablePolicyType' with the minimum fields required to make a request.
--
-- * 'policyType' - The policy type that you want to disable in this root. You can specify one of the following values:
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
-- * 'rootId' - The unique identifier (ID) of the root in which you want to disable a policy type. You can get the ID from the 'ListRoots' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lowercase letters or digits.
mkDisablePolicyType ::
  -- | 'policyType'
  PolicyType ->
  -- | 'rootId'
  Lude.Text ->
  DisablePolicyType
mkDisablePolicyType pPolicyType_ pRootId_ =
  DisablePolicyType' {policyType = pPolicyType_, rootId = pRootId_}

-- | The policy type that you want to disable in this root. You can specify one of the following values:
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
dptPolicyType :: Lens.Lens' DisablePolicyType PolicyType
dptPolicyType = Lens.lens (policyType :: DisablePolicyType -> PolicyType) (\s a -> s {policyType = a} :: DisablePolicyType)
{-# DEPRECATED dptPolicyType "Use generic-lens or generic-optics with 'policyType' instead." #-}

-- | The unique identifier (ID) of the root in which you want to disable a policy type. You can get the ID from the 'ListRoots' operation.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'rootId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptRootId :: Lens.Lens' DisablePolicyType Lude.Text
dptRootId = Lens.lens (rootId :: DisablePolicyType -> Lude.Text) (\s a -> s {rootId = a} :: DisablePolicyType)
{-# DEPRECATED dptRootId "Use generic-lens or generic-optics with 'rootId' instead." #-}

instance Lude.AWSRequest DisablePolicyType where
  type Rs DisablePolicyType = DisablePolicyTypeResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DisablePolicyTypeResponse'
            Lude.<$> (x Lude..?> "Root") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisablePolicyType where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSOrganizationsV20161128.DisablePolicyType" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisablePolicyType where
  toJSON DisablePolicyType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("PolicyType" Lude..= policyType),
            Lude.Just ("RootId" Lude..= rootId)
          ]
      )

instance Lude.ToPath DisablePolicyType where
  toPath = Lude.const "/"

instance Lude.ToQuery DisablePolicyType where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisablePolicyTypeResponse' smart constructor.
data DisablePolicyTypeResponse = DisablePolicyTypeResponse'
  { -- | A structure that shows the root with the updated list of enabled policy types.
    root :: Lude.Maybe Root,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisablePolicyTypeResponse' with the minimum fields required to make a request.
--
-- * 'root' - A structure that shows the root with the updated list of enabled policy types.
-- * 'responseStatus' - The response status code.
mkDisablePolicyTypeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisablePolicyTypeResponse
mkDisablePolicyTypeResponse pResponseStatus_ =
  DisablePolicyTypeResponse'
    { root = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure that shows the root with the updated list of enabled policy types.
--
-- /Note:/ Consider using 'root' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrsRoot :: Lens.Lens' DisablePolicyTypeResponse (Lude.Maybe Root)
dptrsRoot = Lens.lens (root :: DisablePolicyTypeResponse -> Lude.Maybe Root) (\s a -> s {root = a} :: DisablePolicyTypeResponse)
{-# DEPRECATED dptrsRoot "Use generic-lens or generic-optics with 'root' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dptrsResponseStatus :: Lens.Lens' DisablePolicyTypeResponse Lude.Int
dptrsResponseStatus = Lens.lens (responseStatus :: DisablePolicyTypeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisablePolicyTypeResponse)
{-# DEPRECATED dptrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
