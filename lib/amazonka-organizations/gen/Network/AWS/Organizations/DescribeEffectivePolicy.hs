{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DescribeEffectivePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the contents of the effective policy for specified policy type and account. The effective policy is the aggregation of any policies of the specified type that the account inherits, plus any policy of that type that is directly attached to the account.
--
-- This operation applies only to policy types /other/ than service control policies (SCPs).
-- For more information about policy inheritance, see <http://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies-inheritance.html How Policy Inheritance Works> in the /AWS Organizations User Guide/ .
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
module Network.AWS.Organizations.DescribeEffectivePolicy
  ( -- * Creating a request
    DescribeEffectivePolicy (..),
    mkDescribeEffectivePolicy,

    -- ** Request lenses
    depTargetId,
    depPolicyType,

    -- * Destructuring the response
    DescribeEffectivePolicyResponse (..),
    mkDescribeEffectivePolicyResponse,

    -- ** Response lenses
    deprsEffectivePolicy,
    deprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeEffectivePolicy' smart constructor.
data DescribeEffectivePolicy = DescribeEffectivePolicy'
  { -- | When you're signed in as the management account, specify the ID of the account that you want details about. Specifying an organization root or organizational unit (OU) as the target is not supported.
    targetId :: Lude.Maybe Lude.Text,
    -- | The type of policy that you want information about. You can specify one of the following values:
    --
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
    --
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
    --
    --
    --     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
    policyType :: EffectivePolicyType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEffectivePolicy' with the minimum fields required to make a request.
--
-- * 'targetId' - When you're signed in as the management account, specify the ID of the account that you want details about. Specifying an organization root or organizational unit (OU) as the target is not supported.
-- * 'policyType' - The type of policy that you want information about. You can specify one of the following values:
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
mkDescribeEffectivePolicy ::
  -- | 'policyType'
  EffectivePolicyType ->
  DescribeEffectivePolicy
mkDescribeEffectivePolicy pPolicyType_ =
  DescribeEffectivePolicy'
    { targetId = Lude.Nothing,
      policyType = pPolicyType_
    }

-- | When you're signed in as the management account, specify the ID of the account that you want details about. Specifying an organization root or organizational unit (OU) as the target is not supported.
--
-- /Note:/ Consider using 'targetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
depTargetId :: Lens.Lens' DescribeEffectivePolicy (Lude.Maybe Lude.Text)
depTargetId = Lens.lens (targetId :: DescribeEffectivePolicy -> Lude.Maybe Lude.Text) (\s a -> s {targetId = a} :: DescribeEffectivePolicy)
{-# DEPRECATED depTargetId "Use generic-lens or generic-optics with 'targetId' instead." #-}

-- | The type of policy that you want information about. You can specify one of the following values:
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_ai-opt-out.html AISERVICES_OPT_OUT_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_backup.html BACKUP_POLICY>
--
--
--     * <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_manage_policies_tag-policies.html TAG_POLICY>
--
--
--
-- /Note:/ Consider using 'policyType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
depPolicyType :: Lens.Lens' DescribeEffectivePolicy EffectivePolicyType
depPolicyType = Lens.lens (policyType :: DescribeEffectivePolicy -> EffectivePolicyType) (\s a -> s {policyType = a} :: DescribeEffectivePolicy)
{-# DEPRECATED depPolicyType "Use generic-lens or generic-optics with 'policyType' instead." #-}

instance Lude.AWSRequest DescribeEffectivePolicy where
  type Rs DescribeEffectivePolicy = DescribeEffectivePolicyResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeEffectivePolicyResponse'
            Lude.<$> (x Lude..?> "EffectivePolicy")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeEffectivePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSOrganizationsV20161128.DescribeEffectivePolicy" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeEffectivePolicy where
  toJSON DescribeEffectivePolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TargetId" Lude..=) Lude.<$> targetId,
            Lude.Just ("PolicyType" Lude..= policyType)
          ]
      )

instance Lude.ToPath DescribeEffectivePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeEffectivePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeEffectivePolicyResponse' smart constructor.
data DescribeEffectivePolicyResponse = DescribeEffectivePolicyResponse'
  { -- | The contents of the effective policy.
    effectivePolicy :: Lude.Maybe EffectivePolicy,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeEffectivePolicyResponse' with the minimum fields required to make a request.
--
-- * 'effectivePolicy' - The contents of the effective policy.
-- * 'responseStatus' - The response status code.
mkDescribeEffectivePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeEffectivePolicyResponse
mkDescribeEffectivePolicyResponse pResponseStatus_ =
  DescribeEffectivePolicyResponse'
    { effectivePolicy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The contents of the effective policy.
--
-- /Note:/ Consider using 'effectivePolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deprsEffectivePolicy :: Lens.Lens' DescribeEffectivePolicyResponse (Lude.Maybe EffectivePolicy)
deprsEffectivePolicy = Lens.lens (effectivePolicy :: DescribeEffectivePolicyResponse -> Lude.Maybe EffectivePolicy) (\s a -> s {effectivePolicy = a} :: DescribeEffectivePolicyResponse)
{-# DEPRECATED deprsEffectivePolicy "Use generic-lens or generic-optics with 'effectivePolicy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deprsResponseStatus :: Lens.Lens' DescribeEffectivePolicyResponse Lude.Int
deprsResponseStatus = Lens.lens (responseStatus :: DescribeEffectivePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeEffectivePolicyResponse)
{-# DEPRECATED deprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
