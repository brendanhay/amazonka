{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.PutPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Firewall Manager policy.
--
-- Firewall Manager provides the following types of policies:
--
--     * An AWS WAF policy (type WAFV2), which defines rule groups to run first in the corresponding AWS WAF web ACL and rule groups to run last in the web ACL.
--
--
--     * An AWS WAF Classic policy (type WAF), which defines a rule group.
--
--
--     * A Shield Advanced policy, which applies Shield Advanced protection to specified accounts and resources.
--
--
--     * A security group policy, which manages VPC security groups across your AWS organization.
--
--
--     * An AWS Network Firewall policy, which provides firewall rules to filter network traffic in specified Amazon VPCs.
--
--
-- Each policy is specific to one of the types. If you want to enforce more than one policy type across accounts, create multiple policies. You can create multiple policies for each type.
-- You must be subscribed to Shield Advanced to create a Shield Advanced policy. For more information about subscribing to Shield Advanced, see <https://docs.aws.amazon.com/waf/latest/DDOSAPIReference/API_CreateSubscription.html CreateSubscription> .
module Network.AWS.FMS.PutPolicy
  ( -- * Creating a request
    PutPolicy (..),
    mkPutPolicy,

    -- ** Request lenses
    ppTagList,
    ppPolicy,

    -- * Destructuring the response
    PutPolicyResponse (..),
    mkPutPolicyResponse,

    -- ** Response lenses
    pprsPolicyARN,
    pprsPolicy,
    pprsResponseStatus,
  )
where

import Network.AWS.FMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutPolicy' smart constructor.
data PutPolicy = PutPolicy'
  { -- | The tags to add to the AWS resource.
    tagList :: Lude.Maybe [Tag],
    -- | The details of the AWS Firewall Manager policy to be created.
    policy :: Policy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutPolicy' with the minimum fields required to make a request.
--
-- * 'tagList' - The tags to add to the AWS resource.
-- * 'policy' - The details of the AWS Firewall Manager policy to be created.
mkPutPolicy ::
  -- | 'policy'
  Policy ->
  PutPolicy
mkPutPolicy pPolicy_ =
  PutPolicy' {tagList = Lude.Nothing, policy = pPolicy_}

-- | The tags to add to the AWS resource.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppTagList :: Lens.Lens' PutPolicy (Lude.Maybe [Tag])
ppTagList = Lens.lens (tagList :: PutPolicy -> Lude.Maybe [Tag]) (\s a -> s {tagList = a} :: PutPolicy)
{-# DEPRECATED ppTagList "Use generic-lens or generic-optics with 'tagList' instead." #-}

-- | The details of the AWS Firewall Manager policy to be created.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppPolicy :: Lens.Lens' PutPolicy Policy
ppPolicy = Lens.lens (policy :: PutPolicy -> Policy) (\s a -> s {policy = a} :: PutPolicy)
{-# DEPRECATED ppPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

instance Lude.AWSRequest PutPolicy where
  type Rs PutPolicy = PutPolicyResponse
  request = Req.postJSON fmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutPolicyResponse'
            Lude.<$> (x Lude..?> "PolicyArn")
            Lude.<*> (x Lude..?> "Policy")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutPolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSFMS_20180101.PutPolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutPolicy where
  toJSON PutPolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TagList" Lude..=) Lude.<$> tagList,
            Lude.Just ("Policy" Lude..= policy)
          ]
      )

instance Lude.ToPath PutPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutPolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutPolicyResponse' smart constructor.
data PutPolicyResponse = PutPolicyResponse'
  { -- | The Amazon Resource Name (ARN) of the policy.
    policyARN :: Lude.Maybe Lude.Text,
    -- | The details of the AWS Firewall Manager policy.
    policy :: Lude.Maybe Policy,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutPolicyResponse' with the minimum fields required to make a request.
--
-- * 'policyARN' - The Amazon Resource Name (ARN) of the policy.
-- * 'policy' - The details of the AWS Firewall Manager policy.
-- * 'responseStatus' - The response status code.
mkPutPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutPolicyResponse
mkPutPolicyResponse pResponseStatus_ =
  PutPolicyResponse'
    { policyARN = Lude.Nothing,
      policy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the policy.
--
-- /Note:/ Consider using 'policyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pprsPolicyARN :: Lens.Lens' PutPolicyResponse (Lude.Maybe Lude.Text)
pprsPolicyARN = Lens.lens (policyARN :: PutPolicyResponse -> Lude.Maybe Lude.Text) (\s a -> s {policyARN = a} :: PutPolicyResponse)
{-# DEPRECATED pprsPolicyARN "Use generic-lens or generic-optics with 'policyARN' instead." #-}

-- | The details of the AWS Firewall Manager policy.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pprsPolicy :: Lens.Lens' PutPolicyResponse (Lude.Maybe Policy)
pprsPolicy = Lens.lens (policy :: PutPolicyResponse -> Lude.Maybe Policy) (\s a -> s {policy = a} :: PutPolicyResponse)
{-# DEPRECATED pprsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pprsResponseStatus :: Lens.Lens' PutPolicyResponse Lude.Int
pprsResponseStatus = Lens.lens (responseStatus :: PutPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutPolicyResponse)
{-# DEPRECATED pprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
