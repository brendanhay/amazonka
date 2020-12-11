{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DescribePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a policy.
--
-- This operation can be called only from the organization's management account or by a member account that is a delegated administrator for an AWS service.
module Network.AWS.Organizations.DescribePolicy
  ( -- * Creating a request
    DescribePolicy (..),
    mkDescribePolicy,

    -- ** Request lenses
    dpPolicyId,

    -- * Destructuring the response
    DescribePolicyResponse (..),
    mkDescribePolicyResponse,

    -- ** Response lenses
    dprsPolicy,
    dprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribePolicy' smart constructor.
newtype DescribePolicy = DescribePolicy' {policyId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePolicy' with the minimum fields required to make a request.
--
-- * 'policyId' - The unique identifier (ID) of the policy that you want details about. You can get the ID from the 'ListPolicies' or 'ListPoliciesForTarget' operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
mkDescribePolicy ::
  -- | 'policyId'
  Lude.Text ->
  DescribePolicy
mkDescribePolicy pPolicyId_ =
  DescribePolicy' {policyId = pPolicyId_}

-- | The unique identifier (ID) of the policy that you want details about. You can get the ID from the 'ListPolicies' or 'ListPoliciesForTarget' operations.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpPolicyId :: Lens.Lens' DescribePolicy Lude.Text
dpPolicyId = Lens.lens (policyId :: DescribePolicy -> Lude.Text) (\s a -> s {policyId = a} :: DescribePolicy)
{-# DEPRECATED dpPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

instance Lude.AWSRequest DescribePolicy where
  type Rs DescribePolicy = DescribePolicyResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribePolicyResponse'
            Lude.<$> (x Lude..?> "Policy") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSOrganizationsV20161128.DescribePolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribePolicy where
  toJSON DescribePolicy' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("PolicyId" Lude..= policyId)])

instance Lude.ToPath DescribePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribePolicyResponse' smart constructor.
data DescribePolicyResponse = DescribePolicyResponse'
  { policy ::
      Lude.Maybe Policy,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribePolicyResponse' with the minimum fields required to make a request.
--
-- * 'policy' - A structure that contains details about the specified policy.
-- * 'responseStatus' - The response status code.
mkDescribePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribePolicyResponse
mkDescribePolicyResponse pResponseStatus_ =
  DescribePolicyResponse'
    { policy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure that contains details about the specified policy.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsPolicy :: Lens.Lens' DescribePolicyResponse (Lude.Maybe Policy)
dprsPolicy = Lens.lens (policy :: DescribePolicyResponse -> Lude.Maybe Policy) (\s a -> s {policy = a} :: DescribePolicyResponse)
{-# DEPRECATED dprsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dprsResponseStatus :: Lens.Lens' DescribePolicyResponse Lude.Int
dprsResponseStatus = Lens.lens (responseStatus :: DescribePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribePolicyResponse)
{-# DEPRECATED dprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
