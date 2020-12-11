{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.UpdatePolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing policy with a new name, description, or content. If you don't supply any parameter, that value remains unchanged. You can't change a policy's type.
--
-- This operation can be called only from the organization's management account.
module Network.AWS.Organizations.UpdatePolicy
  ( -- * Creating a request
    UpdatePolicy (..),
    mkUpdatePolicy,

    -- ** Request lenses
    upContent,
    upName,
    upDescription,
    upPolicyId,

    -- * Destructuring the response
    UpdatePolicyResponse (..),
    mkUpdatePolicyResponse,

    -- ** Response lenses
    uprsPolicy,
    uprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdatePolicy' smart constructor.
data UpdatePolicy = UpdatePolicy'
  { content :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    policyId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePolicy' with the minimum fields required to make a request.
--
-- * 'content' - If provided, the new content for the policy. The text must be correctly formatted JSON that complies with the syntax for the policy's type. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_scp-syntax.html Service Control Policy Syntax> in the /AWS Organizations User Guide./
-- * 'description' - If provided, the new description for the policy.
-- * 'name' - If provided, the new name for the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
-- * 'policyId' - The unique identifier (ID) of the policy that you want to update.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
mkUpdatePolicy ::
  -- | 'policyId'
  Lude.Text ->
  UpdatePolicy
mkUpdatePolicy pPolicyId_ =
  UpdatePolicy'
    { content = Lude.Nothing,
      name = Lude.Nothing,
      description = Lude.Nothing,
      policyId = pPolicyId_
    }

-- | If provided, the new content for the policy. The text must be correctly formatted JSON that complies with the syntax for the policy's type. For more information, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_reference_scp-syntax.html Service Control Policy Syntax> in the /AWS Organizations User Guide./
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upContent :: Lens.Lens' UpdatePolicy (Lude.Maybe Lude.Text)
upContent = Lens.lens (content :: UpdatePolicy -> Lude.Maybe Lude.Text) (\s a -> s {content = a} :: UpdatePolicy)
{-# DEPRECATED upContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | If provided, the new name for the policy.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upName :: Lens.Lens' UpdatePolicy (Lude.Maybe Lude.Text)
upName = Lens.lens (name :: UpdatePolicy -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdatePolicy)
{-# DEPRECATED upName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | If provided, the new description for the policy.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upDescription :: Lens.Lens' UpdatePolicy (Lude.Maybe Lude.Text)
upDescription = Lens.lens (description :: UpdatePolicy -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdatePolicy)
{-# DEPRECATED upDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The unique identifier (ID) of the policy that you want to update.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
--
-- /Note:/ Consider using 'policyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upPolicyId :: Lens.Lens' UpdatePolicy Lude.Text
upPolicyId = Lens.lens (policyId :: UpdatePolicy -> Lude.Text) (\s a -> s {policyId = a} :: UpdatePolicy)
{-# DEPRECATED upPolicyId "Use generic-lens or generic-optics with 'policyId' instead." #-}

instance Lude.AWSRequest UpdatePolicy where
  type Rs UpdatePolicy = UpdatePolicyResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdatePolicyResponse'
            Lude.<$> (x Lude..?> "Policy") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdatePolicy where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSOrganizationsV20161128.UpdatePolicy" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdatePolicy where
  toJSON UpdatePolicy' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Content" Lude..=) Lude.<$> content,
            ("Name" Lude..=) Lude.<$> name,
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("PolicyId" Lude..= policyId)
          ]
      )

instance Lude.ToPath UpdatePolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdatePolicy where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdatePolicyResponse' smart constructor.
data UpdatePolicyResponse = UpdatePolicyResponse'
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

-- | Creates a value of 'UpdatePolicyResponse' with the minimum fields required to make a request.
--
-- * 'policy' - A structure that contains details about the updated policy, showing the requested changes.
-- * 'responseStatus' - The response status code.
mkUpdatePolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdatePolicyResponse
mkUpdatePolicyResponse pResponseStatus_ =
  UpdatePolicyResponse'
    { policy = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure that contains details about the updated policy, showing the requested changes.
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsPolicy :: Lens.Lens' UpdatePolicyResponse (Lude.Maybe Policy)
uprsPolicy = Lens.lens (policy :: UpdatePolicyResponse -> Lude.Maybe Policy) (\s a -> s {policy = a} :: UpdatePolicyResponse)
{-# DEPRECATED uprsPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uprsResponseStatus :: Lens.Lens' UpdatePolicyResponse Lude.Int
uprsResponseStatus = Lens.lens (responseStatus :: UpdatePolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdatePolicyResponse)
{-# DEPRECATED uprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
