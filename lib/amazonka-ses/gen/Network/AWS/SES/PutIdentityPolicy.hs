{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.PutIdentityPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds or updates a sending authorization policy for the specified identity (an email address or a domain).
--
-- Sending authorization is a feature that enables an identity owner to authorize other senders to use its identities. For information about using sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.PutIdentityPolicy
  ( -- * Creating a request
    PutIdentityPolicy (..),
    mkPutIdentityPolicy,

    -- ** Request lenses
    pipPolicyName,
    pipPolicy,
    pipIdentity,

    -- * Destructuring the response
    PutIdentityPolicyResponse (..),
    mkPutIdentityPolicyResponse,

    -- ** Response lenses
    piprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to add or update a sending authorization policy for an identity. Sending authorization is an Amazon SES feature that enables you to authorize other senders to use your identities. For information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkPutIdentityPolicy' smart constructor.
data PutIdentityPolicy = PutIdentityPolicy'
  { -- | The name of the policy.
    --
    -- The policy name cannot exceed 64 characters and can only include alphanumeric characters, dashes, and underscores.
    policyName :: Lude.Text,
    -- | The text of the policy in JSON format. The policy cannot exceed 4 KB.
    --
    -- For information about the syntax of sending authorization policies, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-policies.html Amazon SES Developer Guide> .
    policy :: Lude.Text,
    -- | The identity that the policy will apply to. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
    --
    -- To successfully call this API, you must own the identity.
    identity :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutIdentityPolicy' with the minimum fields required to make a request.
--
-- * 'policyName' - The name of the policy.
--
-- The policy name cannot exceed 64 characters and can only include alphanumeric characters, dashes, and underscores.
-- * 'policy' - The text of the policy in JSON format. The policy cannot exceed 4 KB.
--
-- For information about the syntax of sending authorization policies, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-policies.html Amazon SES Developer Guide> .
-- * 'identity' - The identity that the policy will apply to. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
--
-- To successfully call this API, you must own the identity.
mkPutIdentityPolicy ::
  -- | 'policyName'
  Lude.Text ->
  -- | 'policy'
  Lude.Text ->
  -- | 'identity'
  Lude.Text ->
  PutIdentityPolicy
mkPutIdentityPolicy pPolicyName_ pPolicy_ pIdentity_ =
  PutIdentityPolicy'
    { policyName = pPolicyName_,
      policy = pPolicy_,
      identity = pIdentity_
    }

-- | The name of the policy.
--
-- The policy name cannot exceed 64 characters and can only include alphanumeric characters, dashes, and underscores.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipPolicyName :: Lens.Lens' PutIdentityPolicy Lude.Text
pipPolicyName = Lens.lens (policyName :: PutIdentityPolicy -> Lude.Text) (\s a -> s {policyName = a} :: PutIdentityPolicy)
{-# DEPRECATED pipPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The text of the policy in JSON format. The policy cannot exceed 4 KB.
--
-- For information about the syntax of sending authorization policies, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-policies.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipPolicy :: Lens.Lens' PutIdentityPolicy Lude.Text
pipPolicy = Lens.lens (policy :: PutIdentityPolicy -> Lude.Text) (\s a -> s {policy = a} :: PutIdentityPolicy)
{-# DEPRECATED pipPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

-- | The identity that the policy will apply to. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
--
-- To successfully call this API, you must own the identity.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipIdentity :: Lens.Lens' PutIdentityPolicy Lude.Text
pipIdentity = Lens.lens (identity :: PutIdentityPolicy -> Lude.Text) (\s a -> s {identity = a} :: PutIdentityPolicy)
{-# DEPRECATED pipIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

instance Lude.AWSRequest PutIdentityPolicy where
  type Rs PutIdentityPolicy = PutIdentityPolicyResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "PutIdentityPolicyResult"
      ( \s h x ->
          PutIdentityPolicyResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutIdentityPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath PutIdentityPolicy where
  toPath = Lude.const "/"

instance Lude.ToQuery PutIdentityPolicy where
  toQuery PutIdentityPolicy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("PutIdentityPolicy" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "PolicyName" Lude.=: policyName,
        "Policy" Lude.=: policy,
        "Identity" Lude.=: identity
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkPutIdentityPolicyResponse' smart constructor.
newtype PutIdentityPolicyResponse = PutIdentityPolicyResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutIdentityPolicyResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkPutIdentityPolicyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutIdentityPolicyResponse
mkPutIdentityPolicyResponse pResponseStatus_ =
  PutIdentityPolicyResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piprsResponseStatus :: Lens.Lens' PutIdentityPolicyResponse Lude.Int
piprsResponseStatus = Lens.lens (responseStatus :: PutIdentityPolicyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutIdentityPolicyResponse)
{-# DEPRECATED piprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
