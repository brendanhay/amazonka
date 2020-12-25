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
    pipIdentity,
    pipPolicyName,
    pipPolicy,

    -- * Destructuring the response
    PutIdentityPolicyResponse (..),
    mkPutIdentityPolicyResponse,

    -- ** Response lenses
    piprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SES.Types as Types

-- | Represents a request to add or update a sending authorization policy for an identity. Sending authorization is an Amazon SES feature that enables you to authorize other senders to use your identities. For information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkPutIdentityPolicy' smart constructor.
data PutIdentityPolicy = PutIdentityPolicy'
  { -- | The identity that the policy will apply to. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
    --
    -- To successfully call this API, you must own the identity.
    identity :: Types.Identity,
    -- | The name of the policy.
    --
    -- The policy name cannot exceed 64 characters and can only include alphanumeric characters, dashes, and underscores.
    policyName :: Types.PolicyName,
    -- | The text of the policy in JSON format. The policy cannot exceed 4 KB.
    --
    -- For information about the syntax of sending authorization policies, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-policies.html Amazon SES Developer Guide> .
    policy :: Types.Policy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PutIdentityPolicy' value with any optional fields omitted.
mkPutIdentityPolicy ::
  -- | 'identity'
  Types.Identity ->
  -- | 'policyName'
  Types.PolicyName ->
  -- | 'policy'
  Types.Policy ->
  PutIdentityPolicy
mkPutIdentityPolicy identity policyName policy =
  PutIdentityPolicy' {identity, policyName, policy}

-- | The identity that the policy will apply to. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
--
-- To successfully call this API, you must own the identity.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipIdentity :: Lens.Lens' PutIdentityPolicy Types.Identity
pipIdentity = Lens.field @"identity"
{-# DEPRECATED pipIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

-- | The name of the policy.
--
-- The policy name cannot exceed 64 characters and can only include alphanumeric characters, dashes, and underscores.
--
-- /Note:/ Consider using 'policyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipPolicyName :: Lens.Lens' PutIdentityPolicy Types.PolicyName
pipPolicyName = Lens.field @"policyName"
{-# DEPRECATED pipPolicyName "Use generic-lens or generic-optics with 'policyName' instead." #-}

-- | The text of the policy in JSON format. The policy cannot exceed 4 KB.
--
-- For information about the syntax of sending authorization policies, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization-policies.html Amazon SES Developer Guide> .
--
-- /Note:/ Consider using 'policy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pipPolicy :: Lens.Lens' PutIdentityPolicy Types.Policy
pipPolicy = Lens.field @"policy"
{-# DEPRECATED pipPolicy "Use generic-lens or generic-optics with 'policy' instead." #-}

instance Core.AWSRequest PutIdentityPolicy where
  type Rs PutIdentityPolicy = PutIdentityPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "PutIdentityPolicy")
                Core.<> (Core.pure ("Version", "2010-12-01"))
                Core.<> (Core.toQueryValue "Identity" identity)
                Core.<> (Core.toQueryValue "PolicyName" policyName)
                Core.<> (Core.toQueryValue "Policy" policy)
            )
      }
  response =
    Response.receiveXMLWrapper
      "PutIdentityPolicyResult"
      ( \s h x ->
          PutIdentityPolicyResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | An empty element returned on a successful request.
--
-- /See:/ 'mkPutIdentityPolicyResponse' smart constructor.
newtype PutIdentityPolicyResponse = PutIdentityPolicyResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutIdentityPolicyResponse' value with any optional fields omitted.
mkPutIdentityPolicyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  PutIdentityPolicyResponse
mkPutIdentityPolicyResponse responseStatus =
  PutIdentityPolicyResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piprrsResponseStatus :: Lens.Lens' PutIdentityPolicyResponse Core.Int
piprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED piprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
