{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.GetIdentityPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the requested sending authorization policies for the given identity (an email address or a domain). The policies are returned as a map of policy names to policy contents. You can retrieve a maximum of 20 policies at a time.
--
-- Sending authorization is a feature that enables an identity owner to authorize other senders to use its identities. For information about using sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.GetIdentityPolicies
  ( -- * Creating a request
    GetIdentityPolicies (..),
    mkGetIdentityPolicies,

    -- ** Request lenses
    gipPolicyNames,
    gipIdentity,

    -- * Destructuring the response
    GetIdentityPoliciesResponse (..),
    mkGetIdentityPoliciesResponse,

    -- ** Response lenses
    giprsPolicies,
    giprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to return the requested sending authorization policies for an identity. Sending authorization is an Amazon SES feature that enables you to authorize other senders to use your identities. For information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkGetIdentityPolicies' smart constructor.
data GetIdentityPolicies = GetIdentityPolicies'
  { -- | A list of the names of policies to be retrieved. You can retrieve a maximum of 20 policies at a time. If you do not know the names of the policies that are attached to the identity, you can use @ListIdentityPolicies@ .
    policyNames :: [Lude.Text],
    -- | The identity for which the policies will be retrieved. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
    --
    -- To successfully call this API, you must own the identity.
    identity :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIdentityPolicies' with the minimum fields required to make a request.
--
-- * 'policyNames' - A list of the names of policies to be retrieved. You can retrieve a maximum of 20 policies at a time. If you do not know the names of the policies that are attached to the identity, you can use @ListIdentityPolicies@ .
-- * 'identity' - The identity for which the policies will be retrieved. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
--
-- To successfully call this API, you must own the identity.
mkGetIdentityPolicies ::
  -- | 'identity'
  Lude.Text ->
  GetIdentityPolicies
mkGetIdentityPolicies pIdentity_ =
  GetIdentityPolicies'
    { policyNames = Lude.mempty,
      identity = pIdentity_
    }

-- | A list of the names of policies to be retrieved. You can retrieve a maximum of 20 policies at a time. If you do not know the names of the policies that are attached to the identity, you can use @ListIdentityPolicies@ .
--
-- /Note:/ Consider using 'policyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipPolicyNames :: Lens.Lens' GetIdentityPolicies [Lude.Text]
gipPolicyNames = Lens.lens (policyNames :: GetIdentityPolicies -> [Lude.Text]) (\s a -> s {policyNames = a} :: GetIdentityPolicies)
{-# DEPRECATED gipPolicyNames "Use generic-lens or generic-optics with 'policyNames' instead." #-}

-- | The identity for which the policies will be retrieved. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
--
-- To successfully call this API, you must own the identity.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gipIdentity :: Lens.Lens' GetIdentityPolicies Lude.Text
gipIdentity = Lens.lens (identity :: GetIdentityPolicies -> Lude.Text) (\s a -> s {identity = a} :: GetIdentityPolicies)
{-# DEPRECATED gipIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

instance Lude.AWSRequest GetIdentityPolicies where
  type Rs GetIdentityPolicies = GetIdentityPoliciesResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "GetIdentityPoliciesResult"
      ( \s h x ->
          GetIdentityPoliciesResponse'
            Lude.<$> ( x Lude..@? "Policies" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLMap "entry" "key" "value"
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetIdentityPolicies where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetIdentityPolicies where
  toPath = Lude.const "/"

instance Lude.ToQuery GetIdentityPolicies where
  toQuery GetIdentityPolicies' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetIdentityPolicies" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "PolicyNames" Lude.=: Lude.toQueryList "member" policyNames,
        "Identity" Lude.=: identity
      ]

-- | Represents the requested sending authorization policies.
--
-- /See:/ 'mkGetIdentityPoliciesResponse' smart constructor.
data GetIdentityPoliciesResponse = GetIdentityPoliciesResponse'
  { -- | A map of policy names to policies.
    policies :: Lude.HashMap Lude.Text (Lude.Text),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIdentityPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'policies' - A map of policy names to policies.
-- * 'responseStatus' - The response status code.
mkGetIdentityPoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetIdentityPoliciesResponse
mkGetIdentityPoliciesResponse pResponseStatus_ =
  GetIdentityPoliciesResponse'
    { policies = Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | A map of policy names to policies.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprsPolicies :: Lens.Lens' GetIdentityPoliciesResponse (Lude.HashMap Lude.Text (Lude.Text))
giprsPolicies = Lens.lens (policies :: GetIdentityPoliciesResponse -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {policies = a} :: GetIdentityPoliciesResponse)
{-# DEPRECATED giprsPolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giprsResponseStatus :: Lens.Lens' GetIdentityPoliciesResponse Lude.Int
giprsResponseStatus = Lens.lens (responseStatus :: GetIdentityPoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetIdentityPoliciesResponse)
{-# DEPRECATED giprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
