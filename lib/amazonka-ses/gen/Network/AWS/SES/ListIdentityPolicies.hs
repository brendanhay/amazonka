{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.ListIdentityPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of sending authorization policies that are attached to the given identity (an email address or a domain). This API returns only a list. If you want the actual policy content, you can use @GetIdentityPolicies@ .
--
-- Sending authorization is a feature that enables an identity owner to authorize other senders to use its identities. For information about using sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
-- You can execute this operation no more than once per second.
module Network.AWS.SES.ListIdentityPolicies
  ( -- * Creating a request
    ListIdentityPolicies (..),
    mkListIdentityPolicies,

    -- ** Request lenses
    lipIdentity,

    -- * Destructuring the response
    ListIdentityPoliciesResponse (..),
    mkListIdentityPoliciesResponse,

    -- ** Response lenses
    liprsResponseStatus,
    liprsPolicyNames,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to return a list of sending authorization policies that are attached to an identity. Sending authorization is an Amazon SES feature that enables you to authorize other senders to use your identities. For information, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkListIdentityPolicies' smart constructor.
newtype ListIdentityPolicies = ListIdentityPolicies'
  { identity ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListIdentityPolicies' with the minimum fields required to make a request.
--
-- * 'identity' - The identity that is associated with the policy for which the policies will be listed. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
--
-- To successfully call this API, you must own the identity.
mkListIdentityPolicies ::
  -- | 'identity'
  Lude.Text ->
  ListIdentityPolicies
mkListIdentityPolicies pIdentity_ =
  ListIdentityPolicies' {identity = pIdentity_}

-- | The identity that is associated with the policy for which the policies will be listed. You can specify an identity by using its name or by using its Amazon Resource Name (ARN). Examples: @user@example.com@ , @example.com@ , @arn:aws:ses:us-east-1:123456789012:identity/example.com@ .
--
-- To successfully call this API, you must own the identity.
--
-- /Note:/ Consider using 'identity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lipIdentity :: Lens.Lens' ListIdentityPolicies Lude.Text
lipIdentity = Lens.lens (identity :: ListIdentityPolicies -> Lude.Text) (\s a -> s {identity = a} :: ListIdentityPolicies)
{-# DEPRECATED lipIdentity "Use generic-lens or generic-optics with 'identity' instead." #-}

instance Lude.AWSRequest ListIdentityPolicies where
  type Rs ListIdentityPolicies = ListIdentityPoliciesResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "ListIdentityPoliciesResult"
      ( \s h x ->
          ListIdentityPoliciesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "PolicyNames" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "member"
                     )
      )

instance Lude.ToHeaders ListIdentityPolicies where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListIdentityPolicies where
  toPath = Lude.const "/"

instance Lude.ToQuery ListIdentityPolicies where
  toQuery ListIdentityPolicies' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("ListIdentityPolicies" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "Identity" Lude.=: identity
      ]

-- | A list of names of sending authorization policies that apply to an identity.
--
-- /See:/ 'mkListIdentityPoliciesResponse' smart constructor.
data ListIdentityPoliciesResponse = ListIdentityPoliciesResponse'
  { responseStatus ::
      Lude.Int,
    policyNames :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListIdentityPoliciesResponse' with the minimum fields required to make a request.
--
-- * 'policyNames' - A list of names of policies that apply to the specified identity.
-- * 'responseStatus' - The response status code.
mkListIdentityPoliciesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListIdentityPoliciesResponse
mkListIdentityPoliciesResponse pResponseStatus_ =
  ListIdentityPoliciesResponse'
    { responseStatus = pResponseStatus_,
      policyNames = Lude.mempty
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprsResponseStatus :: Lens.Lens' ListIdentityPoliciesResponse Lude.Int
liprsResponseStatus = Lens.lens (responseStatus :: ListIdentityPoliciesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListIdentityPoliciesResponse)
{-# DEPRECATED liprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of names of policies that apply to the specified identity.
--
-- /Note:/ Consider using 'policyNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liprsPolicyNames :: Lens.Lens' ListIdentityPoliciesResponse [Lude.Text]
liprsPolicyNames = Lens.lens (policyNames :: ListIdentityPoliciesResponse -> [Lude.Text]) (\s a -> s {policyNames = a} :: ListIdentityPoliciesResponse)
{-# DEPRECATED liprsPolicyNames "Use generic-lens or generic-optics with 'policyNames' instead." #-}
