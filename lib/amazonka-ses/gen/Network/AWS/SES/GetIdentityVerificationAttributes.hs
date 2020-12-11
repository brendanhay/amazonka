{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.GetIdentityVerificationAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Given a list of identities (email addresses and/or domains), returns the verification status and (for domain identities) the verification token for each identity.
--
-- The verification status of an email address is "Pending" until the email address owner clicks the link within the verification email that Amazon SES sent to that address. If the email address owner clicks the link within 24 hours, the verification status of the email address changes to "Success". If the link is not clicked within 24 hours, the verification status changes to "Failed." In that case, if you still want to verify the email address, you must restart the verification process from the beginning.
-- For domain identities, the domain's verification status is "Pending" as Amazon SES searches for the required TXT record in the DNS settings of the domain. When Amazon SES detects the record, the domain's verification status changes to "Success". If Amazon SES is unable to detect the record within 72 hours, the domain's verification status changes to "Failed." In that case, if you still want to verify the domain, you must restart the verification process from the beginning.
-- This operation is throttled at one request per second and can only get verification attributes for up to 100 identities at a time.
module Network.AWS.SES.GetIdentityVerificationAttributes
  ( -- * Creating a request
    GetIdentityVerificationAttributes (..),
    mkGetIdentityVerificationAttributes,

    -- ** Request lenses
    givaIdentities,

    -- * Destructuring the response
    GetIdentityVerificationAttributesResponse (..),
    mkGetIdentityVerificationAttributesResponse,

    -- ** Response lenses
    givarsResponseStatus,
    givarsVerificationAttributes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to return the Amazon SES verification status of a list of identities. For domain identities, this request also returns the verification token. For information about verifying identities with Amazon SES, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkGetIdentityVerificationAttributes' smart constructor.
newtype GetIdentityVerificationAttributes = GetIdentityVerificationAttributes'
  { identities ::
      [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIdentityVerificationAttributes' with the minimum fields required to make a request.
--
-- * 'identities' - A list of identities.
mkGetIdentityVerificationAttributes ::
  GetIdentityVerificationAttributes
mkGetIdentityVerificationAttributes =
  GetIdentityVerificationAttributes' {identities = Lude.mempty}

-- | A list of identities.
--
-- /Note:/ Consider using 'identities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givaIdentities :: Lens.Lens' GetIdentityVerificationAttributes [Lude.Text]
givaIdentities = Lens.lens (identities :: GetIdentityVerificationAttributes -> [Lude.Text]) (\s a -> s {identities = a} :: GetIdentityVerificationAttributes)
{-# DEPRECATED givaIdentities "Use generic-lens or generic-optics with 'identities' instead." #-}

instance Lude.AWSRequest GetIdentityVerificationAttributes where
  type
    Rs GetIdentityVerificationAttributes =
      GetIdentityVerificationAttributesResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "GetIdentityVerificationAttributesResult"
      ( \s h x ->
          GetIdentityVerificationAttributesResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "VerificationAttributes" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLMap "entry" "key" "value"
                     )
      )

instance Lude.ToHeaders GetIdentityVerificationAttributes where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetIdentityVerificationAttributes where
  toPath = Lude.const "/"

instance Lude.ToQuery GetIdentityVerificationAttributes where
  toQuery GetIdentityVerificationAttributes' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("GetIdentityVerificationAttributes" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "Identities" Lude.=: Lude.toQueryList "member" identities
      ]

-- | The Amazon SES verification status of a list of identities. For domain identities, this response also contains the verification token.
--
-- /See:/ 'mkGetIdentityVerificationAttributesResponse' smart constructor.
data GetIdentityVerificationAttributesResponse = GetIdentityVerificationAttributesResponse'
  { responseStatus ::
      Lude.Int,
    verificationAttributes ::
      Lude.HashMap
        Lude.Text
        (IdentityVerificationAttributes)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetIdentityVerificationAttributesResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'verificationAttributes' - A map of Identities to IdentityVerificationAttributes objects.
mkGetIdentityVerificationAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetIdentityVerificationAttributesResponse
mkGetIdentityVerificationAttributesResponse pResponseStatus_ =
  GetIdentityVerificationAttributesResponse'
    { responseStatus =
        pResponseStatus_,
      verificationAttributes = Lude.mempty
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givarsResponseStatus :: Lens.Lens' GetIdentityVerificationAttributesResponse Lude.Int
givarsResponseStatus = Lens.lens (responseStatus :: GetIdentityVerificationAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetIdentityVerificationAttributesResponse)
{-# DEPRECATED givarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A map of Identities to IdentityVerificationAttributes objects.
--
-- /Note:/ Consider using 'verificationAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
givarsVerificationAttributes :: Lens.Lens' GetIdentityVerificationAttributesResponse (Lude.HashMap Lude.Text (IdentityVerificationAttributes))
givarsVerificationAttributes = Lens.lens (verificationAttributes :: GetIdentityVerificationAttributesResponse -> Lude.HashMap Lude.Text (IdentityVerificationAttributes)) (\s a -> s {verificationAttributes = a} :: GetIdentityVerificationAttributesResponse)
{-# DEPRECATED givarsVerificationAttributes "Use generic-lens or generic-optics with 'verificationAttributes' instead." #-}
