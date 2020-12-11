{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.VerifyDomainIdentity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a domain to the list of identities for your Amazon SES account in the current AWS Region and attempts to verify it. For more information about verifying domains, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Verifying Email Addresses and Domains> in the /Amazon SES Developer Guide./
--
-- You can execute this operation no more than once per second.
module Network.AWS.SES.VerifyDomainIdentity
  ( -- * Creating a request
    VerifyDomainIdentity (..),
    mkVerifyDomainIdentity,

    -- ** Request lenses
    vdiDomain,

    -- * Destructuring the response
    VerifyDomainIdentityResponse (..),
    mkVerifyDomainIdentityResponse,

    -- ** Response lenses
    vdirsResponseStatus,
    vdirsVerificationToken,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SES.Types

-- | Represents a request to begin Amazon SES domain verification and to generate the TXT records that you must publish to the DNS server of your domain to complete the verification. For information about domain verification, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-domains.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkVerifyDomainIdentity' smart constructor.
newtype VerifyDomainIdentity = VerifyDomainIdentity'
  { domain ::
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

-- | Creates a value of 'VerifyDomainIdentity' with the minimum fields required to make a request.
--
-- * 'domain' - The domain to be verified.
mkVerifyDomainIdentity ::
  -- | 'domain'
  Lude.Text ->
  VerifyDomainIdentity
mkVerifyDomainIdentity pDomain_ =
  VerifyDomainIdentity' {domain = pDomain_}

-- | The domain to be verified.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdiDomain :: Lens.Lens' VerifyDomainIdentity Lude.Text
vdiDomain = Lens.lens (domain :: VerifyDomainIdentity -> Lude.Text) (\s a -> s {domain = a} :: VerifyDomainIdentity)
{-# DEPRECATED vdiDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

instance Lude.AWSRequest VerifyDomainIdentity where
  type Rs VerifyDomainIdentity = VerifyDomainIdentityResponse
  request = Req.postQuery sesService
  response =
    Res.receiveXMLWrapper
      "VerifyDomainIdentityResult"
      ( \s h x ->
          VerifyDomainIdentityResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..@ "VerificationToken")
      )

instance Lude.ToHeaders VerifyDomainIdentity where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath VerifyDomainIdentity where
  toPath = Lude.const "/"

instance Lude.ToQuery VerifyDomainIdentity where
  toQuery VerifyDomainIdentity' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("VerifyDomainIdentity" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "Domain" Lude.=: domain
      ]

-- | Returns a TXT record that you must publish to the DNS server of your domain to complete domain verification with Amazon SES.
--
-- /See:/ 'mkVerifyDomainIdentityResponse' smart constructor.
data VerifyDomainIdentityResponse = VerifyDomainIdentityResponse'
  { responseStatus ::
      Lude.Int,
    verificationToken :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VerifyDomainIdentityResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'verificationToken' - A TXT record that you must place in the DNS settings of the domain to complete domain verification with Amazon SES.
--
-- As Amazon SES searches for the TXT record, the domain's verification status is "Pending". When Amazon SES detects the record, the domain's verification status changes to "Success". If Amazon SES is unable to detect the record within 72 hours, the domain's verification status changes to "Failed." In that case, if you still want to verify the domain, you must restart the verification process from the beginning.
mkVerifyDomainIdentityResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'verificationToken'
  Lude.Text ->
  VerifyDomainIdentityResponse
mkVerifyDomainIdentityResponse pResponseStatus_ pVerificationToken_ =
  VerifyDomainIdentityResponse'
    { responseStatus = pResponseStatus_,
      verificationToken = pVerificationToken_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdirsResponseStatus :: Lens.Lens' VerifyDomainIdentityResponse Lude.Int
vdirsResponseStatus = Lens.lens (responseStatus :: VerifyDomainIdentityResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: VerifyDomainIdentityResponse)
{-# DEPRECATED vdirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A TXT record that you must place in the DNS settings of the domain to complete domain verification with Amazon SES.
--
-- As Amazon SES searches for the TXT record, the domain's verification status is "Pending". When Amazon SES detects the record, the domain's verification status changes to "Success". If Amazon SES is unable to detect the record within 72 hours, the domain's verification status changes to "Failed." In that case, if you still want to verify the domain, you must restart the verification process from the beginning.
--
-- /Note:/ Consider using 'verificationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vdirsVerificationToken :: Lens.Lens' VerifyDomainIdentityResponse Lude.Text
vdirsVerificationToken = Lens.lens (verificationToken :: VerifyDomainIdentityResponse -> Lude.Text) (\s a -> s {verificationToken = a} :: VerifyDomainIdentityResponse)
{-# DEPRECATED vdirsVerificationToken "Use generic-lens or generic-optics with 'verificationToken' instead." #-}
