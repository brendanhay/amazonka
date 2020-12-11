{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.ResendContactReachabilityEmail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For operations that require confirmation that the email address for the registrant contact is valid, such as registering a new domain, this operation resends the confirmation email to the current email address for the registrant contact.
module Network.AWS.Route53Domains.ResendContactReachabilityEmail
  ( -- * Creating a request
    ResendContactReachabilityEmail (..),
    mkResendContactReachabilityEmail,

    -- ** Request lenses
    rcreDomainName,

    -- * Destructuring the response
    ResendContactReachabilityEmailResponse (..),
    mkResendContactReachabilityEmailResponse,

    -- ** Response lenses
    rcrersDomainName,
    rcrersEmailAddress,
    rcrersIsAlreadyVerified,
    rcrersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53Domains.Types

-- | /See:/ 'mkResendContactReachabilityEmail' smart constructor.
newtype ResendContactReachabilityEmail = ResendContactReachabilityEmail'
  { domainName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResendContactReachabilityEmail' with the minimum fields required to make a request.
--
-- * 'domainName' - The name of the domain for which you want Route 53 to resend a confirmation email to the registrant contact.
mkResendContactReachabilityEmail ::
  ResendContactReachabilityEmail
mkResendContactReachabilityEmail =
  ResendContactReachabilityEmail' {domainName = Lude.Nothing}

-- | The name of the domain for which you want Route 53 to resend a confirmation email to the registrant contact.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcreDomainName :: Lens.Lens' ResendContactReachabilityEmail (Lude.Maybe Lude.Text)
rcreDomainName = Lens.lens (domainName :: ResendContactReachabilityEmail -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: ResendContactReachabilityEmail)
{-# DEPRECATED rcreDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.AWSRequest ResendContactReachabilityEmail where
  type
    Rs ResendContactReachabilityEmail =
      ResendContactReachabilityEmailResponse
  request = Req.postJSON route53DomainsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ResendContactReachabilityEmailResponse'
            Lude.<$> (x Lude..?> "domainName")
            Lude.<*> (x Lude..?> "emailAddress")
            Lude.<*> (x Lude..?> "isAlreadyVerified")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ResendContactReachabilityEmail where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Route53Domains_v20140515.ResendContactReachabilityEmail" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ResendContactReachabilityEmail where
  toJSON ResendContactReachabilityEmail' {..} =
    Lude.object
      (Lude.catMaybes [("domainName" Lude..=) Lude.<$> domainName])

instance Lude.ToPath ResendContactReachabilityEmail where
  toPath = Lude.const "/"

instance Lude.ToQuery ResendContactReachabilityEmail where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkResendContactReachabilityEmailResponse' smart constructor.
data ResendContactReachabilityEmailResponse = ResendContactReachabilityEmailResponse'
  { domainName ::
      Lude.Maybe
        Lude.Text,
    emailAddress ::
      Lude.Maybe
        Lude.Text,
    isAlreadyVerified ::
      Lude.Maybe
        Lude.Bool,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResendContactReachabilityEmailResponse' with the minimum fields required to make a request.
--
-- * 'domainName' - The domain name for which you requested a confirmation email.
-- * 'emailAddress' - The email address for the registrant contact at the time that we sent the verification email.
-- * 'isAlreadyVerified' - @True@ if the email address for the registrant contact has already been verified, and @false@ otherwise. If the email address has already been verified, we don't send another confirmation email.
-- * 'responseStatus' - The response status code.
mkResendContactReachabilityEmailResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ResendContactReachabilityEmailResponse
mkResendContactReachabilityEmailResponse pResponseStatus_ =
  ResendContactReachabilityEmailResponse'
    { domainName =
        Lude.Nothing,
      emailAddress = Lude.Nothing,
      isAlreadyVerified = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The domain name for which you requested a confirmation email.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrersDomainName :: Lens.Lens' ResendContactReachabilityEmailResponse (Lude.Maybe Lude.Text)
rcrersDomainName = Lens.lens (domainName :: ResendContactReachabilityEmailResponse -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: ResendContactReachabilityEmailResponse)
{-# DEPRECATED rcrersDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The email address for the registrant contact at the time that we sent the verification email.
--
-- /Note:/ Consider using 'emailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrersEmailAddress :: Lens.Lens' ResendContactReachabilityEmailResponse (Lude.Maybe Lude.Text)
rcrersEmailAddress = Lens.lens (emailAddress :: ResendContactReachabilityEmailResponse -> Lude.Maybe Lude.Text) (\s a -> s {emailAddress = a} :: ResendContactReachabilityEmailResponse)
{-# DEPRECATED rcrersEmailAddress "Use generic-lens or generic-optics with 'emailAddress' instead." #-}

-- | @True@ if the email address for the registrant contact has already been verified, and @false@ otherwise. If the email address has already been verified, we don't send another confirmation email.
--
-- /Note:/ Consider using 'isAlreadyVerified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrersIsAlreadyVerified :: Lens.Lens' ResendContactReachabilityEmailResponse (Lude.Maybe Lude.Bool)
rcrersIsAlreadyVerified = Lens.lens (isAlreadyVerified :: ResendContactReachabilityEmailResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isAlreadyVerified = a} :: ResendContactReachabilityEmailResponse)
{-# DEPRECATED rcrersIsAlreadyVerified "Use generic-lens or generic-optics with 'isAlreadyVerified' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcrersResponseStatus :: Lens.Lens' ResendContactReachabilityEmailResponse Lude.Int
rcrersResponseStatus = Lens.lens (responseStatus :: ResendContactReachabilityEmailResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ResendContactReachabilityEmailResponse)
{-# DEPRECATED rcrersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
