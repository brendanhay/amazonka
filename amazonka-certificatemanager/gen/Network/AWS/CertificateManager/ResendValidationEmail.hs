{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.ResendValidationEmail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resends the email that requests domain ownership validation. The domain
-- owner or an authorized representative must approve the ACM certificate
-- before it can be issued. The certificate can be approved by clicking a
-- link in the mail to navigate to the Amazon certificate approval website
-- and then clicking __I Approve__. However, the validation email can be
-- blocked by spam filters. Therefore, if you do not receive the original
-- mail, you can request that the mail be resent within 72 hours of
-- requesting the ACM certificate. If more than 72 hours have elapsed since
-- your original request or since your last attempt to resend validation
-- mail, you must request a new certificate. For more information about
-- setting up your contact email addresses, see
-- <https://docs.aws.amazon.com/acm/latest/userguide/setup-email.html Configure Email for your Domain>.
module Network.AWS.CertificateManager.ResendValidationEmail
  ( -- * Creating a Request
    ResendValidationEmail (..),
    newResendValidationEmail,

    -- * Request Lenses
    resendValidationEmail_certificateArn,
    resendValidationEmail_domain,
    resendValidationEmail_validationDomain,

    -- * Destructuring the Response
    ResendValidationEmailResponse (..),
    newResendValidationEmailResponse,
  )
where

import Network.AWS.CertificateManager.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newResendValidationEmail' smart constructor.
data ResendValidationEmail = ResendValidationEmail'
  { -- | String that contains the ARN of the requested certificate. The
    -- certificate ARN is generated and returned by the RequestCertificate
    -- action as soon as the request is made. By default, using this parameter
    -- causes email to be sent to all top-level domains you specified in the
    -- certificate request. The ARN must be of the form:
    --
    -- @arn:aws:acm:us-east-1:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
    certificateArn :: Prelude.Text,
    -- | The fully qualified domain name (FQDN) of the certificate that needs to
    -- be validated.
    domain :: Prelude.Text,
    -- | The base validation domain that will act as the suffix of the email
    -- addresses that are used to send the emails. This must be the same as the
    -- @Domain@ value or a superdomain of the @Domain@ value. For example, if
    -- you requested a certificate for @site.subdomain.example.com@ and specify
    -- a __ValidationDomain__ of @subdomain.example.com@, ACM sends email to
    -- the domain registrant, technical contact, and administrative contact in
    -- WHOIS and the following five addresses:
    --
    -- -   admin\@subdomain.example.com
    --
    -- -   administrator\@subdomain.example.com
    --
    -- -   hostmaster\@subdomain.example.com
    --
    -- -   postmaster\@subdomain.example.com
    --
    -- -   webmaster\@subdomain.example.com
    validationDomain :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResendValidationEmail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'resendValidationEmail_certificateArn' - String that contains the ARN of the requested certificate. The
-- certificate ARN is generated and returned by the RequestCertificate
-- action as soon as the request is made. By default, using this parameter
-- causes email to be sent to all top-level domains you specified in the
-- certificate request. The ARN must be of the form:
--
-- @arn:aws:acm:us-east-1:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
--
-- 'domain', 'resendValidationEmail_domain' - The fully qualified domain name (FQDN) of the certificate that needs to
-- be validated.
--
-- 'validationDomain', 'resendValidationEmail_validationDomain' - The base validation domain that will act as the suffix of the email
-- addresses that are used to send the emails. This must be the same as the
-- @Domain@ value or a superdomain of the @Domain@ value. For example, if
-- you requested a certificate for @site.subdomain.example.com@ and specify
-- a __ValidationDomain__ of @subdomain.example.com@, ACM sends email to
-- the domain registrant, technical contact, and administrative contact in
-- WHOIS and the following five addresses:
--
-- -   admin\@subdomain.example.com
--
-- -   administrator\@subdomain.example.com
--
-- -   hostmaster\@subdomain.example.com
--
-- -   postmaster\@subdomain.example.com
--
-- -   webmaster\@subdomain.example.com
newResendValidationEmail ::
  -- | 'certificateArn'
  Prelude.Text ->
  -- | 'domain'
  Prelude.Text ->
  -- | 'validationDomain'
  Prelude.Text ->
  ResendValidationEmail
newResendValidationEmail
  pCertificateArn_
  pDomain_
  pValidationDomain_ =
    ResendValidationEmail'
      { certificateArn =
          pCertificateArn_,
        domain = pDomain_,
        validationDomain = pValidationDomain_
      }

-- | String that contains the ARN of the requested certificate. The
-- certificate ARN is generated and returned by the RequestCertificate
-- action as soon as the request is made. By default, using this parameter
-- causes email to be sent to all top-level domains you specified in the
-- certificate request. The ARN must be of the form:
--
-- @arn:aws:acm:us-east-1:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
resendValidationEmail_certificateArn :: Lens.Lens' ResendValidationEmail Prelude.Text
resendValidationEmail_certificateArn = Lens.lens (\ResendValidationEmail' {certificateArn} -> certificateArn) (\s@ResendValidationEmail' {} a -> s {certificateArn = a} :: ResendValidationEmail)

-- | The fully qualified domain name (FQDN) of the certificate that needs to
-- be validated.
resendValidationEmail_domain :: Lens.Lens' ResendValidationEmail Prelude.Text
resendValidationEmail_domain = Lens.lens (\ResendValidationEmail' {domain} -> domain) (\s@ResendValidationEmail' {} a -> s {domain = a} :: ResendValidationEmail)

-- | The base validation domain that will act as the suffix of the email
-- addresses that are used to send the emails. This must be the same as the
-- @Domain@ value or a superdomain of the @Domain@ value. For example, if
-- you requested a certificate for @site.subdomain.example.com@ and specify
-- a __ValidationDomain__ of @subdomain.example.com@, ACM sends email to
-- the domain registrant, technical contact, and administrative contact in
-- WHOIS and the following five addresses:
--
-- -   admin\@subdomain.example.com
--
-- -   administrator\@subdomain.example.com
--
-- -   hostmaster\@subdomain.example.com
--
-- -   postmaster\@subdomain.example.com
--
-- -   webmaster\@subdomain.example.com
resendValidationEmail_validationDomain :: Lens.Lens' ResendValidationEmail Prelude.Text
resendValidationEmail_validationDomain = Lens.lens (\ResendValidationEmail' {validationDomain} -> validationDomain) (\s@ResendValidationEmail' {} a -> s {validationDomain = a} :: ResendValidationEmail)

instance Prelude.AWSRequest ResendValidationEmail where
  type
    Rs ResendValidationEmail =
      ResendValidationEmailResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull ResendValidationEmailResponse'

instance Prelude.Hashable ResendValidationEmail

instance Prelude.NFData ResendValidationEmail

instance Prelude.ToHeaders ResendValidationEmail where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CertificateManager.ResendValidationEmail" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ResendValidationEmail where
  toJSON ResendValidationEmail' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CertificateArn" Prelude..= certificateArn),
            Prelude.Just ("Domain" Prelude..= domain),
            Prelude.Just
              ("ValidationDomain" Prelude..= validationDomain)
          ]
      )

instance Prelude.ToPath ResendValidationEmail where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ResendValidationEmail where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResendValidationEmailResponse' smart constructor.
data ResendValidationEmailResponse = ResendValidationEmailResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResendValidationEmailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newResendValidationEmailResponse ::
  ResendValidationEmailResponse
newResendValidationEmailResponse =
  ResendValidationEmailResponse'

instance Prelude.NFData ResendValidationEmailResponse
