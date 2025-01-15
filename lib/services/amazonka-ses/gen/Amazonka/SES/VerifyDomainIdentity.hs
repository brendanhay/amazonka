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
-- Module      : Amazonka.SES.VerifyDomainIdentity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a domain to the list of identities for your Amazon SES account in
-- the current AWS Region and attempts to verify it. For more information
-- about verifying domains, see
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-addresses-and-domains.html Verifying Email Addresses and Domains>
-- in the /Amazon SES Developer Guide./
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.VerifyDomainIdentity
  ( -- * Creating a Request
    VerifyDomainIdentity (..),
    newVerifyDomainIdentity,

    -- * Request Lenses
    verifyDomainIdentity_domain,

    -- * Destructuring the Response
    VerifyDomainIdentityResponse (..),
    newVerifyDomainIdentityResponse,

    -- * Response Lenses
    verifyDomainIdentityResponse_httpStatus,
    verifyDomainIdentityResponse_verificationToken,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to begin Amazon SES domain verification and to
-- generate the TXT records that you must publish to the DNS server of your
-- domain to complete the verification. For information about domain
-- verification, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-domains.html Amazon SES Developer Guide>.
--
-- /See:/ 'newVerifyDomainIdentity' smart constructor.
data VerifyDomainIdentity = VerifyDomainIdentity'
  { -- | The domain to be verified.
    domain :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifyDomainIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'verifyDomainIdentity_domain' - The domain to be verified.
newVerifyDomainIdentity ::
  -- | 'domain'
  Prelude.Text ->
  VerifyDomainIdentity
newVerifyDomainIdentity pDomain_ =
  VerifyDomainIdentity' {domain = pDomain_}

-- | The domain to be verified.
verifyDomainIdentity_domain :: Lens.Lens' VerifyDomainIdentity Prelude.Text
verifyDomainIdentity_domain = Lens.lens (\VerifyDomainIdentity' {domain} -> domain) (\s@VerifyDomainIdentity' {} a -> s {domain = a} :: VerifyDomainIdentity)

instance Core.AWSRequest VerifyDomainIdentity where
  type
    AWSResponse VerifyDomainIdentity =
      VerifyDomainIdentityResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "VerifyDomainIdentityResult"
      ( \s h x ->
          VerifyDomainIdentityResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "VerificationToken")
      )

instance Prelude.Hashable VerifyDomainIdentity where
  hashWithSalt _salt VerifyDomainIdentity' {..} =
    _salt `Prelude.hashWithSalt` domain

instance Prelude.NFData VerifyDomainIdentity where
  rnf VerifyDomainIdentity' {..} = Prelude.rnf domain

instance Data.ToHeaders VerifyDomainIdentity where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath VerifyDomainIdentity where
  toPath = Prelude.const "/"

instance Data.ToQuery VerifyDomainIdentity where
  toQuery VerifyDomainIdentity' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("VerifyDomainIdentity" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "Domain" Data.=: domain
      ]

-- | Returns a TXT record that you must publish to the DNS server of your
-- domain to complete domain verification with Amazon SES.
--
-- /See:/ 'newVerifyDomainIdentityResponse' smart constructor.
data VerifyDomainIdentityResponse = VerifyDomainIdentityResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A TXT record that you must place in the DNS settings of the domain to
    -- complete domain verification with Amazon SES.
    --
    -- As Amazon SES searches for the TXT record, the domain\'s verification
    -- status is \"Pending\". When Amazon SES detects the record, the domain\'s
    -- verification status changes to \"Success\". If Amazon SES is unable to
    -- detect the record within 72 hours, the domain\'s verification status
    -- changes to \"Failed.\" In that case, if you still want to verify the
    -- domain, you must restart the verification process from the beginning.
    verificationToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifyDomainIdentityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'verifyDomainIdentityResponse_httpStatus' - The response's http status code.
--
-- 'verificationToken', 'verifyDomainIdentityResponse_verificationToken' - A TXT record that you must place in the DNS settings of the domain to
-- complete domain verification with Amazon SES.
--
-- As Amazon SES searches for the TXT record, the domain\'s verification
-- status is \"Pending\". When Amazon SES detects the record, the domain\'s
-- verification status changes to \"Success\". If Amazon SES is unable to
-- detect the record within 72 hours, the domain\'s verification status
-- changes to \"Failed.\" In that case, if you still want to verify the
-- domain, you must restart the verification process from the beginning.
newVerifyDomainIdentityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'verificationToken'
  Prelude.Text ->
  VerifyDomainIdentityResponse
newVerifyDomainIdentityResponse
  pHttpStatus_
  pVerificationToken_ =
    VerifyDomainIdentityResponse'
      { httpStatus =
          pHttpStatus_,
        verificationToken = pVerificationToken_
      }

-- | The response's http status code.
verifyDomainIdentityResponse_httpStatus :: Lens.Lens' VerifyDomainIdentityResponse Prelude.Int
verifyDomainIdentityResponse_httpStatus = Lens.lens (\VerifyDomainIdentityResponse' {httpStatus} -> httpStatus) (\s@VerifyDomainIdentityResponse' {} a -> s {httpStatus = a} :: VerifyDomainIdentityResponse)

-- | A TXT record that you must place in the DNS settings of the domain to
-- complete domain verification with Amazon SES.
--
-- As Amazon SES searches for the TXT record, the domain\'s verification
-- status is \"Pending\". When Amazon SES detects the record, the domain\'s
-- verification status changes to \"Success\". If Amazon SES is unable to
-- detect the record within 72 hours, the domain\'s verification status
-- changes to \"Failed.\" In that case, if you still want to verify the
-- domain, you must restart the verification process from the beginning.
verifyDomainIdentityResponse_verificationToken :: Lens.Lens' VerifyDomainIdentityResponse Prelude.Text
verifyDomainIdentityResponse_verificationToken = Lens.lens (\VerifyDomainIdentityResponse' {verificationToken} -> verificationToken) (\s@VerifyDomainIdentityResponse' {} a -> s {verificationToken = a} :: VerifyDomainIdentityResponse)

instance Prelude.NFData VerifyDomainIdentityResponse where
  rnf VerifyDomainIdentityResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf verificationToken
