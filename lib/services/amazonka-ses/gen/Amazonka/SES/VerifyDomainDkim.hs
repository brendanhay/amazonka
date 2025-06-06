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
-- Module      : Amazonka.SES.VerifyDomainDkim
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a set of DKIM tokens for a domain identity.
--
-- When you execute the @VerifyDomainDkim@ operation, the domain that you
-- specify is added to the list of identities that are associated with your
-- account. This is true even if you haven\'t already associated the domain
-- with your account by using the @VerifyDomainIdentity@ operation.
-- However, you can\'t send email from the domain until you either
-- successfully
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/verify-domains.html verify it>
-- or you successfully
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html set up DKIM for it>.
--
-- You use the tokens that are generated by this operation to create CNAME
-- records. When Amazon SES detects that you\'ve added these records to the
-- DNS configuration for a domain, you can start sending email from that
-- domain. You can start sending email even if you haven\'t added the TXT
-- record provided by the VerifyDomainIdentity operation to the DNS
-- configuration for your domain. All email that you send from the domain
-- is authenticated using DKIM.
--
-- To create the CNAME records for DKIM authentication, use the following
-- values:
--
-- -   __Name__: /token/._domainkey./example.com/
--
-- -   __Type__: CNAME
--
-- -   __Value__: /token/.dkim.amazonses.com
--
-- In the preceding example, replace /token/ with one of the tokens that
-- are generated when you execute this operation. Replace /example.com/
-- with your domain. Repeat this process for each token that\'s generated
-- by this operation.
--
-- You can execute this operation no more than once per second.
module Amazonka.SES.VerifyDomainDkim
  ( -- * Creating a Request
    VerifyDomainDkim (..),
    newVerifyDomainDkim,

    -- * Request Lenses
    verifyDomainDkim_domain,

    -- * Destructuring the Response
    VerifyDomainDkimResponse (..),
    newVerifyDomainDkimResponse,

    -- * Response Lenses
    verifyDomainDkimResponse_httpStatus,
    verifyDomainDkimResponse_dkimTokens,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to generate the CNAME records needed to set up Easy
-- DKIM with Amazon SES. For more information about setting up Easy DKIM,
-- see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide>.
--
-- /See:/ 'newVerifyDomainDkim' smart constructor.
data VerifyDomainDkim = VerifyDomainDkim'
  { -- | The name of the domain to be verified for Easy DKIM signing.
    domain :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifyDomainDkim' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'verifyDomainDkim_domain' - The name of the domain to be verified for Easy DKIM signing.
newVerifyDomainDkim ::
  -- | 'domain'
  Prelude.Text ->
  VerifyDomainDkim
newVerifyDomainDkim pDomain_ =
  VerifyDomainDkim' {domain = pDomain_}

-- | The name of the domain to be verified for Easy DKIM signing.
verifyDomainDkim_domain :: Lens.Lens' VerifyDomainDkim Prelude.Text
verifyDomainDkim_domain = Lens.lens (\VerifyDomainDkim' {domain} -> domain) (\s@VerifyDomainDkim' {} a -> s {domain = a} :: VerifyDomainDkim)

instance Core.AWSRequest VerifyDomainDkim where
  type
    AWSResponse VerifyDomainDkim =
      VerifyDomainDkimResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "VerifyDomainDkimResult"
      ( \s h x ->
          VerifyDomainDkimResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..@? "DkimTokens" Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "member"
                        )
      )

instance Prelude.Hashable VerifyDomainDkim where
  hashWithSalt _salt VerifyDomainDkim' {..} =
    _salt `Prelude.hashWithSalt` domain

instance Prelude.NFData VerifyDomainDkim where
  rnf VerifyDomainDkim' {..} = Prelude.rnf domain

instance Data.ToHeaders VerifyDomainDkim where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath VerifyDomainDkim where
  toPath = Prelude.const "/"

instance Data.ToQuery VerifyDomainDkim where
  toQuery VerifyDomainDkim' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("VerifyDomainDkim" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "Domain" Data.=: domain
      ]

-- | Returns CNAME records that you must publish to the DNS server of your
-- domain to set up Easy DKIM with Amazon SES.
--
-- /See:/ 'newVerifyDomainDkimResponse' smart constructor.
data VerifyDomainDkimResponse = VerifyDomainDkimResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A set of character strings that represent the domain\'s identity. If the
    -- identity is an email address, the tokens represent the domain of that
    -- address.
    --
    -- Using these tokens, you need to create DNS CNAME records that point to
    -- DKIM public keys that are hosted by Amazon SES. Amazon Web Services
    -- eventually detects that you\'ve updated your DNS records. This detection
    -- process might take up to 72 hours. After successful detection, Amazon
    -- SES is able to DKIM-sign email originating from that domain. (This only
    -- applies to domain identities, not email address identities.)
    --
    -- For more information about creating DNS records using DKIM tokens, see
    -- the
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide>.
    dkimTokens :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerifyDomainDkimResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'verifyDomainDkimResponse_httpStatus' - The response's http status code.
--
-- 'dkimTokens', 'verifyDomainDkimResponse_dkimTokens' - A set of character strings that represent the domain\'s identity. If the
-- identity is an email address, the tokens represent the domain of that
-- address.
--
-- Using these tokens, you need to create DNS CNAME records that point to
-- DKIM public keys that are hosted by Amazon SES. Amazon Web Services
-- eventually detects that you\'ve updated your DNS records. This detection
-- process might take up to 72 hours. After successful detection, Amazon
-- SES is able to DKIM-sign email originating from that domain. (This only
-- applies to domain identities, not email address identities.)
--
-- For more information about creating DNS records using DKIM tokens, see
-- the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide>.
newVerifyDomainDkimResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  VerifyDomainDkimResponse
newVerifyDomainDkimResponse pHttpStatus_ =
  VerifyDomainDkimResponse'
    { httpStatus =
        pHttpStatus_,
      dkimTokens = Prelude.mempty
    }

-- | The response's http status code.
verifyDomainDkimResponse_httpStatus :: Lens.Lens' VerifyDomainDkimResponse Prelude.Int
verifyDomainDkimResponse_httpStatus = Lens.lens (\VerifyDomainDkimResponse' {httpStatus} -> httpStatus) (\s@VerifyDomainDkimResponse' {} a -> s {httpStatus = a} :: VerifyDomainDkimResponse)

-- | A set of character strings that represent the domain\'s identity. If the
-- identity is an email address, the tokens represent the domain of that
-- address.
--
-- Using these tokens, you need to create DNS CNAME records that point to
-- DKIM public keys that are hosted by Amazon SES. Amazon Web Services
-- eventually detects that you\'ve updated your DNS records. This detection
-- process might take up to 72 hours. After successful detection, Amazon
-- SES is able to DKIM-sign email originating from that domain. (This only
-- applies to domain identities, not email address identities.)
--
-- For more information about creating DNS records using DKIM tokens, see
-- the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Amazon SES Developer Guide>.
verifyDomainDkimResponse_dkimTokens :: Lens.Lens' VerifyDomainDkimResponse [Prelude.Text]
verifyDomainDkimResponse_dkimTokens = Lens.lens (\VerifyDomainDkimResponse' {dkimTokens} -> dkimTokens) (\s@VerifyDomainDkimResponse' {} a -> s {dkimTokens = a} :: VerifyDomainDkimResponse) Prelude.. Lens.coerced

instance Prelude.NFData VerifyDomainDkimResponse where
  rnf VerifyDomainDkimResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf dkimTokens
