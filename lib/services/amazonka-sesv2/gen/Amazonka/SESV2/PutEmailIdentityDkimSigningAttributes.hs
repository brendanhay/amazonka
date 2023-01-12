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
-- Module      : Amazonka.SESV2.PutEmailIdentityDkimSigningAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to configure or change the DKIM authentication settings for an
-- email domain identity. You can use this operation to do any of the
-- following:
--
-- -   Update the signing attributes for an identity that uses Bring Your
--     Own DKIM (BYODKIM).
--
-- -   Update the key length that should be used for Easy DKIM.
--
-- -   Change from using no DKIM authentication to using Easy DKIM.
--
-- -   Change from using no DKIM authentication to using BYODKIM.
--
-- -   Change from using Easy DKIM to using BYODKIM.
--
-- -   Change from using BYODKIM to using Easy DKIM.
module Amazonka.SESV2.PutEmailIdentityDkimSigningAttributes
  ( -- * Creating a Request
    PutEmailIdentityDkimSigningAttributes (..),
    newPutEmailIdentityDkimSigningAttributes,

    -- * Request Lenses
    putEmailIdentityDkimSigningAttributes_signingAttributes,
    putEmailIdentityDkimSigningAttributes_emailIdentity,
    putEmailIdentityDkimSigningAttributes_signingAttributesOrigin,

    -- * Destructuring the Response
    PutEmailIdentityDkimSigningAttributesResponse (..),
    newPutEmailIdentityDkimSigningAttributesResponse,

    -- * Response Lenses
    putEmailIdentityDkimSigningAttributesResponse_dkimStatus,
    putEmailIdentityDkimSigningAttributesResponse_dkimTokens,
    putEmailIdentityDkimSigningAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to change the DKIM attributes for an email identity.
--
-- /See:/ 'newPutEmailIdentityDkimSigningAttributes' smart constructor.
data PutEmailIdentityDkimSigningAttributes = PutEmailIdentityDkimSigningAttributes'
  { -- | An object that contains information about the private key and selector
    -- that you want to use to configure DKIM for the identity for Bring Your
    -- Own DKIM (BYODKIM) for the identity, or, configures the key length to be
    -- used for
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>.
    signingAttributes :: Prelude.Maybe DkimSigningAttributes,
    -- | The email identity.
    emailIdentity :: Prelude.Text,
    -- | The method to use to configure DKIM for the identity. There are the
    -- following possible values:
    --
    -- -   @AWS_SES@ – Configure DKIM for the identity by using
    --     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>.
    --
    -- -   @EXTERNAL@ – Configure DKIM for the identity by using Bring Your Own
    --     DKIM (BYODKIM).
    signingAttributesOrigin :: DkimSigningAttributesOrigin
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEmailIdentityDkimSigningAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signingAttributes', 'putEmailIdentityDkimSigningAttributes_signingAttributes' - An object that contains information about the private key and selector
-- that you want to use to configure DKIM for the identity for Bring Your
-- Own DKIM (BYODKIM) for the identity, or, configures the key length to be
-- used for
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>.
--
-- 'emailIdentity', 'putEmailIdentityDkimSigningAttributes_emailIdentity' - The email identity.
--
-- 'signingAttributesOrigin', 'putEmailIdentityDkimSigningAttributes_signingAttributesOrigin' - The method to use to configure DKIM for the identity. There are the
-- following possible values:
--
-- -   @AWS_SES@ – Configure DKIM for the identity by using
--     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>.
--
-- -   @EXTERNAL@ – Configure DKIM for the identity by using Bring Your Own
--     DKIM (BYODKIM).
newPutEmailIdentityDkimSigningAttributes ::
  -- | 'emailIdentity'
  Prelude.Text ->
  -- | 'signingAttributesOrigin'
  DkimSigningAttributesOrigin ->
  PutEmailIdentityDkimSigningAttributes
newPutEmailIdentityDkimSigningAttributes
  pEmailIdentity_
  pSigningAttributesOrigin_ =
    PutEmailIdentityDkimSigningAttributes'
      { signingAttributes =
          Prelude.Nothing,
        emailIdentity = pEmailIdentity_,
        signingAttributesOrigin =
          pSigningAttributesOrigin_
      }

-- | An object that contains information about the private key and selector
-- that you want to use to configure DKIM for the identity for Bring Your
-- Own DKIM (BYODKIM) for the identity, or, configures the key length to be
-- used for
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>.
putEmailIdentityDkimSigningAttributes_signingAttributes :: Lens.Lens' PutEmailIdentityDkimSigningAttributes (Prelude.Maybe DkimSigningAttributes)
putEmailIdentityDkimSigningAttributes_signingAttributes = Lens.lens (\PutEmailIdentityDkimSigningAttributes' {signingAttributes} -> signingAttributes) (\s@PutEmailIdentityDkimSigningAttributes' {} a -> s {signingAttributes = a} :: PutEmailIdentityDkimSigningAttributes)

-- | The email identity.
putEmailIdentityDkimSigningAttributes_emailIdentity :: Lens.Lens' PutEmailIdentityDkimSigningAttributes Prelude.Text
putEmailIdentityDkimSigningAttributes_emailIdentity = Lens.lens (\PutEmailIdentityDkimSigningAttributes' {emailIdentity} -> emailIdentity) (\s@PutEmailIdentityDkimSigningAttributes' {} a -> s {emailIdentity = a} :: PutEmailIdentityDkimSigningAttributes)

-- | The method to use to configure DKIM for the identity. There are the
-- following possible values:
--
-- -   @AWS_SES@ – Configure DKIM for the identity by using
--     <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>.
--
-- -   @EXTERNAL@ – Configure DKIM for the identity by using Bring Your Own
--     DKIM (BYODKIM).
putEmailIdentityDkimSigningAttributes_signingAttributesOrigin :: Lens.Lens' PutEmailIdentityDkimSigningAttributes DkimSigningAttributesOrigin
putEmailIdentityDkimSigningAttributes_signingAttributesOrigin = Lens.lens (\PutEmailIdentityDkimSigningAttributes' {signingAttributesOrigin} -> signingAttributesOrigin) (\s@PutEmailIdentityDkimSigningAttributes' {} a -> s {signingAttributesOrigin = a} :: PutEmailIdentityDkimSigningAttributes)

instance
  Core.AWSRequest
    PutEmailIdentityDkimSigningAttributes
  where
  type
    AWSResponse
      PutEmailIdentityDkimSigningAttributes =
      PutEmailIdentityDkimSigningAttributesResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutEmailIdentityDkimSigningAttributesResponse'
            Prelude.<$> (x Data..?> "DkimStatus")
              Prelude.<*> (x Data..?> "DkimTokens" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutEmailIdentityDkimSigningAttributes
  where
  hashWithSalt
    _salt
    PutEmailIdentityDkimSigningAttributes' {..} =
      _salt `Prelude.hashWithSalt` signingAttributes
        `Prelude.hashWithSalt` emailIdentity
        `Prelude.hashWithSalt` signingAttributesOrigin

instance
  Prelude.NFData
    PutEmailIdentityDkimSigningAttributes
  where
  rnf PutEmailIdentityDkimSigningAttributes' {..} =
    Prelude.rnf signingAttributes
      `Prelude.seq` Prelude.rnf emailIdentity
      `Prelude.seq` Prelude.rnf signingAttributesOrigin

instance
  Data.ToHeaders
    PutEmailIdentityDkimSigningAttributes
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    PutEmailIdentityDkimSigningAttributes
  where
  toJSON PutEmailIdentityDkimSigningAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SigningAttributes" Data..=)
              Prelude.<$> signingAttributes,
            Prelude.Just
              ( "SigningAttributesOrigin"
                  Data..= signingAttributesOrigin
              )
          ]
      )

instance
  Data.ToPath
    PutEmailIdentityDkimSigningAttributes
  where
  toPath PutEmailIdentityDkimSigningAttributes' {..} =
    Prelude.mconcat
      [ "/v1/email/identities/",
        Data.toBS emailIdentity,
        "/dkim/signing"
      ]

instance
  Data.ToQuery
    PutEmailIdentityDkimSigningAttributes
  where
  toQuery = Prelude.const Prelude.mempty

-- | If the action is successful, the service sends back an HTTP 200
-- response.
--
-- The following data is returned in JSON format by the service.
--
-- /See:/ 'newPutEmailIdentityDkimSigningAttributesResponse' smart constructor.
data PutEmailIdentityDkimSigningAttributesResponse = PutEmailIdentityDkimSigningAttributesResponse'
  { -- | The DKIM authentication status of the identity. Amazon SES determines
    -- the authentication status by searching for specific records in the DNS
    -- configuration for your domain. If you used
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>
    -- to set up DKIM authentication, Amazon SES tries to find three unique
    -- CNAME records in the DNS configuration for your domain.
    --
    -- If you provided a public key to perform DKIM authentication, Amazon SES
    -- tries to find a TXT record that uses the selector that you specified.
    -- The value of the TXT record must be a public key that\'s paired with the
    -- private key that you specified in the process of creating the identity.
    --
    -- The status can be one of the following:
    --
    -- -   @PENDING@ – The verification process was initiated, but Amazon SES
    --     hasn\'t yet detected the DKIM records in the DNS configuration for
    --     the domain.
    --
    -- -   @SUCCESS@ – The verification process completed successfully.
    --
    -- -   @FAILED@ – The verification process failed. This typically occurs
    --     when Amazon SES fails to find the DKIM records in the DNS
    --     configuration of the domain.
    --
    -- -   @TEMPORARY_FAILURE@ – A temporary issue is preventing Amazon SES
    --     from determining the DKIM authentication status of the domain.
    --
    -- -   @NOT_STARTED@ – The DKIM verification process hasn\'t been initiated
    --     for the domain.
    dkimStatus :: Prelude.Maybe DkimStatus,
    -- | If you used
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>
    -- to configure DKIM authentication for the domain, then this object
    -- contains a set of unique strings that you use to create a set of CNAME
    -- records that you add to the DNS configuration for your domain. When
    -- Amazon SES detects these records in the DNS configuration for your
    -- domain, the DKIM authentication process is complete.
    --
    -- If you configured DKIM authentication for the domain by providing your
    -- own public-private key pair, then this object contains the selector
    -- that\'s associated with your public key.
    --
    -- Regardless of the DKIM authentication method you use, Amazon SES
    -- searches for the appropriate records in the DNS configuration of the
    -- domain for up to 72 hours.
    dkimTokens :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEmailIdentityDkimSigningAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dkimStatus', 'putEmailIdentityDkimSigningAttributesResponse_dkimStatus' - The DKIM authentication status of the identity. Amazon SES determines
-- the authentication status by searching for specific records in the DNS
-- configuration for your domain. If you used
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>
-- to set up DKIM authentication, Amazon SES tries to find three unique
-- CNAME records in the DNS configuration for your domain.
--
-- If you provided a public key to perform DKIM authentication, Amazon SES
-- tries to find a TXT record that uses the selector that you specified.
-- The value of the TXT record must be a public key that\'s paired with the
-- private key that you specified in the process of creating the identity.
--
-- The status can be one of the following:
--
-- -   @PENDING@ – The verification process was initiated, but Amazon SES
--     hasn\'t yet detected the DKIM records in the DNS configuration for
--     the domain.
--
-- -   @SUCCESS@ – The verification process completed successfully.
--
-- -   @FAILED@ – The verification process failed. This typically occurs
--     when Amazon SES fails to find the DKIM records in the DNS
--     configuration of the domain.
--
-- -   @TEMPORARY_FAILURE@ – A temporary issue is preventing Amazon SES
--     from determining the DKIM authentication status of the domain.
--
-- -   @NOT_STARTED@ – The DKIM verification process hasn\'t been initiated
--     for the domain.
--
-- 'dkimTokens', 'putEmailIdentityDkimSigningAttributesResponse_dkimTokens' - If you used
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>
-- to configure DKIM authentication for the domain, then this object
-- contains a set of unique strings that you use to create a set of CNAME
-- records that you add to the DNS configuration for your domain. When
-- Amazon SES detects these records in the DNS configuration for your
-- domain, the DKIM authentication process is complete.
--
-- If you configured DKIM authentication for the domain by providing your
-- own public-private key pair, then this object contains the selector
-- that\'s associated with your public key.
--
-- Regardless of the DKIM authentication method you use, Amazon SES
-- searches for the appropriate records in the DNS configuration of the
-- domain for up to 72 hours.
--
-- 'httpStatus', 'putEmailIdentityDkimSigningAttributesResponse_httpStatus' - The response's http status code.
newPutEmailIdentityDkimSigningAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutEmailIdentityDkimSigningAttributesResponse
newPutEmailIdentityDkimSigningAttributesResponse
  pHttpStatus_ =
    PutEmailIdentityDkimSigningAttributesResponse'
      { dkimStatus =
          Prelude.Nothing,
        dkimTokens = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The DKIM authentication status of the identity. Amazon SES determines
-- the authentication status by searching for specific records in the DNS
-- configuration for your domain. If you used
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>
-- to set up DKIM authentication, Amazon SES tries to find three unique
-- CNAME records in the DNS configuration for your domain.
--
-- If you provided a public key to perform DKIM authentication, Amazon SES
-- tries to find a TXT record that uses the selector that you specified.
-- The value of the TXT record must be a public key that\'s paired with the
-- private key that you specified in the process of creating the identity.
--
-- The status can be one of the following:
--
-- -   @PENDING@ – The verification process was initiated, but Amazon SES
--     hasn\'t yet detected the DKIM records in the DNS configuration for
--     the domain.
--
-- -   @SUCCESS@ – The verification process completed successfully.
--
-- -   @FAILED@ – The verification process failed. This typically occurs
--     when Amazon SES fails to find the DKIM records in the DNS
--     configuration of the domain.
--
-- -   @TEMPORARY_FAILURE@ – A temporary issue is preventing Amazon SES
--     from determining the DKIM authentication status of the domain.
--
-- -   @NOT_STARTED@ – The DKIM verification process hasn\'t been initiated
--     for the domain.
putEmailIdentityDkimSigningAttributesResponse_dkimStatus :: Lens.Lens' PutEmailIdentityDkimSigningAttributesResponse (Prelude.Maybe DkimStatus)
putEmailIdentityDkimSigningAttributesResponse_dkimStatus = Lens.lens (\PutEmailIdentityDkimSigningAttributesResponse' {dkimStatus} -> dkimStatus) (\s@PutEmailIdentityDkimSigningAttributesResponse' {} a -> s {dkimStatus = a} :: PutEmailIdentityDkimSigningAttributesResponse)

-- | If you used
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>
-- to configure DKIM authentication for the domain, then this object
-- contains a set of unique strings that you use to create a set of CNAME
-- records that you add to the DNS configuration for your domain. When
-- Amazon SES detects these records in the DNS configuration for your
-- domain, the DKIM authentication process is complete.
--
-- If you configured DKIM authentication for the domain by providing your
-- own public-private key pair, then this object contains the selector
-- that\'s associated with your public key.
--
-- Regardless of the DKIM authentication method you use, Amazon SES
-- searches for the appropriate records in the DNS configuration of the
-- domain for up to 72 hours.
putEmailIdentityDkimSigningAttributesResponse_dkimTokens :: Lens.Lens' PutEmailIdentityDkimSigningAttributesResponse (Prelude.Maybe [Prelude.Text])
putEmailIdentityDkimSigningAttributesResponse_dkimTokens = Lens.lens (\PutEmailIdentityDkimSigningAttributesResponse' {dkimTokens} -> dkimTokens) (\s@PutEmailIdentityDkimSigningAttributesResponse' {} a -> s {dkimTokens = a} :: PutEmailIdentityDkimSigningAttributesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
putEmailIdentityDkimSigningAttributesResponse_httpStatus :: Lens.Lens' PutEmailIdentityDkimSigningAttributesResponse Prelude.Int
putEmailIdentityDkimSigningAttributesResponse_httpStatus = Lens.lens (\PutEmailIdentityDkimSigningAttributesResponse' {httpStatus} -> httpStatus) (\s@PutEmailIdentityDkimSigningAttributesResponse' {} a -> s {httpStatus = a} :: PutEmailIdentityDkimSigningAttributesResponse)

instance
  Prelude.NFData
    PutEmailIdentityDkimSigningAttributesResponse
  where
  rnf
    PutEmailIdentityDkimSigningAttributesResponse' {..} =
      Prelude.rnf dkimStatus
        `Prelude.seq` Prelude.rnf dkimTokens
        `Prelude.seq` Prelude.rnf httpStatus
