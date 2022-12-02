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
-- Module      : Amazonka.PinpointEmail.PutEmailIdentityMailFromAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to enable or disable the custom Mail-From domain configuration for
-- an email identity.
module Amazonka.PinpointEmail.PutEmailIdentityMailFromAttributes
  ( -- * Creating a Request
    PutEmailIdentityMailFromAttributes (..),
    newPutEmailIdentityMailFromAttributes,

    -- * Request Lenses
    putEmailIdentityMailFromAttributes_mailFromDomain,
    putEmailIdentityMailFromAttributes_behaviorOnMxFailure,
    putEmailIdentityMailFromAttributes_emailIdentity,

    -- * Destructuring the Response
    PutEmailIdentityMailFromAttributesResponse (..),
    newPutEmailIdentityMailFromAttributesResponse,

    -- * Response Lenses
    putEmailIdentityMailFromAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointEmail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to configure the custom MAIL FROM domain for a verified
-- identity.
--
-- /See:/ 'newPutEmailIdentityMailFromAttributes' smart constructor.
data PutEmailIdentityMailFromAttributes = PutEmailIdentityMailFromAttributes'
  { -- | The custom MAIL FROM domain that you want the verified identity to use.
    -- The MAIL FROM domain must meet the following criteria:
    --
    -- -   It has to be a subdomain of the verified identity.
    --
    -- -   It can\'t be used to receive email.
    --
    -- -   It can\'t be used in a \"From\" address if the MAIL FROM domain is a
    --     destination for feedback forwarding emails.
    mailFromDomain :: Prelude.Maybe Prelude.Text,
    -- | The action that you want Amazon Pinpoint to take if it can\'t read the
    -- required MX record when you send an email. When you set this value to
    -- @UseDefaultValue@, Amazon Pinpoint uses /amazonses.com/ as the MAIL FROM
    -- domain. When you set this value to @RejectMessage@, Amazon Pinpoint
    -- returns a @MailFromDomainNotVerified@ error, and doesn\'t attempt to
    -- deliver the email.
    --
    -- These behaviors are taken when the custom MAIL FROM domain configuration
    -- is in the @Pending@, @Failed@, and @TemporaryFailure@ states.
    behaviorOnMxFailure :: Prelude.Maybe BehaviorOnMxFailure,
    -- | The verified email identity that you want to set up the custom MAIL FROM
    -- domain for.
    emailIdentity :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEmailIdentityMailFromAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mailFromDomain', 'putEmailIdentityMailFromAttributes_mailFromDomain' - The custom MAIL FROM domain that you want the verified identity to use.
-- The MAIL FROM domain must meet the following criteria:
--
-- -   It has to be a subdomain of the verified identity.
--
-- -   It can\'t be used to receive email.
--
-- -   It can\'t be used in a \"From\" address if the MAIL FROM domain is a
--     destination for feedback forwarding emails.
--
-- 'behaviorOnMxFailure', 'putEmailIdentityMailFromAttributes_behaviorOnMxFailure' - The action that you want Amazon Pinpoint to take if it can\'t read the
-- required MX record when you send an email. When you set this value to
-- @UseDefaultValue@, Amazon Pinpoint uses /amazonses.com/ as the MAIL FROM
-- domain. When you set this value to @RejectMessage@, Amazon Pinpoint
-- returns a @MailFromDomainNotVerified@ error, and doesn\'t attempt to
-- deliver the email.
--
-- These behaviors are taken when the custom MAIL FROM domain configuration
-- is in the @Pending@, @Failed@, and @TemporaryFailure@ states.
--
-- 'emailIdentity', 'putEmailIdentityMailFromAttributes_emailIdentity' - The verified email identity that you want to set up the custom MAIL FROM
-- domain for.
newPutEmailIdentityMailFromAttributes ::
  -- | 'emailIdentity'
  Prelude.Text ->
  PutEmailIdentityMailFromAttributes
newPutEmailIdentityMailFromAttributes pEmailIdentity_ =
  PutEmailIdentityMailFromAttributes'
    { mailFromDomain =
        Prelude.Nothing,
      behaviorOnMxFailure = Prelude.Nothing,
      emailIdentity = pEmailIdentity_
    }

-- | The custom MAIL FROM domain that you want the verified identity to use.
-- The MAIL FROM domain must meet the following criteria:
--
-- -   It has to be a subdomain of the verified identity.
--
-- -   It can\'t be used to receive email.
--
-- -   It can\'t be used in a \"From\" address if the MAIL FROM domain is a
--     destination for feedback forwarding emails.
putEmailIdentityMailFromAttributes_mailFromDomain :: Lens.Lens' PutEmailIdentityMailFromAttributes (Prelude.Maybe Prelude.Text)
putEmailIdentityMailFromAttributes_mailFromDomain = Lens.lens (\PutEmailIdentityMailFromAttributes' {mailFromDomain} -> mailFromDomain) (\s@PutEmailIdentityMailFromAttributes' {} a -> s {mailFromDomain = a} :: PutEmailIdentityMailFromAttributes)

-- | The action that you want Amazon Pinpoint to take if it can\'t read the
-- required MX record when you send an email. When you set this value to
-- @UseDefaultValue@, Amazon Pinpoint uses /amazonses.com/ as the MAIL FROM
-- domain. When you set this value to @RejectMessage@, Amazon Pinpoint
-- returns a @MailFromDomainNotVerified@ error, and doesn\'t attempt to
-- deliver the email.
--
-- These behaviors are taken when the custom MAIL FROM domain configuration
-- is in the @Pending@, @Failed@, and @TemporaryFailure@ states.
putEmailIdentityMailFromAttributes_behaviorOnMxFailure :: Lens.Lens' PutEmailIdentityMailFromAttributes (Prelude.Maybe BehaviorOnMxFailure)
putEmailIdentityMailFromAttributes_behaviorOnMxFailure = Lens.lens (\PutEmailIdentityMailFromAttributes' {behaviorOnMxFailure} -> behaviorOnMxFailure) (\s@PutEmailIdentityMailFromAttributes' {} a -> s {behaviorOnMxFailure = a} :: PutEmailIdentityMailFromAttributes)

-- | The verified email identity that you want to set up the custom MAIL FROM
-- domain for.
putEmailIdentityMailFromAttributes_emailIdentity :: Lens.Lens' PutEmailIdentityMailFromAttributes Prelude.Text
putEmailIdentityMailFromAttributes_emailIdentity = Lens.lens (\PutEmailIdentityMailFromAttributes' {emailIdentity} -> emailIdentity) (\s@PutEmailIdentityMailFromAttributes' {} a -> s {emailIdentity = a} :: PutEmailIdentityMailFromAttributes)

instance
  Core.AWSRequest
    PutEmailIdentityMailFromAttributes
  where
  type
    AWSResponse PutEmailIdentityMailFromAttributes =
      PutEmailIdentityMailFromAttributesResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutEmailIdentityMailFromAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutEmailIdentityMailFromAttributes
  where
  hashWithSalt
    _salt
    PutEmailIdentityMailFromAttributes' {..} =
      _salt `Prelude.hashWithSalt` mailFromDomain
        `Prelude.hashWithSalt` behaviorOnMxFailure
        `Prelude.hashWithSalt` emailIdentity

instance
  Prelude.NFData
    PutEmailIdentityMailFromAttributes
  where
  rnf PutEmailIdentityMailFromAttributes' {..} =
    Prelude.rnf mailFromDomain
      `Prelude.seq` Prelude.rnf behaviorOnMxFailure
      `Prelude.seq` Prelude.rnf emailIdentity

instance
  Data.ToHeaders
    PutEmailIdentityMailFromAttributes
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
    PutEmailIdentityMailFromAttributes
  where
  toJSON PutEmailIdentityMailFromAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MailFromDomain" Data..=)
              Prelude.<$> mailFromDomain,
            ("BehaviorOnMxFailure" Data..=)
              Prelude.<$> behaviorOnMxFailure
          ]
      )

instance
  Data.ToPath
    PutEmailIdentityMailFromAttributes
  where
  toPath PutEmailIdentityMailFromAttributes' {..} =
    Prelude.mconcat
      [ "/v1/email/identities/",
        Data.toBS emailIdentity,
        "/mail-from"
      ]

instance
  Data.ToQuery
    PutEmailIdentityMailFromAttributes
  where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newPutEmailIdentityMailFromAttributesResponse' smart constructor.
data PutEmailIdentityMailFromAttributesResponse = PutEmailIdentityMailFromAttributesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutEmailIdentityMailFromAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putEmailIdentityMailFromAttributesResponse_httpStatus' - The response's http status code.
newPutEmailIdentityMailFromAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutEmailIdentityMailFromAttributesResponse
newPutEmailIdentityMailFromAttributesResponse
  pHttpStatus_ =
    PutEmailIdentityMailFromAttributesResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putEmailIdentityMailFromAttributesResponse_httpStatus :: Lens.Lens' PutEmailIdentityMailFromAttributesResponse Prelude.Int
putEmailIdentityMailFromAttributesResponse_httpStatus = Lens.lens (\PutEmailIdentityMailFromAttributesResponse' {httpStatus} -> httpStatus) (\s@PutEmailIdentityMailFromAttributesResponse' {} a -> s {httpStatus = a} :: PutEmailIdentityMailFromAttributesResponse)

instance
  Prelude.NFData
    PutEmailIdentityMailFromAttributesResponse
  where
  rnf PutEmailIdentityMailFromAttributesResponse' {..} =
    Prelude.rnf httpStatus
