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
-- Module      : Network.AWS.SESv2.PutEmailIdentityMailFromAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to enable or disable the custom Mail-From domain configuration for
-- an email identity.
module Network.AWS.SESv2.PutEmailIdentityMailFromAttributes
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

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
    -- | The action that you want to take if the required MX record isn\'t found
    -- when you send an email. When you set this value to @UseDefaultValue@,
    -- the mail is sent using /amazonses.com/ as the MAIL FROM domain. When you
    -- set this value to @RejectMessage@, the Amazon SES API v2 returns a
    -- @MailFromDomainNotVerified@ error, and doesn\'t attempt to deliver the
    -- email.
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
-- 'behaviorOnMxFailure', 'putEmailIdentityMailFromAttributes_behaviorOnMxFailure' - The action that you want to take if the required MX record isn\'t found
-- when you send an email. When you set this value to @UseDefaultValue@,
-- the mail is sent using /amazonses.com/ as the MAIL FROM domain. When you
-- set this value to @RejectMessage@, the Amazon SES API v2 returns a
-- @MailFromDomainNotVerified@ error, and doesn\'t attempt to deliver the
-- email.
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

-- | The action that you want to take if the required MX record isn\'t found
-- when you send an email. When you set this value to @UseDefaultValue@,
-- the mail is sent using /amazonses.com/ as the MAIL FROM domain. When you
-- set this value to @RejectMessage@, the Amazon SES API v2 returns a
-- @MailFromDomainNotVerified@ error, and doesn\'t attempt to deliver the
-- email.
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
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutEmailIdentityMailFromAttributesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutEmailIdentityMailFromAttributes

instance
  Prelude.NFData
    PutEmailIdentityMailFromAttributes

instance
  Core.ToHeaders
    PutEmailIdentityMailFromAttributes
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    PutEmailIdentityMailFromAttributes
  where
  toJSON PutEmailIdentityMailFromAttributes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MailFromDomain" Core..=)
              Prelude.<$> mailFromDomain,
            ("BehaviorOnMxFailure" Core..=)
              Prelude.<$> behaviorOnMxFailure
          ]
      )

instance
  Core.ToPath
    PutEmailIdentityMailFromAttributes
  where
  toPath PutEmailIdentityMailFromAttributes' {..} =
    Prelude.mconcat
      [ "/v2/email/identities/",
        Core.toBS emailIdentity,
        "/mail-from"
      ]

instance
  Core.ToQuery
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
