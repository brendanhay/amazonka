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
-- Module      : Amazonka.SESV2.CreateEmailIdentity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the process of verifying an email identity. An /identity/ is an
-- email address or domain that you use when you send email. Before you can
-- use an identity to send email, you first have to verify it. By verifying
-- an identity, you demonstrate that you\'re the owner of the identity, and
-- that you\'ve given Amazon SES API v2 permission to send email from the
-- identity.
--
-- When you verify an email address, Amazon SES sends an email to the
-- address. Your email address is verified as soon as you follow the link
-- in the verification email.
--
-- When you verify a domain without specifying the @DkimSigningAttributes@
-- object, this operation provides a set of DKIM tokens. You can convert
-- these tokens into CNAME records, which you then add to the DNS
-- configuration for your domain. Your domain is verified when Amazon SES
-- detects these records in the DNS configuration for your domain. This
-- verification method is known as
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>.
--
-- Alternatively, you can perform the verification process by providing
-- your own public-private key pair. This verification method is known as
-- Bring Your Own DKIM (BYODKIM). To use BYODKIM, your call to the
-- @CreateEmailIdentity@ operation has to include the
-- @DkimSigningAttributes@ object. When you specify this object, you
-- provide a selector (a component of the DNS record name that identifies
-- the public key to use for DKIM authentication) and a private key.
--
-- When you verify a domain, this operation provides a set of DKIM tokens,
-- which you can convert into CNAME tokens. You add these CNAME tokens to
-- the DNS configuration for your domain. Your domain is verified when
-- Amazon SES detects these records in the DNS configuration for your
-- domain. For some DNS providers, it can take 72 hours or more to complete
-- the domain verification process.
--
-- Additionally, you can associate an existing configuration set with the
-- email identity that you\'re verifying.
module Amazonka.SESV2.CreateEmailIdentity
  ( -- * Creating a Request
    CreateEmailIdentity (..),
    newCreateEmailIdentity,

    -- * Request Lenses
    createEmailIdentity_configurationSetName,
    createEmailIdentity_dkimSigningAttributes,
    createEmailIdentity_tags,
    createEmailIdentity_emailIdentity,

    -- * Destructuring the Response
    CreateEmailIdentityResponse (..),
    newCreateEmailIdentityResponse,

    -- * Response Lenses
    createEmailIdentityResponse_dkimAttributes,
    createEmailIdentityResponse_identityType,
    createEmailIdentityResponse_verifiedForSendingStatus,
    createEmailIdentityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to begin the verification process for an email identity (an
-- email address or domain).
--
-- /See:/ 'newCreateEmailIdentity' smart constructor.
data CreateEmailIdentity = CreateEmailIdentity'
  { -- | The configuration set to use by default when sending from this identity.
    -- Note that any configuration set defined in the email sending request
    -- takes precedence.
    configurationSetName :: Prelude.Maybe Prelude.Text,
    -- | If your request includes this object, Amazon SES configures the identity
    -- to use Bring Your Own DKIM (BYODKIM) for DKIM authentication purposes,
    -- or, configures the key length to be used for
    -- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>.
    --
    -- You can only specify this object if the email identity is a domain, as
    -- opposed to an address.
    dkimSigningAttributes :: Prelude.Maybe DkimSigningAttributes,
    -- | An array of objects that define the tags (keys and values) to associate
    -- with the email identity.
    tags :: Prelude.Maybe [Tag],
    -- | The email address or domain to verify.
    emailIdentity :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEmailIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetName', 'createEmailIdentity_configurationSetName' - The configuration set to use by default when sending from this identity.
-- Note that any configuration set defined in the email sending request
-- takes precedence.
--
-- 'dkimSigningAttributes', 'createEmailIdentity_dkimSigningAttributes' - If your request includes this object, Amazon SES configures the identity
-- to use Bring Your Own DKIM (BYODKIM) for DKIM authentication purposes,
-- or, configures the key length to be used for
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>.
--
-- You can only specify this object if the email identity is a domain, as
-- opposed to an address.
--
-- 'tags', 'createEmailIdentity_tags' - An array of objects that define the tags (keys and values) to associate
-- with the email identity.
--
-- 'emailIdentity', 'createEmailIdentity_emailIdentity' - The email address or domain to verify.
newCreateEmailIdentity ::
  -- | 'emailIdentity'
  Prelude.Text ->
  CreateEmailIdentity
newCreateEmailIdentity pEmailIdentity_ =
  CreateEmailIdentity'
    { configurationSetName =
        Prelude.Nothing,
      dkimSigningAttributes = Prelude.Nothing,
      tags = Prelude.Nothing,
      emailIdentity = pEmailIdentity_
    }

-- | The configuration set to use by default when sending from this identity.
-- Note that any configuration set defined in the email sending request
-- takes precedence.
createEmailIdentity_configurationSetName :: Lens.Lens' CreateEmailIdentity (Prelude.Maybe Prelude.Text)
createEmailIdentity_configurationSetName = Lens.lens (\CreateEmailIdentity' {configurationSetName} -> configurationSetName) (\s@CreateEmailIdentity' {} a -> s {configurationSetName = a} :: CreateEmailIdentity)

-- | If your request includes this object, Amazon SES configures the identity
-- to use Bring Your Own DKIM (BYODKIM) for DKIM authentication purposes,
-- or, configures the key length to be used for
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/easy-dkim.html Easy DKIM>.
--
-- You can only specify this object if the email identity is a domain, as
-- opposed to an address.
createEmailIdentity_dkimSigningAttributes :: Lens.Lens' CreateEmailIdentity (Prelude.Maybe DkimSigningAttributes)
createEmailIdentity_dkimSigningAttributes = Lens.lens (\CreateEmailIdentity' {dkimSigningAttributes} -> dkimSigningAttributes) (\s@CreateEmailIdentity' {} a -> s {dkimSigningAttributes = a} :: CreateEmailIdentity)

-- | An array of objects that define the tags (keys and values) to associate
-- with the email identity.
createEmailIdentity_tags :: Lens.Lens' CreateEmailIdentity (Prelude.Maybe [Tag])
createEmailIdentity_tags = Lens.lens (\CreateEmailIdentity' {tags} -> tags) (\s@CreateEmailIdentity' {} a -> s {tags = a} :: CreateEmailIdentity) Prelude.. Lens.mapping Lens.coerced

-- | The email address or domain to verify.
createEmailIdentity_emailIdentity :: Lens.Lens' CreateEmailIdentity Prelude.Text
createEmailIdentity_emailIdentity = Lens.lens (\CreateEmailIdentity' {emailIdentity} -> emailIdentity) (\s@CreateEmailIdentity' {} a -> s {emailIdentity = a} :: CreateEmailIdentity)

instance Core.AWSRequest CreateEmailIdentity where
  type
    AWSResponse CreateEmailIdentity =
      CreateEmailIdentityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEmailIdentityResponse'
            Prelude.<$> (x Data..?> "DkimAttributes")
            Prelude.<*> (x Data..?> "IdentityType")
            Prelude.<*> (x Data..?> "VerifiedForSendingStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEmailIdentity where
  hashWithSalt _salt CreateEmailIdentity' {..} =
    _salt `Prelude.hashWithSalt` configurationSetName
      `Prelude.hashWithSalt` dkimSigningAttributes
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` emailIdentity

instance Prelude.NFData CreateEmailIdentity where
  rnf CreateEmailIdentity' {..} =
    Prelude.rnf configurationSetName
      `Prelude.seq` Prelude.rnf dkimSigningAttributes
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf emailIdentity

instance Data.ToHeaders CreateEmailIdentity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEmailIdentity where
  toJSON CreateEmailIdentity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConfigurationSetName" Data..=)
              Prelude.<$> configurationSetName,
            ("DkimSigningAttributes" Data..=)
              Prelude.<$> dkimSigningAttributes,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("EmailIdentity" Data..= emailIdentity)
          ]
      )

instance Data.ToPath CreateEmailIdentity where
  toPath = Prelude.const "/v2/email/identities"

instance Data.ToQuery CreateEmailIdentity where
  toQuery = Prelude.const Prelude.mempty

-- | If the email identity is a domain, this object contains information
-- about the DKIM verification status for the domain.
--
-- If the email identity is an email address, this object is empty.
--
-- /See:/ 'newCreateEmailIdentityResponse' smart constructor.
data CreateEmailIdentityResponse = CreateEmailIdentityResponse'
  { -- | An object that contains information about the DKIM attributes for the
    -- identity.
    dkimAttributes :: Prelude.Maybe DkimAttributes,
    -- | The email identity type. Note: the @MANAGED_DOMAIN@ identity type is not
    -- supported.
    identityType :: Prelude.Maybe IdentityType,
    -- | Specifies whether or not the identity is verified. You can only send
    -- email from verified email addresses or domains. For more information
    -- about verifying identities, see the
    -- <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-email-manage-verify.html Amazon Pinpoint User Guide>.
    verifiedForSendingStatus :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEmailIdentityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dkimAttributes', 'createEmailIdentityResponse_dkimAttributes' - An object that contains information about the DKIM attributes for the
-- identity.
--
-- 'identityType', 'createEmailIdentityResponse_identityType' - The email identity type. Note: the @MANAGED_DOMAIN@ identity type is not
-- supported.
--
-- 'verifiedForSendingStatus', 'createEmailIdentityResponse_verifiedForSendingStatus' - Specifies whether or not the identity is verified. You can only send
-- email from verified email addresses or domains. For more information
-- about verifying identities, see the
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-email-manage-verify.html Amazon Pinpoint User Guide>.
--
-- 'httpStatus', 'createEmailIdentityResponse_httpStatus' - The response's http status code.
newCreateEmailIdentityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEmailIdentityResponse
newCreateEmailIdentityResponse pHttpStatus_ =
  CreateEmailIdentityResponse'
    { dkimAttributes =
        Prelude.Nothing,
      identityType = Prelude.Nothing,
      verifiedForSendingStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains information about the DKIM attributes for the
-- identity.
createEmailIdentityResponse_dkimAttributes :: Lens.Lens' CreateEmailIdentityResponse (Prelude.Maybe DkimAttributes)
createEmailIdentityResponse_dkimAttributes = Lens.lens (\CreateEmailIdentityResponse' {dkimAttributes} -> dkimAttributes) (\s@CreateEmailIdentityResponse' {} a -> s {dkimAttributes = a} :: CreateEmailIdentityResponse)

-- | The email identity type. Note: the @MANAGED_DOMAIN@ identity type is not
-- supported.
createEmailIdentityResponse_identityType :: Lens.Lens' CreateEmailIdentityResponse (Prelude.Maybe IdentityType)
createEmailIdentityResponse_identityType = Lens.lens (\CreateEmailIdentityResponse' {identityType} -> identityType) (\s@CreateEmailIdentityResponse' {} a -> s {identityType = a} :: CreateEmailIdentityResponse)

-- | Specifies whether or not the identity is verified. You can only send
-- email from verified email addresses or domains. For more information
-- about verifying identities, see the
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-email-manage-verify.html Amazon Pinpoint User Guide>.
createEmailIdentityResponse_verifiedForSendingStatus :: Lens.Lens' CreateEmailIdentityResponse (Prelude.Maybe Prelude.Bool)
createEmailIdentityResponse_verifiedForSendingStatus = Lens.lens (\CreateEmailIdentityResponse' {verifiedForSendingStatus} -> verifiedForSendingStatus) (\s@CreateEmailIdentityResponse' {} a -> s {verifiedForSendingStatus = a} :: CreateEmailIdentityResponse)

-- | The response's http status code.
createEmailIdentityResponse_httpStatus :: Lens.Lens' CreateEmailIdentityResponse Prelude.Int
createEmailIdentityResponse_httpStatus = Lens.lens (\CreateEmailIdentityResponse' {httpStatus} -> httpStatus) (\s@CreateEmailIdentityResponse' {} a -> s {httpStatus = a} :: CreateEmailIdentityResponse)

instance Prelude.NFData CreateEmailIdentityResponse where
  rnf CreateEmailIdentityResponse' {..} =
    Prelude.rnf dkimAttributes
      `Prelude.seq` Prelude.rnf identityType
      `Prelude.seq` Prelude.rnf verifiedForSendingStatus
      `Prelude.seq` Prelude.rnf httpStatus
