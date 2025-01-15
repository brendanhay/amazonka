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
-- Module      : Amazonka.PinpointEmail.CreateEmailIdentity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Verifies an email identity for use with Amazon Pinpoint. In Amazon
-- Pinpoint, an identity is an email address or domain that you use when
-- you send email. Before you can use an identity to send email with Amazon
-- Pinpoint, you first have to verify it. By verifying an address, you
-- demonstrate that you\'re the owner of the address, and that you\'ve
-- given Amazon Pinpoint permission to send email from the address.
--
-- When you verify an email address, Amazon Pinpoint sends an email to the
-- address. Your email address is verified as soon as you follow the link
-- in the verification email.
--
-- When you verify a domain, this operation provides a set of DKIM tokens,
-- which you can convert into CNAME tokens. You add these CNAME tokens to
-- the DNS configuration for your domain. Your domain is verified when
-- Amazon Pinpoint detects these records in the DNS configuration for your
-- domain. It usually takes around 72 hours to complete the domain
-- verification process.
module Amazonka.PinpointEmail.CreateEmailIdentity
  ( -- * Creating a Request
    CreateEmailIdentity (..),
    newCreateEmailIdentity,

    -- * Request Lenses
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
import Amazonka.PinpointEmail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to begin the verification process for an email identity (an
-- email address or domain).
--
-- /See:/ 'newCreateEmailIdentity' smart constructor.
data CreateEmailIdentity = CreateEmailIdentity'
  { -- | An array of objects that define the tags (keys and values) that you want
    -- to associate with the email identity.
    tags :: Prelude.Maybe [Tag],
    -- | The email address or domain that you want to verify.
    emailIdentity :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEmailIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createEmailIdentity_tags' - An array of objects that define the tags (keys and values) that you want
-- to associate with the email identity.
--
-- 'emailIdentity', 'createEmailIdentity_emailIdentity' - The email address or domain that you want to verify.
newCreateEmailIdentity ::
  -- | 'emailIdentity'
  Prelude.Text ->
  CreateEmailIdentity
newCreateEmailIdentity pEmailIdentity_ =
  CreateEmailIdentity'
    { tags = Prelude.Nothing,
      emailIdentity = pEmailIdentity_
    }

-- | An array of objects that define the tags (keys and values) that you want
-- to associate with the email identity.
createEmailIdentity_tags :: Lens.Lens' CreateEmailIdentity (Prelude.Maybe [Tag])
createEmailIdentity_tags = Lens.lens (\CreateEmailIdentity' {tags} -> tags) (\s@CreateEmailIdentity' {} a -> s {tags = a} :: CreateEmailIdentity) Prelude.. Lens.mapping Lens.coerced

-- | The email address or domain that you want to verify.
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
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` emailIdentity

instance Prelude.NFData CreateEmailIdentity where
  rnf CreateEmailIdentity' {..} =
    Prelude.rnf tags `Prelude.seq`
      Prelude.rnf emailIdentity

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
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("EmailIdentity" Data..= emailIdentity)
          ]
      )

instance Data.ToPath CreateEmailIdentity where
  toPath = Prelude.const "/v1/email/identities"

instance Data.ToQuery CreateEmailIdentity where
  toQuery = Prelude.const Prelude.mempty

-- | If the email identity is a domain, this object contains tokens that you
-- can use to create a set of CNAME records. To sucessfully verify your
-- domain, you have to add these records to the DNS configuration for your
-- domain.
--
-- If the email identity is an email address, this object is empty.
--
-- /See:/ 'newCreateEmailIdentityResponse' smart constructor.
data CreateEmailIdentityResponse = CreateEmailIdentityResponse'
  { -- | An object that contains information about the DKIM attributes for the
    -- identity. This object includes the tokens that you use to create the
    -- CNAME records that are required to complete the DKIM verification
    -- process.
    dkimAttributes :: Prelude.Maybe DkimAttributes,
    -- | The email identity type.
    identityType :: Prelude.Maybe IdentityType,
    -- | Specifies whether or not the identity is verified. In Amazon Pinpoint,
    -- you can only send email from verified email addresses or domains. For
    -- more information about verifying identities, see the
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
-- identity. This object includes the tokens that you use to create the
-- CNAME records that are required to complete the DKIM verification
-- process.
--
-- 'identityType', 'createEmailIdentityResponse_identityType' - The email identity type.
--
-- 'verifiedForSendingStatus', 'createEmailIdentityResponse_verifiedForSendingStatus' - Specifies whether or not the identity is verified. In Amazon Pinpoint,
-- you can only send email from verified email addresses or domains. For
-- more information about verifying identities, see the
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
-- identity. This object includes the tokens that you use to create the
-- CNAME records that are required to complete the DKIM verification
-- process.
createEmailIdentityResponse_dkimAttributes :: Lens.Lens' CreateEmailIdentityResponse (Prelude.Maybe DkimAttributes)
createEmailIdentityResponse_dkimAttributes = Lens.lens (\CreateEmailIdentityResponse' {dkimAttributes} -> dkimAttributes) (\s@CreateEmailIdentityResponse' {} a -> s {dkimAttributes = a} :: CreateEmailIdentityResponse)

-- | The email identity type.
createEmailIdentityResponse_identityType :: Lens.Lens' CreateEmailIdentityResponse (Prelude.Maybe IdentityType)
createEmailIdentityResponse_identityType = Lens.lens (\CreateEmailIdentityResponse' {identityType} -> identityType) (\s@CreateEmailIdentityResponse' {} a -> s {identityType = a} :: CreateEmailIdentityResponse)

-- | Specifies whether or not the identity is verified. In Amazon Pinpoint,
-- you can only send email from verified email addresses or domains. For
-- more information about verifying identities, see the
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-email-manage-verify.html Amazon Pinpoint User Guide>.
createEmailIdentityResponse_verifiedForSendingStatus :: Lens.Lens' CreateEmailIdentityResponse (Prelude.Maybe Prelude.Bool)
createEmailIdentityResponse_verifiedForSendingStatus = Lens.lens (\CreateEmailIdentityResponse' {verifiedForSendingStatus} -> verifiedForSendingStatus) (\s@CreateEmailIdentityResponse' {} a -> s {verifiedForSendingStatus = a} :: CreateEmailIdentityResponse)

-- | The response's http status code.
createEmailIdentityResponse_httpStatus :: Lens.Lens' CreateEmailIdentityResponse Prelude.Int
createEmailIdentityResponse_httpStatus = Lens.lens (\CreateEmailIdentityResponse' {httpStatus} -> httpStatus) (\s@CreateEmailIdentityResponse' {} a -> s {httpStatus = a} :: CreateEmailIdentityResponse)

instance Prelude.NFData CreateEmailIdentityResponse where
  rnf CreateEmailIdentityResponse' {..} =
    Prelude.rnf dkimAttributes `Prelude.seq`
      Prelude.rnf identityType `Prelude.seq`
        Prelude.rnf verifiedForSendingStatus `Prelude.seq`
          Prelude.rnf httpStatus
