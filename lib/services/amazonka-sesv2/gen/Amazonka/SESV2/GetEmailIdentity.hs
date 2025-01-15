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
-- Module      : Amazonka.SESV2.GetEmailIdentity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about a specific identity, including the
-- identity\'s verification status, sending authorization policies, its
-- DKIM authentication status, and its custom Mail-From settings.
module Amazonka.SESV2.GetEmailIdentity
  ( -- * Creating a Request
    GetEmailIdentity (..),
    newGetEmailIdentity,

    -- * Request Lenses
    getEmailIdentity_emailIdentity,

    -- * Destructuring the Response
    GetEmailIdentityResponse (..),
    newGetEmailIdentityResponse,

    -- * Response Lenses
    getEmailIdentityResponse_configurationSetName,
    getEmailIdentityResponse_dkimAttributes,
    getEmailIdentityResponse_feedbackForwardingStatus,
    getEmailIdentityResponse_identityType,
    getEmailIdentityResponse_mailFromAttributes,
    getEmailIdentityResponse_policies,
    getEmailIdentityResponse_tags,
    getEmailIdentityResponse_verificationStatus,
    getEmailIdentityResponse_verifiedForSendingStatus,
    getEmailIdentityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to return details about an email identity.
--
-- /See:/ 'newGetEmailIdentity' smart constructor.
data GetEmailIdentity = GetEmailIdentity'
  { -- | The email identity.
    emailIdentity :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEmailIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailIdentity', 'getEmailIdentity_emailIdentity' - The email identity.
newGetEmailIdentity ::
  -- | 'emailIdentity'
  Prelude.Text ->
  GetEmailIdentity
newGetEmailIdentity pEmailIdentity_ =
  GetEmailIdentity' {emailIdentity = pEmailIdentity_}

-- | The email identity.
getEmailIdentity_emailIdentity :: Lens.Lens' GetEmailIdentity Prelude.Text
getEmailIdentity_emailIdentity = Lens.lens (\GetEmailIdentity' {emailIdentity} -> emailIdentity) (\s@GetEmailIdentity' {} a -> s {emailIdentity = a} :: GetEmailIdentity)

instance Core.AWSRequest GetEmailIdentity where
  type
    AWSResponse GetEmailIdentity =
      GetEmailIdentityResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEmailIdentityResponse'
            Prelude.<$> (x Data..?> "ConfigurationSetName")
            Prelude.<*> (x Data..?> "DkimAttributes")
            Prelude.<*> (x Data..?> "FeedbackForwardingStatus")
            Prelude.<*> (x Data..?> "IdentityType")
            Prelude.<*> (x Data..?> "MailFromAttributes")
            Prelude.<*> (x Data..?> "Policies" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "VerificationStatus")
            Prelude.<*> (x Data..?> "VerifiedForSendingStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEmailIdentity where
  hashWithSalt _salt GetEmailIdentity' {..} =
    _salt `Prelude.hashWithSalt` emailIdentity

instance Prelude.NFData GetEmailIdentity where
  rnf GetEmailIdentity' {..} = Prelude.rnf emailIdentity

instance Data.ToHeaders GetEmailIdentity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetEmailIdentity where
  toPath GetEmailIdentity' {..} =
    Prelude.mconcat
      ["/v2/email/identities/", Data.toBS emailIdentity]

instance Data.ToQuery GetEmailIdentity where
  toQuery = Prelude.const Prelude.mempty

-- | Details about an email identity.
--
-- /See:/ 'newGetEmailIdentityResponse' smart constructor.
data GetEmailIdentityResponse = GetEmailIdentityResponse'
  { -- | The configuration set used by default when sending from this identity.
    configurationSetName :: Prelude.Maybe Prelude.Text,
    -- | An object that contains information about the DKIM attributes for the
    -- identity.
    dkimAttributes :: Prelude.Maybe DkimAttributes,
    -- | The feedback forwarding configuration for the identity.
    --
    -- If the value is @true@, you receive email notifications when bounce or
    -- complaint events occur. These notifications are sent to the address that
    -- you specified in the @Return-Path@ header of the original email.
    --
    -- You\'re required to have a method of tracking bounces and complaints. If
    -- you haven\'t set up another mechanism for receiving bounce or complaint
    -- notifications (for example, by setting up an event destination), you
    -- receive an email notification when these events occur (even if this
    -- setting is disabled).
    feedbackForwardingStatus :: Prelude.Maybe Prelude.Bool,
    -- | The email identity type. Note: the @MANAGED_DOMAIN@ identity type is not
    -- supported.
    identityType :: Prelude.Maybe IdentityType,
    -- | An object that contains information about the Mail-From attributes for
    -- the email identity.
    mailFromAttributes :: Prelude.Maybe MailFromAttributes,
    -- | A map of policy names to policies.
    policies :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An array of objects that define the tags (keys and values) that are
    -- associated with the email identity.
    tags :: Prelude.Maybe [Tag],
    -- | The verification status of the identity. The status can be one of the
    -- following:
    --
    -- -   @PENDING@ – The verification process was initiated, but Amazon SES
    --     hasn\'t yet been able to verify the identity.
    --
    -- -   @SUCCESS@ – The verification process completed successfully.
    --
    -- -   @FAILED@ – The verification process failed.
    --
    -- -   @TEMPORARY_FAILURE@ – A temporary issue is preventing Amazon SES
    --     from determining the verification status of the identity.
    --
    -- -   @NOT_STARTED@ – The verification process hasn\'t been initiated for
    --     the identity.
    verificationStatus :: Prelude.Maybe VerificationStatus,
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
-- Create a value of 'GetEmailIdentityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSetName', 'getEmailIdentityResponse_configurationSetName' - The configuration set used by default when sending from this identity.
--
-- 'dkimAttributes', 'getEmailIdentityResponse_dkimAttributes' - An object that contains information about the DKIM attributes for the
-- identity.
--
-- 'feedbackForwardingStatus', 'getEmailIdentityResponse_feedbackForwardingStatus' - The feedback forwarding configuration for the identity.
--
-- If the value is @true@, you receive email notifications when bounce or
-- complaint events occur. These notifications are sent to the address that
-- you specified in the @Return-Path@ header of the original email.
--
-- You\'re required to have a method of tracking bounces and complaints. If
-- you haven\'t set up another mechanism for receiving bounce or complaint
-- notifications (for example, by setting up an event destination), you
-- receive an email notification when these events occur (even if this
-- setting is disabled).
--
-- 'identityType', 'getEmailIdentityResponse_identityType' - The email identity type. Note: the @MANAGED_DOMAIN@ identity type is not
-- supported.
--
-- 'mailFromAttributes', 'getEmailIdentityResponse_mailFromAttributes' - An object that contains information about the Mail-From attributes for
-- the email identity.
--
-- 'policies', 'getEmailIdentityResponse_policies' - A map of policy names to policies.
--
-- 'tags', 'getEmailIdentityResponse_tags' - An array of objects that define the tags (keys and values) that are
-- associated with the email identity.
--
-- 'verificationStatus', 'getEmailIdentityResponse_verificationStatus' - The verification status of the identity. The status can be one of the
-- following:
--
-- -   @PENDING@ – The verification process was initiated, but Amazon SES
--     hasn\'t yet been able to verify the identity.
--
-- -   @SUCCESS@ – The verification process completed successfully.
--
-- -   @FAILED@ – The verification process failed.
--
-- -   @TEMPORARY_FAILURE@ – A temporary issue is preventing Amazon SES
--     from determining the verification status of the identity.
--
-- -   @NOT_STARTED@ – The verification process hasn\'t been initiated for
--     the identity.
--
-- 'verifiedForSendingStatus', 'getEmailIdentityResponse_verifiedForSendingStatus' - Specifies whether or not the identity is verified. You can only send
-- email from verified email addresses or domains. For more information
-- about verifying identities, see the
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-email-manage-verify.html Amazon Pinpoint User Guide>.
--
-- 'httpStatus', 'getEmailIdentityResponse_httpStatus' - The response's http status code.
newGetEmailIdentityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEmailIdentityResponse
newGetEmailIdentityResponse pHttpStatus_ =
  GetEmailIdentityResponse'
    { configurationSetName =
        Prelude.Nothing,
      dkimAttributes = Prelude.Nothing,
      feedbackForwardingStatus = Prelude.Nothing,
      identityType = Prelude.Nothing,
      mailFromAttributes = Prelude.Nothing,
      policies = Prelude.Nothing,
      tags = Prelude.Nothing,
      verificationStatus = Prelude.Nothing,
      verifiedForSendingStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The configuration set used by default when sending from this identity.
getEmailIdentityResponse_configurationSetName :: Lens.Lens' GetEmailIdentityResponse (Prelude.Maybe Prelude.Text)
getEmailIdentityResponse_configurationSetName = Lens.lens (\GetEmailIdentityResponse' {configurationSetName} -> configurationSetName) (\s@GetEmailIdentityResponse' {} a -> s {configurationSetName = a} :: GetEmailIdentityResponse)

-- | An object that contains information about the DKIM attributes for the
-- identity.
getEmailIdentityResponse_dkimAttributes :: Lens.Lens' GetEmailIdentityResponse (Prelude.Maybe DkimAttributes)
getEmailIdentityResponse_dkimAttributes = Lens.lens (\GetEmailIdentityResponse' {dkimAttributes} -> dkimAttributes) (\s@GetEmailIdentityResponse' {} a -> s {dkimAttributes = a} :: GetEmailIdentityResponse)

-- | The feedback forwarding configuration for the identity.
--
-- If the value is @true@, you receive email notifications when bounce or
-- complaint events occur. These notifications are sent to the address that
-- you specified in the @Return-Path@ header of the original email.
--
-- You\'re required to have a method of tracking bounces and complaints. If
-- you haven\'t set up another mechanism for receiving bounce or complaint
-- notifications (for example, by setting up an event destination), you
-- receive an email notification when these events occur (even if this
-- setting is disabled).
getEmailIdentityResponse_feedbackForwardingStatus :: Lens.Lens' GetEmailIdentityResponse (Prelude.Maybe Prelude.Bool)
getEmailIdentityResponse_feedbackForwardingStatus = Lens.lens (\GetEmailIdentityResponse' {feedbackForwardingStatus} -> feedbackForwardingStatus) (\s@GetEmailIdentityResponse' {} a -> s {feedbackForwardingStatus = a} :: GetEmailIdentityResponse)

-- | The email identity type. Note: the @MANAGED_DOMAIN@ identity type is not
-- supported.
getEmailIdentityResponse_identityType :: Lens.Lens' GetEmailIdentityResponse (Prelude.Maybe IdentityType)
getEmailIdentityResponse_identityType = Lens.lens (\GetEmailIdentityResponse' {identityType} -> identityType) (\s@GetEmailIdentityResponse' {} a -> s {identityType = a} :: GetEmailIdentityResponse)

-- | An object that contains information about the Mail-From attributes for
-- the email identity.
getEmailIdentityResponse_mailFromAttributes :: Lens.Lens' GetEmailIdentityResponse (Prelude.Maybe MailFromAttributes)
getEmailIdentityResponse_mailFromAttributes = Lens.lens (\GetEmailIdentityResponse' {mailFromAttributes} -> mailFromAttributes) (\s@GetEmailIdentityResponse' {} a -> s {mailFromAttributes = a} :: GetEmailIdentityResponse)

-- | A map of policy names to policies.
getEmailIdentityResponse_policies :: Lens.Lens' GetEmailIdentityResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getEmailIdentityResponse_policies = Lens.lens (\GetEmailIdentityResponse' {policies} -> policies) (\s@GetEmailIdentityResponse' {} a -> s {policies = a} :: GetEmailIdentityResponse) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects that define the tags (keys and values) that are
-- associated with the email identity.
getEmailIdentityResponse_tags :: Lens.Lens' GetEmailIdentityResponse (Prelude.Maybe [Tag])
getEmailIdentityResponse_tags = Lens.lens (\GetEmailIdentityResponse' {tags} -> tags) (\s@GetEmailIdentityResponse' {} a -> s {tags = a} :: GetEmailIdentityResponse) Prelude.. Lens.mapping Lens.coerced

-- | The verification status of the identity. The status can be one of the
-- following:
--
-- -   @PENDING@ – The verification process was initiated, but Amazon SES
--     hasn\'t yet been able to verify the identity.
--
-- -   @SUCCESS@ – The verification process completed successfully.
--
-- -   @FAILED@ – The verification process failed.
--
-- -   @TEMPORARY_FAILURE@ – A temporary issue is preventing Amazon SES
--     from determining the verification status of the identity.
--
-- -   @NOT_STARTED@ – The verification process hasn\'t been initiated for
--     the identity.
getEmailIdentityResponse_verificationStatus :: Lens.Lens' GetEmailIdentityResponse (Prelude.Maybe VerificationStatus)
getEmailIdentityResponse_verificationStatus = Lens.lens (\GetEmailIdentityResponse' {verificationStatus} -> verificationStatus) (\s@GetEmailIdentityResponse' {} a -> s {verificationStatus = a} :: GetEmailIdentityResponse)

-- | Specifies whether or not the identity is verified. You can only send
-- email from verified email addresses or domains. For more information
-- about verifying identities, see the
-- <https://docs.aws.amazon.com/pinpoint/latest/userguide/channels-email-manage-verify.html Amazon Pinpoint User Guide>.
getEmailIdentityResponse_verifiedForSendingStatus :: Lens.Lens' GetEmailIdentityResponse (Prelude.Maybe Prelude.Bool)
getEmailIdentityResponse_verifiedForSendingStatus = Lens.lens (\GetEmailIdentityResponse' {verifiedForSendingStatus} -> verifiedForSendingStatus) (\s@GetEmailIdentityResponse' {} a -> s {verifiedForSendingStatus = a} :: GetEmailIdentityResponse)

-- | The response's http status code.
getEmailIdentityResponse_httpStatus :: Lens.Lens' GetEmailIdentityResponse Prelude.Int
getEmailIdentityResponse_httpStatus = Lens.lens (\GetEmailIdentityResponse' {httpStatus} -> httpStatus) (\s@GetEmailIdentityResponse' {} a -> s {httpStatus = a} :: GetEmailIdentityResponse)

instance Prelude.NFData GetEmailIdentityResponse where
  rnf GetEmailIdentityResponse' {..} =
    Prelude.rnf configurationSetName `Prelude.seq`
      Prelude.rnf dkimAttributes `Prelude.seq`
        Prelude.rnf feedbackForwardingStatus `Prelude.seq`
          Prelude.rnf identityType `Prelude.seq`
            Prelude.rnf mailFromAttributes `Prelude.seq`
              Prelude.rnf policies `Prelude.seq`
                Prelude.rnf tags `Prelude.seq`
                  Prelude.rnf verificationStatus `Prelude.seq`
                    Prelude.rnf verifiedForSendingStatus `Prelude.seq`
                      Prelude.rnf httpStatus
