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
-- Module      : Network.AWS.SESv2.PutAccountDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update your Amazon SES account details.
module Network.AWS.SESv2.PutAccountDetails
  ( -- * Creating a Request
    PutAccountDetails (..),
    newPutAccountDetails,

    -- * Request Lenses
    putAccountDetails_productionAccessEnabled,
    putAccountDetails_contactLanguage,
    putAccountDetails_additionalContactEmailAddresses,
    putAccountDetails_mailType,
    putAccountDetails_websiteURL,
    putAccountDetails_useCaseDescription,

    -- * Destructuring the Response
    PutAccountDetailsResponse (..),
    newPutAccountDetailsResponse,

    -- * Response Lenses
    putAccountDetailsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SESv2.Types

-- | A request to submit new account details.
--
-- /See:/ 'newPutAccountDetails' smart constructor.
data PutAccountDetails = PutAccountDetails'
  { -- | Indicates whether or not your account should have production access in
    -- the current AWS Region.
    --
    -- If the value is @false@, then your account is in the /sandbox/. When
    -- your account is in the sandbox, you can only send email to verified
    -- identities. Additionally, the maximum number of emails you can send in a
    -- 24-hour period (your sending quota) is 200, and the maximum number of
    -- emails you can send per second (your maximum sending rate) is 1.
    --
    -- If the value is @true@, then your account has production access. When
    -- your account has production access, you can send email to any address.
    -- The sending quota and maximum sending rate for your account vary based
    -- on your specific use case.
    productionAccessEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The language you would prefer to be contacted with.
    contactLanguage :: Prelude.Maybe ContactLanguage,
    -- | Additional email addresses that you would like to be notified regarding
    -- Amazon SES matters.
    additionalContactEmailAddresses :: Prelude.Maybe (Core.Sensitive (Prelude.NonEmpty (Core.Sensitive Prelude.Text))),
    -- | The type of email your account will send.
    mailType :: MailType,
    -- | The URL of your website. This information helps us better understand the
    -- type of content that you plan to send.
    websiteURL :: Core.Sensitive Prelude.Text,
    -- | A description of the types of email that you plan to send.
    useCaseDescription :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccountDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productionAccessEnabled', 'putAccountDetails_productionAccessEnabled' - Indicates whether or not your account should have production access in
-- the current AWS Region.
--
-- If the value is @false@, then your account is in the /sandbox/. When
-- your account is in the sandbox, you can only send email to verified
-- identities. Additionally, the maximum number of emails you can send in a
-- 24-hour period (your sending quota) is 200, and the maximum number of
-- emails you can send per second (your maximum sending rate) is 1.
--
-- If the value is @true@, then your account has production access. When
-- your account has production access, you can send email to any address.
-- The sending quota and maximum sending rate for your account vary based
-- on your specific use case.
--
-- 'contactLanguage', 'putAccountDetails_contactLanguage' - The language you would prefer to be contacted with.
--
-- 'additionalContactEmailAddresses', 'putAccountDetails_additionalContactEmailAddresses' - Additional email addresses that you would like to be notified regarding
-- Amazon SES matters.
--
-- 'mailType', 'putAccountDetails_mailType' - The type of email your account will send.
--
-- 'websiteURL', 'putAccountDetails_websiteURL' - The URL of your website. This information helps us better understand the
-- type of content that you plan to send.
--
-- 'useCaseDescription', 'putAccountDetails_useCaseDescription' - A description of the types of email that you plan to send.
newPutAccountDetails ::
  -- | 'mailType'
  MailType ->
  -- | 'websiteURL'
  Prelude.Text ->
  -- | 'useCaseDescription'
  Prelude.Text ->
  PutAccountDetails
newPutAccountDetails
  pMailType_
  pWebsiteURL_
  pUseCaseDescription_ =
    PutAccountDetails'
      { productionAccessEnabled =
          Prelude.Nothing,
        contactLanguage = Prelude.Nothing,
        additionalContactEmailAddresses = Prelude.Nothing,
        mailType = pMailType_,
        websiteURL = Core._Sensitive Lens.# pWebsiteURL_,
        useCaseDescription =
          Core._Sensitive Lens.# pUseCaseDescription_
      }

-- | Indicates whether or not your account should have production access in
-- the current AWS Region.
--
-- If the value is @false@, then your account is in the /sandbox/. When
-- your account is in the sandbox, you can only send email to verified
-- identities. Additionally, the maximum number of emails you can send in a
-- 24-hour period (your sending quota) is 200, and the maximum number of
-- emails you can send per second (your maximum sending rate) is 1.
--
-- If the value is @true@, then your account has production access. When
-- your account has production access, you can send email to any address.
-- The sending quota and maximum sending rate for your account vary based
-- on your specific use case.
putAccountDetails_productionAccessEnabled :: Lens.Lens' PutAccountDetails (Prelude.Maybe Prelude.Bool)
putAccountDetails_productionAccessEnabled = Lens.lens (\PutAccountDetails' {productionAccessEnabled} -> productionAccessEnabled) (\s@PutAccountDetails' {} a -> s {productionAccessEnabled = a} :: PutAccountDetails)

-- | The language you would prefer to be contacted with.
putAccountDetails_contactLanguage :: Lens.Lens' PutAccountDetails (Prelude.Maybe ContactLanguage)
putAccountDetails_contactLanguage = Lens.lens (\PutAccountDetails' {contactLanguage} -> contactLanguage) (\s@PutAccountDetails' {} a -> s {contactLanguage = a} :: PutAccountDetails)

-- | Additional email addresses that you would like to be notified regarding
-- Amazon SES matters.
putAccountDetails_additionalContactEmailAddresses :: Lens.Lens' PutAccountDetails (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
putAccountDetails_additionalContactEmailAddresses = Lens.lens (\PutAccountDetails' {additionalContactEmailAddresses} -> additionalContactEmailAddresses) (\s@PutAccountDetails' {} a -> s {additionalContactEmailAddresses = a} :: PutAccountDetails) Prelude.. Lens.mapping (Core._Sensitive Prelude.. Lens._Coerce)

-- | The type of email your account will send.
putAccountDetails_mailType :: Lens.Lens' PutAccountDetails MailType
putAccountDetails_mailType = Lens.lens (\PutAccountDetails' {mailType} -> mailType) (\s@PutAccountDetails' {} a -> s {mailType = a} :: PutAccountDetails)

-- | The URL of your website. This information helps us better understand the
-- type of content that you plan to send.
putAccountDetails_websiteURL :: Lens.Lens' PutAccountDetails Prelude.Text
putAccountDetails_websiteURL = Lens.lens (\PutAccountDetails' {websiteURL} -> websiteURL) (\s@PutAccountDetails' {} a -> s {websiteURL = a} :: PutAccountDetails) Prelude.. Core._Sensitive

-- | A description of the types of email that you plan to send.
putAccountDetails_useCaseDescription :: Lens.Lens' PutAccountDetails Prelude.Text
putAccountDetails_useCaseDescription = Lens.lens (\PutAccountDetails' {useCaseDescription} -> useCaseDescription) (\s@PutAccountDetails' {} a -> s {useCaseDescription = a} :: PutAccountDetails) Prelude.. Core._Sensitive

instance Core.AWSRequest PutAccountDetails where
  type
    AWSResponse PutAccountDetails =
      PutAccountDetailsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutAccountDetailsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAccountDetails

instance Prelude.NFData PutAccountDetails

instance Core.ToHeaders PutAccountDetails where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutAccountDetails where
  toJSON PutAccountDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ProductionAccessEnabled" Core..=)
              Prelude.<$> productionAccessEnabled,
            ("ContactLanguage" Core..=)
              Prelude.<$> contactLanguage,
            ("AdditionalContactEmailAddresses" Core..=)
              Prelude.<$> additionalContactEmailAddresses,
            Prelude.Just ("MailType" Core..= mailType),
            Prelude.Just ("WebsiteURL" Core..= websiteURL),
            Prelude.Just
              ("UseCaseDescription" Core..= useCaseDescription)
          ]
      )

instance Core.ToPath PutAccountDetails where
  toPath = Prelude.const "/v2/email/account/details"

instance Core.ToQuery PutAccountDetails where
  toQuery = Prelude.const Prelude.mempty

-- | An HTTP 200 response if the request succeeds, or an error message if the
-- request fails.
--
-- /See:/ 'newPutAccountDetailsResponse' smart constructor.
data PutAccountDetailsResponse = PutAccountDetailsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAccountDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putAccountDetailsResponse_httpStatus' - The response's http status code.
newPutAccountDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutAccountDetailsResponse
newPutAccountDetailsResponse pHttpStatus_ =
  PutAccountDetailsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putAccountDetailsResponse_httpStatus :: Lens.Lens' PutAccountDetailsResponse Prelude.Int
putAccountDetailsResponse_httpStatus = Lens.lens (\PutAccountDetailsResponse' {httpStatus} -> httpStatus) (\s@PutAccountDetailsResponse' {} a -> s {httpStatus = a} :: PutAccountDetailsResponse)

instance Prelude.NFData PutAccountDetailsResponse
