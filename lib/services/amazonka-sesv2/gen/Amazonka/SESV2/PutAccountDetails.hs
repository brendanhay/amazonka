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
-- Module      : Amazonka.SESV2.PutAccountDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update your Amazon SES account details.
module Amazonka.SESV2.PutAccountDetails
  ( -- * Creating a Request
    PutAccountDetails (..),
    newPutAccountDetails,

    -- * Request Lenses
    putAccountDetails_additionalContactEmailAddresses,
    putAccountDetails_contactLanguage,
    putAccountDetails_productionAccessEnabled,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to submit new account details.
--
-- /See:/ 'newPutAccountDetails' smart constructor.
data PutAccountDetails = PutAccountDetails'
  { -- | Additional email addresses that you would like to be notified regarding
    -- Amazon SES matters.
    additionalContactEmailAddresses :: Prelude.Maybe (Data.Sensitive (Prelude.NonEmpty (Data.Sensitive Prelude.Text))),
    -- | The language you would prefer to be contacted with.
    contactLanguage :: Prelude.Maybe ContactLanguage,
    -- | Indicates whether or not your account should have production access in
    -- the current Amazon Web Services Region.
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
    -- | The type of email your account will send.
    mailType :: MailType,
    -- | The URL of your website. This information helps us better understand the
    -- type of content that you plan to send.
    websiteURL :: Data.Sensitive Prelude.Text,
    -- | A description of the types of email that you plan to send.
    useCaseDescription :: Data.Sensitive Prelude.Text
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
-- 'additionalContactEmailAddresses', 'putAccountDetails_additionalContactEmailAddresses' - Additional email addresses that you would like to be notified regarding
-- Amazon SES matters.
--
-- 'contactLanguage', 'putAccountDetails_contactLanguage' - The language you would prefer to be contacted with.
--
-- 'productionAccessEnabled', 'putAccountDetails_productionAccessEnabled' - Indicates whether or not your account should have production access in
-- the current Amazon Web Services Region.
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
      { additionalContactEmailAddresses =
          Prelude.Nothing,
        contactLanguage = Prelude.Nothing,
        productionAccessEnabled = Prelude.Nothing,
        mailType = pMailType_,
        websiteURL = Data._Sensitive Lens.# pWebsiteURL_,
        useCaseDescription =
          Data._Sensitive Lens.# pUseCaseDescription_
      }

-- | Additional email addresses that you would like to be notified regarding
-- Amazon SES matters.
putAccountDetails_additionalContactEmailAddresses :: Lens.Lens' PutAccountDetails (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
putAccountDetails_additionalContactEmailAddresses = Lens.lens (\PutAccountDetails' {additionalContactEmailAddresses} -> additionalContactEmailAddresses) (\s@PutAccountDetails' {} a -> s {additionalContactEmailAddresses = a} :: PutAccountDetails) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | The language you would prefer to be contacted with.
putAccountDetails_contactLanguage :: Lens.Lens' PutAccountDetails (Prelude.Maybe ContactLanguage)
putAccountDetails_contactLanguage = Lens.lens (\PutAccountDetails' {contactLanguage} -> contactLanguage) (\s@PutAccountDetails' {} a -> s {contactLanguage = a} :: PutAccountDetails)

-- | Indicates whether or not your account should have production access in
-- the current Amazon Web Services Region.
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

-- | The type of email your account will send.
putAccountDetails_mailType :: Lens.Lens' PutAccountDetails MailType
putAccountDetails_mailType = Lens.lens (\PutAccountDetails' {mailType} -> mailType) (\s@PutAccountDetails' {} a -> s {mailType = a} :: PutAccountDetails)

-- | The URL of your website. This information helps us better understand the
-- type of content that you plan to send.
putAccountDetails_websiteURL :: Lens.Lens' PutAccountDetails Prelude.Text
putAccountDetails_websiteURL = Lens.lens (\PutAccountDetails' {websiteURL} -> websiteURL) (\s@PutAccountDetails' {} a -> s {websiteURL = a} :: PutAccountDetails) Prelude.. Data._Sensitive

-- | A description of the types of email that you plan to send.
putAccountDetails_useCaseDescription :: Lens.Lens' PutAccountDetails Prelude.Text
putAccountDetails_useCaseDescription = Lens.lens (\PutAccountDetails' {useCaseDescription} -> useCaseDescription) (\s@PutAccountDetails' {} a -> s {useCaseDescription = a} :: PutAccountDetails) Prelude.. Data._Sensitive

instance Core.AWSRequest PutAccountDetails where
  type
    AWSResponse PutAccountDetails =
      PutAccountDetailsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutAccountDetailsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutAccountDetails where
  hashWithSalt _salt PutAccountDetails' {..} =
    _salt
      `Prelude.hashWithSalt` additionalContactEmailAddresses
      `Prelude.hashWithSalt` contactLanguage
      `Prelude.hashWithSalt` productionAccessEnabled
      `Prelude.hashWithSalt` mailType
      `Prelude.hashWithSalt` websiteURL
      `Prelude.hashWithSalt` useCaseDescription

instance Prelude.NFData PutAccountDetails where
  rnf PutAccountDetails' {..} =
    Prelude.rnf additionalContactEmailAddresses
      `Prelude.seq` Prelude.rnf contactLanguage
      `Prelude.seq` Prelude.rnf productionAccessEnabled
      `Prelude.seq` Prelude.rnf mailType
      `Prelude.seq` Prelude.rnf websiteURL
      `Prelude.seq` Prelude.rnf useCaseDescription

instance Data.ToHeaders PutAccountDetails where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutAccountDetails where
  toJSON PutAccountDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AdditionalContactEmailAddresses" Data..=)
              Prelude.<$> additionalContactEmailAddresses,
            ("ContactLanguage" Data..=)
              Prelude.<$> contactLanguage,
            ("ProductionAccessEnabled" Data..=)
              Prelude.<$> productionAccessEnabled,
            Prelude.Just ("MailType" Data..= mailType),
            Prelude.Just ("WebsiteURL" Data..= websiteURL),
            Prelude.Just
              ("UseCaseDescription" Data..= useCaseDescription)
          ]
      )

instance Data.ToPath PutAccountDetails where
  toPath = Prelude.const "/v2/email/account/details"

instance Data.ToQuery PutAccountDetails where
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

instance Prelude.NFData PutAccountDetailsResponse where
  rnf PutAccountDetailsResponse' {..} =
    Prelude.rnf httpStatus
