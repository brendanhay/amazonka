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
-- Module      : Amazonka.SESV2.GetAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtain information about the email-sending status and capabilities of
-- your Amazon SES account in the current Amazon Web Services Region.
module Amazonka.SESV2.GetAccount
  ( -- * Creating a Request
    GetAccount (..),
    newGetAccount,

    -- * Destructuring the Response
    GetAccountResponse (..),
    newGetAccountResponse,

    -- * Response Lenses
    getAccountResponse_dedicatedIpAutoWarmupEnabled,
    getAccountResponse_details,
    getAccountResponse_enforcementStatus,
    getAccountResponse_productionAccessEnabled,
    getAccountResponse_sendQuota,
    getAccountResponse_sendingEnabled,
    getAccountResponse_suppressionAttributes,
    getAccountResponse_vdmAttributes,
    getAccountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to obtain information about the email-sending capabilities of
-- your Amazon SES account.
--
-- /See:/ 'newGetAccount' smart constructor.
data GetAccount = GetAccount'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetAccount ::
  GetAccount
newGetAccount = GetAccount'

instance Core.AWSRequest GetAccount where
  type AWSResponse GetAccount = GetAccountResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAccountResponse'
            Prelude.<$> (x Data..?> "DedicatedIpAutoWarmupEnabled")
            Prelude.<*> (x Data..?> "Details")
            Prelude.<*> (x Data..?> "EnforcementStatus")
            Prelude.<*> (x Data..?> "ProductionAccessEnabled")
            Prelude.<*> (x Data..?> "SendQuota")
            Prelude.<*> (x Data..?> "SendingEnabled")
            Prelude.<*> (x Data..?> "SuppressionAttributes")
            Prelude.<*> (x Data..?> "VdmAttributes")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAccount where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetAccount where
  rnf _ = ()

instance Data.ToHeaders GetAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetAccount where
  toPath = Prelude.const "/v2/email/account"

instance Data.ToQuery GetAccount where
  toQuery = Prelude.const Prelude.mempty

-- | A list of details about the email-sending capabilities of your Amazon
-- SES account in the current Amazon Web Services Region.
--
-- /See:/ 'newGetAccountResponse' smart constructor.
data GetAccountResponse = GetAccountResponse'
  { -- | Indicates whether or not the automatic warm-up feature is enabled for
    -- dedicated IP addresses that are associated with your account.
    dedicatedIpAutoWarmupEnabled :: Prelude.Maybe Prelude.Bool,
    -- | An object that defines your account details.
    details :: Prelude.Maybe AccountDetails,
    -- | The reputation status of your Amazon SES account. The status can be one
    -- of the following:
    --
    -- -   @HEALTHY@ – There are no reputation-related issues that currently
    --     impact your account.
    --
    -- -   @PROBATION@ – We\'ve identified potential issues with your Amazon
    --     SES account. We\'re placing your account under review while you work
    --     on correcting these issues.
    --
    -- -   @SHUTDOWN@ – Your account\'s ability to send email is currently
    --     paused because of an issue with the email sent from your account.
    --     When you correct the issue, you can contact us and request that your
    --     account\'s ability to send email is resumed.
    enforcementStatus :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether or not your account has production access in the
    -- current Amazon Web Services Region.
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
    -- | An object that contains information about the per-day and per-second
    -- sending limits for your Amazon SES account in the current Amazon Web
    -- Services Region.
    sendQuota :: Prelude.Maybe SendQuota,
    -- | Indicates whether or not email sending is enabled for your Amazon SES
    -- account in the current Amazon Web Services Region.
    sendingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | An object that contains information about the email address suppression
    -- preferences for your account in the current Amazon Web Services Region.
    suppressionAttributes :: Prelude.Maybe SuppressionAttributes,
    -- | The VDM attributes that apply to your Amazon SES account.
    vdmAttributes :: Prelude.Maybe VdmAttributes,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dedicatedIpAutoWarmupEnabled', 'getAccountResponse_dedicatedIpAutoWarmupEnabled' - Indicates whether or not the automatic warm-up feature is enabled for
-- dedicated IP addresses that are associated with your account.
--
-- 'details', 'getAccountResponse_details' - An object that defines your account details.
--
-- 'enforcementStatus', 'getAccountResponse_enforcementStatus' - The reputation status of your Amazon SES account. The status can be one
-- of the following:
--
-- -   @HEALTHY@ – There are no reputation-related issues that currently
--     impact your account.
--
-- -   @PROBATION@ – We\'ve identified potential issues with your Amazon
--     SES account. We\'re placing your account under review while you work
--     on correcting these issues.
--
-- -   @SHUTDOWN@ – Your account\'s ability to send email is currently
--     paused because of an issue with the email sent from your account.
--     When you correct the issue, you can contact us and request that your
--     account\'s ability to send email is resumed.
--
-- 'productionAccessEnabled', 'getAccountResponse_productionAccessEnabled' - Indicates whether or not your account has production access in the
-- current Amazon Web Services Region.
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
-- 'sendQuota', 'getAccountResponse_sendQuota' - An object that contains information about the per-day and per-second
-- sending limits for your Amazon SES account in the current Amazon Web
-- Services Region.
--
-- 'sendingEnabled', 'getAccountResponse_sendingEnabled' - Indicates whether or not email sending is enabled for your Amazon SES
-- account in the current Amazon Web Services Region.
--
-- 'suppressionAttributes', 'getAccountResponse_suppressionAttributes' - An object that contains information about the email address suppression
-- preferences for your account in the current Amazon Web Services Region.
--
-- 'vdmAttributes', 'getAccountResponse_vdmAttributes' - The VDM attributes that apply to your Amazon SES account.
--
-- 'httpStatus', 'getAccountResponse_httpStatus' - The response's http status code.
newGetAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAccountResponse
newGetAccountResponse pHttpStatus_ =
  GetAccountResponse'
    { dedicatedIpAutoWarmupEnabled =
        Prelude.Nothing,
      details = Prelude.Nothing,
      enforcementStatus = Prelude.Nothing,
      productionAccessEnabled = Prelude.Nothing,
      sendQuota = Prelude.Nothing,
      sendingEnabled = Prelude.Nothing,
      suppressionAttributes = Prelude.Nothing,
      vdmAttributes = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether or not the automatic warm-up feature is enabled for
-- dedicated IP addresses that are associated with your account.
getAccountResponse_dedicatedIpAutoWarmupEnabled :: Lens.Lens' GetAccountResponse (Prelude.Maybe Prelude.Bool)
getAccountResponse_dedicatedIpAutoWarmupEnabled = Lens.lens (\GetAccountResponse' {dedicatedIpAutoWarmupEnabled} -> dedicatedIpAutoWarmupEnabled) (\s@GetAccountResponse' {} a -> s {dedicatedIpAutoWarmupEnabled = a} :: GetAccountResponse)

-- | An object that defines your account details.
getAccountResponse_details :: Lens.Lens' GetAccountResponse (Prelude.Maybe AccountDetails)
getAccountResponse_details = Lens.lens (\GetAccountResponse' {details} -> details) (\s@GetAccountResponse' {} a -> s {details = a} :: GetAccountResponse)

-- | The reputation status of your Amazon SES account. The status can be one
-- of the following:
--
-- -   @HEALTHY@ – There are no reputation-related issues that currently
--     impact your account.
--
-- -   @PROBATION@ – We\'ve identified potential issues with your Amazon
--     SES account. We\'re placing your account under review while you work
--     on correcting these issues.
--
-- -   @SHUTDOWN@ – Your account\'s ability to send email is currently
--     paused because of an issue with the email sent from your account.
--     When you correct the issue, you can contact us and request that your
--     account\'s ability to send email is resumed.
getAccountResponse_enforcementStatus :: Lens.Lens' GetAccountResponse (Prelude.Maybe Prelude.Text)
getAccountResponse_enforcementStatus = Lens.lens (\GetAccountResponse' {enforcementStatus} -> enforcementStatus) (\s@GetAccountResponse' {} a -> s {enforcementStatus = a} :: GetAccountResponse)

-- | Indicates whether or not your account has production access in the
-- current Amazon Web Services Region.
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
getAccountResponse_productionAccessEnabled :: Lens.Lens' GetAccountResponse (Prelude.Maybe Prelude.Bool)
getAccountResponse_productionAccessEnabled = Lens.lens (\GetAccountResponse' {productionAccessEnabled} -> productionAccessEnabled) (\s@GetAccountResponse' {} a -> s {productionAccessEnabled = a} :: GetAccountResponse)

-- | An object that contains information about the per-day and per-second
-- sending limits for your Amazon SES account in the current Amazon Web
-- Services Region.
getAccountResponse_sendQuota :: Lens.Lens' GetAccountResponse (Prelude.Maybe SendQuota)
getAccountResponse_sendQuota = Lens.lens (\GetAccountResponse' {sendQuota} -> sendQuota) (\s@GetAccountResponse' {} a -> s {sendQuota = a} :: GetAccountResponse)

-- | Indicates whether or not email sending is enabled for your Amazon SES
-- account in the current Amazon Web Services Region.
getAccountResponse_sendingEnabled :: Lens.Lens' GetAccountResponse (Prelude.Maybe Prelude.Bool)
getAccountResponse_sendingEnabled = Lens.lens (\GetAccountResponse' {sendingEnabled} -> sendingEnabled) (\s@GetAccountResponse' {} a -> s {sendingEnabled = a} :: GetAccountResponse)

-- | An object that contains information about the email address suppression
-- preferences for your account in the current Amazon Web Services Region.
getAccountResponse_suppressionAttributes :: Lens.Lens' GetAccountResponse (Prelude.Maybe SuppressionAttributes)
getAccountResponse_suppressionAttributes = Lens.lens (\GetAccountResponse' {suppressionAttributes} -> suppressionAttributes) (\s@GetAccountResponse' {} a -> s {suppressionAttributes = a} :: GetAccountResponse)

-- | The VDM attributes that apply to your Amazon SES account.
getAccountResponse_vdmAttributes :: Lens.Lens' GetAccountResponse (Prelude.Maybe VdmAttributes)
getAccountResponse_vdmAttributes = Lens.lens (\GetAccountResponse' {vdmAttributes} -> vdmAttributes) (\s@GetAccountResponse' {} a -> s {vdmAttributes = a} :: GetAccountResponse)

-- | The response's http status code.
getAccountResponse_httpStatus :: Lens.Lens' GetAccountResponse Prelude.Int
getAccountResponse_httpStatus = Lens.lens (\GetAccountResponse' {httpStatus} -> httpStatus) (\s@GetAccountResponse' {} a -> s {httpStatus = a} :: GetAccountResponse)

instance Prelude.NFData GetAccountResponse where
  rnf GetAccountResponse' {..} =
    Prelude.rnf dedicatedIpAutoWarmupEnabled `Prelude.seq`
      Prelude.rnf details `Prelude.seq`
        Prelude.rnf enforcementStatus `Prelude.seq`
          Prelude.rnf productionAccessEnabled `Prelude.seq`
            Prelude.rnf sendQuota `Prelude.seq`
              Prelude.rnf sendingEnabled `Prelude.seq`
                Prelude.rnf suppressionAttributes `Prelude.seq`
                  Prelude.rnf vdmAttributes `Prelude.seq`
                    Prelude.rnf httpStatus
