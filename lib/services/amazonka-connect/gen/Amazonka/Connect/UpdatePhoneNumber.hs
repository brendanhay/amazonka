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
-- Module      : Amazonka.Connect.UpdatePhoneNumber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates your claimed phone number from its current Amazon Connect
-- instance or traffic distribution group to another Amazon Connect
-- instance or traffic distribution group in the same Amazon Web Services
-- Region.
--
-- You can call
-- <https://docs.aws.amazon.com/connect/latest/APIReference/API_DescribePhoneNumber.html DescribePhoneNumber>
-- API to verify the status of a previous
-- <https://docs.aws.amazon.com/connect/latest/APIReference/API_UpdatePhoneNumber.html UpdatePhoneNumber>
-- operation.
module Amazonka.Connect.UpdatePhoneNumber
  ( -- * Creating a Request
    UpdatePhoneNumber (..),
    newUpdatePhoneNumber,

    -- * Request Lenses
    updatePhoneNumber_clientToken,
    updatePhoneNumber_phoneNumberId,
    updatePhoneNumber_targetArn,

    -- * Destructuring the Response
    UpdatePhoneNumberResponse (..),
    newUpdatePhoneNumberResponse,

    -- * Response Lenses
    updatePhoneNumberResponse_phoneNumberArn,
    updatePhoneNumberResponse_phoneNumberId,
    updatePhoneNumberResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePhoneNumber' smart constructor.
data UpdatePhoneNumber = UpdatePhoneNumber'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the phone number.
    phoneNumberId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for Amazon Connect instances or traffic
    -- distribution groups that phone numbers are claimed to.
    targetArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePhoneNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updatePhoneNumber_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
--
-- 'phoneNumberId', 'updatePhoneNumber_phoneNumberId' - A unique identifier for the phone number.
--
-- 'targetArn', 'updatePhoneNumber_targetArn' - The Amazon Resource Name (ARN) for Amazon Connect instances or traffic
-- distribution groups that phone numbers are claimed to.
newUpdatePhoneNumber ::
  -- | 'phoneNumberId'
  Prelude.Text ->
  -- | 'targetArn'
  Prelude.Text ->
  UpdatePhoneNumber
newUpdatePhoneNumber pPhoneNumberId_ pTargetArn_ =
  UpdatePhoneNumber'
    { clientToken = Prelude.Nothing,
      phoneNumberId = pPhoneNumberId_,
      targetArn = pTargetArn_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
updatePhoneNumber_clientToken :: Lens.Lens' UpdatePhoneNumber (Prelude.Maybe Prelude.Text)
updatePhoneNumber_clientToken = Lens.lens (\UpdatePhoneNumber' {clientToken} -> clientToken) (\s@UpdatePhoneNumber' {} a -> s {clientToken = a} :: UpdatePhoneNumber)

-- | A unique identifier for the phone number.
updatePhoneNumber_phoneNumberId :: Lens.Lens' UpdatePhoneNumber Prelude.Text
updatePhoneNumber_phoneNumberId = Lens.lens (\UpdatePhoneNumber' {phoneNumberId} -> phoneNumberId) (\s@UpdatePhoneNumber' {} a -> s {phoneNumberId = a} :: UpdatePhoneNumber)

-- | The Amazon Resource Name (ARN) for Amazon Connect instances or traffic
-- distribution groups that phone numbers are claimed to.
updatePhoneNumber_targetArn :: Lens.Lens' UpdatePhoneNumber Prelude.Text
updatePhoneNumber_targetArn = Lens.lens (\UpdatePhoneNumber' {targetArn} -> targetArn) (\s@UpdatePhoneNumber' {} a -> s {targetArn = a} :: UpdatePhoneNumber)

instance Core.AWSRequest UpdatePhoneNumber where
  type
    AWSResponse UpdatePhoneNumber =
      UpdatePhoneNumberResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePhoneNumberResponse'
            Prelude.<$> (x Data..?> "PhoneNumberArn")
            Prelude.<*> (x Data..?> "PhoneNumberId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePhoneNumber where
  hashWithSalt _salt UpdatePhoneNumber' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` phoneNumberId
      `Prelude.hashWithSalt` targetArn

instance Prelude.NFData UpdatePhoneNumber where
  rnf UpdatePhoneNumber' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf phoneNumberId
      `Prelude.seq` Prelude.rnf targetArn

instance Data.ToHeaders UpdatePhoneNumber where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePhoneNumber where
  toJSON UpdatePhoneNumber' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("TargetArn" Data..= targetArn)
          ]
      )

instance Data.ToPath UpdatePhoneNumber where
  toPath UpdatePhoneNumber' {..} =
    Prelude.mconcat
      ["/phone-number/", Data.toBS phoneNumberId]

instance Data.ToQuery UpdatePhoneNumber where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePhoneNumberResponse' smart constructor.
data UpdatePhoneNumberResponse = UpdatePhoneNumberResponse'
  { -- | The Amazon Resource Name (ARN) of the phone number.
    phoneNumberArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the phone number.
    phoneNumberId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePhoneNumberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberArn', 'updatePhoneNumberResponse_phoneNumberArn' - The Amazon Resource Name (ARN) of the phone number.
--
-- 'phoneNumberId', 'updatePhoneNumberResponse_phoneNumberId' - A unique identifier for the phone number.
--
-- 'httpStatus', 'updatePhoneNumberResponse_httpStatus' - The response's http status code.
newUpdatePhoneNumberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePhoneNumberResponse
newUpdatePhoneNumberResponse pHttpStatus_ =
  UpdatePhoneNumberResponse'
    { phoneNumberArn =
        Prelude.Nothing,
      phoneNumberId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the phone number.
updatePhoneNumberResponse_phoneNumberArn :: Lens.Lens' UpdatePhoneNumberResponse (Prelude.Maybe Prelude.Text)
updatePhoneNumberResponse_phoneNumberArn = Lens.lens (\UpdatePhoneNumberResponse' {phoneNumberArn} -> phoneNumberArn) (\s@UpdatePhoneNumberResponse' {} a -> s {phoneNumberArn = a} :: UpdatePhoneNumberResponse)

-- | A unique identifier for the phone number.
updatePhoneNumberResponse_phoneNumberId :: Lens.Lens' UpdatePhoneNumberResponse (Prelude.Maybe Prelude.Text)
updatePhoneNumberResponse_phoneNumberId = Lens.lens (\UpdatePhoneNumberResponse' {phoneNumberId} -> phoneNumberId) (\s@UpdatePhoneNumberResponse' {} a -> s {phoneNumberId = a} :: UpdatePhoneNumberResponse)

-- | The response's http status code.
updatePhoneNumberResponse_httpStatus :: Lens.Lens' UpdatePhoneNumberResponse Prelude.Int
updatePhoneNumberResponse_httpStatus = Lens.lens (\UpdatePhoneNumberResponse' {httpStatus} -> httpStatus) (\s@UpdatePhoneNumberResponse' {} a -> s {httpStatus = a} :: UpdatePhoneNumberResponse)

instance Prelude.NFData UpdatePhoneNumberResponse where
  rnf UpdatePhoneNumberResponse' {..} =
    Prelude.rnf phoneNumberArn
      `Prelude.seq` Prelude.rnf phoneNumberId
      `Prelude.seq` Prelude.rnf httpStatus
