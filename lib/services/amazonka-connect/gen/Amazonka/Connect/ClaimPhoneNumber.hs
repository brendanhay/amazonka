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
-- Module      : Amazonka.Connect.ClaimPhoneNumber
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Claims an available phone number to your Amazon Connect instance or
-- traffic distribution group. You can call this API only in the same
-- Amazon Web Services Region where the Amazon Connect instance or traffic
-- distribution group was created.
--
-- You can call the
-- <https://docs.aws.amazon.com/connect/latest/APIReference/API_DescribePhoneNumber.html DescribePhoneNumber>
-- API to verify the status of a previous
-- <https://docs.aws.amazon.com/connect/latest/APIReference/API_ClaimPhoneNumber.html ClaimPhoneNumber>
-- operation.
module Amazonka.Connect.ClaimPhoneNumber
  ( -- * Creating a Request
    ClaimPhoneNumber (..),
    newClaimPhoneNumber,

    -- * Request Lenses
    claimPhoneNumber_tags,
    claimPhoneNumber_clientToken,
    claimPhoneNumber_phoneNumberDescription,
    claimPhoneNumber_targetArn,
    claimPhoneNumber_phoneNumber,

    -- * Destructuring the Response
    ClaimPhoneNumberResponse (..),
    newClaimPhoneNumberResponse,

    -- * Response Lenses
    claimPhoneNumberResponse_phoneNumberArn,
    claimPhoneNumberResponse_phoneNumberId,
    claimPhoneNumberResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newClaimPhoneNumber' smart constructor.
data ClaimPhoneNumber = ClaimPhoneNumber'
  { -- | The tags used to organize, track, or control access for this resource.
    -- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    --
    -- Pattern:
    -- @^[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$@
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The description of the phone number.
    phoneNumberDescription :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for Amazon Connect instances or traffic
    -- distribution groups that phone numbers are claimed to.
    targetArn :: Prelude.Text,
    -- | The phone number you want to claim. Phone numbers are formatted
    -- @[+] [country code] [subscriber number including area code]@.
    phoneNumber :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClaimPhoneNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'claimPhoneNumber_tags' - The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
--
-- 'clientToken', 'claimPhoneNumber_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
--
-- Pattern:
-- @^[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$@
--
-- 'phoneNumberDescription', 'claimPhoneNumber_phoneNumberDescription' - The description of the phone number.
--
-- 'targetArn', 'claimPhoneNumber_targetArn' - The Amazon Resource Name (ARN) for Amazon Connect instances or traffic
-- distribution groups that phone numbers are claimed to.
--
-- 'phoneNumber', 'claimPhoneNumber_phoneNumber' - The phone number you want to claim. Phone numbers are formatted
-- @[+] [country code] [subscriber number including area code]@.
newClaimPhoneNumber ::
  -- | 'targetArn'
  Prelude.Text ->
  -- | 'phoneNumber'
  Prelude.Text ->
  ClaimPhoneNumber
newClaimPhoneNumber pTargetArn_ pPhoneNumber_ =
  ClaimPhoneNumber'
    { tags = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      phoneNumberDescription = Prelude.Nothing,
      targetArn = pTargetArn_,
      phoneNumber = pPhoneNumber_
    }

-- | The tags used to organize, track, or control access for this resource.
-- For example, { \"tags\": {\"key1\":\"value1\", \"key2\":\"value2\"} }.
claimPhoneNumber_tags :: Lens.Lens' ClaimPhoneNumber (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
claimPhoneNumber_tags = Lens.lens (\ClaimPhoneNumber' {tags} -> tags) (\s@ClaimPhoneNumber' {} a -> s {tags = a} :: ClaimPhoneNumber) Prelude.. Lens.mapping Lens.coerced

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
--
-- Pattern:
-- @^[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$@
claimPhoneNumber_clientToken :: Lens.Lens' ClaimPhoneNumber (Prelude.Maybe Prelude.Text)
claimPhoneNumber_clientToken = Lens.lens (\ClaimPhoneNumber' {clientToken} -> clientToken) (\s@ClaimPhoneNumber' {} a -> s {clientToken = a} :: ClaimPhoneNumber)

-- | The description of the phone number.
claimPhoneNumber_phoneNumberDescription :: Lens.Lens' ClaimPhoneNumber (Prelude.Maybe Prelude.Text)
claimPhoneNumber_phoneNumberDescription = Lens.lens (\ClaimPhoneNumber' {phoneNumberDescription} -> phoneNumberDescription) (\s@ClaimPhoneNumber' {} a -> s {phoneNumberDescription = a} :: ClaimPhoneNumber)

-- | The Amazon Resource Name (ARN) for Amazon Connect instances or traffic
-- distribution groups that phone numbers are claimed to.
claimPhoneNumber_targetArn :: Lens.Lens' ClaimPhoneNumber Prelude.Text
claimPhoneNumber_targetArn = Lens.lens (\ClaimPhoneNumber' {targetArn} -> targetArn) (\s@ClaimPhoneNumber' {} a -> s {targetArn = a} :: ClaimPhoneNumber)

-- | The phone number you want to claim. Phone numbers are formatted
-- @[+] [country code] [subscriber number including area code]@.
claimPhoneNumber_phoneNumber :: Lens.Lens' ClaimPhoneNumber Prelude.Text
claimPhoneNumber_phoneNumber = Lens.lens (\ClaimPhoneNumber' {phoneNumber} -> phoneNumber) (\s@ClaimPhoneNumber' {} a -> s {phoneNumber = a} :: ClaimPhoneNumber)

instance Core.AWSRequest ClaimPhoneNumber where
  type
    AWSResponse ClaimPhoneNumber =
      ClaimPhoneNumberResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ClaimPhoneNumberResponse'
            Prelude.<$> (x Core..?> "PhoneNumberArn")
            Prelude.<*> (x Core..?> "PhoneNumberId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ClaimPhoneNumber where
  hashWithSalt _salt ClaimPhoneNumber' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` phoneNumberDescription
      `Prelude.hashWithSalt` targetArn
      `Prelude.hashWithSalt` phoneNumber

instance Prelude.NFData ClaimPhoneNumber where
  rnf ClaimPhoneNumber' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf phoneNumberDescription
      `Prelude.seq` Prelude.rnf targetArn
      `Prelude.seq` Prelude.rnf phoneNumber

instance Core.ToHeaders ClaimPhoneNumber where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ClaimPhoneNumber where
  toJSON ClaimPhoneNumber' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("ClientToken" Core..=) Prelude.<$> clientToken,
            ("PhoneNumberDescription" Core..=)
              Prelude.<$> phoneNumberDescription,
            Prelude.Just ("TargetArn" Core..= targetArn),
            Prelude.Just ("PhoneNumber" Core..= phoneNumber)
          ]
      )

instance Core.ToPath ClaimPhoneNumber where
  toPath = Prelude.const "/phone-number/claim"

instance Core.ToQuery ClaimPhoneNumber where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newClaimPhoneNumberResponse' smart constructor.
data ClaimPhoneNumberResponse = ClaimPhoneNumberResponse'
  { -- | The Amazon Resource Name (ARN) of the phone number.
    phoneNumberArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the phone number.
    phoneNumberId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClaimPhoneNumberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'phoneNumberArn', 'claimPhoneNumberResponse_phoneNumberArn' - The Amazon Resource Name (ARN) of the phone number.
--
-- 'phoneNumberId', 'claimPhoneNumberResponse_phoneNumberId' - A unique identifier for the phone number.
--
-- 'httpStatus', 'claimPhoneNumberResponse_httpStatus' - The response's http status code.
newClaimPhoneNumberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ClaimPhoneNumberResponse
newClaimPhoneNumberResponse pHttpStatus_ =
  ClaimPhoneNumberResponse'
    { phoneNumberArn =
        Prelude.Nothing,
      phoneNumberId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the phone number.
claimPhoneNumberResponse_phoneNumberArn :: Lens.Lens' ClaimPhoneNumberResponse (Prelude.Maybe Prelude.Text)
claimPhoneNumberResponse_phoneNumberArn = Lens.lens (\ClaimPhoneNumberResponse' {phoneNumberArn} -> phoneNumberArn) (\s@ClaimPhoneNumberResponse' {} a -> s {phoneNumberArn = a} :: ClaimPhoneNumberResponse)

-- | A unique identifier for the phone number.
claimPhoneNumberResponse_phoneNumberId :: Lens.Lens' ClaimPhoneNumberResponse (Prelude.Maybe Prelude.Text)
claimPhoneNumberResponse_phoneNumberId = Lens.lens (\ClaimPhoneNumberResponse' {phoneNumberId} -> phoneNumberId) (\s@ClaimPhoneNumberResponse' {} a -> s {phoneNumberId = a} :: ClaimPhoneNumberResponse)

-- | The response's http status code.
claimPhoneNumberResponse_httpStatus :: Lens.Lens' ClaimPhoneNumberResponse Prelude.Int
claimPhoneNumberResponse_httpStatus = Lens.lens (\ClaimPhoneNumberResponse' {httpStatus} -> httpStatus) (\s@ClaimPhoneNumberResponse' {} a -> s {httpStatus = a} :: ClaimPhoneNumberResponse)

instance Prelude.NFData ClaimPhoneNumberResponse where
  rnf ClaimPhoneNumberResponse' {..} =
    Prelude.rnf phoneNumberArn
      `Prelude.seq` Prelude.rnf phoneNumberId
      `Prelude.seq` Prelude.rnf httpStatus
