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
-- Module      : Amazonka.Connect.ReleasePhoneNumber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Releases a phone number previously claimed to an Amazon Connect instance
-- or traffic distribution group. You can call this API only in the Amazon
-- Web Services Region where the number was claimed.
--
-- To release phone numbers from a traffic distribution group, use the
-- @ReleasePhoneNumber@ API, not the Amazon Connect console.
--
-- After releasing a phone number, the phone number enters into a cooldown
-- period of 30 days. It cannot be searched for or claimed again until the
-- period has ended. If you accidentally release a phone number, contact
-- Amazon Web Services Support.
module Amazonka.Connect.ReleasePhoneNumber
  ( -- * Creating a Request
    ReleasePhoneNumber (..),
    newReleasePhoneNumber,

    -- * Request Lenses
    releasePhoneNumber_clientToken,
    releasePhoneNumber_phoneNumberId,

    -- * Destructuring the Response
    ReleasePhoneNumberResponse (..),
    newReleasePhoneNumberResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newReleasePhoneNumber' smart constructor.
data ReleasePhoneNumber = ReleasePhoneNumber'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. If not provided, the Amazon Web Services SDK
    -- populates this field. For more information about idempotency, see
    -- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the phone number.
    phoneNumberId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReleasePhoneNumber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'releasePhoneNumber_clientToken' - A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
--
-- 'phoneNumberId', 'releasePhoneNumber_phoneNumberId' - A unique identifier for the phone number.
newReleasePhoneNumber ::
  -- | 'phoneNumberId'
  Prelude.Text ->
  ReleasePhoneNumber
newReleasePhoneNumber pPhoneNumberId_ =
  ReleasePhoneNumber'
    { clientToken = Prelude.Nothing,
      phoneNumberId = pPhoneNumberId_
    }

-- | A unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. If not provided, the Amazon Web Services SDK
-- populates this field. For more information about idempotency, see
-- <https://aws.amazon.com/builders-library/making-retries-safe-with-idempotent-APIs/ Making retries safe with idempotent APIs>.
releasePhoneNumber_clientToken :: Lens.Lens' ReleasePhoneNumber (Prelude.Maybe Prelude.Text)
releasePhoneNumber_clientToken = Lens.lens (\ReleasePhoneNumber' {clientToken} -> clientToken) (\s@ReleasePhoneNumber' {} a -> s {clientToken = a} :: ReleasePhoneNumber)

-- | A unique identifier for the phone number.
releasePhoneNumber_phoneNumberId :: Lens.Lens' ReleasePhoneNumber Prelude.Text
releasePhoneNumber_phoneNumberId = Lens.lens (\ReleasePhoneNumber' {phoneNumberId} -> phoneNumberId) (\s@ReleasePhoneNumber' {} a -> s {phoneNumberId = a} :: ReleasePhoneNumber)

instance Core.AWSRequest ReleasePhoneNumber where
  type
    AWSResponse ReleasePhoneNumber =
      ReleasePhoneNumberResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull ReleasePhoneNumberResponse'

instance Prelude.Hashable ReleasePhoneNumber where
  hashWithSalt _salt ReleasePhoneNumber' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` phoneNumberId

instance Prelude.NFData ReleasePhoneNumber where
  rnf ReleasePhoneNumber' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf phoneNumberId

instance Data.ToHeaders ReleasePhoneNumber where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ReleasePhoneNumber where
  toPath ReleasePhoneNumber' {..} =
    Prelude.mconcat
      ["/phone-number/", Data.toBS phoneNumberId]

instance Data.ToQuery ReleasePhoneNumber where
  toQuery ReleasePhoneNumber' {..} =
    Prelude.mconcat ["clientToken" Data.=: clientToken]

-- | /See:/ 'newReleasePhoneNumberResponse' smart constructor.
data ReleasePhoneNumberResponse = ReleasePhoneNumberResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReleasePhoneNumberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newReleasePhoneNumberResponse ::
  ReleasePhoneNumberResponse
newReleasePhoneNumberResponse =
  ReleasePhoneNumberResponse'

instance Prelude.NFData ReleasePhoneNumberResponse where
  rnf _ = ()
