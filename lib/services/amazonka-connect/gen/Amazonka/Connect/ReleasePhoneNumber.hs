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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Releases a phone number previously claimed to an Amazon Connect
-- instance.
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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newReleasePhoneNumber' smart constructor.
data ReleasePhoneNumber = ReleasePhoneNumber'
  { -- | A unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
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
-- idempotency of the request.
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
-- idempotency of the request.
releasePhoneNumber_clientToken :: Lens.Lens' ReleasePhoneNumber (Prelude.Maybe Prelude.Text)
releasePhoneNumber_clientToken = Lens.lens (\ReleasePhoneNumber' {clientToken} -> clientToken) (\s@ReleasePhoneNumber' {} a -> s {clientToken = a} :: ReleasePhoneNumber)

-- | A unique identifier for the phone number.
releasePhoneNumber_phoneNumberId :: Lens.Lens' ReleasePhoneNumber Prelude.Text
releasePhoneNumber_phoneNumberId = Lens.lens (\ReleasePhoneNumber' {phoneNumberId} -> phoneNumberId) (\s@ReleasePhoneNumber' {} a -> s {phoneNumberId = a} :: ReleasePhoneNumber)

instance Core.AWSRequest ReleasePhoneNumber where
  type
    AWSResponse ReleasePhoneNumber =
      ReleasePhoneNumberResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull ReleasePhoneNumberResponse'

instance Prelude.Hashable ReleasePhoneNumber where
  hashWithSalt _salt ReleasePhoneNumber' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` phoneNumberId

instance Prelude.NFData ReleasePhoneNumber where
  rnf ReleasePhoneNumber' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf phoneNumberId

instance Core.ToHeaders ReleasePhoneNumber where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ReleasePhoneNumber where
  toPath ReleasePhoneNumber' {..} =
    Prelude.mconcat
      ["/phone-number/", Core.toBS phoneNumberId]

instance Core.ToQuery ReleasePhoneNumber where
  toQuery ReleasePhoneNumber' {..} =
    Prelude.mconcat ["clientToken" Core.=: clientToken]

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
