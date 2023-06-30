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
-- Module      : Amazonka.IoTWireless.GetPartnerAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a partner account. If @PartnerAccountId@ and
-- @PartnerType@ are @null@, returns all partner accounts.
module Amazonka.IoTWireless.GetPartnerAccount
  ( -- * Creating a Request
    GetPartnerAccount (..),
    newGetPartnerAccount,

    -- * Request Lenses
    getPartnerAccount_partnerAccountId,
    getPartnerAccount_partnerType,

    -- * Destructuring the Response
    GetPartnerAccountResponse (..),
    newGetPartnerAccountResponse,

    -- * Response Lenses
    getPartnerAccountResponse_accountLinked,
    getPartnerAccountResponse_sidewalk,
    getPartnerAccountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPartnerAccount' smart constructor.
data GetPartnerAccount = GetPartnerAccount'
  { -- | The partner account ID to disassociate from the AWS account.
    partnerAccountId :: Prelude.Text,
    -- | The partner type.
    partnerType :: PartnerType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPartnerAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partnerAccountId', 'getPartnerAccount_partnerAccountId' - The partner account ID to disassociate from the AWS account.
--
-- 'partnerType', 'getPartnerAccount_partnerType' - The partner type.
newGetPartnerAccount ::
  -- | 'partnerAccountId'
  Prelude.Text ->
  -- | 'partnerType'
  PartnerType ->
  GetPartnerAccount
newGetPartnerAccount pPartnerAccountId_ pPartnerType_ =
  GetPartnerAccount'
    { partnerAccountId =
        pPartnerAccountId_,
      partnerType = pPartnerType_
    }

-- | The partner account ID to disassociate from the AWS account.
getPartnerAccount_partnerAccountId :: Lens.Lens' GetPartnerAccount Prelude.Text
getPartnerAccount_partnerAccountId = Lens.lens (\GetPartnerAccount' {partnerAccountId} -> partnerAccountId) (\s@GetPartnerAccount' {} a -> s {partnerAccountId = a} :: GetPartnerAccount)

-- | The partner type.
getPartnerAccount_partnerType :: Lens.Lens' GetPartnerAccount PartnerType
getPartnerAccount_partnerType = Lens.lens (\GetPartnerAccount' {partnerType} -> partnerType) (\s@GetPartnerAccount' {} a -> s {partnerType = a} :: GetPartnerAccount)

instance Core.AWSRequest GetPartnerAccount where
  type
    AWSResponse GetPartnerAccount =
      GetPartnerAccountResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPartnerAccountResponse'
            Prelude.<$> (x Data..?> "AccountLinked")
            Prelude.<*> (x Data..?> "Sidewalk")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPartnerAccount where
  hashWithSalt _salt GetPartnerAccount' {..} =
    _salt
      `Prelude.hashWithSalt` partnerAccountId
      `Prelude.hashWithSalt` partnerType

instance Prelude.NFData GetPartnerAccount where
  rnf GetPartnerAccount' {..} =
    Prelude.rnf partnerAccountId
      `Prelude.seq` Prelude.rnf partnerType

instance Data.ToHeaders GetPartnerAccount where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetPartnerAccount where
  toPath GetPartnerAccount' {..} =
    Prelude.mconcat
      ["/partner-accounts/", Data.toBS partnerAccountId]

instance Data.ToQuery GetPartnerAccount where
  toQuery GetPartnerAccount' {..} =
    Prelude.mconcat ["partnerType" Data.=: partnerType]

-- | /See:/ 'newGetPartnerAccountResponse' smart constructor.
data GetPartnerAccountResponse = GetPartnerAccountResponse'
  { -- | Whether the partner account is linked to the AWS account.
    accountLinked :: Prelude.Maybe Prelude.Bool,
    -- | The Sidewalk account credentials.
    sidewalk :: Prelude.Maybe SidewalkAccountInfoWithFingerprint,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPartnerAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountLinked', 'getPartnerAccountResponse_accountLinked' - Whether the partner account is linked to the AWS account.
--
-- 'sidewalk', 'getPartnerAccountResponse_sidewalk' - The Sidewalk account credentials.
--
-- 'httpStatus', 'getPartnerAccountResponse_httpStatus' - The response's http status code.
newGetPartnerAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPartnerAccountResponse
newGetPartnerAccountResponse pHttpStatus_ =
  GetPartnerAccountResponse'
    { accountLinked =
        Prelude.Nothing,
      sidewalk = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Whether the partner account is linked to the AWS account.
getPartnerAccountResponse_accountLinked :: Lens.Lens' GetPartnerAccountResponse (Prelude.Maybe Prelude.Bool)
getPartnerAccountResponse_accountLinked = Lens.lens (\GetPartnerAccountResponse' {accountLinked} -> accountLinked) (\s@GetPartnerAccountResponse' {} a -> s {accountLinked = a} :: GetPartnerAccountResponse)

-- | The Sidewalk account credentials.
getPartnerAccountResponse_sidewalk :: Lens.Lens' GetPartnerAccountResponse (Prelude.Maybe SidewalkAccountInfoWithFingerprint)
getPartnerAccountResponse_sidewalk = Lens.lens (\GetPartnerAccountResponse' {sidewalk} -> sidewalk) (\s@GetPartnerAccountResponse' {} a -> s {sidewalk = a} :: GetPartnerAccountResponse)

-- | The response's http status code.
getPartnerAccountResponse_httpStatus :: Lens.Lens' GetPartnerAccountResponse Prelude.Int
getPartnerAccountResponse_httpStatus = Lens.lens (\GetPartnerAccountResponse' {httpStatus} -> httpStatus) (\s@GetPartnerAccountResponse' {} a -> s {httpStatus = a} :: GetPartnerAccountResponse)

instance Prelude.NFData GetPartnerAccountResponse where
  rnf GetPartnerAccountResponse' {..} =
    Prelude.rnf accountLinked
      `Prelude.seq` Prelude.rnf sidewalk
      `Prelude.seq` Prelude.rnf httpStatus
