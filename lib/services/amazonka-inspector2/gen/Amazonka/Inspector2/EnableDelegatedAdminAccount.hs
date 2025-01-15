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
-- Module      : Amazonka.Inspector2.EnableDelegatedAdminAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the Amazon Inspector delegated administrator for your
-- Organizations organization.
module Amazonka.Inspector2.EnableDelegatedAdminAccount
  ( -- * Creating a Request
    EnableDelegatedAdminAccount (..),
    newEnableDelegatedAdminAccount,

    -- * Request Lenses
    enableDelegatedAdminAccount_clientToken,
    enableDelegatedAdminAccount_delegatedAdminAccountId,

    -- * Destructuring the Response
    EnableDelegatedAdminAccountResponse (..),
    newEnableDelegatedAdminAccountResponse,

    -- * Response Lenses
    enableDelegatedAdminAccountResponse_httpStatus,
    enableDelegatedAdminAccountResponse_delegatedAdminAccountId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableDelegatedAdminAccount' smart constructor.
data EnableDelegatedAdminAccount = EnableDelegatedAdminAccount'
  { -- | The idempotency token for the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the Amazon Inspector delegated
    -- administrator.
    delegatedAdminAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableDelegatedAdminAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'enableDelegatedAdminAccount_clientToken' - The idempotency token for the request.
--
-- 'delegatedAdminAccountId', 'enableDelegatedAdminAccount_delegatedAdminAccountId' - The Amazon Web Services account ID of the Amazon Inspector delegated
-- administrator.
newEnableDelegatedAdminAccount ::
  -- | 'delegatedAdminAccountId'
  Prelude.Text ->
  EnableDelegatedAdminAccount
newEnableDelegatedAdminAccount
  pDelegatedAdminAccountId_ =
    EnableDelegatedAdminAccount'
      { clientToken =
          Prelude.Nothing,
        delegatedAdminAccountId =
          pDelegatedAdminAccountId_
      }

-- | The idempotency token for the request.
enableDelegatedAdminAccount_clientToken :: Lens.Lens' EnableDelegatedAdminAccount (Prelude.Maybe Prelude.Text)
enableDelegatedAdminAccount_clientToken = Lens.lens (\EnableDelegatedAdminAccount' {clientToken} -> clientToken) (\s@EnableDelegatedAdminAccount' {} a -> s {clientToken = a} :: EnableDelegatedAdminAccount)

-- | The Amazon Web Services account ID of the Amazon Inspector delegated
-- administrator.
enableDelegatedAdminAccount_delegatedAdminAccountId :: Lens.Lens' EnableDelegatedAdminAccount Prelude.Text
enableDelegatedAdminAccount_delegatedAdminAccountId = Lens.lens (\EnableDelegatedAdminAccount' {delegatedAdminAccountId} -> delegatedAdminAccountId) (\s@EnableDelegatedAdminAccount' {} a -> s {delegatedAdminAccountId = a} :: EnableDelegatedAdminAccount)

instance Core.AWSRequest EnableDelegatedAdminAccount where
  type
    AWSResponse EnableDelegatedAdminAccount =
      EnableDelegatedAdminAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          EnableDelegatedAdminAccountResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "delegatedAdminAccountId")
      )

instance Prelude.Hashable EnableDelegatedAdminAccount where
  hashWithSalt _salt EnableDelegatedAdminAccount' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` delegatedAdminAccountId

instance Prelude.NFData EnableDelegatedAdminAccount where
  rnf EnableDelegatedAdminAccount' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf delegatedAdminAccountId

instance Data.ToHeaders EnableDelegatedAdminAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON EnableDelegatedAdminAccount where
  toJSON EnableDelegatedAdminAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just
              ( "delegatedAdminAccountId"
                  Data..= delegatedAdminAccountId
              )
          ]
      )

instance Data.ToPath EnableDelegatedAdminAccount where
  toPath =
    Prelude.const "/delegatedadminaccounts/enable"

instance Data.ToQuery EnableDelegatedAdminAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableDelegatedAdminAccountResponse' smart constructor.
data EnableDelegatedAdminAccountResponse = EnableDelegatedAdminAccountResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Web Services account ID of the successfully Amazon Inspector
    -- delegated administrator.
    delegatedAdminAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableDelegatedAdminAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'enableDelegatedAdminAccountResponse_httpStatus' - The response's http status code.
--
-- 'delegatedAdminAccountId', 'enableDelegatedAdminAccountResponse_delegatedAdminAccountId' - The Amazon Web Services account ID of the successfully Amazon Inspector
-- delegated administrator.
newEnableDelegatedAdminAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'delegatedAdminAccountId'
  Prelude.Text ->
  EnableDelegatedAdminAccountResponse
newEnableDelegatedAdminAccountResponse
  pHttpStatus_
  pDelegatedAdminAccountId_ =
    EnableDelegatedAdminAccountResponse'
      { httpStatus =
          pHttpStatus_,
        delegatedAdminAccountId =
          pDelegatedAdminAccountId_
      }

-- | The response's http status code.
enableDelegatedAdminAccountResponse_httpStatus :: Lens.Lens' EnableDelegatedAdminAccountResponse Prelude.Int
enableDelegatedAdminAccountResponse_httpStatus = Lens.lens (\EnableDelegatedAdminAccountResponse' {httpStatus} -> httpStatus) (\s@EnableDelegatedAdminAccountResponse' {} a -> s {httpStatus = a} :: EnableDelegatedAdminAccountResponse)

-- | The Amazon Web Services account ID of the successfully Amazon Inspector
-- delegated administrator.
enableDelegatedAdminAccountResponse_delegatedAdminAccountId :: Lens.Lens' EnableDelegatedAdminAccountResponse Prelude.Text
enableDelegatedAdminAccountResponse_delegatedAdminAccountId = Lens.lens (\EnableDelegatedAdminAccountResponse' {delegatedAdminAccountId} -> delegatedAdminAccountId) (\s@EnableDelegatedAdminAccountResponse' {} a -> s {delegatedAdminAccountId = a} :: EnableDelegatedAdminAccountResponse)

instance
  Prelude.NFData
    EnableDelegatedAdminAccountResponse
  where
  rnf EnableDelegatedAdminAccountResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf delegatedAdminAccountId
