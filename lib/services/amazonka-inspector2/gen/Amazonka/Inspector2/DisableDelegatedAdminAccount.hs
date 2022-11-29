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
-- Module      : Amazonka.Inspector2.DisableDelegatedAdminAccount
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the Amazon Inspector delegated administrator for your
-- organization.
module Amazonka.Inspector2.DisableDelegatedAdminAccount
  ( -- * Creating a Request
    DisableDelegatedAdminAccount (..),
    newDisableDelegatedAdminAccount,

    -- * Request Lenses
    disableDelegatedAdminAccount_delegatedAdminAccountId,

    -- * Destructuring the Response
    DisableDelegatedAdminAccountResponse (..),
    newDisableDelegatedAdminAccountResponse,

    -- * Response Lenses
    disableDelegatedAdminAccountResponse_httpStatus,
    disableDelegatedAdminAccountResponse_delegatedAdminAccountId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableDelegatedAdminAccount' smart constructor.
data DisableDelegatedAdminAccount = DisableDelegatedAdminAccount'
  { -- | The Amazon Web Services account ID of the current Amazon Inspector
    -- delegated administrator.
    delegatedAdminAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableDelegatedAdminAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'delegatedAdminAccountId', 'disableDelegatedAdminAccount_delegatedAdminAccountId' - The Amazon Web Services account ID of the current Amazon Inspector
-- delegated administrator.
newDisableDelegatedAdminAccount ::
  -- | 'delegatedAdminAccountId'
  Prelude.Text ->
  DisableDelegatedAdminAccount
newDisableDelegatedAdminAccount
  pDelegatedAdminAccountId_ =
    DisableDelegatedAdminAccount'
      { delegatedAdminAccountId =
          pDelegatedAdminAccountId_
      }

-- | The Amazon Web Services account ID of the current Amazon Inspector
-- delegated administrator.
disableDelegatedAdminAccount_delegatedAdminAccountId :: Lens.Lens' DisableDelegatedAdminAccount Prelude.Text
disableDelegatedAdminAccount_delegatedAdminAccountId = Lens.lens (\DisableDelegatedAdminAccount' {delegatedAdminAccountId} -> delegatedAdminAccountId) (\s@DisableDelegatedAdminAccount' {} a -> s {delegatedAdminAccountId = a} :: DisableDelegatedAdminAccount)

instance Core.AWSRequest DisableDelegatedAdminAccount where
  type
    AWSResponse DisableDelegatedAdminAccount =
      DisableDelegatedAdminAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisableDelegatedAdminAccountResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "delegatedAdminAccountId")
      )

instance
  Prelude.Hashable
    DisableDelegatedAdminAccount
  where
  hashWithSalt _salt DisableDelegatedAdminAccount' {..} =
    _salt
      `Prelude.hashWithSalt` delegatedAdminAccountId

instance Prelude.NFData DisableDelegatedAdminAccount where
  rnf DisableDelegatedAdminAccount' {..} =
    Prelude.rnf delegatedAdminAccountId

instance Core.ToHeaders DisableDelegatedAdminAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DisableDelegatedAdminAccount where
  toJSON DisableDelegatedAdminAccount' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "delegatedAdminAccountId"
                  Core..= delegatedAdminAccountId
              )
          ]
      )

instance Core.ToPath DisableDelegatedAdminAccount where
  toPath =
    Prelude.const "/delegatedadminaccounts/disable"

instance Core.ToQuery DisableDelegatedAdminAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableDelegatedAdminAccountResponse' smart constructor.
data DisableDelegatedAdminAccountResponse = DisableDelegatedAdminAccountResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Web Services account ID of the successfully disabled
    -- delegated administrator.
    delegatedAdminAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableDelegatedAdminAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disableDelegatedAdminAccountResponse_httpStatus' - The response's http status code.
--
-- 'delegatedAdminAccountId', 'disableDelegatedAdminAccountResponse_delegatedAdminAccountId' - The Amazon Web Services account ID of the successfully disabled
-- delegated administrator.
newDisableDelegatedAdminAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'delegatedAdminAccountId'
  Prelude.Text ->
  DisableDelegatedAdminAccountResponse
newDisableDelegatedAdminAccountResponse
  pHttpStatus_
  pDelegatedAdminAccountId_ =
    DisableDelegatedAdminAccountResponse'
      { httpStatus =
          pHttpStatus_,
        delegatedAdminAccountId =
          pDelegatedAdminAccountId_
      }

-- | The response's http status code.
disableDelegatedAdminAccountResponse_httpStatus :: Lens.Lens' DisableDelegatedAdminAccountResponse Prelude.Int
disableDelegatedAdminAccountResponse_httpStatus = Lens.lens (\DisableDelegatedAdminAccountResponse' {httpStatus} -> httpStatus) (\s@DisableDelegatedAdminAccountResponse' {} a -> s {httpStatus = a} :: DisableDelegatedAdminAccountResponse)

-- | The Amazon Web Services account ID of the successfully disabled
-- delegated administrator.
disableDelegatedAdminAccountResponse_delegatedAdminAccountId :: Lens.Lens' DisableDelegatedAdminAccountResponse Prelude.Text
disableDelegatedAdminAccountResponse_delegatedAdminAccountId = Lens.lens (\DisableDelegatedAdminAccountResponse' {delegatedAdminAccountId} -> delegatedAdminAccountId) (\s@DisableDelegatedAdminAccountResponse' {} a -> s {delegatedAdminAccountId = a} :: DisableDelegatedAdminAccountResponse)

instance
  Prelude.NFData
    DisableDelegatedAdminAccountResponse
  where
  rnf DisableDelegatedAdminAccountResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf delegatedAdminAccountId
