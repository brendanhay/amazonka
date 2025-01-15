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
-- Module      : Amazonka.MacieV2.GetAdministratorAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the Amazon Macie administrator account for
-- an account.
module Amazonka.MacieV2.GetAdministratorAccount
  ( -- * Creating a Request
    GetAdministratorAccount (..),
    newGetAdministratorAccount,

    -- * Destructuring the Response
    GetAdministratorAccountResponse (..),
    newGetAdministratorAccountResponse,

    -- * Response Lenses
    getAdministratorAccountResponse_administrator,
    getAdministratorAccountResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAdministratorAccount' smart constructor.
data GetAdministratorAccount = GetAdministratorAccount'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAdministratorAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetAdministratorAccount ::
  GetAdministratorAccount
newGetAdministratorAccount = GetAdministratorAccount'

instance Core.AWSRequest GetAdministratorAccount where
  type
    AWSResponse GetAdministratorAccount =
      GetAdministratorAccountResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAdministratorAccountResponse'
            Prelude.<$> (x Data..?> "administrator")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAdministratorAccount where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetAdministratorAccount where
  rnf _ = ()

instance Data.ToHeaders GetAdministratorAccount where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetAdministratorAccount where
  toPath = Prelude.const "/administrator"

instance Data.ToQuery GetAdministratorAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAdministratorAccountResponse' smart constructor.
data GetAdministratorAccountResponse = GetAdministratorAccountResponse'
  { -- | The Amazon Web Services account ID for the administrator account. If the
    -- accounts are associated by an Amazon Macie membership invitation, this
    -- object also provides details about the invitation that was sent to
    -- establish the relationship between the accounts.
    administrator :: Prelude.Maybe Invitation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAdministratorAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'administrator', 'getAdministratorAccountResponse_administrator' - The Amazon Web Services account ID for the administrator account. If the
-- accounts are associated by an Amazon Macie membership invitation, this
-- object also provides details about the invitation that was sent to
-- establish the relationship between the accounts.
--
-- 'httpStatus', 'getAdministratorAccountResponse_httpStatus' - The response's http status code.
newGetAdministratorAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAdministratorAccountResponse
newGetAdministratorAccountResponse pHttpStatus_ =
  GetAdministratorAccountResponse'
    { administrator =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Web Services account ID for the administrator account. If the
-- accounts are associated by an Amazon Macie membership invitation, this
-- object also provides details about the invitation that was sent to
-- establish the relationship between the accounts.
getAdministratorAccountResponse_administrator :: Lens.Lens' GetAdministratorAccountResponse (Prelude.Maybe Invitation)
getAdministratorAccountResponse_administrator = Lens.lens (\GetAdministratorAccountResponse' {administrator} -> administrator) (\s@GetAdministratorAccountResponse' {} a -> s {administrator = a} :: GetAdministratorAccountResponse)

-- | The response's http status code.
getAdministratorAccountResponse_httpStatus :: Lens.Lens' GetAdministratorAccountResponse Prelude.Int
getAdministratorAccountResponse_httpStatus = Lens.lens (\GetAdministratorAccountResponse' {httpStatus} -> httpStatus) (\s@GetAdministratorAccountResponse' {} a -> s {httpStatus = a} :: GetAdministratorAccountResponse)

instance
  Prelude.NFData
    GetAdministratorAccountResponse
  where
  rnf GetAdministratorAccountResponse' {..} =
    Prelude.rnf administrator `Prelude.seq`
      Prelude.rnf httpStatus
