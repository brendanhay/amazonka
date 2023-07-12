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
-- Module      : Amazonka.Chime.CreateAccount
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Chime account under the administrator\'s AWS account.
-- Only @Team@ account types are currently supported for this action. For
-- more information about different account types, see
-- <https://docs.aws.amazon.com/chime/latest/ag/manage-chime-account.html Managing Your Amazon Chime Accounts>
-- in the /Amazon Chime Administration Guide/.
module Amazonka.Chime.CreateAccount
  ( -- * Creating a Request
    CreateAccount (..),
    newCreateAccount,

    -- * Request Lenses
    createAccount_name,

    -- * Destructuring the Response
    CreateAccountResponse (..),
    newCreateAccountResponse,

    -- * Response Lenses
    createAccountResponse_account,
    createAccountResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAccount' smart constructor.
data CreateAccount = CreateAccount'
  { -- | The name of the Amazon Chime account.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createAccount_name' - The name of the Amazon Chime account.
newCreateAccount ::
  -- | 'name'
  Prelude.Text ->
  CreateAccount
newCreateAccount pName_ =
  CreateAccount' {name = pName_}

-- | The name of the Amazon Chime account.
createAccount_name :: Lens.Lens' CreateAccount Prelude.Text
createAccount_name = Lens.lens (\CreateAccount' {name} -> name) (\s@CreateAccount' {} a -> s {name = a} :: CreateAccount)

instance Core.AWSRequest CreateAccount where
  type
    AWSResponse CreateAccount =
      CreateAccountResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAccountResponse'
            Prelude.<$> (x Data..?> "Account")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAccount where
  hashWithSalt _salt CreateAccount' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData CreateAccount where
  rnf CreateAccount' {..} = Prelude.rnf name

instance Data.ToHeaders CreateAccount where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateAccount where
  toJSON CreateAccount' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath CreateAccount where
  toPath = Prelude.const "/accounts"

instance Data.ToQuery CreateAccount where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAccountResponse' smart constructor.
data CreateAccountResponse = CreateAccountResponse'
  { -- | The Amazon Chime account details.
    account :: Prelude.Maybe Account,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccountResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'account', 'createAccountResponse_account' - The Amazon Chime account details.
--
-- 'httpStatus', 'createAccountResponse_httpStatus' - The response's http status code.
newCreateAccountResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAccountResponse
newCreateAccountResponse pHttpStatus_ =
  CreateAccountResponse'
    { account = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Chime account details.
createAccountResponse_account :: Lens.Lens' CreateAccountResponse (Prelude.Maybe Account)
createAccountResponse_account = Lens.lens (\CreateAccountResponse' {account} -> account) (\s@CreateAccountResponse' {} a -> s {account = a} :: CreateAccountResponse)

-- | The response's http status code.
createAccountResponse_httpStatus :: Lens.Lens' CreateAccountResponse Prelude.Int
createAccountResponse_httpStatus = Lens.lens (\CreateAccountResponse' {httpStatus} -> httpStatus) (\s@CreateAccountResponse' {} a -> s {httpStatus = a} :: CreateAccountResponse)

instance Prelude.NFData CreateAccountResponse where
  rnf CreateAccountResponse' {..} =
    Prelude.rnf account
      `Prelude.seq` Prelude.rnf httpStatus
