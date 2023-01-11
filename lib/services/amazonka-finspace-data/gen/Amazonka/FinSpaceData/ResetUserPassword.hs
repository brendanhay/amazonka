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
-- Module      : Amazonka.FinSpaceData.ResetUserPassword
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the password for a specified user ID and generates a temporary
-- one. Only a superuser can reset password for other users. Resetting the
-- password immediately invalidates the previous password associated with
-- the user.
module Amazonka.FinSpaceData.ResetUserPassword
  ( -- * Creating a Request
    ResetUserPassword (..),
    newResetUserPassword,

    -- * Request Lenses
    resetUserPassword_clientToken,
    resetUserPassword_userId,

    -- * Destructuring the Response
    ResetUserPasswordResponse (..),
    newResetUserPasswordResponse,

    -- * Response Lenses
    resetUserPasswordResponse_temporaryPassword,
    resetUserPasswordResponse_userId,
    resetUserPasswordResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newResetUserPassword' smart constructor.
data ResetUserPassword = ResetUserPassword'
  { -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the user that a temporary password is requested
    -- for.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetUserPassword' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'resetUserPassword_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
--
-- 'userId', 'resetUserPassword_userId' - The unique identifier of the user that a temporary password is requested
-- for.
newResetUserPassword ::
  -- | 'userId'
  Prelude.Text ->
  ResetUserPassword
newResetUserPassword pUserId_ =
  ResetUserPassword'
    { clientToken = Prelude.Nothing,
      userId = pUserId_
    }

-- | A token that ensures idempotency. This token expires in 10 minutes.
resetUserPassword_clientToken :: Lens.Lens' ResetUserPassword (Prelude.Maybe Prelude.Text)
resetUserPassword_clientToken = Lens.lens (\ResetUserPassword' {clientToken} -> clientToken) (\s@ResetUserPassword' {} a -> s {clientToken = a} :: ResetUserPassword)

-- | The unique identifier of the user that a temporary password is requested
-- for.
resetUserPassword_userId :: Lens.Lens' ResetUserPassword Prelude.Text
resetUserPassword_userId = Lens.lens (\ResetUserPassword' {userId} -> userId) (\s@ResetUserPassword' {} a -> s {userId = a} :: ResetUserPassword)

instance Core.AWSRequest ResetUserPassword where
  type
    AWSResponse ResetUserPassword =
      ResetUserPasswordResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ResetUserPasswordResponse'
            Prelude.<$> (x Data..?> "temporaryPassword")
            Prelude.<*> (x Data..?> "userId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResetUserPassword where
  hashWithSalt _salt ResetUserPassword' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` userId

instance Prelude.NFData ResetUserPassword where
  rnf ResetUserPassword' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf userId

instance Data.ToHeaders ResetUserPassword where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ResetUserPassword where
  toJSON ResetUserPassword' {..} =
    Data.object
      ( Prelude.catMaybes
          [("clientToken" Data..=) Prelude.<$> clientToken]
      )

instance Data.ToPath ResetUserPassword where
  toPath ResetUserPassword' {..} =
    Prelude.mconcat
      ["/user/", Data.toBS userId, "/password"]

instance Data.ToQuery ResetUserPassword where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResetUserPasswordResponse' smart constructor.
data ResetUserPasswordResponse = ResetUserPasswordResponse'
  { -- | A randomly generated temporary password for the requested user account.
    -- This password expires in 7 days.
    temporaryPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The unique identifier of the user that a new password is generated for.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetUserPasswordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'temporaryPassword', 'resetUserPasswordResponse_temporaryPassword' - A randomly generated temporary password for the requested user account.
-- This password expires in 7 days.
--
-- 'userId', 'resetUserPasswordResponse_userId' - The unique identifier of the user that a new password is generated for.
--
-- 'httpStatus', 'resetUserPasswordResponse_httpStatus' - The response's http status code.
newResetUserPasswordResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResetUserPasswordResponse
newResetUserPasswordResponse pHttpStatus_ =
  ResetUserPasswordResponse'
    { temporaryPassword =
        Prelude.Nothing,
      userId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A randomly generated temporary password for the requested user account.
-- This password expires in 7 days.
resetUserPasswordResponse_temporaryPassword :: Lens.Lens' ResetUserPasswordResponse (Prelude.Maybe Prelude.Text)
resetUserPasswordResponse_temporaryPassword = Lens.lens (\ResetUserPasswordResponse' {temporaryPassword} -> temporaryPassword) (\s@ResetUserPasswordResponse' {} a -> s {temporaryPassword = a} :: ResetUserPasswordResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The unique identifier of the user that a new password is generated for.
resetUserPasswordResponse_userId :: Lens.Lens' ResetUserPasswordResponse (Prelude.Maybe Prelude.Text)
resetUserPasswordResponse_userId = Lens.lens (\ResetUserPasswordResponse' {userId} -> userId) (\s@ResetUserPasswordResponse' {} a -> s {userId = a} :: ResetUserPasswordResponse)

-- | The response's http status code.
resetUserPasswordResponse_httpStatus :: Lens.Lens' ResetUserPasswordResponse Prelude.Int
resetUserPasswordResponse_httpStatus = Lens.lens (\ResetUserPasswordResponse' {httpStatus} -> httpStatus) (\s@ResetUserPasswordResponse' {} a -> s {httpStatus = a} :: ResetUserPasswordResponse)

instance Prelude.NFData ResetUserPasswordResponse where
  rnf ResetUserPasswordResponse' {..} =
    Prelude.rnf temporaryPassword
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf httpStatus
