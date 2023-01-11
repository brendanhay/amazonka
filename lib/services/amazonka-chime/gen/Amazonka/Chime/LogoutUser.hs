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
-- Module      : Amazonka.Chime.LogoutUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Logs out the specified user from all of the devices they are currently
-- logged into.
module Amazonka.Chime.LogoutUser
  ( -- * Creating a Request
    LogoutUser (..),
    newLogoutUser,

    -- * Request Lenses
    logoutUser_accountId,
    logoutUser_userId,

    -- * Destructuring the Response
    LogoutUserResponse (..),
    newLogoutUserResponse,

    -- * Response Lenses
    logoutUserResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newLogoutUser' smart constructor.
data LogoutUser = LogoutUser'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Text,
    -- | The user ID.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogoutUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'logoutUser_accountId' - The Amazon Chime account ID.
--
-- 'userId', 'logoutUser_userId' - The user ID.
newLogoutUser ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  LogoutUser
newLogoutUser pAccountId_ pUserId_ =
  LogoutUser'
    { accountId = pAccountId_,
      userId = pUserId_
    }

-- | The Amazon Chime account ID.
logoutUser_accountId :: Lens.Lens' LogoutUser Prelude.Text
logoutUser_accountId = Lens.lens (\LogoutUser' {accountId} -> accountId) (\s@LogoutUser' {} a -> s {accountId = a} :: LogoutUser)

-- | The user ID.
logoutUser_userId :: Lens.Lens' LogoutUser Prelude.Text
logoutUser_userId = Lens.lens (\LogoutUser' {userId} -> userId) (\s@LogoutUser' {} a -> s {userId = a} :: LogoutUser)

instance Core.AWSRequest LogoutUser where
  type AWSResponse LogoutUser = LogoutUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          LogoutUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable LogoutUser where
  hashWithSalt _salt LogoutUser' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` userId

instance Prelude.NFData LogoutUser where
  rnf LogoutUser' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf userId

instance Data.ToHeaders LogoutUser where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON LogoutUser where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath LogoutUser where
  toPath LogoutUser' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS accountId,
        "/users/",
        Data.toBS userId
      ]

instance Data.ToQuery LogoutUser where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["operation=logout"])

-- | /See:/ 'newLogoutUserResponse' smart constructor.
data LogoutUserResponse = LogoutUserResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogoutUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'logoutUserResponse_httpStatus' - The response's http status code.
newLogoutUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  LogoutUserResponse
newLogoutUserResponse pHttpStatus_ =
  LogoutUserResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
logoutUserResponse_httpStatus :: Lens.Lens' LogoutUserResponse Prelude.Int
logoutUserResponse_httpStatus = Lens.lens (\LogoutUserResponse' {httpStatus} -> httpStatus) (\s@LogoutUserResponse' {} a -> s {httpStatus = a} :: LogoutUserResponse)

instance Prelude.NFData LogoutUserResponse where
  rnf LogoutUserResponse' {..} = Prelude.rnf httpStatus
