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
-- Module      : Amazonka.FinSpaceData.DisableUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Denies access to the FinSpace web application and API for the specified
-- user.
module Amazonka.FinSpaceData.DisableUser
  ( -- * Creating a Request
    DisableUser (..),
    newDisableUser,

    -- * Request Lenses
    disableUser_clientToken,
    disableUser_userId,

    -- * Destructuring the Response
    DisableUserResponse (..),
    newDisableUserResponse,

    -- * Response Lenses
    disableUserResponse_userId,
    disableUserResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableUser' smart constructor.
data DisableUser = DisableUser'
  { -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the user account that you want to disable.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'disableUser_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
--
-- 'userId', 'disableUser_userId' - The unique identifier for the user account that you want to disable.
newDisableUser ::
  -- | 'userId'
  Prelude.Text ->
  DisableUser
newDisableUser pUserId_ =
  DisableUser'
    { clientToken = Prelude.Nothing,
      userId = pUserId_
    }

-- | A token that ensures idempotency. This token expires in 10 minutes.
disableUser_clientToken :: Lens.Lens' DisableUser (Prelude.Maybe Prelude.Text)
disableUser_clientToken = Lens.lens (\DisableUser' {clientToken} -> clientToken) (\s@DisableUser' {} a -> s {clientToken = a} :: DisableUser)

-- | The unique identifier for the user account that you want to disable.
disableUser_userId :: Lens.Lens' DisableUser Prelude.Text
disableUser_userId = Lens.lens (\DisableUser' {userId} -> userId) (\s@DisableUser' {} a -> s {userId = a} :: DisableUser)

instance Core.AWSRequest DisableUser where
  type AWSResponse DisableUser = DisableUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisableUserResponse'
            Prelude.<$> (x Data..?> "userId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableUser where
  hashWithSalt _salt DisableUser' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` userId

instance Prelude.NFData DisableUser where
  rnf DisableUser' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf userId

instance Data.ToHeaders DisableUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisableUser where
  toJSON DisableUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [("clientToken" Data..=) Prelude.<$> clientToken]
      )

instance Data.ToPath DisableUser where
  toPath DisableUser' {..} =
    Prelude.mconcat
      ["/user/", Data.toBS userId, "/disable"]

instance Data.ToQuery DisableUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableUserResponse' smart constructor.
data DisableUserResponse = DisableUserResponse'
  { -- | The unique identifier for the disabled user account.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userId', 'disableUserResponse_userId' - The unique identifier for the disabled user account.
--
-- 'httpStatus', 'disableUserResponse_httpStatus' - The response's http status code.
newDisableUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableUserResponse
newDisableUserResponse pHttpStatus_ =
  DisableUserResponse'
    { userId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for the disabled user account.
disableUserResponse_userId :: Lens.Lens' DisableUserResponse (Prelude.Maybe Prelude.Text)
disableUserResponse_userId = Lens.lens (\DisableUserResponse' {userId} -> userId) (\s@DisableUserResponse' {} a -> s {userId = a} :: DisableUserResponse)

-- | The response's http status code.
disableUserResponse_httpStatus :: Lens.Lens' DisableUserResponse Prelude.Int
disableUserResponse_httpStatus = Lens.lens (\DisableUserResponse' {httpStatus} -> httpStatus) (\s@DisableUserResponse' {} a -> s {httpStatus = a} :: DisableUserResponse)

instance Prelude.NFData DisableUserResponse where
  rnf DisableUserResponse' {..} =
    Prelude.rnf userId `Prelude.seq`
      Prelude.rnf httpStatus
