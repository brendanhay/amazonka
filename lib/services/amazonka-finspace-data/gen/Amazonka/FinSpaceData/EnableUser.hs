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
-- Module      : Amazonka.FinSpaceData.EnableUser
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the specified user to access the FinSpace web application and
-- API.
module Amazonka.FinSpaceData.EnableUser
  ( -- * Creating a Request
    EnableUser (..),
    newEnableUser,

    -- * Request Lenses
    enableUser_clientToken,
    enableUser_userId,

    -- * Destructuring the Response
    EnableUserResponse (..),
    newEnableUserResponse,

    -- * Response Lenses
    enableUserResponse_userId,
    enableUserResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FinSpaceData.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newEnableUser' smart constructor.
data EnableUser = EnableUser'
  { -- | A token that ensures idempotency. This token expires in 10 minutes.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the user account that you want to enable.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'enableUser_clientToken' - A token that ensures idempotency. This token expires in 10 minutes.
--
-- 'userId', 'enableUser_userId' - The unique identifier for the user account that you want to enable.
newEnableUser ::
  -- | 'userId'
  Prelude.Text ->
  EnableUser
newEnableUser pUserId_ =
  EnableUser'
    { clientToken = Prelude.Nothing,
      userId = pUserId_
    }

-- | A token that ensures idempotency. This token expires in 10 minutes.
enableUser_clientToken :: Lens.Lens' EnableUser (Prelude.Maybe Prelude.Text)
enableUser_clientToken = Lens.lens (\EnableUser' {clientToken} -> clientToken) (\s@EnableUser' {} a -> s {clientToken = a} :: EnableUser)

-- | The unique identifier for the user account that you want to enable.
enableUser_userId :: Lens.Lens' EnableUser Prelude.Text
enableUser_userId = Lens.lens (\EnableUser' {userId} -> userId) (\s@EnableUser' {} a -> s {userId = a} :: EnableUser)

instance Core.AWSRequest EnableUser where
  type AWSResponse EnableUser = EnableUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          EnableUserResponse'
            Prelude.<$> (x Core..?> "userId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable EnableUser where
  hashWithSalt _salt EnableUser' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` userId

instance Prelude.NFData EnableUser where
  rnf EnableUser' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf userId

instance Core.ToHeaders EnableUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON EnableUser where
  toJSON EnableUser' {..} =
    Core.object
      ( Prelude.catMaybes
          [("clientToken" Core..=) Prelude.<$> clientToken]
      )

instance Core.ToPath EnableUser where
  toPath EnableUser' {..} =
    Prelude.mconcat
      ["/user/", Core.toBS userId, "/enable"]

instance Core.ToQuery EnableUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableUserResponse' smart constructor.
data EnableUserResponse = EnableUserResponse'
  { -- | The unique identifier for the enabled user account.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnableUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userId', 'enableUserResponse_userId' - The unique identifier for the enabled user account.
--
-- 'httpStatus', 'enableUserResponse_httpStatus' - The response's http status code.
newEnableUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  EnableUserResponse
newEnableUserResponse pHttpStatus_ =
  EnableUserResponse'
    { userId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier for the enabled user account.
enableUserResponse_userId :: Lens.Lens' EnableUserResponse (Prelude.Maybe Prelude.Text)
enableUserResponse_userId = Lens.lens (\EnableUserResponse' {userId} -> userId) (\s@EnableUserResponse' {} a -> s {userId = a} :: EnableUserResponse)

-- | The response's http status code.
enableUserResponse_httpStatus :: Lens.Lens' EnableUserResponse Prelude.Int
enableUserResponse_httpStatus = Lens.lens (\EnableUserResponse' {httpStatus} -> httpStatus) (\s@EnableUserResponse' {} a -> s {httpStatus = a} :: EnableUserResponse)

instance Prelude.NFData EnableUserResponse where
  rnf EnableUserResponse' {..} =
    Prelude.rnf userId
      `Prelude.seq` Prelude.rnf httpStatus
