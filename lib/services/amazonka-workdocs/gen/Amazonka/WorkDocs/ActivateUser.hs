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
-- Module      : Amazonka.WorkDocs.ActivateUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates the specified user. Only active users can access Amazon
-- WorkDocs.
module Amazonka.WorkDocs.ActivateUser
  ( -- * Creating a Request
    ActivateUser (..),
    newActivateUser,

    -- * Request Lenses
    activateUser_authenticationToken,
    activateUser_userId,

    -- * Destructuring the Response
    ActivateUserResponse (..),
    newActivateUserResponse,

    -- * Response Lenses
    activateUserResponse_user,
    activateUserResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newActivateUser' smart constructor.
data ActivateUser = ActivateUser'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the user.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'activateUser_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'userId', 'activateUser_userId' - The ID of the user.
newActivateUser ::
  -- | 'userId'
  Prelude.Text ->
  ActivateUser
newActivateUser pUserId_ =
  ActivateUser'
    { authenticationToken =
        Prelude.Nothing,
      userId = pUserId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
activateUser_authenticationToken :: Lens.Lens' ActivateUser (Prelude.Maybe Prelude.Text)
activateUser_authenticationToken = Lens.lens (\ActivateUser' {authenticationToken} -> authenticationToken) (\s@ActivateUser' {} a -> s {authenticationToken = a} :: ActivateUser) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the user.
activateUser_userId :: Lens.Lens' ActivateUser Prelude.Text
activateUser_userId = Lens.lens (\ActivateUser' {userId} -> userId) (\s@ActivateUser' {} a -> s {userId = a} :: ActivateUser)

instance Core.AWSRequest ActivateUser where
  type AWSResponse ActivateUser = ActivateUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ActivateUserResponse'
            Prelude.<$> (x Data..?> "User")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ActivateUser where
  hashWithSalt _salt ActivateUser' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` userId

instance Prelude.NFData ActivateUser where
  rnf ActivateUser' {..} =
    Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf userId

instance Data.ToHeaders ActivateUser where
  toHeaders ActivateUser' {..} =
    Prelude.mconcat
      [ "Authentication" Data.=# authenticationToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToJSON ActivateUser where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath ActivateUser where
  toPath ActivateUser' {..} =
    Prelude.mconcat
      ["/api/v1/users/", Data.toBS userId, "/activation"]

instance Data.ToQuery ActivateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newActivateUserResponse' smart constructor.
data ActivateUserResponse = ActivateUserResponse'
  { -- | The user information.
    user :: Prelude.Maybe User,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivateUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'user', 'activateUserResponse_user' - The user information.
--
-- 'httpStatus', 'activateUserResponse_httpStatus' - The response's http status code.
newActivateUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ActivateUserResponse
newActivateUserResponse pHttpStatus_ =
  ActivateUserResponse'
    { user = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user information.
activateUserResponse_user :: Lens.Lens' ActivateUserResponse (Prelude.Maybe User)
activateUserResponse_user = Lens.lens (\ActivateUserResponse' {user} -> user) (\s@ActivateUserResponse' {} a -> s {user = a} :: ActivateUserResponse)

-- | The response's http status code.
activateUserResponse_httpStatus :: Lens.Lens' ActivateUserResponse Prelude.Int
activateUserResponse_httpStatus = Lens.lens (\ActivateUserResponse' {httpStatus} -> httpStatus) (\s@ActivateUserResponse' {} a -> s {httpStatus = a} :: ActivateUserResponse)

instance Prelude.NFData ActivateUserResponse where
  rnf ActivateUserResponse' {..} =
    Prelude.rnf user
      `Prelude.seq` Prelude.rnf httpStatus
