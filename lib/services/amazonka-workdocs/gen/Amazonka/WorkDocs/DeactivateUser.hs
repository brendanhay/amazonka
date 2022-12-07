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
-- Module      : Amazonka.WorkDocs.DeactivateUser
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deactivates the specified user, which revokes the user\'s access to
-- Amazon WorkDocs.
module Amazonka.WorkDocs.DeactivateUser
  ( -- * Creating a Request
    DeactivateUser (..),
    newDeactivateUser,

    -- * Request Lenses
    deactivateUser_authenticationToken,
    deactivateUser_userId,

    -- * Destructuring the Response
    DeactivateUserResponse (..),
    newDeactivateUserResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newDeactivateUser' smart constructor.
data DeactivateUser = DeactivateUser'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the user.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeactivateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'deactivateUser_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'userId', 'deactivateUser_userId' - The ID of the user.
newDeactivateUser ::
  -- | 'userId'
  Prelude.Text ->
  DeactivateUser
newDeactivateUser pUserId_ =
  DeactivateUser'
    { authenticationToken =
        Prelude.Nothing,
      userId = pUserId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
deactivateUser_authenticationToken :: Lens.Lens' DeactivateUser (Prelude.Maybe Prelude.Text)
deactivateUser_authenticationToken = Lens.lens (\DeactivateUser' {authenticationToken} -> authenticationToken) (\s@DeactivateUser' {} a -> s {authenticationToken = a} :: DeactivateUser) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the user.
deactivateUser_userId :: Lens.Lens' DeactivateUser Prelude.Text
deactivateUser_userId = Lens.lens (\DeactivateUser' {userId} -> userId) (\s@DeactivateUser' {} a -> s {userId = a} :: DeactivateUser)

instance Core.AWSRequest DeactivateUser where
  type
    AWSResponse DeactivateUser =
      DeactivateUserResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeactivateUserResponse'

instance Prelude.Hashable DeactivateUser where
  hashWithSalt _salt DeactivateUser' {..} =
    _salt `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` userId

instance Prelude.NFData DeactivateUser where
  rnf DeactivateUser' {..} =
    Prelude.rnf authenticationToken
      `Prelude.seq` Prelude.rnf userId

instance Data.ToHeaders DeactivateUser where
  toHeaders DeactivateUser' {..} =
    Prelude.mconcat
      [ "Authentication" Data.=# authenticationToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath DeactivateUser where
  toPath DeactivateUser' {..} =
    Prelude.mconcat
      ["/api/v1/users/", Data.toBS userId, "/activation"]

instance Data.ToQuery DeactivateUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeactivateUserResponse' smart constructor.
data DeactivateUserResponse = DeactivateUserResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeactivateUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeactivateUserResponse ::
  DeactivateUserResponse
newDeactivateUserResponse = DeactivateUserResponse'

instance Prelude.NFData DeactivateUserResponse where
  rnf _ = ()
