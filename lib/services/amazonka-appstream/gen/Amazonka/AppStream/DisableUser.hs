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
-- Module      : Amazonka.AppStream.DisableUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified user in the user pool. Users can\'t sign in to
-- AppStream 2.0 until they are re-enabled. This action does not delete the
-- user.
module Amazonka.AppStream.DisableUser
  ( -- * Creating a Request
    DisableUser (..),
    newDisableUser,

    -- * Request Lenses
    disableUser_userName,
    disableUser_authenticationType,

    -- * Destructuring the Response
    DisableUserResponse (..),
    newDisableUserResponse,

    -- * Response Lenses
    disableUserResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisableUser' smart constructor.
data DisableUser = DisableUser'
  { -- | The email address of the user.
    --
    -- Users\' email addresses are case-sensitive.
    userName :: Data.Sensitive Prelude.Text,
    -- | The authentication type for the user. You must specify USERPOOL.
    authenticationType :: AuthenticationType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisableUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userName', 'disableUser_userName' - The email address of the user.
--
-- Users\' email addresses are case-sensitive.
--
-- 'authenticationType', 'disableUser_authenticationType' - The authentication type for the user. You must specify USERPOOL.
newDisableUser ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'authenticationType'
  AuthenticationType ->
  DisableUser
newDisableUser pUserName_ pAuthenticationType_ =
  DisableUser'
    { userName =
        Data._Sensitive Lens.# pUserName_,
      authenticationType = pAuthenticationType_
    }

-- | The email address of the user.
--
-- Users\' email addresses are case-sensitive.
disableUser_userName :: Lens.Lens' DisableUser Prelude.Text
disableUser_userName = Lens.lens (\DisableUser' {userName} -> userName) (\s@DisableUser' {} a -> s {userName = a} :: DisableUser) Prelude.. Data._Sensitive

-- | The authentication type for the user. You must specify USERPOOL.
disableUser_authenticationType :: Lens.Lens' DisableUser AuthenticationType
disableUser_authenticationType = Lens.lens (\DisableUser' {authenticationType} -> authenticationType) (\s@DisableUser' {} a -> s {authenticationType = a} :: DisableUser)

instance Core.AWSRequest DisableUser where
  type AWSResponse DisableUser = DisableUserResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisableUserResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisableUser where
  hashWithSalt _salt DisableUser' {..} =
    _salt
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` authenticationType

instance Prelude.NFData DisableUser where
  rnf DisableUser' {..} =
    Prelude.rnf userName
      `Prelude.seq` Prelude.rnf authenticationType

instance Data.ToHeaders DisableUser where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.DisableUser" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisableUser where
  toJSON DisableUser' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserName" Data..= userName),
            Prelude.Just
              ("AuthenticationType" Data..= authenticationType)
          ]
      )

instance Data.ToPath DisableUser where
  toPath = Prelude.const "/"

instance Data.ToQuery DisableUser where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableUserResponse' smart constructor.
data DisableUserResponse = DisableUserResponse'
  { -- | The response's http status code.
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
-- 'httpStatus', 'disableUserResponse_httpStatus' - The response's http status code.
newDisableUserResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisableUserResponse
newDisableUserResponse pHttpStatus_ =
  DisableUserResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
disableUserResponse_httpStatus :: Lens.Lens' DisableUserResponse Prelude.Int
disableUserResponse_httpStatus = Lens.lens (\DisableUserResponse' {httpStatus} -> httpStatus) (\s@DisableUserResponse' {} a -> s {httpStatus = a} :: DisableUserResponse)

instance Prelude.NFData DisableUserResponse where
  rnf DisableUserResponse' {..} = Prelude.rnf httpStatus
