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
-- Module      : Amazonka.DirectoryService.ResetUserPassword
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resets the password for any user in your Managed Microsoft AD or Simple
-- AD directory.
--
-- You can reset the password for any user in your directory with the
-- following exceptions:
--
-- -   For Simple AD, you cannot reset the password for any user that is a
--     member of either the __Domain Admins__ or __Enterprise Admins__
--     group except for the administrator user.
--
-- -   For Managed Microsoft AD, you can only reset the password for a user
--     that is in an OU based off of the NetBIOS name that you typed when
--     you created your directory. For example, you cannot reset the
--     password for a user in the __Amazon Web Services Reserved__ OU. For
--     more information about the OU structure for an Managed Microsoft AD
--     directory, see
--     <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/ms_ad_getting_started_what_gets_created.html What Gets Created>
--     in the /Directory Service Administration Guide/.
module Amazonka.DirectoryService.ResetUserPassword
  ( -- * Creating a Request
    ResetUserPassword (..),
    newResetUserPassword,

    -- * Request Lenses
    resetUserPassword_directoryId,
    resetUserPassword_userName,
    resetUserPassword_newPassword,

    -- * Destructuring the Response
    ResetUserPasswordResponse (..),
    newResetUserPasswordResponse,

    -- * Response Lenses
    resetUserPasswordResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newResetUserPassword' smart constructor.
data ResetUserPassword = ResetUserPassword'
  { -- | Identifier of the Managed Microsoft AD or Simple AD directory in which
    -- the user resides.
    directoryId :: Prelude.Text,
    -- | The user name of the user whose password will be reset.
    userName :: Prelude.Text,
    -- | The new password that will be reset.
    newPassword' :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetUserPassword' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'resetUserPassword_directoryId' - Identifier of the Managed Microsoft AD or Simple AD directory in which
-- the user resides.
--
-- 'userName', 'resetUserPassword_userName' - The user name of the user whose password will be reset.
--
-- 'newPassword'', 'resetUserPassword_newPassword' - The new password that will be reset.
newResetUserPassword ::
  -- | 'directoryId'
  Prelude.Text ->
  -- | 'userName'
  Prelude.Text ->
  -- | 'newPassword''
  Prelude.Text ->
  ResetUserPassword
newResetUserPassword
  pDirectoryId_
  pUserName_
  pNewPassword_ =
    ResetUserPassword'
      { directoryId = pDirectoryId_,
        userName = pUserName_,
        newPassword' = Data._Sensitive Lens.# pNewPassword_
      }

-- | Identifier of the Managed Microsoft AD or Simple AD directory in which
-- the user resides.
resetUserPassword_directoryId :: Lens.Lens' ResetUserPassword Prelude.Text
resetUserPassword_directoryId = Lens.lens (\ResetUserPassword' {directoryId} -> directoryId) (\s@ResetUserPassword' {} a -> s {directoryId = a} :: ResetUserPassword)

-- | The user name of the user whose password will be reset.
resetUserPassword_userName :: Lens.Lens' ResetUserPassword Prelude.Text
resetUserPassword_userName = Lens.lens (\ResetUserPassword' {userName} -> userName) (\s@ResetUserPassword' {} a -> s {userName = a} :: ResetUserPassword)

-- | The new password that will be reset.
resetUserPassword_newPassword :: Lens.Lens' ResetUserPassword Prelude.Text
resetUserPassword_newPassword = Lens.lens (\ResetUserPassword' {newPassword'} -> newPassword') (\s@ResetUserPassword' {} a -> s {newPassword' = a} :: ResetUserPassword) Prelude.. Data._Sensitive

instance Core.AWSRequest ResetUserPassword where
  type
    AWSResponse ResetUserPassword =
      ResetUserPasswordResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ResetUserPasswordResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ResetUserPassword where
  hashWithSalt _salt ResetUserPassword' {..} =
    _salt
      `Prelude.hashWithSalt` directoryId
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` newPassword'

instance Prelude.NFData ResetUserPassword where
  rnf ResetUserPassword' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf newPassword'

instance Data.ToHeaders ResetUserPassword where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.ResetUserPassword" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ResetUserPassword where
  toJSON ResetUserPassword' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DirectoryId" Data..= directoryId),
            Prelude.Just ("UserName" Data..= userName),
            Prelude.Just ("NewPassword" Data..= newPassword')
          ]
      )

instance Data.ToPath ResetUserPassword where
  toPath = Prelude.const "/"

instance Data.ToQuery ResetUserPassword where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newResetUserPasswordResponse' smart constructor.
data ResetUserPasswordResponse = ResetUserPasswordResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResetUserPasswordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'resetUserPasswordResponse_httpStatus' - The response's http status code.
newResetUserPasswordResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ResetUserPasswordResponse
newResetUserPasswordResponse pHttpStatus_ =
  ResetUserPasswordResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
resetUserPasswordResponse_httpStatus :: Lens.Lens' ResetUserPasswordResponse Prelude.Int
resetUserPasswordResponse_httpStatus = Lens.lens (\ResetUserPasswordResponse' {httpStatus} -> httpStatus) (\s@ResetUserPasswordResponse' {} a -> s {httpStatus = a} :: ResetUserPasswordResponse)

instance Prelude.NFData ResetUserPasswordResponse where
  rnf ResetUserPasswordResponse' {..} =
    Prelude.rnf httpStatus
