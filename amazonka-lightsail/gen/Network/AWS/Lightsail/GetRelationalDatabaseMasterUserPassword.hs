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
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseMasterUserPassword
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current, previous, or pending versions of the master user
-- password for a Lightsail database.
--
-- The @GetRelationalDatabaseMasterUserPassword@ operation supports
-- tag-based access control via resource tags applied to the resource
-- identified by relationalDatabaseName.
module Network.AWS.Lightsail.GetRelationalDatabaseMasterUserPassword
  ( -- * Creating a Request
    GetRelationalDatabaseMasterUserPassword (..),
    newGetRelationalDatabaseMasterUserPassword,

    -- * Request Lenses
    getRelationalDatabaseMasterUserPassword_passwordVersion,
    getRelationalDatabaseMasterUserPassword_relationalDatabaseName,

    -- * Destructuring the Response
    GetRelationalDatabaseMasterUserPasswordResponse (..),
    newGetRelationalDatabaseMasterUserPasswordResponse,

    -- * Response Lenses
    getRelationalDatabaseMasterUserPasswordResponse_createdAt,
    getRelationalDatabaseMasterUserPasswordResponse_masterUserPassword,
    getRelationalDatabaseMasterUserPasswordResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRelationalDatabaseMasterUserPassword' smart constructor.
data GetRelationalDatabaseMasterUserPassword = GetRelationalDatabaseMasterUserPassword'
  { -- | The password version to return.
    --
    -- Specifying @CURRENT@ or @PREVIOUS@ returns the current or previous
    -- passwords respectively. Specifying @PENDING@ returns the newest version
    -- of the password that will rotate to @CURRENT@. After the @PENDING@
    -- password rotates to @CURRENT@, the @PENDING@ password is no longer
    -- available.
    --
    -- Default: @CURRENT@
    passwordVersion :: Core.Maybe RelationalDatabasePasswordVersion,
    -- | The name of your database for which to get the master user password.
    relationalDatabaseName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRelationalDatabaseMasterUserPassword' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'passwordVersion', 'getRelationalDatabaseMasterUserPassword_passwordVersion' - The password version to return.
--
-- Specifying @CURRENT@ or @PREVIOUS@ returns the current or previous
-- passwords respectively. Specifying @PENDING@ returns the newest version
-- of the password that will rotate to @CURRENT@. After the @PENDING@
-- password rotates to @CURRENT@, the @PENDING@ password is no longer
-- available.
--
-- Default: @CURRENT@
--
-- 'relationalDatabaseName', 'getRelationalDatabaseMasterUserPassword_relationalDatabaseName' - The name of your database for which to get the master user password.
newGetRelationalDatabaseMasterUserPassword ::
  -- | 'relationalDatabaseName'
  Core.Text ->
  GetRelationalDatabaseMasterUserPassword
newGetRelationalDatabaseMasterUserPassword
  pRelationalDatabaseName_ =
    GetRelationalDatabaseMasterUserPassword'
      { passwordVersion =
          Core.Nothing,
        relationalDatabaseName =
          pRelationalDatabaseName_
      }

-- | The password version to return.
--
-- Specifying @CURRENT@ or @PREVIOUS@ returns the current or previous
-- passwords respectively. Specifying @PENDING@ returns the newest version
-- of the password that will rotate to @CURRENT@. After the @PENDING@
-- password rotates to @CURRENT@, the @PENDING@ password is no longer
-- available.
--
-- Default: @CURRENT@
getRelationalDatabaseMasterUserPassword_passwordVersion :: Lens.Lens' GetRelationalDatabaseMasterUserPassword (Core.Maybe RelationalDatabasePasswordVersion)
getRelationalDatabaseMasterUserPassword_passwordVersion = Lens.lens (\GetRelationalDatabaseMasterUserPassword' {passwordVersion} -> passwordVersion) (\s@GetRelationalDatabaseMasterUserPassword' {} a -> s {passwordVersion = a} :: GetRelationalDatabaseMasterUserPassword)

-- | The name of your database for which to get the master user password.
getRelationalDatabaseMasterUserPassword_relationalDatabaseName :: Lens.Lens' GetRelationalDatabaseMasterUserPassword Core.Text
getRelationalDatabaseMasterUserPassword_relationalDatabaseName = Lens.lens (\GetRelationalDatabaseMasterUserPassword' {relationalDatabaseName} -> relationalDatabaseName) (\s@GetRelationalDatabaseMasterUserPassword' {} a -> s {relationalDatabaseName = a} :: GetRelationalDatabaseMasterUserPassword)

instance
  Core.AWSRequest
    GetRelationalDatabaseMasterUserPassword
  where
  type
    AWSResponse
      GetRelationalDatabaseMasterUserPassword =
      GetRelationalDatabaseMasterUserPasswordResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseMasterUserPasswordResponse'
            Core.<$> (x Core..?> "createdAt")
              Core.<*> (x Core..?> "masterUserPassword")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetRelationalDatabaseMasterUserPassword

instance
  Core.NFData
    GetRelationalDatabaseMasterUserPassword

instance
  Core.ToHeaders
    GetRelationalDatabaseMasterUserPassword
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetRelationalDatabaseMasterUserPassword" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    GetRelationalDatabaseMasterUserPassword
  where
  toJSON GetRelationalDatabaseMasterUserPassword' {..} =
    Core.object
      ( Core.catMaybes
          [ ("passwordVersion" Core..=)
              Core.<$> passwordVersion,
            Core.Just
              ( "relationalDatabaseName"
                  Core..= relationalDatabaseName
              )
          ]
      )

instance
  Core.ToPath
    GetRelationalDatabaseMasterUserPassword
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetRelationalDatabaseMasterUserPassword
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetRelationalDatabaseMasterUserPasswordResponse' smart constructor.
data GetRelationalDatabaseMasterUserPasswordResponse = GetRelationalDatabaseMasterUserPasswordResponse'
  { -- | The timestamp when the specified version of the master user password was
    -- created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The master user password for the @password version@ specified.
    masterUserPassword :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetRelationalDatabaseMasterUserPasswordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAt', 'getRelationalDatabaseMasterUserPasswordResponse_createdAt' - The timestamp when the specified version of the master user password was
-- created.
--
-- 'masterUserPassword', 'getRelationalDatabaseMasterUserPasswordResponse_masterUserPassword' - The master user password for the @password version@ specified.
--
-- 'httpStatus', 'getRelationalDatabaseMasterUserPasswordResponse_httpStatus' - The response's http status code.
newGetRelationalDatabaseMasterUserPasswordResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetRelationalDatabaseMasterUserPasswordResponse
newGetRelationalDatabaseMasterUserPasswordResponse
  pHttpStatus_ =
    GetRelationalDatabaseMasterUserPasswordResponse'
      { createdAt =
          Core.Nothing,
        masterUserPassword =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The timestamp when the specified version of the master user password was
-- created.
getRelationalDatabaseMasterUserPasswordResponse_createdAt :: Lens.Lens' GetRelationalDatabaseMasterUserPasswordResponse (Core.Maybe Core.UTCTime)
getRelationalDatabaseMasterUserPasswordResponse_createdAt = Lens.lens (\GetRelationalDatabaseMasterUserPasswordResponse' {createdAt} -> createdAt) (\s@GetRelationalDatabaseMasterUserPasswordResponse' {} a -> s {createdAt = a} :: GetRelationalDatabaseMasterUserPasswordResponse) Core.. Lens.mapping Core._Time

-- | The master user password for the @password version@ specified.
getRelationalDatabaseMasterUserPasswordResponse_masterUserPassword :: Lens.Lens' GetRelationalDatabaseMasterUserPasswordResponse (Core.Maybe Core.Text)
getRelationalDatabaseMasterUserPasswordResponse_masterUserPassword = Lens.lens (\GetRelationalDatabaseMasterUserPasswordResponse' {masterUserPassword} -> masterUserPassword) (\s@GetRelationalDatabaseMasterUserPasswordResponse' {} a -> s {masterUserPassword = a} :: GetRelationalDatabaseMasterUserPasswordResponse) Core.. Lens.mapping Core._Sensitive

-- | The response's http status code.
getRelationalDatabaseMasterUserPasswordResponse_httpStatus :: Lens.Lens' GetRelationalDatabaseMasterUserPasswordResponse Core.Int
getRelationalDatabaseMasterUserPasswordResponse_httpStatus = Lens.lens (\GetRelationalDatabaseMasterUserPasswordResponse' {httpStatus} -> httpStatus) (\s@GetRelationalDatabaseMasterUserPasswordResponse' {} a -> s {httpStatus = a} :: GetRelationalDatabaseMasterUserPasswordResponse)

instance
  Core.NFData
    GetRelationalDatabaseMasterUserPasswordResponse
