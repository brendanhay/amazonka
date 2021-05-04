{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
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
    passwordVersion :: Prelude.Maybe RelationalDatabasePasswordVersion,
    -- | The name of your database for which to get the master user password.
    relationalDatabaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetRelationalDatabaseMasterUserPassword
newGetRelationalDatabaseMasterUserPassword
  pRelationalDatabaseName_ =
    GetRelationalDatabaseMasterUserPassword'
      { passwordVersion =
          Prelude.Nothing,
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
getRelationalDatabaseMasterUserPassword_passwordVersion :: Lens.Lens' GetRelationalDatabaseMasterUserPassword (Prelude.Maybe RelationalDatabasePasswordVersion)
getRelationalDatabaseMasterUserPassword_passwordVersion = Lens.lens (\GetRelationalDatabaseMasterUserPassword' {passwordVersion} -> passwordVersion) (\s@GetRelationalDatabaseMasterUserPassword' {} a -> s {passwordVersion = a} :: GetRelationalDatabaseMasterUserPassword)

-- | The name of your database for which to get the master user password.
getRelationalDatabaseMasterUserPassword_relationalDatabaseName :: Lens.Lens' GetRelationalDatabaseMasterUserPassword Prelude.Text
getRelationalDatabaseMasterUserPassword_relationalDatabaseName = Lens.lens (\GetRelationalDatabaseMasterUserPassword' {relationalDatabaseName} -> relationalDatabaseName) (\s@GetRelationalDatabaseMasterUserPassword' {} a -> s {relationalDatabaseName = a} :: GetRelationalDatabaseMasterUserPassword)

instance
  Prelude.AWSRequest
    GetRelationalDatabaseMasterUserPassword
  where
  type
    Rs GetRelationalDatabaseMasterUserPassword =
      GetRelationalDatabaseMasterUserPasswordResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseMasterUserPasswordResponse'
            Prelude.<$> (x Prelude..?> "createdAt")
              Prelude.<*> (x Prelude..?> "masterUserPassword")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetRelationalDatabaseMasterUserPassword

instance
  Prelude.NFData
    GetRelationalDatabaseMasterUserPassword

instance
  Prelude.ToHeaders
    GetRelationalDatabaseMasterUserPassword
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Lightsail_20161128.GetRelationalDatabaseMasterUserPassword" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    GetRelationalDatabaseMasterUserPassword
  where
  toJSON GetRelationalDatabaseMasterUserPassword' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("passwordVersion" Prelude..=)
              Prelude.<$> passwordVersion,
            Prelude.Just
              ( "relationalDatabaseName"
                  Prelude..= relationalDatabaseName
              )
          ]
      )

instance
  Prelude.ToPath
    GetRelationalDatabaseMasterUserPassword
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    GetRelationalDatabaseMasterUserPassword
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRelationalDatabaseMasterUserPasswordResponse' smart constructor.
data GetRelationalDatabaseMasterUserPasswordResponse = GetRelationalDatabaseMasterUserPasswordResponse'
  { -- | The timestamp when the specified version of the master user password was
    -- created.
    createdAt :: Prelude.Maybe Prelude.POSIX,
    -- | The master user password for the @password version@ specified.
    masterUserPassword :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetRelationalDatabaseMasterUserPasswordResponse
newGetRelationalDatabaseMasterUserPasswordResponse
  pHttpStatus_ =
    GetRelationalDatabaseMasterUserPasswordResponse'
      { createdAt =
          Prelude.Nothing,
        masterUserPassword =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The timestamp when the specified version of the master user password was
-- created.
getRelationalDatabaseMasterUserPasswordResponse_createdAt :: Lens.Lens' GetRelationalDatabaseMasterUserPasswordResponse (Prelude.Maybe Prelude.UTCTime)
getRelationalDatabaseMasterUserPasswordResponse_createdAt = Lens.lens (\GetRelationalDatabaseMasterUserPasswordResponse' {createdAt} -> createdAt) (\s@GetRelationalDatabaseMasterUserPasswordResponse' {} a -> s {createdAt = a} :: GetRelationalDatabaseMasterUserPasswordResponse) Prelude.. Lens.mapping Prelude._Time

-- | The master user password for the @password version@ specified.
getRelationalDatabaseMasterUserPasswordResponse_masterUserPassword :: Lens.Lens' GetRelationalDatabaseMasterUserPasswordResponse (Prelude.Maybe Prelude.Text)
getRelationalDatabaseMasterUserPasswordResponse_masterUserPassword = Lens.lens (\GetRelationalDatabaseMasterUserPasswordResponse' {masterUserPassword} -> masterUserPassword) (\s@GetRelationalDatabaseMasterUserPasswordResponse' {} a -> s {masterUserPassword = a} :: GetRelationalDatabaseMasterUserPasswordResponse) Prelude.. Lens.mapping Prelude._Sensitive

-- | The response's http status code.
getRelationalDatabaseMasterUserPasswordResponse_httpStatus :: Lens.Lens' GetRelationalDatabaseMasterUserPasswordResponse Prelude.Int
getRelationalDatabaseMasterUserPasswordResponse_httpStatus = Lens.lens (\GetRelationalDatabaseMasterUserPasswordResponse' {httpStatus} -> httpStatus) (\s@GetRelationalDatabaseMasterUserPasswordResponse' {} a -> s {httpStatus = a} :: GetRelationalDatabaseMasterUserPasswordResponse)

instance
  Prelude.NFData
    GetRelationalDatabaseMasterUserPasswordResponse
