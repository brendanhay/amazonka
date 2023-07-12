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
-- Module      : Amazonka.RedshiftServerLess.GetCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a database user name and temporary password with temporary
-- authorization to log in to Amazon Redshift Serverless.
--
-- By default, the temporary credentials expire in 900 seconds. You can
-- optionally specify a duration between 900 seconds (15 minutes) and 3600
-- seconds (60 minutes).
--
-- >  <p>The Identity and Access Management (IAM) user or role that runs GetCredentials must have an IAM policy attached that allows access to all necessary actions and resources.</p> <p>If the <code>DbName</code> parameter is specified, the IAM policy must allow access to the resource dbname for the specified database name.</p>
module Amazonka.RedshiftServerLess.GetCredentials
  ( -- * Creating a Request
    GetCredentials (..),
    newGetCredentials,

    -- * Request Lenses
    getCredentials_dbName,
    getCredentials_durationSeconds,
    getCredentials_workgroupName,

    -- * Destructuring the Response
    GetCredentialsResponse (..),
    newGetCredentialsResponse,

    -- * Response Lenses
    getCredentialsResponse_dbPassword,
    getCredentialsResponse_dbUser,
    getCredentialsResponse_expiration,
    getCredentialsResponse_nextRefreshTime,
    getCredentialsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCredentials' smart constructor.
data GetCredentials = GetCredentials'
  { -- | The name of the database to get temporary authorization to log on to.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 64 alphanumeric characters or hyphens.
    --
    -- -   Must contain only uppercase or lowercase letters, numbers,
    --     underscore, plus sign, period (dot), at symbol (\@), or hyphen.
    --
    -- -   The first character must be a letter.
    --
    -- -   Must not contain a colon ( : ) or slash ( \/ ).
    --
    -- -   Cannot be a reserved word. A list of reserved words can be found in
    --     <https://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
    --     in the Amazon Redshift Database Developer Guide
    dbName :: Prelude.Maybe Prelude.Text,
    -- | The number of seconds until the returned temporary password expires. The
    -- minimum is 900 seconds, and the maximum is 3600 seconds.
    durationSeconds :: Prelude.Maybe Prelude.Int,
    -- | The name of the workgroup associated with the database.
    workgroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbName', 'getCredentials_dbName' - The name of the database to get temporary authorization to log on to.
--
-- Constraints:
--
-- -   Must be 1 to 64 alphanumeric characters or hyphens.
--
-- -   Must contain only uppercase or lowercase letters, numbers,
--     underscore, plus sign, period (dot), at symbol (\@), or hyphen.
--
-- -   The first character must be a letter.
--
-- -   Must not contain a colon ( : ) or slash ( \/ ).
--
-- -   Cannot be a reserved word. A list of reserved words can be found in
--     <https://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
--     in the Amazon Redshift Database Developer Guide
--
-- 'durationSeconds', 'getCredentials_durationSeconds' - The number of seconds until the returned temporary password expires. The
-- minimum is 900 seconds, and the maximum is 3600 seconds.
--
-- 'workgroupName', 'getCredentials_workgroupName' - The name of the workgroup associated with the database.
newGetCredentials ::
  -- | 'workgroupName'
  Prelude.Text ->
  GetCredentials
newGetCredentials pWorkgroupName_ =
  GetCredentials'
    { dbName = Prelude.Nothing,
      durationSeconds = Prelude.Nothing,
      workgroupName = pWorkgroupName_
    }

-- | The name of the database to get temporary authorization to log on to.
--
-- Constraints:
--
-- -   Must be 1 to 64 alphanumeric characters or hyphens.
--
-- -   Must contain only uppercase or lowercase letters, numbers,
--     underscore, plus sign, period (dot), at symbol (\@), or hyphen.
--
-- -   The first character must be a letter.
--
-- -   Must not contain a colon ( : ) or slash ( \/ ).
--
-- -   Cannot be a reserved word. A list of reserved words can be found in
--     <https://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
--     in the Amazon Redshift Database Developer Guide
getCredentials_dbName :: Lens.Lens' GetCredentials (Prelude.Maybe Prelude.Text)
getCredentials_dbName = Lens.lens (\GetCredentials' {dbName} -> dbName) (\s@GetCredentials' {} a -> s {dbName = a} :: GetCredentials)

-- | The number of seconds until the returned temporary password expires. The
-- minimum is 900 seconds, and the maximum is 3600 seconds.
getCredentials_durationSeconds :: Lens.Lens' GetCredentials (Prelude.Maybe Prelude.Int)
getCredentials_durationSeconds = Lens.lens (\GetCredentials' {durationSeconds} -> durationSeconds) (\s@GetCredentials' {} a -> s {durationSeconds = a} :: GetCredentials)

-- | The name of the workgroup associated with the database.
getCredentials_workgroupName :: Lens.Lens' GetCredentials Prelude.Text
getCredentials_workgroupName = Lens.lens (\GetCredentials' {workgroupName} -> workgroupName) (\s@GetCredentials' {} a -> s {workgroupName = a} :: GetCredentials)

instance Core.AWSRequest GetCredentials where
  type
    AWSResponse GetCredentials =
      GetCredentialsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCredentialsResponse'
            Prelude.<$> (x Data..?> "dbPassword")
            Prelude.<*> (x Data..?> "dbUser")
            Prelude.<*> (x Data..?> "expiration")
            Prelude.<*> (x Data..?> "nextRefreshTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCredentials where
  hashWithSalt _salt GetCredentials' {..} =
    _salt
      `Prelude.hashWithSalt` dbName
      `Prelude.hashWithSalt` durationSeconds
      `Prelude.hashWithSalt` workgroupName

instance Prelude.NFData GetCredentials where
  rnf GetCredentials' {..} =
    Prelude.rnf dbName
      `Prelude.seq` Prelude.rnf durationSeconds
      `Prelude.seq` Prelude.rnf workgroupName

instance Data.ToHeaders GetCredentials where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.GetCredentials" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetCredentials where
  toJSON GetCredentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("dbName" Data..=) Prelude.<$> dbName,
            ("durationSeconds" Data..=)
              Prelude.<$> durationSeconds,
            Prelude.Just
              ("workgroupName" Data..= workgroupName)
          ]
      )

instance Data.ToPath GetCredentials where
  toPath = Prelude.const "/"

instance Data.ToQuery GetCredentials where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCredentialsResponse' smart constructor.
data GetCredentialsResponse = GetCredentialsResponse'
  { -- | A temporary password that authorizes the user name returned by @DbUser@
    -- to log on to the database @DbName@.
    dbPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A database user name that is authorized to log on to the database
    -- @DbName@ using the password @DbPassword@. If the specified @DbUser@
    -- exists in the database, the new user name has the same database
    -- privileges as the the user named in @DbUser@. By default, the user is
    -- added to PUBLIC.
    dbUser :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The date and time the password in @DbPassword@ expires.
    expiration :: Prelude.Maybe Data.POSIX,
    -- | The date and time of when the @DbUser@ and @DbPassword@ authorization
    -- refreshes.
    nextRefreshTime :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCredentialsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbPassword', 'getCredentialsResponse_dbPassword' - A temporary password that authorizes the user name returned by @DbUser@
-- to log on to the database @DbName@.
--
-- 'dbUser', 'getCredentialsResponse_dbUser' - A database user name that is authorized to log on to the database
-- @DbName@ using the password @DbPassword@. If the specified @DbUser@
-- exists in the database, the new user name has the same database
-- privileges as the the user named in @DbUser@. By default, the user is
-- added to PUBLIC.
--
-- 'expiration', 'getCredentialsResponse_expiration' - The date and time the password in @DbPassword@ expires.
--
-- 'nextRefreshTime', 'getCredentialsResponse_nextRefreshTime' - The date and time of when the @DbUser@ and @DbPassword@ authorization
-- refreshes.
--
-- 'httpStatus', 'getCredentialsResponse_httpStatus' - The response's http status code.
newGetCredentialsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCredentialsResponse
newGetCredentialsResponse pHttpStatus_ =
  GetCredentialsResponse'
    { dbPassword =
        Prelude.Nothing,
      dbUser = Prelude.Nothing,
      expiration = Prelude.Nothing,
      nextRefreshTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A temporary password that authorizes the user name returned by @DbUser@
-- to log on to the database @DbName@.
getCredentialsResponse_dbPassword :: Lens.Lens' GetCredentialsResponse (Prelude.Maybe Prelude.Text)
getCredentialsResponse_dbPassword = Lens.lens (\GetCredentialsResponse' {dbPassword} -> dbPassword) (\s@GetCredentialsResponse' {} a -> s {dbPassword = a} :: GetCredentialsResponse) Prelude.. Lens.mapping Data._Sensitive

-- | A database user name that is authorized to log on to the database
-- @DbName@ using the password @DbPassword@. If the specified @DbUser@
-- exists in the database, the new user name has the same database
-- privileges as the the user named in @DbUser@. By default, the user is
-- added to PUBLIC.
getCredentialsResponse_dbUser :: Lens.Lens' GetCredentialsResponse (Prelude.Maybe Prelude.Text)
getCredentialsResponse_dbUser = Lens.lens (\GetCredentialsResponse' {dbUser} -> dbUser) (\s@GetCredentialsResponse' {} a -> s {dbUser = a} :: GetCredentialsResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The date and time the password in @DbPassword@ expires.
getCredentialsResponse_expiration :: Lens.Lens' GetCredentialsResponse (Prelude.Maybe Prelude.UTCTime)
getCredentialsResponse_expiration = Lens.lens (\GetCredentialsResponse' {expiration} -> expiration) (\s@GetCredentialsResponse' {} a -> s {expiration = a} :: GetCredentialsResponse) Prelude.. Lens.mapping Data._Time

-- | The date and time of when the @DbUser@ and @DbPassword@ authorization
-- refreshes.
getCredentialsResponse_nextRefreshTime :: Lens.Lens' GetCredentialsResponse (Prelude.Maybe Prelude.UTCTime)
getCredentialsResponse_nextRefreshTime = Lens.lens (\GetCredentialsResponse' {nextRefreshTime} -> nextRefreshTime) (\s@GetCredentialsResponse' {} a -> s {nextRefreshTime = a} :: GetCredentialsResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getCredentialsResponse_httpStatus :: Lens.Lens' GetCredentialsResponse Prelude.Int
getCredentialsResponse_httpStatus = Lens.lens (\GetCredentialsResponse' {httpStatus} -> httpStatus) (\s@GetCredentialsResponse' {} a -> s {httpStatus = a} :: GetCredentialsResponse)

instance Prelude.NFData GetCredentialsResponse where
  rnf GetCredentialsResponse' {..} =
    Prelude.rnf dbPassword
      `Prelude.seq` Prelude.rnf dbUser
      `Prelude.seq` Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf nextRefreshTime
      `Prelude.seq` Prelude.rnf httpStatus
