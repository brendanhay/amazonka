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
-- Module      : Amazonka.Redshift.GetClusterCredentialsWithIAM
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a database user name and temporary password with temporary
-- authorization to log in to an Amazon Redshift database. The database
-- user is mapped 1:1 to the source Identity and Access Management (IAM)
-- identity. For more information about IAM identities, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id.html IAM Identities (users, user groups, and roles)>
-- in the Amazon Web Services Identity and Access Management User Guide.
--
-- The Identity and Access Management (IAM) identity that runs this
-- operation must have an IAM policy attached that allows access to all
-- necessary actions and resources. For more information about permissions,
-- see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-identity-based.html Using identity-based policies (IAM policies)>
-- in the Amazon Redshift Cluster Management Guide.
module Amazonka.Redshift.GetClusterCredentialsWithIAM
  ( -- * Creating a Request
    GetClusterCredentialsWithIAM (..),
    newGetClusterCredentialsWithIAM,

    -- * Request Lenses
    getClusterCredentialsWithIAM_durationSeconds,
    getClusterCredentialsWithIAM_dbName,
    getClusterCredentialsWithIAM_clusterIdentifier,

    -- * Destructuring the Response
    GetClusterCredentialsWithIAMResponse (..),
    newGetClusterCredentialsWithIAMResponse,

    -- * Response Lenses
    getClusterCredentialsWithIAMResponse_expiration,
    getClusterCredentialsWithIAMResponse_dbPassword,
    getClusterCredentialsWithIAMResponse_nextRefreshTime,
    getClusterCredentialsWithIAMResponse_dbUser,
    getClusterCredentialsWithIAMResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetClusterCredentialsWithIAM' smart constructor.
data GetClusterCredentialsWithIAM = GetClusterCredentialsWithIAM'
  { -- | The number of seconds until the returned temporary password expires.
    --
    -- Range: 900-3600. Default: 900.
    durationSeconds :: Prelude.Maybe Prelude.Int,
    -- | The name of the database for which you are requesting credentials. If
    -- the database name is specified, the IAM policy must allow access to the
    -- resource @dbname@ for the specified database name. If the database name
    -- is not specified, access to all databases is allowed.
    dbName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the cluster that contains the database for
    -- which you are requesting credentials.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetClusterCredentialsWithIAM' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'durationSeconds', 'getClusterCredentialsWithIAM_durationSeconds' - The number of seconds until the returned temporary password expires.
--
-- Range: 900-3600. Default: 900.
--
-- 'dbName', 'getClusterCredentialsWithIAM_dbName' - The name of the database for which you are requesting credentials. If
-- the database name is specified, the IAM policy must allow access to the
-- resource @dbname@ for the specified database name. If the database name
-- is not specified, access to all databases is allowed.
--
-- 'clusterIdentifier', 'getClusterCredentialsWithIAM_clusterIdentifier' - The unique identifier of the cluster that contains the database for
-- which you are requesting credentials.
newGetClusterCredentialsWithIAM ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  GetClusterCredentialsWithIAM
newGetClusterCredentialsWithIAM pClusterIdentifier_ =
  GetClusterCredentialsWithIAM'
    { durationSeconds =
        Prelude.Nothing,
      dbName = Prelude.Nothing,
      clusterIdentifier = pClusterIdentifier_
    }

-- | The number of seconds until the returned temporary password expires.
--
-- Range: 900-3600. Default: 900.
getClusterCredentialsWithIAM_durationSeconds :: Lens.Lens' GetClusterCredentialsWithIAM (Prelude.Maybe Prelude.Int)
getClusterCredentialsWithIAM_durationSeconds = Lens.lens (\GetClusterCredentialsWithIAM' {durationSeconds} -> durationSeconds) (\s@GetClusterCredentialsWithIAM' {} a -> s {durationSeconds = a} :: GetClusterCredentialsWithIAM)

-- | The name of the database for which you are requesting credentials. If
-- the database name is specified, the IAM policy must allow access to the
-- resource @dbname@ for the specified database name. If the database name
-- is not specified, access to all databases is allowed.
getClusterCredentialsWithIAM_dbName :: Lens.Lens' GetClusterCredentialsWithIAM (Prelude.Maybe Prelude.Text)
getClusterCredentialsWithIAM_dbName = Lens.lens (\GetClusterCredentialsWithIAM' {dbName} -> dbName) (\s@GetClusterCredentialsWithIAM' {} a -> s {dbName = a} :: GetClusterCredentialsWithIAM)

-- | The unique identifier of the cluster that contains the database for
-- which you are requesting credentials.
getClusterCredentialsWithIAM_clusterIdentifier :: Lens.Lens' GetClusterCredentialsWithIAM Prelude.Text
getClusterCredentialsWithIAM_clusterIdentifier = Lens.lens (\GetClusterCredentialsWithIAM' {clusterIdentifier} -> clusterIdentifier) (\s@GetClusterCredentialsWithIAM' {} a -> s {clusterIdentifier = a} :: GetClusterCredentialsWithIAM)

instance Core.AWSRequest GetClusterCredentialsWithIAM where
  type
    AWSResponse GetClusterCredentialsWithIAM =
      GetClusterCredentialsWithIAMResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetClusterCredentialsWithIAMResult"
      ( \s h x ->
          GetClusterCredentialsWithIAMResponse'
            Prelude.<$> (x Data..@? "Expiration")
            Prelude.<*> (x Data..@? "DbPassword")
            Prelude.<*> (x Data..@? "NextRefreshTime")
            Prelude.<*> (x Data..@? "DbUser")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetClusterCredentialsWithIAM
  where
  hashWithSalt _salt GetClusterCredentialsWithIAM' {..} =
    _salt `Prelude.hashWithSalt` durationSeconds
      `Prelude.hashWithSalt` dbName
      `Prelude.hashWithSalt` clusterIdentifier

instance Prelude.NFData GetClusterCredentialsWithIAM where
  rnf GetClusterCredentialsWithIAM' {..} =
    Prelude.rnf durationSeconds
      `Prelude.seq` Prelude.rnf dbName
      `Prelude.seq` Prelude.rnf clusterIdentifier

instance Data.ToHeaders GetClusterCredentialsWithIAM where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetClusterCredentialsWithIAM where
  toPath = Prelude.const "/"

instance Data.ToQuery GetClusterCredentialsWithIAM where
  toQuery GetClusterCredentialsWithIAM' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "GetClusterCredentialsWithIAM" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "DurationSeconds" Data.=: durationSeconds,
        "DbName" Data.=: dbName,
        "ClusterIdentifier" Data.=: clusterIdentifier
      ]

-- | /See:/ 'newGetClusterCredentialsWithIAMResponse' smart constructor.
data GetClusterCredentialsWithIAMResponse = GetClusterCredentialsWithIAMResponse'
  { -- | The time (UTC) when the temporary password expires. After this
    -- timestamp, a log in with the temporary password fails.
    expiration :: Prelude.Maybe Data.ISO8601,
    -- | A temporary password that you provide when you connect to a database.
    dbPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Reserved for future use.
    nextRefreshTime :: Prelude.Maybe Data.ISO8601,
    -- | A database user name that you provide when you connect to a database.
    -- The database user is mapped 1:1 to the source IAM identity.
    dbUser :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetClusterCredentialsWithIAMResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiration', 'getClusterCredentialsWithIAMResponse_expiration' - The time (UTC) when the temporary password expires. After this
-- timestamp, a log in with the temporary password fails.
--
-- 'dbPassword', 'getClusterCredentialsWithIAMResponse_dbPassword' - A temporary password that you provide when you connect to a database.
--
-- 'nextRefreshTime', 'getClusterCredentialsWithIAMResponse_nextRefreshTime' - Reserved for future use.
--
-- 'dbUser', 'getClusterCredentialsWithIAMResponse_dbUser' - A database user name that you provide when you connect to a database.
-- The database user is mapped 1:1 to the source IAM identity.
--
-- 'httpStatus', 'getClusterCredentialsWithIAMResponse_httpStatus' - The response's http status code.
newGetClusterCredentialsWithIAMResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetClusterCredentialsWithIAMResponse
newGetClusterCredentialsWithIAMResponse pHttpStatus_ =
  GetClusterCredentialsWithIAMResponse'
    { expiration =
        Prelude.Nothing,
      dbPassword = Prelude.Nothing,
      nextRefreshTime = Prelude.Nothing,
      dbUser = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time (UTC) when the temporary password expires. After this
-- timestamp, a log in with the temporary password fails.
getClusterCredentialsWithIAMResponse_expiration :: Lens.Lens' GetClusterCredentialsWithIAMResponse (Prelude.Maybe Prelude.UTCTime)
getClusterCredentialsWithIAMResponse_expiration = Lens.lens (\GetClusterCredentialsWithIAMResponse' {expiration} -> expiration) (\s@GetClusterCredentialsWithIAMResponse' {} a -> s {expiration = a} :: GetClusterCredentialsWithIAMResponse) Prelude.. Lens.mapping Data._Time

-- | A temporary password that you provide when you connect to a database.
getClusterCredentialsWithIAMResponse_dbPassword :: Lens.Lens' GetClusterCredentialsWithIAMResponse (Prelude.Maybe Prelude.Text)
getClusterCredentialsWithIAMResponse_dbPassword = Lens.lens (\GetClusterCredentialsWithIAMResponse' {dbPassword} -> dbPassword) (\s@GetClusterCredentialsWithIAMResponse' {} a -> s {dbPassword = a} :: GetClusterCredentialsWithIAMResponse) Prelude.. Lens.mapping Data._Sensitive

-- | Reserved for future use.
getClusterCredentialsWithIAMResponse_nextRefreshTime :: Lens.Lens' GetClusterCredentialsWithIAMResponse (Prelude.Maybe Prelude.UTCTime)
getClusterCredentialsWithIAMResponse_nextRefreshTime = Lens.lens (\GetClusterCredentialsWithIAMResponse' {nextRefreshTime} -> nextRefreshTime) (\s@GetClusterCredentialsWithIAMResponse' {} a -> s {nextRefreshTime = a} :: GetClusterCredentialsWithIAMResponse) Prelude.. Lens.mapping Data._Time

-- | A database user name that you provide when you connect to a database.
-- The database user is mapped 1:1 to the source IAM identity.
getClusterCredentialsWithIAMResponse_dbUser :: Lens.Lens' GetClusterCredentialsWithIAMResponse (Prelude.Maybe Prelude.Text)
getClusterCredentialsWithIAMResponse_dbUser = Lens.lens (\GetClusterCredentialsWithIAMResponse' {dbUser} -> dbUser) (\s@GetClusterCredentialsWithIAMResponse' {} a -> s {dbUser = a} :: GetClusterCredentialsWithIAMResponse)

-- | The response's http status code.
getClusterCredentialsWithIAMResponse_httpStatus :: Lens.Lens' GetClusterCredentialsWithIAMResponse Prelude.Int
getClusterCredentialsWithIAMResponse_httpStatus = Lens.lens (\GetClusterCredentialsWithIAMResponse' {httpStatus} -> httpStatus) (\s@GetClusterCredentialsWithIAMResponse' {} a -> s {httpStatus = a} :: GetClusterCredentialsWithIAMResponse)

instance
  Prelude.NFData
    GetClusterCredentialsWithIAMResponse
  where
  rnf GetClusterCredentialsWithIAMResponse' {..} =
    Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf dbPassword
      `Prelude.seq` Prelude.rnf nextRefreshTime
      `Prelude.seq` Prelude.rnf dbUser
      `Prelude.seq` Prelude.rnf httpStatus
