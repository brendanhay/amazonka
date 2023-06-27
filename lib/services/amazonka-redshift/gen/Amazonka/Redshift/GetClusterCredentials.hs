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
-- Module      : Amazonka.Redshift.GetClusterCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a database user name and temporary password with temporary
-- authorization to log on to an Amazon Redshift database. The action
-- returns the database user name prefixed with @IAM:@ if @AutoCreate@ is
-- @False@ or @IAMA:@ if @AutoCreate@ is @True@. You can optionally specify
-- one or more database user groups that the user will join at log on. By
-- default, the temporary credentials expire in 900 seconds. You can
-- optionally specify a duration between 900 seconds (15 minutes) and 3600
-- seconds (60 minutes). For more information, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/generating-user-credentials.html Using IAM Authentication to Generate Database User Credentials>
-- in the Amazon Redshift Cluster Management Guide.
--
-- The Identity and Access Management (IAM) user or role that runs
-- GetClusterCredentials must have an IAM policy attached that allows
-- access to all necessary actions and resources. For more information
-- about permissions, see
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-identity-based.html#redshift-policy-resources.getclustercredentials-resources Resource Policies for GetClusterCredentials>
-- in the Amazon Redshift Cluster Management Guide.
--
-- If the @DbGroups@ parameter is specified, the IAM policy must allow the
-- @redshift:JoinGroup@ action with access to the listed @dbgroups@.
--
-- In addition, if the @AutoCreate@ parameter is set to @True@, then the
-- policy must include the @redshift:CreateClusterUser@ permission.
--
-- If the @DbName@ parameter is specified, the IAM policy must allow access
-- to the resource @dbname@ for the specified database name.
module Amazonka.Redshift.GetClusterCredentials
  ( -- * Creating a Request
    GetClusterCredentials (..),
    newGetClusterCredentials,

    -- * Request Lenses
    getClusterCredentials_autoCreate,
    getClusterCredentials_clusterIdentifier,
    getClusterCredentials_customDomainName,
    getClusterCredentials_dbGroups,
    getClusterCredentials_dbName,
    getClusterCredentials_durationSeconds,
    getClusterCredentials_dbUser,

    -- * Destructuring the Response
    GetClusterCredentialsResponse (..),
    newGetClusterCredentialsResponse,

    -- * Response Lenses
    getClusterCredentialsResponse_dbPassword,
    getClusterCredentialsResponse_dbUser,
    getClusterCredentialsResponse_expiration,
    getClusterCredentialsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request parameters to get cluster credentials.
--
-- /See:/ 'newGetClusterCredentials' smart constructor.
data GetClusterCredentials = GetClusterCredentials'
  { -- | Create a database user with the name specified for the user named in
    -- @DbUser@ if one does not exist.
    autoCreate :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier of the cluster that contains the database for
    -- which you are requesting credentials. This parameter is case sensitive.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The custom domain name for the cluster credentials.
    customDomainName :: Prelude.Maybe Prelude.Text,
    -- | A list of the names of existing database groups that the user named in
    -- @DbUser@ will join for the current session, in addition to any group
    -- memberships for an existing user. If not specified, a new user is added
    -- only to PUBLIC.
    --
    -- Database group name constraints
    --
    -- -   Must be 1 to 64 alphanumeric characters or hyphens
    --
    -- -   Must contain only lowercase letters, numbers, underscore, plus sign,
    --     period (dot), at symbol (\@), or hyphen.
    --
    -- -   First character must be a letter.
    --
    -- -   Must not contain a colon ( : ) or slash ( \/ ).
    --
    -- -   Cannot be a reserved word. A list of reserved words can be found in
    --     <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
    --     in the Amazon Redshift Database Developer Guide.
    dbGroups :: Prelude.Maybe [Prelude.Text],
    -- | The name of a database that @DbUser@ is authorized to log on to. If
    -- @DbName@ is not specified, @DbUser@ can log on to any existing database.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 64 alphanumeric characters or hyphens
    --
    -- -   Must contain uppercase or lowercase letters, numbers, underscore,
    --     plus sign, period (dot), at symbol (\@), or hyphen.
    --
    -- -   First character must be a letter.
    --
    -- -   Must not contain a colon ( : ) or slash ( \/ ).
    --
    -- -   Cannot be a reserved word. A list of reserved words can be found in
    --     <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
    --     in the Amazon Redshift Database Developer Guide.
    dbName :: Prelude.Maybe Prelude.Text,
    -- | The number of seconds until the returned temporary password expires.
    --
    -- Constraint: minimum 900, maximum 3600.
    --
    -- Default: 900
    durationSeconds :: Prelude.Maybe Prelude.Int,
    -- | The name of a database user. If a user name matching @DbUser@ exists in
    -- the database, the temporary user credentials have the same permissions
    -- as the existing user. If @DbUser@ doesn\'t exist in the database and
    -- @Autocreate@ is @True@, a new user is created using the value for
    -- @DbUser@ with PUBLIC permissions. If a database user matching the value
    -- for @DbUser@ doesn\'t exist and @Autocreate@ is @False@, then the
    -- command succeeds but the connection attempt will fail because the user
    -- doesn\'t exist in the database.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/redshift/latest/dg/r_CREATE_USER.html CREATE USER>
    -- in the Amazon Redshift Database Developer Guide.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 64 alphanumeric characters or hyphens. The user name
    --     can\'t be @PUBLIC@.
    --
    -- -   Must contain uppercase or lowercase letters, numbers, underscore,
    --     plus sign, period (dot), at symbol (\@), or hyphen.
    --
    -- -   First character must be a letter.
    --
    -- -   Must not contain a colon ( : ) or slash ( \/ ).
    --
    -- -   Cannot be a reserved word. A list of reserved words can be found in
    --     <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
    --     in the Amazon Redshift Database Developer Guide.
    dbUser :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetClusterCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoCreate', 'getClusterCredentials_autoCreate' - Create a database user with the name specified for the user named in
-- @DbUser@ if one does not exist.
--
-- 'clusterIdentifier', 'getClusterCredentials_clusterIdentifier' - The unique identifier of the cluster that contains the database for
-- which you are requesting credentials. This parameter is case sensitive.
--
-- 'customDomainName', 'getClusterCredentials_customDomainName' - The custom domain name for the cluster credentials.
--
-- 'dbGroups', 'getClusterCredentials_dbGroups' - A list of the names of existing database groups that the user named in
-- @DbUser@ will join for the current session, in addition to any group
-- memberships for an existing user. If not specified, a new user is added
-- only to PUBLIC.
--
-- Database group name constraints
--
-- -   Must be 1 to 64 alphanumeric characters or hyphens
--
-- -   Must contain only lowercase letters, numbers, underscore, plus sign,
--     period (dot), at symbol (\@), or hyphen.
--
-- -   First character must be a letter.
--
-- -   Must not contain a colon ( : ) or slash ( \/ ).
--
-- -   Cannot be a reserved word. A list of reserved words can be found in
--     <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
--     in the Amazon Redshift Database Developer Guide.
--
-- 'dbName', 'getClusterCredentials_dbName' - The name of a database that @DbUser@ is authorized to log on to. If
-- @DbName@ is not specified, @DbUser@ can log on to any existing database.
--
-- Constraints:
--
-- -   Must be 1 to 64 alphanumeric characters or hyphens
--
-- -   Must contain uppercase or lowercase letters, numbers, underscore,
--     plus sign, period (dot), at symbol (\@), or hyphen.
--
-- -   First character must be a letter.
--
-- -   Must not contain a colon ( : ) or slash ( \/ ).
--
-- -   Cannot be a reserved word. A list of reserved words can be found in
--     <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
--     in the Amazon Redshift Database Developer Guide.
--
-- 'durationSeconds', 'getClusterCredentials_durationSeconds' - The number of seconds until the returned temporary password expires.
--
-- Constraint: minimum 900, maximum 3600.
--
-- Default: 900
--
-- 'dbUser', 'getClusterCredentials_dbUser' - The name of a database user. If a user name matching @DbUser@ exists in
-- the database, the temporary user credentials have the same permissions
-- as the existing user. If @DbUser@ doesn\'t exist in the database and
-- @Autocreate@ is @True@, a new user is created using the value for
-- @DbUser@ with PUBLIC permissions. If a database user matching the value
-- for @DbUser@ doesn\'t exist and @Autocreate@ is @False@, then the
-- command succeeds but the connection attempt will fail because the user
-- doesn\'t exist in the database.
--
-- For more information, see
-- <https://docs.aws.amazon.com/redshift/latest/dg/r_CREATE_USER.html CREATE USER>
-- in the Amazon Redshift Database Developer Guide.
--
-- Constraints:
--
-- -   Must be 1 to 64 alphanumeric characters or hyphens. The user name
--     can\'t be @PUBLIC@.
--
-- -   Must contain uppercase or lowercase letters, numbers, underscore,
--     plus sign, period (dot), at symbol (\@), or hyphen.
--
-- -   First character must be a letter.
--
-- -   Must not contain a colon ( : ) or slash ( \/ ).
--
-- -   Cannot be a reserved word. A list of reserved words can be found in
--     <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
--     in the Amazon Redshift Database Developer Guide.
newGetClusterCredentials ::
  -- | 'dbUser'
  Prelude.Text ->
  GetClusterCredentials
newGetClusterCredentials pDbUser_ =
  GetClusterCredentials'
    { autoCreate =
        Prelude.Nothing,
      clusterIdentifier = Prelude.Nothing,
      customDomainName = Prelude.Nothing,
      dbGroups = Prelude.Nothing,
      dbName = Prelude.Nothing,
      durationSeconds = Prelude.Nothing,
      dbUser = pDbUser_
    }

-- | Create a database user with the name specified for the user named in
-- @DbUser@ if one does not exist.
getClusterCredentials_autoCreate :: Lens.Lens' GetClusterCredentials (Prelude.Maybe Prelude.Bool)
getClusterCredentials_autoCreate = Lens.lens (\GetClusterCredentials' {autoCreate} -> autoCreate) (\s@GetClusterCredentials' {} a -> s {autoCreate = a} :: GetClusterCredentials)

-- | The unique identifier of the cluster that contains the database for
-- which you are requesting credentials. This parameter is case sensitive.
getClusterCredentials_clusterIdentifier :: Lens.Lens' GetClusterCredentials (Prelude.Maybe Prelude.Text)
getClusterCredentials_clusterIdentifier = Lens.lens (\GetClusterCredentials' {clusterIdentifier} -> clusterIdentifier) (\s@GetClusterCredentials' {} a -> s {clusterIdentifier = a} :: GetClusterCredentials)

-- | The custom domain name for the cluster credentials.
getClusterCredentials_customDomainName :: Lens.Lens' GetClusterCredentials (Prelude.Maybe Prelude.Text)
getClusterCredentials_customDomainName = Lens.lens (\GetClusterCredentials' {customDomainName} -> customDomainName) (\s@GetClusterCredentials' {} a -> s {customDomainName = a} :: GetClusterCredentials)

-- | A list of the names of existing database groups that the user named in
-- @DbUser@ will join for the current session, in addition to any group
-- memberships for an existing user. If not specified, a new user is added
-- only to PUBLIC.
--
-- Database group name constraints
--
-- -   Must be 1 to 64 alphanumeric characters or hyphens
--
-- -   Must contain only lowercase letters, numbers, underscore, plus sign,
--     period (dot), at symbol (\@), or hyphen.
--
-- -   First character must be a letter.
--
-- -   Must not contain a colon ( : ) or slash ( \/ ).
--
-- -   Cannot be a reserved word. A list of reserved words can be found in
--     <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
--     in the Amazon Redshift Database Developer Guide.
getClusterCredentials_dbGroups :: Lens.Lens' GetClusterCredentials (Prelude.Maybe [Prelude.Text])
getClusterCredentials_dbGroups = Lens.lens (\GetClusterCredentials' {dbGroups} -> dbGroups) (\s@GetClusterCredentials' {} a -> s {dbGroups = a} :: GetClusterCredentials) Prelude.. Lens.mapping Lens.coerced

-- | The name of a database that @DbUser@ is authorized to log on to. If
-- @DbName@ is not specified, @DbUser@ can log on to any existing database.
--
-- Constraints:
--
-- -   Must be 1 to 64 alphanumeric characters or hyphens
--
-- -   Must contain uppercase or lowercase letters, numbers, underscore,
--     plus sign, period (dot), at symbol (\@), or hyphen.
--
-- -   First character must be a letter.
--
-- -   Must not contain a colon ( : ) or slash ( \/ ).
--
-- -   Cannot be a reserved word. A list of reserved words can be found in
--     <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
--     in the Amazon Redshift Database Developer Guide.
getClusterCredentials_dbName :: Lens.Lens' GetClusterCredentials (Prelude.Maybe Prelude.Text)
getClusterCredentials_dbName = Lens.lens (\GetClusterCredentials' {dbName} -> dbName) (\s@GetClusterCredentials' {} a -> s {dbName = a} :: GetClusterCredentials)

-- | The number of seconds until the returned temporary password expires.
--
-- Constraint: minimum 900, maximum 3600.
--
-- Default: 900
getClusterCredentials_durationSeconds :: Lens.Lens' GetClusterCredentials (Prelude.Maybe Prelude.Int)
getClusterCredentials_durationSeconds = Lens.lens (\GetClusterCredentials' {durationSeconds} -> durationSeconds) (\s@GetClusterCredentials' {} a -> s {durationSeconds = a} :: GetClusterCredentials)

-- | The name of a database user. If a user name matching @DbUser@ exists in
-- the database, the temporary user credentials have the same permissions
-- as the existing user. If @DbUser@ doesn\'t exist in the database and
-- @Autocreate@ is @True@, a new user is created using the value for
-- @DbUser@ with PUBLIC permissions. If a database user matching the value
-- for @DbUser@ doesn\'t exist and @Autocreate@ is @False@, then the
-- command succeeds but the connection attempt will fail because the user
-- doesn\'t exist in the database.
--
-- For more information, see
-- <https://docs.aws.amazon.com/redshift/latest/dg/r_CREATE_USER.html CREATE USER>
-- in the Amazon Redshift Database Developer Guide.
--
-- Constraints:
--
-- -   Must be 1 to 64 alphanumeric characters or hyphens. The user name
--     can\'t be @PUBLIC@.
--
-- -   Must contain uppercase or lowercase letters, numbers, underscore,
--     plus sign, period (dot), at symbol (\@), or hyphen.
--
-- -   First character must be a letter.
--
-- -   Must not contain a colon ( : ) or slash ( \/ ).
--
-- -   Cannot be a reserved word. A list of reserved words can be found in
--     <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words>
--     in the Amazon Redshift Database Developer Guide.
getClusterCredentials_dbUser :: Lens.Lens' GetClusterCredentials Prelude.Text
getClusterCredentials_dbUser = Lens.lens (\GetClusterCredentials' {dbUser} -> dbUser) (\s@GetClusterCredentials' {} a -> s {dbUser = a} :: GetClusterCredentials)

instance Core.AWSRequest GetClusterCredentials where
  type
    AWSResponse GetClusterCredentials =
      GetClusterCredentialsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetClusterCredentialsResult"
      ( \s h x ->
          GetClusterCredentialsResponse'
            Prelude.<$> (x Data..@? "DbPassword")
            Prelude.<*> (x Data..@? "DbUser")
            Prelude.<*> (x Data..@? "Expiration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetClusterCredentials where
  hashWithSalt _salt GetClusterCredentials' {..} =
    _salt
      `Prelude.hashWithSalt` autoCreate
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` customDomainName
      `Prelude.hashWithSalt` dbGroups
      `Prelude.hashWithSalt` dbName
      `Prelude.hashWithSalt` durationSeconds
      `Prelude.hashWithSalt` dbUser

instance Prelude.NFData GetClusterCredentials where
  rnf GetClusterCredentials' {..} =
    Prelude.rnf autoCreate
      `Prelude.seq` Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf customDomainName
      `Prelude.seq` Prelude.rnf dbGroups
      `Prelude.seq` Prelude.rnf dbName
      `Prelude.seq` Prelude.rnf durationSeconds
      `Prelude.seq` Prelude.rnf dbUser

instance Data.ToHeaders GetClusterCredentials where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetClusterCredentials where
  toPath = Prelude.const "/"

instance Data.ToQuery GetClusterCredentials where
  toQuery GetClusterCredentials' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("GetClusterCredentials" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "AutoCreate" Data.=: autoCreate,
        "ClusterIdentifier" Data.=: clusterIdentifier,
        "CustomDomainName" Data.=: customDomainName,
        "DbGroups"
          Data.=: Data.toQuery
            (Data.toQueryList "DbGroup" Prelude.<$> dbGroups),
        "DbName" Data.=: dbName,
        "DurationSeconds" Data.=: durationSeconds,
        "DbUser" Data.=: dbUser
      ]

-- | Temporary credentials with authorization to log on to an Amazon Redshift
-- database.
--
-- /See:/ 'newGetClusterCredentialsResponse' smart constructor.
data GetClusterCredentialsResponse = GetClusterCredentialsResponse'
  { -- | A temporary password that authorizes the user name returned by @DbUser@
    -- to log on to the database @DbName@.
    dbPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | A database user name that is authorized to log on to the database
    -- @DbName@ using the password @DbPassword@. If the specified DbUser exists
    -- in the database, the new user name has the same database permissions as
    -- the the user named in DbUser. By default, the user is added to PUBLIC.
    -- If the @DbGroups@ parameter is specifed, @DbUser@ is added to the listed
    -- groups for any sessions created using these credentials.
    dbUser :: Prelude.Maybe Prelude.Text,
    -- | The date and time the password in @DbPassword@ expires.
    expiration :: Prelude.Maybe Data.ISO8601,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetClusterCredentialsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbPassword', 'getClusterCredentialsResponse_dbPassword' - A temporary password that authorizes the user name returned by @DbUser@
-- to log on to the database @DbName@.
--
-- 'dbUser', 'getClusterCredentialsResponse_dbUser' - A database user name that is authorized to log on to the database
-- @DbName@ using the password @DbPassword@. If the specified DbUser exists
-- in the database, the new user name has the same database permissions as
-- the the user named in DbUser. By default, the user is added to PUBLIC.
-- If the @DbGroups@ parameter is specifed, @DbUser@ is added to the listed
-- groups for any sessions created using these credentials.
--
-- 'expiration', 'getClusterCredentialsResponse_expiration' - The date and time the password in @DbPassword@ expires.
--
-- 'httpStatus', 'getClusterCredentialsResponse_httpStatus' - The response's http status code.
newGetClusterCredentialsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetClusterCredentialsResponse
newGetClusterCredentialsResponse pHttpStatus_ =
  GetClusterCredentialsResponse'
    { dbPassword =
        Prelude.Nothing,
      dbUser = Prelude.Nothing,
      expiration = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A temporary password that authorizes the user name returned by @DbUser@
-- to log on to the database @DbName@.
getClusterCredentialsResponse_dbPassword :: Lens.Lens' GetClusterCredentialsResponse (Prelude.Maybe Prelude.Text)
getClusterCredentialsResponse_dbPassword = Lens.lens (\GetClusterCredentialsResponse' {dbPassword} -> dbPassword) (\s@GetClusterCredentialsResponse' {} a -> s {dbPassword = a} :: GetClusterCredentialsResponse) Prelude.. Lens.mapping Data._Sensitive

-- | A database user name that is authorized to log on to the database
-- @DbName@ using the password @DbPassword@. If the specified DbUser exists
-- in the database, the new user name has the same database permissions as
-- the the user named in DbUser. By default, the user is added to PUBLIC.
-- If the @DbGroups@ parameter is specifed, @DbUser@ is added to the listed
-- groups for any sessions created using these credentials.
getClusterCredentialsResponse_dbUser :: Lens.Lens' GetClusterCredentialsResponse (Prelude.Maybe Prelude.Text)
getClusterCredentialsResponse_dbUser = Lens.lens (\GetClusterCredentialsResponse' {dbUser} -> dbUser) (\s@GetClusterCredentialsResponse' {} a -> s {dbUser = a} :: GetClusterCredentialsResponse)

-- | The date and time the password in @DbPassword@ expires.
getClusterCredentialsResponse_expiration :: Lens.Lens' GetClusterCredentialsResponse (Prelude.Maybe Prelude.UTCTime)
getClusterCredentialsResponse_expiration = Lens.lens (\GetClusterCredentialsResponse' {expiration} -> expiration) (\s@GetClusterCredentialsResponse' {} a -> s {expiration = a} :: GetClusterCredentialsResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
getClusterCredentialsResponse_httpStatus :: Lens.Lens' GetClusterCredentialsResponse Prelude.Int
getClusterCredentialsResponse_httpStatus = Lens.lens (\GetClusterCredentialsResponse' {httpStatus} -> httpStatus) (\s@GetClusterCredentialsResponse' {} a -> s {httpStatus = a} :: GetClusterCredentialsResponse)

instance Prelude.NFData GetClusterCredentialsResponse where
  rnf GetClusterCredentialsResponse' {..} =
    Prelude.rnf dbPassword
      `Prelude.seq` Prelude.rnf dbUser
      `Prelude.seq` Prelude.rnf expiration
      `Prelude.seq` Prelude.rnf httpStatus
