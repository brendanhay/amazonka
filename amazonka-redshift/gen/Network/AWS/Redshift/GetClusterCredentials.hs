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
-- Module      : Network.AWS.Redshift.GetClusterCredentials
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- The AWS Identity and Access Management (IAM)user or role that executes
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
-- policy must include the @redshift:CreateClusterUser@ privilege.
--
-- If the @DbName@ parameter is specified, the IAM policy must allow access
-- to the resource @dbname@ for the specified database name.
module Network.AWS.Redshift.GetClusterCredentials
  ( -- * Creating a Request
    GetClusterCredentials (..),
    newGetClusterCredentials,

    -- * Request Lenses
    getClusterCredentials_dbGroups,
    getClusterCredentials_dbName,
    getClusterCredentials_autoCreate,
    getClusterCredentials_durationSeconds,
    getClusterCredentials_dbUser,
    getClusterCredentials_clusterIdentifier,

    -- * Destructuring the Response
    GetClusterCredentialsResponse (..),
    newGetClusterCredentialsResponse,

    -- * Response Lenses
    getClusterCredentialsResponse_dbUser,
    getClusterCredentialsResponse_expiration,
    getClusterCredentialsResponse_dbPassword,
    getClusterCredentialsResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request parameters to get cluster credentials.
--
-- /See:/ 'newGetClusterCredentials' smart constructor.
data GetClusterCredentials = GetClusterCredentials'
  { -- | A list of the names of existing database groups that the user named in
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
    dbName :: Prelude.Maybe Prelude.Text,
    -- | Create a database user with the name specified for the user named in
    -- @DbUser@ if one does not exist.
    autoCreate :: Prelude.Maybe Prelude.Bool,
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
    dbUser :: Prelude.Text,
    -- | The unique identifier of the cluster that contains the database for
    -- which your are requesting credentials. This parameter is case sensitive.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetClusterCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'autoCreate', 'getClusterCredentials_autoCreate' - Create a database user with the name specified for the user named in
-- @DbUser@ if one does not exist.
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
-- 'clusterIdentifier', 'getClusterCredentials_clusterIdentifier' - The unique identifier of the cluster that contains the database for
-- which your are requesting credentials. This parameter is case sensitive.
newGetClusterCredentials ::
  -- | 'dbUser'
  Prelude.Text ->
  -- | 'clusterIdentifier'
  Prelude.Text ->
  GetClusterCredentials
newGetClusterCredentials pDbUser_ pClusterIdentifier_ =
  GetClusterCredentials'
    { dbGroups = Prelude.Nothing,
      dbName = Prelude.Nothing,
      autoCreate = Prelude.Nothing,
      durationSeconds = Prelude.Nothing,
      dbUser = pDbUser_,
      clusterIdentifier = pClusterIdentifier_
    }

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
getClusterCredentials_dbGroups = Lens.lens (\GetClusterCredentials' {dbGroups} -> dbGroups) (\s@GetClusterCredentials' {} a -> s {dbGroups = a} :: GetClusterCredentials) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of a database that @DbUser@ is authorized to log on to. If
-- @DbName@ is not specified, @DbUser@ can log on to any existing database.
--
-- Constraints:
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
getClusterCredentials_dbName :: Lens.Lens' GetClusterCredentials (Prelude.Maybe Prelude.Text)
getClusterCredentials_dbName = Lens.lens (\GetClusterCredentials' {dbName} -> dbName) (\s@GetClusterCredentials' {} a -> s {dbName = a} :: GetClusterCredentials)

-- | Create a database user with the name specified for the user named in
-- @DbUser@ if one does not exist.
getClusterCredentials_autoCreate :: Lens.Lens' GetClusterCredentials (Prelude.Maybe Prelude.Bool)
getClusterCredentials_autoCreate = Lens.lens (\GetClusterCredentials' {autoCreate} -> autoCreate) (\s@GetClusterCredentials' {} a -> s {autoCreate = a} :: GetClusterCredentials)

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
getClusterCredentials_dbUser :: Lens.Lens' GetClusterCredentials Prelude.Text
getClusterCredentials_dbUser = Lens.lens (\GetClusterCredentials' {dbUser} -> dbUser) (\s@GetClusterCredentials' {} a -> s {dbUser = a} :: GetClusterCredentials)

-- | The unique identifier of the cluster that contains the database for
-- which your are requesting credentials. This parameter is case sensitive.
getClusterCredentials_clusterIdentifier :: Lens.Lens' GetClusterCredentials Prelude.Text
getClusterCredentials_clusterIdentifier = Lens.lens (\GetClusterCredentials' {clusterIdentifier} -> clusterIdentifier) (\s@GetClusterCredentials' {} a -> s {clusterIdentifier = a} :: GetClusterCredentials)

instance Prelude.AWSRequest GetClusterCredentials where
  type
    Rs GetClusterCredentials =
      GetClusterCredentialsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "GetClusterCredentialsResult"
      ( \s h x ->
          GetClusterCredentialsResponse'
            Prelude.<$> (x Prelude..@? "DbUser")
            Prelude.<*> (x Prelude..@? "Expiration")
            Prelude.<*> (x Prelude..@? "DbPassword")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetClusterCredentials

instance Prelude.NFData GetClusterCredentials

instance Prelude.ToHeaders GetClusterCredentials where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath GetClusterCredentials where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetClusterCredentials where
  toQuery GetClusterCredentials' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("GetClusterCredentials" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2012-12-01" :: Prelude.ByteString),
        "DbGroups"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "DbGroup" Prelude.<$> dbGroups),
        "DbName" Prelude.=: dbName,
        "AutoCreate" Prelude.=: autoCreate,
        "DurationSeconds" Prelude.=: durationSeconds,
        "DbUser" Prelude.=: dbUser,
        "ClusterIdentifier" Prelude.=: clusterIdentifier
      ]

-- | Temporary credentials with authorization to log on to an Amazon Redshift
-- database.
--
-- /See:/ 'newGetClusterCredentialsResponse' smart constructor.
data GetClusterCredentialsResponse = GetClusterCredentialsResponse'
  { -- | A database user name that is authorized to log on to the database
    -- @DbName@ using the password @DbPassword@. If the specified DbUser exists
    -- in the database, the new user name has the same database privileges as
    -- the the user named in DbUser. By default, the user is added to PUBLIC.
    -- If the @DbGroups@ parameter is specifed, @DbUser@ is added to the listed
    -- groups for any sessions created using these credentials.
    dbUser :: Prelude.Maybe Prelude.Text,
    -- | The date and time the password in @DbPassword@ expires.
    expiration :: Prelude.Maybe Prelude.ISO8601,
    -- | A temporary password that authorizes the user name returned by @DbUser@
    -- to log on to the database @DbName@.
    dbPassword :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetClusterCredentialsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbUser', 'getClusterCredentialsResponse_dbUser' - A database user name that is authorized to log on to the database
-- @DbName@ using the password @DbPassword@. If the specified DbUser exists
-- in the database, the new user name has the same database privileges as
-- the the user named in DbUser. By default, the user is added to PUBLIC.
-- If the @DbGroups@ parameter is specifed, @DbUser@ is added to the listed
-- groups for any sessions created using these credentials.
--
-- 'expiration', 'getClusterCredentialsResponse_expiration' - The date and time the password in @DbPassword@ expires.
--
-- 'dbPassword', 'getClusterCredentialsResponse_dbPassword' - A temporary password that authorizes the user name returned by @DbUser@
-- to log on to the database @DbName@.
--
-- 'httpStatus', 'getClusterCredentialsResponse_httpStatus' - The response's http status code.
newGetClusterCredentialsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetClusterCredentialsResponse
newGetClusterCredentialsResponse pHttpStatus_ =
  GetClusterCredentialsResponse'
    { dbUser =
        Prelude.Nothing,
      expiration = Prelude.Nothing,
      dbPassword = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A database user name that is authorized to log on to the database
-- @DbName@ using the password @DbPassword@. If the specified DbUser exists
-- in the database, the new user name has the same database privileges as
-- the the user named in DbUser. By default, the user is added to PUBLIC.
-- If the @DbGroups@ parameter is specifed, @DbUser@ is added to the listed
-- groups for any sessions created using these credentials.
getClusterCredentialsResponse_dbUser :: Lens.Lens' GetClusterCredentialsResponse (Prelude.Maybe Prelude.Text)
getClusterCredentialsResponse_dbUser = Lens.lens (\GetClusterCredentialsResponse' {dbUser} -> dbUser) (\s@GetClusterCredentialsResponse' {} a -> s {dbUser = a} :: GetClusterCredentialsResponse)

-- | The date and time the password in @DbPassword@ expires.
getClusterCredentialsResponse_expiration :: Lens.Lens' GetClusterCredentialsResponse (Prelude.Maybe Prelude.UTCTime)
getClusterCredentialsResponse_expiration = Lens.lens (\GetClusterCredentialsResponse' {expiration} -> expiration) (\s@GetClusterCredentialsResponse' {} a -> s {expiration = a} :: GetClusterCredentialsResponse) Prelude.. Lens.mapping Prelude._Time

-- | A temporary password that authorizes the user name returned by @DbUser@
-- to log on to the database @DbName@.
getClusterCredentialsResponse_dbPassword :: Lens.Lens' GetClusterCredentialsResponse (Prelude.Maybe Prelude.Text)
getClusterCredentialsResponse_dbPassword = Lens.lens (\GetClusterCredentialsResponse' {dbPassword} -> dbPassword) (\s@GetClusterCredentialsResponse' {} a -> s {dbPassword = a} :: GetClusterCredentialsResponse) Prelude.. Lens.mapping Prelude._Sensitive

-- | The response's http status code.
getClusterCredentialsResponse_httpStatus :: Lens.Lens' GetClusterCredentialsResponse Prelude.Int
getClusterCredentialsResponse_httpStatus = Lens.lens (\GetClusterCredentialsResponse' {httpStatus} -> httpStatus) (\s@GetClusterCredentialsResponse' {} a -> s {httpStatus = a} :: GetClusterCredentialsResponse)

instance Prelude.NFData GetClusterCredentialsResponse
