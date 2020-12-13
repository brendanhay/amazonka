{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.GetClusterCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a database user name and temporary password with temporary authorization to log on to an Amazon Redshift database. The action returns the database user name prefixed with @IAM:@ if @AutoCreate@ is @False@ or @IAMA:@ if @AutoCreate@ is @True@ . You can optionally specify one or more database user groups that the user will join at log on. By default, the temporary credentials expire in 900 seconds. You can optionally specify a duration between 900 seconds (15 minutes) and 3600 seconds (60 minutes). For more information, see <https://docs.aws.amazon.com/redshift/latest/mgmt/generating-user-credentials.html Using IAM Authentication to Generate Database User Credentials> in the Amazon Redshift Cluster Management Guide.
--
-- The AWS Identity and Access Management (IAM)user or role that executes GetClusterCredentials must have an IAM policy attached that allows access to all necessary actions and resources. For more information about permissions, see <https://docs.aws.amazon.com/redshift/latest/mgmt/redshift-iam-access-control-identity-based.html#redshift-policy-resources.getclustercredentials-resources Resource Policies for GetClusterCredentials> in the Amazon Redshift Cluster Management Guide.
-- If the @DbGroups@ parameter is specified, the IAM policy must allow the @redshift:JoinGroup@ action with access to the listed @dbgroups@ .
-- In addition, if the @AutoCreate@ parameter is set to @True@ , then the policy must include the @redshift:CreateClusterUser@ privilege.
-- If the @DbName@ parameter is specified, the IAM policy must allow access to the resource @dbname@ for the specified database name.
module Network.AWS.Redshift.GetClusterCredentials
  ( -- * Creating a request
    GetClusterCredentials (..),
    mkGetClusterCredentials,

    -- ** Request lenses
    gccDBUser,
    gccDBGroups,
    gccClusterIdentifier,
    gccDurationSeconds,
    gccAutoCreate,
    gccDBName,

    -- * Destructuring the response
    GetClusterCredentialsResponse (..),
    mkGetClusterCredentialsResponse,

    -- ** Response lenses
    gccrsDBUser,
    gccrsExpiration,
    gccrsDBPassword,
    gccrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The request parameters to get cluster credentials.
--
-- /See:/ 'mkGetClusterCredentials' smart constructor.
data GetClusterCredentials = GetClusterCredentials'
  { -- | The name of a database user. If a user name matching @DbUser@ exists in the database, the temporary user credentials have the same permissions as the existing user. If @DbUser@ doesn't exist in the database and @Autocreate@ is @True@ , a new user is created using the value for @DbUser@ with PUBLIC permissions. If a database user matching the value for @DbUser@ doesn't exist and @Autocreate@ is @False@ , then the command succeeds but the connection attempt will fail because the user doesn't exist in the database.
    --
    -- For more information, see <https://docs.aws.amazon.com/redshift/latest/dg/r_CREATE_USER.html CREATE USER> in the Amazon Redshift Database Developer Guide.
    -- Constraints:
    --
    --     * Must be 1 to 64 alphanumeric characters or hyphens. The user name can't be @PUBLIC@ .
    --
    --
    --     * Must contain only lowercase letters, numbers, underscore, plus sign, period (dot), at symbol (@), or hyphen.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Must not contain a colon ( : ) or slash ( / ).
    --
    --
    --     * Cannot be a reserved word. A list of reserved words can be found in <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
    dbUser :: Lude.Text,
    -- | A list of the names of existing database groups that the user named in @DbUser@ will join for the current session, in addition to any group memberships for an existing user. If not specified, a new user is added only to PUBLIC.
    --
    -- Database group name constraints
    --
    --     * Must be 1 to 64 alphanumeric characters or hyphens
    --
    --
    --     * Must contain only lowercase letters, numbers, underscore, plus sign, period (dot), at symbol (@), or hyphen.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Must not contain a colon ( : ) or slash ( / ).
    --
    --
    --     * Cannot be a reserved word. A list of reserved words can be found in <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
    dbGroups :: Lude.Maybe [Lude.Text],
    -- | The unique identifier of the cluster that contains the database for which your are requesting credentials. This parameter is case sensitive.
    clusterIdentifier :: Lude.Text,
    -- | The number of seconds until the returned temporary password expires.
    --
    -- Constraint: minimum 900, maximum 3600.
    -- Default: 900
    durationSeconds :: Lude.Maybe Lude.Int,
    -- | Create a database user with the name specified for the user named in @DbUser@ if one does not exist.
    autoCreate :: Lude.Maybe Lude.Bool,
    -- | The name of a database that @DbUser@ is authorized to log on to. If @DbName@ is not specified, @DbUser@ can log on to any existing database.
    --
    -- Constraints:
    --
    --     * Must be 1 to 64 alphanumeric characters or hyphens
    --
    --
    --     * Must contain only lowercase letters, numbers, underscore, plus sign, period (dot), at symbol (@), or hyphen.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Must not contain a colon ( : ) or slash ( / ).
    --
    --
    --     * Cannot be a reserved word. A list of reserved words can be found in <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
    dbName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetClusterCredentials' with the minimum fields required to make a request.
--
-- * 'dbUser' - The name of a database user. If a user name matching @DbUser@ exists in the database, the temporary user credentials have the same permissions as the existing user. If @DbUser@ doesn't exist in the database and @Autocreate@ is @True@ , a new user is created using the value for @DbUser@ with PUBLIC permissions. If a database user matching the value for @DbUser@ doesn't exist and @Autocreate@ is @False@ , then the command succeeds but the connection attempt will fail because the user doesn't exist in the database.
--
-- For more information, see <https://docs.aws.amazon.com/redshift/latest/dg/r_CREATE_USER.html CREATE USER> in the Amazon Redshift Database Developer Guide.
-- Constraints:
--
--     * Must be 1 to 64 alphanumeric characters or hyphens. The user name can't be @PUBLIC@ .
--
--
--     * Must contain only lowercase letters, numbers, underscore, plus sign, period (dot), at symbol (@), or hyphen.
--
--
--     * First character must be a letter.
--
--
--     * Must not contain a colon ( : ) or slash ( / ).
--
--
--     * Cannot be a reserved word. A list of reserved words can be found in <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
--
--
-- * 'dbGroups' - A list of the names of existing database groups that the user named in @DbUser@ will join for the current session, in addition to any group memberships for an existing user. If not specified, a new user is added only to PUBLIC.
--
-- Database group name constraints
--
--     * Must be 1 to 64 alphanumeric characters or hyphens
--
--
--     * Must contain only lowercase letters, numbers, underscore, plus sign, period (dot), at symbol (@), or hyphen.
--
--
--     * First character must be a letter.
--
--
--     * Must not contain a colon ( : ) or slash ( / ).
--
--
--     * Cannot be a reserved word. A list of reserved words can be found in <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
--
--
-- * 'clusterIdentifier' - The unique identifier of the cluster that contains the database for which your are requesting credentials. This parameter is case sensitive.
-- * 'durationSeconds' - The number of seconds until the returned temporary password expires.
--
-- Constraint: minimum 900, maximum 3600.
-- Default: 900
-- * 'autoCreate' - Create a database user with the name specified for the user named in @DbUser@ if one does not exist.
-- * 'dbName' - The name of a database that @DbUser@ is authorized to log on to. If @DbName@ is not specified, @DbUser@ can log on to any existing database.
--
-- Constraints:
--
--     * Must be 1 to 64 alphanumeric characters or hyphens
--
--
--     * Must contain only lowercase letters, numbers, underscore, plus sign, period (dot), at symbol (@), or hyphen.
--
--
--     * First character must be a letter.
--
--
--     * Must not contain a colon ( : ) or slash ( / ).
--
--
--     * Cannot be a reserved word. A list of reserved words can be found in <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
mkGetClusterCredentials ::
  -- | 'dbUser'
  Lude.Text ->
  -- | 'clusterIdentifier'
  Lude.Text ->
  GetClusterCredentials
mkGetClusterCredentials pDBUser_ pClusterIdentifier_ =
  GetClusterCredentials'
    { dbUser = pDBUser_,
      dbGroups = Lude.Nothing,
      clusterIdentifier = pClusterIdentifier_,
      durationSeconds = Lude.Nothing,
      autoCreate = Lude.Nothing,
      dbName = Lude.Nothing
    }

-- | The name of a database user. If a user name matching @DbUser@ exists in the database, the temporary user credentials have the same permissions as the existing user. If @DbUser@ doesn't exist in the database and @Autocreate@ is @True@ , a new user is created using the value for @DbUser@ with PUBLIC permissions. If a database user matching the value for @DbUser@ doesn't exist and @Autocreate@ is @False@ , then the command succeeds but the connection attempt will fail because the user doesn't exist in the database.
--
-- For more information, see <https://docs.aws.amazon.com/redshift/latest/dg/r_CREATE_USER.html CREATE USER> in the Amazon Redshift Database Developer Guide.
-- Constraints:
--
--     * Must be 1 to 64 alphanumeric characters or hyphens. The user name can't be @PUBLIC@ .
--
--
--     * Must contain only lowercase letters, numbers, underscore, plus sign, period (dot), at symbol (@), or hyphen.
--
--
--     * First character must be a letter.
--
--
--     * Must not contain a colon ( : ) or slash ( / ).
--
--
--     * Cannot be a reserved word. A list of reserved words can be found in <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
--
--
--
-- /Note:/ Consider using 'dbUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccDBUser :: Lens.Lens' GetClusterCredentials Lude.Text
gccDBUser = Lens.lens (dbUser :: GetClusterCredentials -> Lude.Text) (\s a -> s {dbUser = a} :: GetClusterCredentials)
{-# DEPRECATED gccDBUser "Use generic-lens or generic-optics with 'dbUser' instead." #-}

-- | A list of the names of existing database groups that the user named in @DbUser@ will join for the current session, in addition to any group memberships for an existing user. If not specified, a new user is added only to PUBLIC.
--
-- Database group name constraints
--
--     * Must be 1 to 64 alphanumeric characters or hyphens
--
--
--     * Must contain only lowercase letters, numbers, underscore, plus sign, period (dot), at symbol (@), or hyphen.
--
--
--     * First character must be a letter.
--
--
--     * Must not contain a colon ( : ) or slash ( / ).
--
--
--     * Cannot be a reserved word. A list of reserved words can be found in <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
--
--
--
-- /Note:/ Consider using 'dbGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccDBGroups :: Lens.Lens' GetClusterCredentials (Lude.Maybe [Lude.Text])
gccDBGroups = Lens.lens (dbGroups :: GetClusterCredentials -> Lude.Maybe [Lude.Text]) (\s a -> s {dbGroups = a} :: GetClusterCredentials)
{-# DEPRECATED gccDBGroups "Use generic-lens or generic-optics with 'dbGroups' instead." #-}

-- | The unique identifier of the cluster that contains the database for which your are requesting credentials. This parameter is case sensitive.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccClusterIdentifier :: Lens.Lens' GetClusterCredentials Lude.Text
gccClusterIdentifier = Lens.lens (clusterIdentifier :: GetClusterCredentials -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: GetClusterCredentials)
{-# DEPRECATED gccClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The number of seconds until the returned temporary password expires.
--
-- Constraint: minimum 900, maximum 3600.
-- Default: 900
--
-- /Note:/ Consider using 'durationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccDurationSeconds :: Lens.Lens' GetClusterCredentials (Lude.Maybe Lude.Int)
gccDurationSeconds = Lens.lens (durationSeconds :: GetClusterCredentials -> Lude.Maybe Lude.Int) (\s a -> s {durationSeconds = a} :: GetClusterCredentials)
{-# DEPRECATED gccDurationSeconds "Use generic-lens or generic-optics with 'durationSeconds' instead." #-}

-- | Create a database user with the name specified for the user named in @DbUser@ if one does not exist.
--
-- /Note:/ Consider using 'autoCreate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccAutoCreate :: Lens.Lens' GetClusterCredentials (Lude.Maybe Lude.Bool)
gccAutoCreate = Lens.lens (autoCreate :: GetClusterCredentials -> Lude.Maybe Lude.Bool) (\s a -> s {autoCreate = a} :: GetClusterCredentials)
{-# DEPRECATED gccAutoCreate "Use generic-lens or generic-optics with 'autoCreate' instead." #-}

-- | The name of a database that @DbUser@ is authorized to log on to. If @DbName@ is not specified, @DbUser@ can log on to any existing database.
--
-- Constraints:
--
--     * Must be 1 to 64 alphanumeric characters or hyphens
--
--
--     * Must contain only lowercase letters, numbers, underscore, plus sign, period (dot), at symbol (@), or hyphen.
--
--
--     * First character must be a letter.
--
--
--     * Must not contain a colon ( : ) or slash ( / ).
--
--
--     * Cannot be a reserved word. A list of reserved words can be found in <http://docs.aws.amazon.com/redshift/latest/dg/r_pg_keywords.html Reserved Words> in the Amazon Redshift Database Developer Guide.
--
--
--
-- /Note:/ Consider using 'dbName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccDBName :: Lens.Lens' GetClusterCredentials (Lude.Maybe Lude.Text)
gccDBName = Lens.lens (dbName :: GetClusterCredentials -> Lude.Maybe Lude.Text) (\s a -> s {dbName = a} :: GetClusterCredentials)
{-# DEPRECATED gccDBName "Use generic-lens or generic-optics with 'dbName' instead." #-}

instance Lude.AWSRequest GetClusterCredentials where
  type Rs GetClusterCredentials = GetClusterCredentialsResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "GetClusterCredentialsResult"
      ( \s h x ->
          GetClusterCredentialsResponse'
            Lude.<$> (x Lude..@? "DbUser")
            Lude.<*> (x Lude..@? "Expiration")
            Lude.<*> (x Lude..@? "DbPassword")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetClusterCredentials where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetClusterCredentials where
  toPath = Lude.const "/"

instance Lude.ToQuery GetClusterCredentials where
  toQuery GetClusterCredentials' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetClusterCredentials" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "DbUser" Lude.=: dbUser,
        "DbGroups"
          Lude.=: Lude.toQuery (Lude.toQueryList "DbGroup" Lude.<$> dbGroups),
        "ClusterIdentifier" Lude.=: clusterIdentifier,
        "DurationSeconds" Lude.=: durationSeconds,
        "AutoCreate" Lude.=: autoCreate,
        "DbName" Lude.=: dbName
      ]

-- | Temporary credentials with authorization to log on to an Amazon Redshift database.
--
-- /See:/ 'mkGetClusterCredentialsResponse' smart constructor.
data GetClusterCredentialsResponse = GetClusterCredentialsResponse'
  { -- | A database user name that is authorized to log on to the database @DbName@ using the password @DbPassword@ . If the specified DbUser exists in the database, the new user name has the same database privileges as the the user named in DbUser. By default, the user is added to PUBLIC. If the @DbGroups@ parameter is specifed, @DbUser@ is added to the listed groups for any sessions created using these credentials.
    dbUser :: Lude.Maybe Lude.Text,
    -- | The date and time the password in @DbPassword@ expires.
    expiration :: Lude.Maybe Lude.DateTime,
    -- | A temporary password that authorizes the user name returned by @DbUser@ to log on to the database @DbName@ .
    dbPassword :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetClusterCredentialsResponse' with the minimum fields required to make a request.
--
-- * 'dbUser' - A database user name that is authorized to log on to the database @DbName@ using the password @DbPassword@ . If the specified DbUser exists in the database, the new user name has the same database privileges as the the user named in DbUser. By default, the user is added to PUBLIC. If the @DbGroups@ parameter is specifed, @DbUser@ is added to the listed groups for any sessions created using these credentials.
-- * 'expiration' - The date and time the password in @DbPassword@ expires.
-- * 'dbPassword' - A temporary password that authorizes the user name returned by @DbUser@ to log on to the database @DbName@ .
-- * 'responseStatus' - The response status code.
mkGetClusterCredentialsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetClusterCredentialsResponse
mkGetClusterCredentialsResponse pResponseStatus_ =
  GetClusterCredentialsResponse'
    { dbUser = Lude.Nothing,
      expiration = Lude.Nothing,
      dbPassword = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A database user name that is authorized to log on to the database @DbName@ using the password @DbPassword@ . If the specified DbUser exists in the database, the new user name has the same database privileges as the the user named in DbUser. By default, the user is added to PUBLIC. If the @DbGroups@ parameter is specifed, @DbUser@ is added to the listed groups for any sessions created using these credentials.
--
-- /Note:/ Consider using 'dbUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccrsDBUser :: Lens.Lens' GetClusterCredentialsResponse (Lude.Maybe Lude.Text)
gccrsDBUser = Lens.lens (dbUser :: GetClusterCredentialsResponse -> Lude.Maybe Lude.Text) (\s a -> s {dbUser = a} :: GetClusterCredentialsResponse)
{-# DEPRECATED gccrsDBUser "Use generic-lens or generic-optics with 'dbUser' instead." #-}

-- | The date and time the password in @DbPassword@ expires.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccrsExpiration :: Lens.Lens' GetClusterCredentialsResponse (Lude.Maybe Lude.DateTime)
gccrsExpiration = Lens.lens (expiration :: GetClusterCredentialsResponse -> Lude.Maybe Lude.DateTime) (\s a -> s {expiration = a} :: GetClusterCredentialsResponse)
{-# DEPRECATED gccrsExpiration "Use generic-lens or generic-optics with 'expiration' instead." #-}

-- | A temporary password that authorizes the user name returned by @DbUser@ to log on to the database @DbName@ .
--
-- /Note:/ Consider using 'dbPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccrsDBPassword :: Lens.Lens' GetClusterCredentialsResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
gccrsDBPassword = Lens.lens (dbPassword :: GetClusterCredentialsResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {dbPassword = a} :: GetClusterCredentialsResponse)
{-# DEPRECATED gccrsDBPassword "Use generic-lens or generic-optics with 'dbPassword' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccrsResponseStatus :: Lens.Lens' GetClusterCredentialsResponse Lude.Int
gccrsResponseStatus = Lens.lens (responseStatus :: GetClusterCredentialsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetClusterCredentialsResponse)
{-# DEPRECATED gccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
