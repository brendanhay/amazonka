{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetClusterCredentials (..)
    , mkGetClusterCredentials
    -- ** Request lenses
    , gccDbUser
    , gccClusterIdentifier
    , gccAutoCreate
    , gccDbGroups
    , gccDbName
    , gccDurationSeconds

    -- * Destructuring the response
    , GetClusterCredentialsResponse (..)
    , mkGetClusterCredentialsResponse
    -- ** Response lenses
    , gccrrsDbPassword
    , gccrrsDbUser
    , gccrrsExpiration
    , gccrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The request parameters to get cluster credentials.
--
-- /See:/ 'mkGetClusterCredentials' smart constructor.
data GetClusterCredentials = GetClusterCredentials'
  { dbUser :: Core.Text
    -- ^ The name of a database user. If a user name matching @DbUser@ exists in the database, the temporary user credentials have the same permissions as the existing user. If @DbUser@ doesn't exist in the database and @Autocreate@ is @True@ , a new user is created using the value for @DbUser@ with PUBLIC permissions. If a database user matching the value for @DbUser@ doesn't exist and @Autocreate@ is @False@ , then the command succeeds but the connection attempt will fail because the user doesn't exist in the database.
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
  , clusterIdentifier :: Core.Text
    -- ^ The unique identifier of the cluster that contains the database for which your are requesting credentials. This parameter is case sensitive.
  , autoCreate :: Core.Maybe Core.Bool
    -- ^ Create a database user with the name specified for the user named in @DbUser@ if one does not exist.
  , dbGroups :: Core.Maybe [Core.Text]
    -- ^ A list of the names of existing database groups that the user named in @DbUser@ will join for the current session, in addition to any group memberships for an existing user. If not specified, a new user is added only to PUBLIC.
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
  , dbName :: Core.Maybe Core.Text
    -- ^ The name of a database that @DbUser@ is authorized to log on to. If @DbName@ is not specified, @DbUser@ can log on to any existing database.
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
  , durationSeconds :: Core.Maybe Core.Int
    -- ^ The number of seconds until the returned temporary password expires.
--
-- Constraint: minimum 900, maximum 3600.
-- Default: 900
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetClusterCredentials' value with any optional fields omitted.
mkGetClusterCredentials
    :: Core.Text -- ^ 'dbUser'
    -> Core.Text -- ^ 'clusterIdentifier'
    -> GetClusterCredentials
mkGetClusterCredentials dbUser clusterIdentifier
  = GetClusterCredentials'{dbUser, clusterIdentifier,
                           autoCreate = Core.Nothing, dbGroups = Core.Nothing,
                           dbName = Core.Nothing, durationSeconds = Core.Nothing}

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
gccDbUser :: Lens.Lens' GetClusterCredentials Core.Text
gccDbUser = Lens.field @"dbUser"
{-# INLINEABLE gccDbUser #-}
{-# DEPRECATED dbUser "Use generic-lens or generic-optics with 'dbUser' instead"  #-}

-- | The unique identifier of the cluster that contains the database for which your are requesting credentials. This parameter is case sensitive.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccClusterIdentifier :: Lens.Lens' GetClusterCredentials Core.Text
gccClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE gccClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

-- | Create a database user with the name specified for the user named in @DbUser@ if one does not exist.
--
-- /Note:/ Consider using 'autoCreate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccAutoCreate :: Lens.Lens' GetClusterCredentials (Core.Maybe Core.Bool)
gccAutoCreate = Lens.field @"autoCreate"
{-# INLINEABLE gccAutoCreate #-}
{-# DEPRECATED autoCreate "Use generic-lens or generic-optics with 'autoCreate' instead"  #-}

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
gccDbGroups :: Lens.Lens' GetClusterCredentials (Core.Maybe [Core.Text])
gccDbGroups = Lens.field @"dbGroups"
{-# INLINEABLE gccDbGroups #-}
{-# DEPRECATED dbGroups "Use generic-lens or generic-optics with 'dbGroups' instead"  #-}

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
gccDbName :: Lens.Lens' GetClusterCredentials (Core.Maybe Core.Text)
gccDbName = Lens.field @"dbName"
{-# INLINEABLE gccDbName #-}
{-# DEPRECATED dbName "Use generic-lens or generic-optics with 'dbName' instead"  #-}

-- | The number of seconds until the returned temporary password expires.
--
-- Constraint: minimum 900, maximum 3600.
-- Default: 900
--
-- /Note:/ Consider using 'durationSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccDurationSeconds :: Lens.Lens' GetClusterCredentials (Core.Maybe Core.Int)
gccDurationSeconds = Lens.field @"durationSeconds"
{-# INLINEABLE gccDurationSeconds #-}
{-# DEPRECATED durationSeconds "Use generic-lens or generic-optics with 'durationSeconds' instead"  #-}

instance Core.ToQuery GetClusterCredentials where
        toQuery GetClusterCredentials{..}
          = Core.toQueryPair "Action" ("GetClusterCredentials" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2012-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "DbUser" dbUser
              Core.<> Core.toQueryPair "ClusterIdentifier" clusterIdentifier
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "AutoCreate") autoCreate
              Core.<>
              Core.toQueryPair "DbGroups"
                (Core.maybe Core.mempty (Core.toQueryList "DbGroup") dbGroups)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "DbName") dbName
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DurationSeconds")
                durationSeconds

instance Core.ToHeaders GetClusterCredentials where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetClusterCredentials where
        type Rs GetClusterCredentials = GetClusterCredentialsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "GetClusterCredentialsResult"
              (\ s h x ->
                 GetClusterCredentialsResponse' Core.<$>
                   (x Core..@? "DbPassword") Core.<*> x Core..@? "DbUser" Core.<*>
                     x Core..@? "Expiration"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Temporary credentials with authorization to log on to an Amazon Redshift database. 
--
-- /See:/ 'mkGetClusterCredentialsResponse' smart constructor.
data GetClusterCredentialsResponse = GetClusterCredentialsResponse'
  { dbPassword :: Core.Maybe Types.DbPassword
    -- ^ A temporary password that authorizes the user name returned by @DbUser@ to log on to the database @DbName@ . 
  , dbUser :: Core.Maybe Core.Text
    -- ^ A database user name that is authorized to log on to the database @DbName@ using the password @DbPassword@ . If the specified DbUser exists in the database, the new user name has the same database privileges as the the user named in DbUser. By default, the user is added to PUBLIC. If the @DbGroups@ parameter is specifed, @DbUser@ is added to the listed groups for any sessions created using these credentials.
  , expiration :: Core.Maybe Core.UTCTime
    -- ^ The date and time the password in @DbPassword@ expires.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetClusterCredentialsResponse' value with any optional fields omitted.
mkGetClusterCredentialsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetClusterCredentialsResponse
mkGetClusterCredentialsResponse responseStatus
  = GetClusterCredentialsResponse'{dbPassword = Core.Nothing,
                                   dbUser = Core.Nothing, expiration = Core.Nothing, responseStatus}

-- | A temporary password that authorizes the user name returned by @DbUser@ to log on to the database @DbName@ . 
--
-- /Note:/ Consider using 'dbPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccrrsDbPassword :: Lens.Lens' GetClusterCredentialsResponse (Core.Maybe Types.DbPassword)
gccrrsDbPassword = Lens.field @"dbPassword"
{-# INLINEABLE gccrrsDbPassword #-}
{-# DEPRECATED dbPassword "Use generic-lens or generic-optics with 'dbPassword' instead"  #-}

-- | A database user name that is authorized to log on to the database @DbName@ using the password @DbPassword@ . If the specified DbUser exists in the database, the new user name has the same database privileges as the the user named in DbUser. By default, the user is added to PUBLIC. If the @DbGroups@ parameter is specifed, @DbUser@ is added to the listed groups for any sessions created using these credentials.
--
-- /Note:/ Consider using 'dbUser' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccrrsDbUser :: Lens.Lens' GetClusterCredentialsResponse (Core.Maybe Core.Text)
gccrrsDbUser = Lens.field @"dbUser"
{-# INLINEABLE gccrrsDbUser #-}
{-# DEPRECATED dbUser "Use generic-lens or generic-optics with 'dbUser' instead"  #-}

-- | The date and time the password in @DbPassword@ expires.
--
-- /Note:/ Consider using 'expiration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccrrsExpiration :: Lens.Lens' GetClusterCredentialsResponse (Core.Maybe Core.UTCTime)
gccrrsExpiration = Lens.field @"expiration"
{-# INLINEABLE gccrrsExpiration #-}
{-# DEPRECATED expiration "Use generic-lens or generic-optics with 'expiration' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gccrrsResponseStatus :: Lens.Lens' GetClusterCredentialsResponse Core.Int
gccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
