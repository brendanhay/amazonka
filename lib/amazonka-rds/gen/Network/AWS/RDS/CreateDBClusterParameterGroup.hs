{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBClusterParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DB cluster parameter group.
--
-- Parameters in a DB cluster parameter group apply to all of the instances in a DB cluster.
-- A DB cluster parameter group is initially created with the default parameters for the database engine used by instances in the DB cluster. To provide custom values for any of the parameters, you must modify the group after creating it using @ModifyDBClusterParameterGroup@ . Once you've created a DB cluster parameter group, you need to associate it with your DB cluster using @ModifyDBCluster@ . When you associate a new DB cluster parameter group with a running DB cluster, you need to reboot the DB instances in the DB cluster without failover for the new DB cluster parameter group and associated settings to take effect. 
-- /Important:/ After you create a DB cluster parameter group, you should wait at least 5 minutes before creating your first DB cluster that uses that DB cluster parameter group as the default parameter group. This allows Amazon RDS to fully complete the create action before the DB cluster parameter group is used as the default for a new DB cluster. This is especially important for parameters that are critical when creating the default database for a DB cluster, such as the character set for the default database defined by the @character_set_database@ parameter. You can use the /Parameter Groups/ option of the <https://console.aws.amazon.com/rds/ Amazon RDS console> or the @DescribeDBClusterParameters@ action to verify that your DB cluster parameter group has been created or modified.
-- For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./ 
module Network.AWS.RDS.CreateDBClusterParameterGroup
    (
    -- * Creating a request
      CreateDBClusterParameterGroup (..)
    , mkCreateDBClusterParameterGroup
    -- ** Request lenses
    , cdbcpgDBClusterParameterGroupName
    , cdbcpgDBParameterGroupFamily
    , cdbcpgDescription
    , cdbcpgTags

    -- * Destructuring the response
    , CreateDBClusterParameterGroupResponse (..)
    , mkCreateDBClusterParameterGroupResponse
    -- ** Response lenses
    , cdbcpgrrsDBClusterParameterGroup
    , cdbcpgrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkCreateDBClusterParameterGroup' smart constructor.
data CreateDBClusterParameterGroup = CreateDBClusterParameterGroup'
  { dBClusterParameterGroupName :: Core.Text
    -- ^ The name of the DB cluster parameter group.
--
-- Constraints:
--
--     * Must match the name of an existing DB cluster parameter group.
--
--
  , dBParameterGroupFamily :: Core.Text
    -- ^ The DB cluster parameter group family name. A DB cluster parameter group can be associated with one and only one DB cluster parameter group family, and can be applied only to a DB cluster running a database engine and engine version compatible with that DB cluster parameter group family.
--
-- __Aurora MySQL__ 
-- Example: @aurora5.6@ , @aurora-mysql5.7@ 
-- __Aurora PostgreSQL__ 
-- Example: @aurora-postgresql9.6@ 
  , description :: Core.Text
    -- ^ The description for the DB cluster parameter group.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Tags to assign to the DB cluster parameter group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDBClusterParameterGroup' value with any optional fields omitted.
mkCreateDBClusterParameterGroup
    :: Core.Text -- ^ 'dBClusterParameterGroupName'
    -> Core.Text -- ^ 'dBParameterGroupFamily'
    -> Core.Text -- ^ 'description'
    -> CreateDBClusterParameterGroup
mkCreateDBClusterParameterGroup dBClusterParameterGroupName
  dBParameterGroupFamily description
  = CreateDBClusterParameterGroup'{dBClusterParameterGroupName,
                                   dBParameterGroupFamily, description, tags = Core.Nothing}

-- | The name of the DB cluster parameter group.
--
-- Constraints:
--
--     * Must match the name of an existing DB cluster parameter group.
--
--
--
-- /Note:/ Consider using 'dBClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgDBClusterParameterGroupName :: Lens.Lens' CreateDBClusterParameterGroup Core.Text
cdbcpgDBClusterParameterGroupName = Lens.field @"dBClusterParameterGroupName"
{-# INLINEABLE cdbcpgDBClusterParameterGroupName #-}
{-# DEPRECATED dBClusterParameterGroupName "Use generic-lens or generic-optics with 'dBClusterParameterGroupName' instead"  #-}

-- | The DB cluster parameter group family name. A DB cluster parameter group can be associated with one and only one DB cluster parameter group family, and can be applied only to a DB cluster running a database engine and engine version compatible with that DB cluster parameter group family.
--
-- __Aurora MySQL__ 
-- Example: @aurora5.6@ , @aurora-mysql5.7@ 
-- __Aurora PostgreSQL__ 
-- Example: @aurora-postgresql9.6@ 
--
-- /Note:/ Consider using 'dBParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgDBParameterGroupFamily :: Lens.Lens' CreateDBClusterParameterGroup Core.Text
cdbcpgDBParameterGroupFamily = Lens.field @"dBParameterGroupFamily"
{-# INLINEABLE cdbcpgDBParameterGroupFamily #-}
{-# DEPRECATED dBParameterGroupFamily "Use generic-lens or generic-optics with 'dBParameterGroupFamily' instead"  #-}

-- | The description for the DB cluster parameter group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgDescription :: Lens.Lens' CreateDBClusterParameterGroup Core.Text
cdbcpgDescription = Lens.field @"description"
{-# INLINEABLE cdbcpgDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Tags to assign to the DB cluster parameter group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgTags :: Lens.Lens' CreateDBClusterParameterGroup (Core.Maybe [Types.Tag])
cdbcpgTags = Lens.field @"tags"
{-# INLINEABLE cdbcpgTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery CreateDBClusterParameterGroup where
        toQuery CreateDBClusterParameterGroup{..}
          = Core.toQueryPair "Action"
              ("CreateDBClusterParameterGroup" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2014-10-31" :: Core.Text)
              Core.<>
              Core.toQueryPair "DBClusterParameterGroupName"
                dBClusterParameterGroupName
              Core.<>
              Core.toQueryPair "DBParameterGroupFamily" dBParameterGroupFamily
              Core.<> Core.toQueryPair "Description" description
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "Tag") tags)

instance Core.ToHeaders CreateDBClusterParameterGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateDBClusterParameterGroup where
        type Rs CreateDBClusterParameterGroup =
             CreateDBClusterParameterGroupResponse
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
          = Response.receiveXMLWrapper "CreateDBClusterParameterGroupResult"
              (\ s h x ->
                 CreateDBClusterParameterGroupResponse' Core.<$>
                   (x Core..@? "DBClusterParameterGroup") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateDBClusterParameterGroupResponse' smart constructor.
data CreateDBClusterParameterGroupResponse = CreateDBClusterParameterGroupResponse'
  { dBClusterParameterGroup :: Core.Maybe Types.DBClusterParameterGroup
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDBClusterParameterGroupResponse' value with any optional fields omitted.
mkCreateDBClusterParameterGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateDBClusterParameterGroupResponse
mkCreateDBClusterParameterGroupResponse responseStatus
  = CreateDBClusterParameterGroupResponse'{dBClusterParameterGroup =
                                             Core.Nothing,
                                           responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBClusterParameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgrrsDBClusterParameterGroup :: Lens.Lens' CreateDBClusterParameterGroupResponse (Core.Maybe Types.DBClusterParameterGroup)
cdbcpgrrsDBClusterParameterGroup = Lens.field @"dBClusterParameterGroup"
{-# INLINEABLE cdbcpgrrsDBClusterParameterGroup #-}
{-# DEPRECATED dBClusterParameterGroup "Use generic-lens or generic-optics with 'dBClusterParameterGroup' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgrrsResponseStatus :: Lens.Lens' CreateDBClusterParameterGroupResponse Core.Int
cdbcpgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cdbcpgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
