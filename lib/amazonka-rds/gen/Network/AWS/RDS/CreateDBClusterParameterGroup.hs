{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateDBClusterParameterGroup (..),
    mkCreateDBClusterParameterGroup,

    -- ** Request lenses
    cdbcpgDBClusterParameterGroupName,
    cdbcpgDBParameterGroupFamily,
    cdbcpgDescription,
    cdbcpgTags,

    -- * Destructuring the response
    CreateDBClusterParameterGroupResponse (..),
    mkCreateDBClusterParameterGroupResponse,

    -- ** Response lenses
    cdbcpgrrsDBClusterParameterGroup,
    cdbcpgrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkCreateDBClusterParameterGroup' smart constructor.
data CreateDBClusterParameterGroup = CreateDBClusterParameterGroup'
  { -- | The name of the DB cluster parameter group.
    --
    -- Constraints:
    --
    --     * Must match the name of an existing DB cluster parameter group.
    dBClusterParameterGroupName :: Types.String,
    -- | The DB cluster parameter group family name. A DB cluster parameter group can be associated with one and only one DB cluster parameter group family, and can be applied only to a DB cluster running a database engine and engine version compatible with that DB cluster parameter group family.
    --
    -- __Aurora MySQL__
    -- Example: @aurora5.6@ , @aurora-mysql5.7@
    -- __Aurora PostgreSQL__
    -- Example: @aurora-postgresql9.6@
    dBParameterGroupFamily :: Types.String,
    -- | The description for the DB cluster parameter group.
    description :: Types.String,
    -- | Tags to assign to the DB cluster parameter group.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDBClusterParameterGroup' value with any optional fields omitted.
mkCreateDBClusterParameterGroup ::
  -- | 'dBClusterParameterGroupName'
  Types.String ->
  -- | 'dBParameterGroupFamily'
  Types.String ->
  -- | 'description'
  Types.String ->
  CreateDBClusterParameterGroup
mkCreateDBClusterParameterGroup
  dBClusterParameterGroupName
  dBParameterGroupFamily
  description =
    CreateDBClusterParameterGroup'
      { dBClusterParameterGroupName,
        dBParameterGroupFamily,
        description,
        tags = Core.Nothing
      }

-- | The name of the DB cluster parameter group.
--
-- Constraints:
--
--     * Must match the name of an existing DB cluster parameter group.
--
--
--
-- /Note:/ Consider using 'dBClusterParameterGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgDBClusterParameterGroupName :: Lens.Lens' CreateDBClusterParameterGroup Types.String
cdbcpgDBClusterParameterGroupName = Lens.field @"dBClusterParameterGroupName"
{-# DEPRECATED cdbcpgDBClusterParameterGroupName "Use generic-lens or generic-optics with 'dBClusterParameterGroupName' instead." #-}

-- | The DB cluster parameter group family name. A DB cluster parameter group can be associated with one and only one DB cluster parameter group family, and can be applied only to a DB cluster running a database engine and engine version compatible with that DB cluster parameter group family.
--
-- __Aurora MySQL__
-- Example: @aurora5.6@ , @aurora-mysql5.7@
-- __Aurora PostgreSQL__
-- Example: @aurora-postgresql9.6@
--
-- /Note:/ Consider using 'dBParameterGroupFamily' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgDBParameterGroupFamily :: Lens.Lens' CreateDBClusterParameterGroup Types.String
cdbcpgDBParameterGroupFamily = Lens.field @"dBParameterGroupFamily"
{-# DEPRECATED cdbcpgDBParameterGroupFamily "Use generic-lens or generic-optics with 'dBParameterGroupFamily' instead." #-}

-- | The description for the DB cluster parameter group.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgDescription :: Lens.Lens' CreateDBClusterParameterGroup Types.String
cdbcpgDescription = Lens.field @"description"
{-# DEPRECATED cdbcpgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Tags to assign to the DB cluster parameter group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgTags :: Lens.Lens' CreateDBClusterParameterGroup (Core.Maybe [Types.Tag])
cdbcpgTags = Lens.field @"tags"
{-# DEPRECATED cdbcpgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest CreateDBClusterParameterGroup where
  type
    Rs CreateDBClusterParameterGroup =
      CreateDBClusterParameterGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateDBClusterParameterGroup")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue
                            "DBClusterParameterGroupName"
                            dBClusterParameterGroupName
                        )
                Core.<> (Core.toQueryValue "DBParameterGroupFamily" dBParameterGroupFamily)
                Core.<> (Core.toQueryValue "Description" description)
                Core.<> (Core.toQueryValue "Tags" (Core.toQueryList "Tag" Core.<$> tags))
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateDBClusterParameterGroupResult"
      ( \s h x ->
          CreateDBClusterParameterGroupResponse'
            Core.<$> (x Core..@? "DBClusterParameterGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateDBClusterParameterGroupResponse' smart constructor.
data CreateDBClusterParameterGroupResponse = CreateDBClusterParameterGroupResponse'
  { dBClusterParameterGroup :: Core.Maybe Types.DBClusterParameterGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDBClusterParameterGroupResponse' value with any optional fields omitted.
mkCreateDBClusterParameterGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateDBClusterParameterGroupResponse
mkCreateDBClusterParameterGroupResponse responseStatus =
  CreateDBClusterParameterGroupResponse'
    { dBClusterParameterGroup =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBClusterParameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgrrsDBClusterParameterGroup :: Lens.Lens' CreateDBClusterParameterGroupResponse (Core.Maybe Types.DBClusterParameterGroup)
cdbcpgrrsDBClusterParameterGroup = Lens.field @"dBClusterParameterGroup"
{-# DEPRECATED cdbcpgrrsDBClusterParameterGroup "Use generic-lens or generic-optics with 'dBClusterParameterGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgrrsResponseStatus :: Lens.Lens' CreateDBClusterParameterGroupResponse Core.Int
cdbcpgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdbcpgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
