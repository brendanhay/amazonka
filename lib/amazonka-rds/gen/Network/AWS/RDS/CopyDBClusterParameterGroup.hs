{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CopyDBClusterParameterGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified DB cluster parameter group.
module Network.AWS.RDS.CopyDBClusterParameterGroup
  ( -- * Creating a request
    CopyDBClusterParameterGroup (..),
    mkCopyDBClusterParameterGroup,

    -- ** Request lenses
    cdbcpgfSourceDBClusterParameterGroupIdentifier,
    cdbcpgfTargetDBClusterParameterGroupIdentifier,
    cdbcpgfTargetDBClusterParameterGroupDescription,
    cdbcpgfTags,

    -- * Destructuring the response
    CopyDBClusterParameterGroupResponse (..),
    mkCopyDBClusterParameterGroupResponse,

    -- ** Response lenses
    cdbcpgrfrsDBClusterParameterGroup,
    cdbcpgrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCopyDBClusterParameterGroup' smart constructor.
data CopyDBClusterParameterGroup = CopyDBClusterParameterGroup'
  { -- | The identifier or Amazon Resource Name (ARN) for the source DB cluster parameter group. For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS> in the /Amazon Aurora User Guide/ .
    --
    -- Constraints:
    --
    --     * Must specify a valid DB cluster parameter group.
    --
    --
    --     * If the source DB cluster parameter group is in the same AWS Region as the copy, specify a valid DB parameter group identifier, for example @my-db-cluster-param-group@ , or a valid ARN.
    --
    --
    --     * If the source DB parameter group is in a different AWS Region than the copy, specify a valid DB cluster parameter group ARN, for example @arn:aws:rds:us-east-1:123456789012:cluster-pg:custom-cluster-group1@ .
    sourceDBClusterParameterGroupIdentifier :: Types.String,
    -- | The identifier for the copied DB cluster parameter group.
    --
    -- Constraints:
    --
    --     * Can't be null, empty, or blank
    --
    --
    --     * Must contain from 1 to 255 letters, numbers, or hyphens
    --
    --
    --     * First character must be a letter
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens
    --
    --
    -- Example: @my-cluster-param-group1@
    targetDBClusterParameterGroupIdentifier :: Types.String,
    -- | A description for the copied DB cluster parameter group.
    targetDBClusterParameterGroupDescription :: Types.String,
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyDBClusterParameterGroup' value with any optional fields omitted.
mkCopyDBClusterParameterGroup ::
  -- | 'sourceDBClusterParameterGroupIdentifier'
  Types.String ->
  -- | 'targetDBClusterParameterGroupIdentifier'
  Types.String ->
  -- | 'targetDBClusterParameterGroupDescription'
  Types.String ->
  CopyDBClusterParameterGroup
mkCopyDBClusterParameterGroup
  sourceDBClusterParameterGroupIdentifier
  targetDBClusterParameterGroupIdentifier
  targetDBClusterParameterGroupDescription =
    CopyDBClusterParameterGroup'
      { sourceDBClusterParameterGroupIdentifier,
        targetDBClusterParameterGroupIdentifier,
        targetDBClusterParameterGroupDescription,
        tags = Core.Nothing
      }

-- | The identifier or Amazon Resource Name (ARN) for the source DB cluster parameter group. For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an ARN for Amazon RDS> in the /Amazon Aurora User Guide/ .
--
-- Constraints:
--
--     * Must specify a valid DB cluster parameter group.
--
--
--     * If the source DB cluster parameter group is in the same AWS Region as the copy, specify a valid DB parameter group identifier, for example @my-db-cluster-param-group@ , or a valid ARN.
--
--
--     * If the source DB parameter group is in a different AWS Region than the copy, specify a valid DB cluster parameter group ARN, for example @arn:aws:rds:us-east-1:123456789012:cluster-pg:custom-cluster-group1@ .
--
--
--
-- /Note:/ Consider using 'sourceDBClusterParameterGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgfSourceDBClusterParameterGroupIdentifier :: Lens.Lens' CopyDBClusterParameterGroup Types.String
cdbcpgfSourceDBClusterParameterGroupIdentifier = Lens.field @"sourceDBClusterParameterGroupIdentifier"
{-# DEPRECATED cdbcpgfSourceDBClusterParameterGroupIdentifier "Use generic-lens or generic-optics with 'sourceDBClusterParameterGroupIdentifier' instead." #-}

-- | The identifier for the copied DB cluster parameter group.
--
-- Constraints:
--
--     * Can't be null, empty, or blank
--
--
--     * Must contain from 1 to 255 letters, numbers, or hyphens
--
--
--     * First character must be a letter
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens
--
--
-- Example: @my-cluster-param-group1@
--
-- /Note:/ Consider using 'targetDBClusterParameterGroupIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgfTargetDBClusterParameterGroupIdentifier :: Lens.Lens' CopyDBClusterParameterGroup Types.String
cdbcpgfTargetDBClusterParameterGroupIdentifier = Lens.field @"targetDBClusterParameterGroupIdentifier"
{-# DEPRECATED cdbcpgfTargetDBClusterParameterGroupIdentifier "Use generic-lens or generic-optics with 'targetDBClusterParameterGroupIdentifier' instead." #-}

-- | A description for the copied DB cluster parameter group.
--
-- /Note:/ Consider using 'targetDBClusterParameterGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgfTargetDBClusterParameterGroupDescription :: Lens.Lens' CopyDBClusterParameterGroup Types.String
cdbcpgfTargetDBClusterParameterGroupDescription = Lens.field @"targetDBClusterParameterGroupDescription"
{-# DEPRECATED cdbcpgfTargetDBClusterParameterGroupDescription "Use generic-lens or generic-optics with 'targetDBClusterParameterGroupDescription' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgfTags :: Lens.Lens' CopyDBClusterParameterGroup (Core.Maybe [Types.Tag])
cdbcpgfTags = Lens.field @"tags"
{-# DEPRECATED cdbcpgfTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest CopyDBClusterParameterGroup where
  type
    Rs CopyDBClusterParameterGroup =
      CopyDBClusterParameterGroupResponse
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
            ( Core.pure ("Action", "CopyDBClusterParameterGroup")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue
                            "SourceDBClusterParameterGroupIdentifier"
                            sourceDBClusterParameterGroupIdentifier
                        )
                Core.<> ( Core.toQueryValue
                            "TargetDBClusterParameterGroupIdentifier"
                            targetDBClusterParameterGroupIdentifier
                        )
                Core.<> ( Core.toQueryValue
                            "TargetDBClusterParameterGroupDescription"
                            targetDBClusterParameterGroupDescription
                        )
                Core.<> (Core.toQueryValue "Tags" (Core.toQueryList "Tag" Core.<$> tags))
            )
      }
  response =
    Response.receiveXMLWrapper
      "CopyDBClusterParameterGroupResult"
      ( \s h x ->
          CopyDBClusterParameterGroupResponse'
            Core.<$> (x Core..@? "DBClusterParameterGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCopyDBClusterParameterGroupResponse' smart constructor.
data CopyDBClusterParameterGroupResponse = CopyDBClusterParameterGroupResponse'
  { dBClusterParameterGroup :: Core.Maybe Types.DBClusterParameterGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CopyDBClusterParameterGroupResponse' value with any optional fields omitted.
mkCopyDBClusterParameterGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CopyDBClusterParameterGroupResponse
mkCopyDBClusterParameterGroupResponse responseStatus =
  CopyDBClusterParameterGroupResponse'
    { dBClusterParameterGroup =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBClusterParameterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgrfrsDBClusterParameterGroup :: Lens.Lens' CopyDBClusterParameterGroupResponse (Core.Maybe Types.DBClusterParameterGroup)
cdbcpgrfrsDBClusterParameterGroup = Lens.field @"dBClusterParameterGroup"
{-# DEPRECATED cdbcpgrfrsDBClusterParameterGroup "Use generic-lens or generic-optics with 'dBClusterParameterGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcpgrfrsResponseStatus :: Lens.Lens' CopyDBClusterParameterGroupResponse Core.Int
cdbcpgrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdbcpgrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
