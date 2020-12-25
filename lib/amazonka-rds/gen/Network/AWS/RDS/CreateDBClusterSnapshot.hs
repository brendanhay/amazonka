{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBClusterSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a snapshot of a DB cluster. For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
module Network.AWS.RDS.CreateDBClusterSnapshot
  ( -- * Creating a request
    CreateDBClusterSnapshot (..),
    mkCreateDBClusterSnapshot,

    -- ** Request lenses
    cdbcsDBClusterSnapshotIdentifier,
    cdbcsDBClusterIdentifier,
    cdbcsTags,

    -- * Destructuring the response
    CreateDBClusterSnapshotResponse (..),
    mkCreateDBClusterSnapshotResponse,

    -- ** Response lenses
    cdbcsrrsDBClusterSnapshot,
    cdbcsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkCreateDBClusterSnapshot' smart constructor.
data CreateDBClusterSnapshot = CreateDBClusterSnapshot'
  { -- | The identifier of the DB cluster snapshot. This parameter is stored as a lowercase string.
    --
    -- Constraints:
    --
    --     * Must contain from 1 to 63 letters, numbers, or hyphens.
    --
    --
    --     * First character must be a letter.
    --
    --
    --     * Can't end with a hyphen or contain two consecutive hyphens.
    --
    --
    -- Example: @my-cluster1-snapshot1@
    dBClusterSnapshotIdentifier :: Types.DBClusterSnapshotIdentifier,
    -- | The identifier of the DB cluster to create a snapshot for. This parameter isn't case-sensitive.
    --
    -- Constraints:
    --
    --     * Must match the identifier of an existing DBCluster.
    --
    --
    -- Example: @my-cluster1@
    dBClusterIdentifier :: Types.DBClusterIdentifier,
    -- | The tags to be assigned to the DB cluster snapshot.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDBClusterSnapshot' value with any optional fields omitted.
mkCreateDBClusterSnapshot ::
  -- | 'dBClusterSnapshotIdentifier'
  Types.DBClusterSnapshotIdentifier ->
  -- | 'dBClusterIdentifier'
  Types.DBClusterIdentifier ->
  CreateDBClusterSnapshot
mkCreateDBClusterSnapshot
  dBClusterSnapshotIdentifier
  dBClusterIdentifier =
    CreateDBClusterSnapshot'
      { dBClusterSnapshotIdentifier,
        dBClusterIdentifier,
        tags = Core.Nothing
      }

-- | The identifier of the DB cluster snapshot. This parameter is stored as a lowercase string.
--
-- Constraints:
--
--     * Must contain from 1 to 63 letters, numbers, or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Can't end with a hyphen or contain two consecutive hyphens.
--
--
-- Example: @my-cluster1-snapshot1@
--
-- /Note:/ Consider using 'dBClusterSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsDBClusterSnapshotIdentifier :: Lens.Lens' CreateDBClusterSnapshot Types.DBClusterSnapshotIdentifier
cdbcsDBClusterSnapshotIdentifier = Lens.field @"dBClusterSnapshotIdentifier"
{-# DEPRECATED cdbcsDBClusterSnapshotIdentifier "Use generic-lens or generic-optics with 'dBClusterSnapshotIdentifier' instead." #-}

-- | The identifier of the DB cluster to create a snapshot for. This parameter isn't case-sensitive.
--
-- Constraints:
--
--     * Must match the identifier of an existing DBCluster.
--
--
-- Example: @my-cluster1@
--
-- /Note:/ Consider using 'dBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsDBClusterIdentifier :: Lens.Lens' CreateDBClusterSnapshot Types.DBClusterIdentifier
cdbcsDBClusterIdentifier = Lens.field @"dBClusterIdentifier"
{-# DEPRECATED cdbcsDBClusterIdentifier "Use generic-lens or generic-optics with 'dBClusterIdentifier' instead." #-}

-- | The tags to be assigned to the DB cluster snapshot.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsTags :: Lens.Lens' CreateDBClusterSnapshot (Core.Maybe [Types.Tag])
cdbcsTags = Lens.field @"tags"
{-# DEPRECATED cdbcsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.AWSRequest CreateDBClusterSnapshot where
  type Rs CreateDBClusterSnapshot = CreateDBClusterSnapshotResponse
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
            ( Core.pure ("Action", "CreateDBClusterSnapshot")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> ( Core.toQueryValue
                            "DBClusterSnapshotIdentifier"
                            dBClusterSnapshotIdentifier
                        )
                Core.<> (Core.toQueryValue "DBClusterIdentifier" dBClusterIdentifier)
                Core.<> (Core.toQueryValue "Tags" (Core.toQueryList "Tag" Core.<$> tags))
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateDBClusterSnapshotResult"
      ( \s h x ->
          CreateDBClusterSnapshotResponse'
            Core.<$> (x Core..@? "DBClusterSnapshot")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateDBClusterSnapshotResponse' smart constructor.
data CreateDBClusterSnapshotResponse = CreateDBClusterSnapshotResponse'
  { dBClusterSnapshot :: Core.Maybe Types.DBClusterSnapshot,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateDBClusterSnapshotResponse' value with any optional fields omitted.
mkCreateDBClusterSnapshotResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateDBClusterSnapshotResponse
mkCreateDBClusterSnapshotResponse responseStatus =
  CreateDBClusterSnapshotResponse'
    { dBClusterSnapshot =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'dBClusterSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsrrsDBClusterSnapshot :: Lens.Lens' CreateDBClusterSnapshotResponse (Core.Maybe Types.DBClusterSnapshot)
cdbcsrrsDBClusterSnapshot = Lens.field @"dBClusterSnapshot"
{-# DEPRECATED cdbcsrrsDBClusterSnapshot "Use generic-lens or generic-optics with 'dBClusterSnapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdbcsrrsResponseStatus :: Lens.Lens' CreateDBClusterSnapshotResponse Core.Int
cdbcsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdbcsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
