{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Global Datastore for Redis offers fully managed, fast, reliable and secure cross-region replication. Using Global Datastore for Redis, you can create cross-region read replica clusters for ElastiCache for Redis to enable low-latency reads and disaster recovery across regions. For more information, see </AmazonElastiCache/latest/red-ug/Redis-Global-Clusters.html Replication Across Regions Using Global Datastore> .
--
--
--     * The __GlobalReplicationGroupIdSuffix__ is the name of the Global Datastore.
--
--
--     * The __PrimaryReplicationGroupId__ represents the name of the primary cluster that accepts writes and will replicate updates to the secondary cluster.
module Network.AWS.ElastiCache.CreateGlobalReplicationGroup
  ( -- * Creating a request
    CreateGlobalReplicationGroup (..),
    mkCreateGlobalReplicationGroup,

    -- ** Request lenses
    cgrgGlobalReplicationGroupIdSuffix,
    cgrgPrimaryReplicationGroupId,
    cgrgGlobalReplicationGroupDescription,

    -- * Destructuring the response
    CreateGlobalReplicationGroupResponse (..),
    mkCreateGlobalReplicationGroupResponse,

    -- ** Response lenses
    cgrgrrsGlobalReplicationGroup,
    cgrgrrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateGlobalReplicationGroup' smart constructor.
data CreateGlobalReplicationGroup = CreateGlobalReplicationGroup'
  { -- | The suffix name of a Global Datastore. Amazon ElastiCache automatically applies a prefix to the Global Datastore ID when it is created. Each AWS Region has its own prefix. For instance, a Global Datastore ID created in the US-West-1 region will begin with "dsdfu" along with the suffix name you provide. The suffix, combined with the auto-generated prefix, guarantees uniqueness of the Global Datastore name across multiple regions.
    --
    -- For a full list of AWS Regions and their respective Global Datastore iD prefixes, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Redis-Global-Clusters-CLI.html Using the AWS CLI with Global Datastores > .
    globalReplicationGroupIdSuffix :: Types.GlobalReplicationGroupIdSuffix,
    -- | The name of the primary cluster that accepts writes and will replicate updates to the secondary cluster.
    primaryReplicationGroupId :: Types.PrimaryReplicationGroupId,
    -- | Provides details of the Global Datastore
    globalReplicationGroupDescription :: Core.Maybe Types.GlobalReplicationGroupDescription
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGlobalReplicationGroup' value with any optional fields omitted.
mkCreateGlobalReplicationGroup ::
  -- | 'globalReplicationGroupIdSuffix'
  Types.GlobalReplicationGroupIdSuffix ->
  -- | 'primaryReplicationGroupId'
  Types.PrimaryReplicationGroupId ->
  CreateGlobalReplicationGroup
mkCreateGlobalReplicationGroup
  globalReplicationGroupIdSuffix
  primaryReplicationGroupId =
    CreateGlobalReplicationGroup'
      { globalReplicationGroupIdSuffix,
        primaryReplicationGroupId,
        globalReplicationGroupDescription = Core.Nothing
      }

-- | The suffix name of a Global Datastore. Amazon ElastiCache automatically applies a prefix to the Global Datastore ID when it is created. Each AWS Region has its own prefix. For instance, a Global Datastore ID created in the US-West-1 region will begin with "dsdfu" along with the suffix name you provide. The suffix, combined with the auto-generated prefix, guarantees uniqueness of the Global Datastore name across multiple regions.
--
-- For a full list of AWS Regions and their respective Global Datastore iD prefixes, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Redis-Global-Clusters-CLI.html Using the AWS CLI with Global Datastores > .
--
-- /Note:/ Consider using 'globalReplicationGroupIdSuffix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrgGlobalReplicationGroupIdSuffix :: Lens.Lens' CreateGlobalReplicationGroup Types.GlobalReplicationGroupIdSuffix
cgrgGlobalReplicationGroupIdSuffix = Lens.field @"globalReplicationGroupIdSuffix"
{-# DEPRECATED cgrgGlobalReplicationGroupIdSuffix "Use generic-lens or generic-optics with 'globalReplicationGroupIdSuffix' instead." #-}

-- | The name of the primary cluster that accepts writes and will replicate updates to the secondary cluster.
--
-- /Note:/ Consider using 'primaryReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrgPrimaryReplicationGroupId :: Lens.Lens' CreateGlobalReplicationGroup Types.PrimaryReplicationGroupId
cgrgPrimaryReplicationGroupId = Lens.field @"primaryReplicationGroupId"
{-# DEPRECATED cgrgPrimaryReplicationGroupId "Use generic-lens or generic-optics with 'primaryReplicationGroupId' instead." #-}

-- | Provides details of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrgGlobalReplicationGroupDescription :: Lens.Lens' CreateGlobalReplicationGroup (Core.Maybe Types.GlobalReplicationGroupDescription)
cgrgGlobalReplicationGroupDescription = Lens.field @"globalReplicationGroupDescription"
{-# DEPRECATED cgrgGlobalReplicationGroupDescription "Use generic-lens or generic-optics with 'globalReplicationGroupDescription' instead." #-}

instance Core.AWSRequest CreateGlobalReplicationGroup where
  type
    Rs CreateGlobalReplicationGroup =
      CreateGlobalReplicationGroupResponse
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
            ( Core.pure ("Action", "CreateGlobalReplicationGroup")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> ( Core.toQueryValue
                            "GlobalReplicationGroupIdSuffix"
                            globalReplicationGroupIdSuffix
                        )
                Core.<> ( Core.toQueryValue
                            "PrimaryReplicationGroupId"
                            primaryReplicationGroupId
                        )
                Core.<> ( Core.toQueryValue "GlobalReplicationGroupDescription"
                            Core.<$> globalReplicationGroupDescription
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateGlobalReplicationGroupResult"
      ( \s h x ->
          CreateGlobalReplicationGroupResponse'
            Core.<$> (x Core..@? "GlobalReplicationGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateGlobalReplicationGroupResponse' smart constructor.
data CreateGlobalReplicationGroupResponse = CreateGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Core.Maybe Types.GlobalReplicationGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGlobalReplicationGroupResponse' value with any optional fields omitted.
mkCreateGlobalReplicationGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateGlobalReplicationGroupResponse
mkCreateGlobalReplicationGroupResponse responseStatus =
  CreateGlobalReplicationGroupResponse'
    { globalReplicationGroup =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrgrrsGlobalReplicationGroup :: Lens.Lens' CreateGlobalReplicationGroupResponse (Core.Maybe Types.GlobalReplicationGroup)
cgrgrrsGlobalReplicationGroup = Lens.field @"globalReplicationGroup"
{-# DEPRECATED cgrgrrsGlobalReplicationGroup "Use generic-lens or generic-optics with 'globalReplicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgrgrrsResponseStatus :: Lens.Lens' CreateGlobalReplicationGroupResponse Core.Int
cgrgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cgrgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
