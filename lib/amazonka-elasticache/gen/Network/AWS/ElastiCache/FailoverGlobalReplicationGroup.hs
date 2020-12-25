{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.FailoverGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to failover the primary region to a selected secondary region. The selected secondary region will become primary, and all other clusters will become secondary.
module Network.AWS.ElastiCache.FailoverGlobalReplicationGroup
  ( -- * Creating a request
    FailoverGlobalReplicationGroup (..),
    mkFailoverGlobalReplicationGroup,

    -- ** Request lenses
    fgrgGlobalReplicationGroupId,
    fgrgPrimaryRegion,
    fgrgPrimaryReplicationGroupId,

    -- * Destructuring the response
    FailoverGlobalReplicationGroupResponse (..),
    mkFailoverGlobalReplicationGroupResponse,

    -- ** Response lenses
    fgrgrrsGlobalReplicationGroup,
    fgrgrrsResponseStatus,
  )
where

import qualified Network.AWS.ElastiCache.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkFailoverGlobalReplicationGroup' smart constructor.
data FailoverGlobalReplicationGroup = FailoverGlobalReplicationGroup'
  { -- | The name of the Global Datastore
    globalReplicationGroupId :: Types.GlobalReplicationGroupId,
    -- | The AWS region of the primary cluster of the Global Datastore
    primaryRegion :: Types.PrimaryRegion,
    -- | The name of the primary replication group
    primaryReplicationGroupId :: Types.PrimaryReplicationGroupId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FailoverGlobalReplicationGroup' value with any optional fields omitted.
mkFailoverGlobalReplicationGroup ::
  -- | 'globalReplicationGroupId'
  Types.GlobalReplicationGroupId ->
  -- | 'primaryRegion'
  Types.PrimaryRegion ->
  -- | 'primaryReplicationGroupId'
  Types.PrimaryReplicationGroupId ->
  FailoverGlobalReplicationGroup
mkFailoverGlobalReplicationGroup
  globalReplicationGroupId
  primaryRegion
  primaryReplicationGroupId =
    FailoverGlobalReplicationGroup'
      { globalReplicationGroupId,
        primaryRegion,
        primaryReplicationGroupId
      }

-- | The name of the Global Datastore
--
-- /Note:/ Consider using 'globalReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fgrgGlobalReplicationGroupId :: Lens.Lens' FailoverGlobalReplicationGroup Types.GlobalReplicationGroupId
fgrgGlobalReplicationGroupId = Lens.field @"globalReplicationGroupId"
{-# DEPRECATED fgrgGlobalReplicationGroupId "Use generic-lens or generic-optics with 'globalReplicationGroupId' instead." #-}

-- | The AWS region of the primary cluster of the Global Datastore
--
-- /Note:/ Consider using 'primaryRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fgrgPrimaryRegion :: Lens.Lens' FailoverGlobalReplicationGroup Types.PrimaryRegion
fgrgPrimaryRegion = Lens.field @"primaryRegion"
{-# DEPRECATED fgrgPrimaryRegion "Use generic-lens or generic-optics with 'primaryRegion' instead." #-}

-- | The name of the primary replication group
--
-- /Note:/ Consider using 'primaryReplicationGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fgrgPrimaryReplicationGroupId :: Lens.Lens' FailoverGlobalReplicationGroup Types.PrimaryReplicationGroupId
fgrgPrimaryReplicationGroupId = Lens.field @"primaryReplicationGroupId"
{-# DEPRECATED fgrgPrimaryReplicationGroupId "Use generic-lens or generic-optics with 'primaryReplicationGroupId' instead." #-}

instance Core.AWSRequest FailoverGlobalReplicationGroup where
  type
    Rs FailoverGlobalReplicationGroup =
      FailoverGlobalReplicationGroupResponse
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
            ( Core.pure ("Action", "FailoverGlobalReplicationGroup")
                Core.<> (Core.pure ("Version", "2015-02-02"))
                Core.<> ( Core.toQueryValue
                            "GlobalReplicationGroupId"
                            globalReplicationGroupId
                        )
                Core.<> (Core.toQueryValue "PrimaryRegion" primaryRegion)
                Core.<> ( Core.toQueryValue
                            "PrimaryReplicationGroupId"
                            primaryReplicationGroupId
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "FailoverGlobalReplicationGroupResult"
      ( \s h x ->
          FailoverGlobalReplicationGroupResponse'
            Core.<$> (x Core..@? "GlobalReplicationGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkFailoverGlobalReplicationGroupResponse' smart constructor.
data FailoverGlobalReplicationGroupResponse = FailoverGlobalReplicationGroupResponse'
  { globalReplicationGroup :: Core.Maybe Types.GlobalReplicationGroup,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FailoverGlobalReplicationGroupResponse' value with any optional fields omitted.
mkFailoverGlobalReplicationGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  FailoverGlobalReplicationGroupResponse
mkFailoverGlobalReplicationGroupResponse responseStatus =
  FailoverGlobalReplicationGroupResponse'
    { globalReplicationGroup =
        Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalReplicationGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fgrgrrsGlobalReplicationGroup :: Lens.Lens' FailoverGlobalReplicationGroupResponse (Core.Maybe Types.GlobalReplicationGroup)
fgrgrrsGlobalReplicationGroup = Lens.field @"globalReplicationGroup"
{-# DEPRECATED fgrgrrsGlobalReplicationGroup "Use generic-lens or generic-optics with 'globalReplicationGroup' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fgrgrrsResponseStatus :: Lens.Lens' FailoverGlobalReplicationGroupResponse Core.Int
fgrgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED fgrgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
