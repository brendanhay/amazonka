{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describe a thing group.
module Network.AWS.IoT.DescribeThingGroup
  ( -- * Creating a request
    DescribeThingGroup (..),
    mkDescribeThingGroup,

    -- ** Request lenses
    dtgThingGroupName,

    -- * Destructuring the response
    DescribeThingGroupResponse (..),
    mkDescribeThingGroupResponse,

    -- ** Response lenses
    dtgrrsIndexName,
    dtgrrsQueryString,
    dtgrrsQueryVersion,
    dtgrrsStatus,
    dtgrrsThingGroupArn,
    dtgrrsThingGroupId,
    dtgrrsThingGroupMetadata,
    dtgrrsThingGroupName,
    dtgrrsThingGroupProperties,
    dtgrrsVersion,
    dtgrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeThingGroup' smart constructor.
newtype DescribeThingGroup = DescribeThingGroup'
  { -- | The name of the thing group.
    thingGroupName :: Types.ThingGroupName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeThingGroup' value with any optional fields omitted.
mkDescribeThingGroup ::
  -- | 'thingGroupName'
  Types.ThingGroupName ->
  DescribeThingGroup
mkDescribeThingGroup thingGroupName =
  DescribeThingGroup' {thingGroupName}

-- | The name of the thing group.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgThingGroupName :: Lens.Lens' DescribeThingGroup Types.ThingGroupName
dtgThingGroupName = Lens.field @"thingGroupName"
{-# DEPRECATED dtgThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

instance Core.AWSRequest DescribeThingGroup where
  type Rs DescribeThingGroup = DescribeThingGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/thing-groups/" Core.<> (Core.toText thingGroupName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeThingGroupResponse'
            Core.<$> (x Core..:? "indexName")
            Core.<*> (x Core..:? "queryString")
            Core.<*> (x Core..:? "queryVersion")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "thingGroupArn")
            Core.<*> (x Core..:? "thingGroupId")
            Core.<*> (x Core..:? "thingGroupMetadata")
            Core.<*> (x Core..:? "thingGroupName")
            Core.<*> (x Core..:? "thingGroupProperties")
            Core.<*> (x Core..:? "version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDescribeThingGroupResponse' smart constructor.
data DescribeThingGroupResponse = DescribeThingGroupResponse'
  { -- | The dynamic thing group index name.
    indexName :: Core.Maybe Types.IndexName,
    -- | The dynamic thing group search query string.
    queryString :: Core.Maybe Types.QueryString,
    -- | The dynamic thing group query version.
    queryVersion :: Core.Maybe Types.QueryVersion,
    -- | The dynamic thing group status.
    status :: Core.Maybe Types.DynamicGroupStatus,
    -- | The thing group ARN.
    thingGroupArn :: Core.Maybe Types.ThingGroupArn,
    -- | The thing group ID.
    thingGroupId :: Core.Maybe Types.ThingGroupId,
    -- | Thing group metadata.
    thingGroupMetadata :: Core.Maybe Types.ThingGroupMetadata,
    -- | The name of the thing group.
    thingGroupName :: Core.Maybe Types.ThingGroupName,
    -- | The thing group properties.
    thingGroupProperties :: Core.Maybe Types.ThingGroupProperties,
    -- | The version of the thing group.
    version :: Core.Maybe Core.Integer,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeThingGroupResponse' value with any optional fields omitted.
mkDescribeThingGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeThingGroupResponse
mkDescribeThingGroupResponse responseStatus =
  DescribeThingGroupResponse'
    { indexName = Core.Nothing,
      queryString = Core.Nothing,
      queryVersion = Core.Nothing,
      status = Core.Nothing,
      thingGroupArn = Core.Nothing,
      thingGroupId = Core.Nothing,
      thingGroupMetadata = Core.Nothing,
      thingGroupName = Core.Nothing,
      thingGroupProperties = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | The dynamic thing group index name.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrrsIndexName :: Lens.Lens' DescribeThingGroupResponse (Core.Maybe Types.IndexName)
dtgrrsIndexName = Lens.field @"indexName"
{-# DEPRECATED dtgrrsIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The dynamic thing group search query string.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrrsQueryString :: Lens.Lens' DescribeThingGroupResponse (Core.Maybe Types.QueryString)
dtgrrsQueryString = Lens.field @"queryString"
{-# DEPRECATED dtgrrsQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

-- | The dynamic thing group query version.
--
-- /Note:/ Consider using 'queryVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrrsQueryVersion :: Lens.Lens' DescribeThingGroupResponse (Core.Maybe Types.QueryVersion)
dtgrrsQueryVersion = Lens.field @"queryVersion"
{-# DEPRECATED dtgrrsQueryVersion "Use generic-lens or generic-optics with 'queryVersion' instead." #-}

-- | The dynamic thing group status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrrsStatus :: Lens.Lens' DescribeThingGroupResponse (Core.Maybe Types.DynamicGroupStatus)
dtgrrsStatus = Lens.field @"status"
{-# DEPRECATED dtgrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The thing group ARN.
--
-- /Note:/ Consider using 'thingGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrrsThingGroupArn :: Lens.Lens' DescribeThingGroupResponse (Core.Maybe Types.ThingGroupArn)
dtgrrsThingGroupArn = Lens.field @"thingGroupArn"
{-# DEPRECATED dtgrrsThingGroupArn "Use generic-lens or generic-optics with 'thingGroupArn' instead." #-}

-- | The thing group ID.
--
-- /Note:/ Consider using 'thingGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrrsThingGroupId :: Lens.Lens' DescribeThingGroupResponse (Core.Maybe Types.ThingGroupId)
dtgrrsThingGroupId = Lens.field @"thingGroupId"
{-# DEPRECATED dtgrrsThingGroupId "Use generic-lens or generic-optics with 'thingGroupId' instead." #-}

-- | Thing group metadata.
--
-- /Note:/ Consider using 'thingGroupMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrrsThingGroupMetadata :: Lens.Lens' DescribeThingGroupResponse (Core.Maybe Types.ThingGroupMetadata)
dtgrrsThingGroupMetadata = Lens.field @"thingGroupMetadata"
{-# DEPRECATED dtgrrsThingGroupMetadata "Use generic-lens or generic-optics with 'thingGroupMetadata' instead." #-}

-- | The name of the thing group.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrrsThingGroupName :: Lens.Lens' DescribeThingGroupResponse (Core.Maybe Types.ThingGroupName)
dtgrrsThingGroupName = Lens.field @"thingGroupName"
{-# DEPRECATED dtgrrsThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

-- | The thing group properties.
--
-- /Note:/ Consider using 'thingGroupProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrrsThingGroupProperties :: Lens.Lens' DescribeThingGroupResponse (Core.Maybe Types.ThingGroupProperties)
dtgrrsThingGroupProperties = Lens.field @"thingGroupProperties"
{-# DEPRECATED dtgrrsThingGroupProperties "Use generic-lens or generic-optics with 'thingGroupProperties' instead." #-}

-- | The version of the thing group.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrrsVersion :: Lens.Lens' DescribeThingGroupResponse (Core.Maybe Core.Integer)
dtgrrsVersion = Lens.field @"version"
{-# DEPRECATED dtgrrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgrrsResponseStatus :: Lens.Lens' DescribeThingGroupResponse Core.Int
dtgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
