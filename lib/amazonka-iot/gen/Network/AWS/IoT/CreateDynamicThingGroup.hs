{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateDynamicThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a dynamic thing group.
module Network.AWS.IoT.CreateDynamicThingGroup
  ( -- * Creating a request
    CreateDynamicThingGroup (..),
    mkCreateDynamicThingGroup,

    -- ** Request lenses
    cdtgThingGroupName,
    cdtgQueryString,
    cdtgIndexName,
    cdtgQueryVersion,
    cdtgTags,
    cdtgThingGroupProperties,

    -- * Destructuring the response
    CreateDynamicThingGroupResponse (..),
    mkCreateDynamicThingGroupResponse,

    -- ** Response lenses
    cdtgrrsIndexName,
    cdtgrrsQueryString,
    cdtgrrsQueryVersion,
    cdtgrrsThingGroupArn,
    cdtgrrsThingGroupId,
    cdtgrrsThingGroupName,
    cdtgrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDynamicThingGroup' smart constructor.
data CreateDynamicThingGroup = CreateDynamicThingGroup'
  { -- | The dynamic thing group name to create.
    thingGroupName :: Types.ThingGroupName,
    -- | The dynamic thing group search query string.
    --
    -- See <https://docs.aws.amazon.com/iot/latest/developerguide/query-syntax.html Query Syntax> for information about query string syntax.
    queryString :: Types.QueryString,
    -- | The dynamic thing group index name.
    indexName :: Core.Maybe Types.IndexName,
    -- | The dynamic thing group query version.
    queryVersion :: Core.Maybe Types.QueryVersion,
    -- | Metadata which can be used to manage the dynamic thing group.
    tags :: Core.Maybe [Types.Tag],
    -- | The dynamic thing group properties.
    thingGroupProperties :: Core.Maybe Types.ThingGroupProperties
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDynamicThingGroup' value with any optional fields omitted.
mkCreateDynamicThingGroup ::
  -- | 'thingGroupName'
  Types.ThingGroupName ->
  -- | 'queryString'
  Types.QueryString ->
  CreateDynamicThingGroup
mkCreateDynamicThingGroup thingGroupName queryString =
  CreateDynamicThingGroup'
    { thingGroupName,
      queryString,
      indexName = Core.Nothing,
      queryVersion = Core.Nothing,
      tags = Core.Nothing,
      thingGroupProperties = Core.Nothing
    }

-- | The dynamic thing group name to create.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgThingGroupName :: Lens.Lens' CreateDynamicThingGroup Types.ThingGroupName
cdtgThingGroupName = Lens.field @"thingGroupName"
{-# DEPRECATED cdtgThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

-- | The dynamic thing group search query string.
--
-- See <https://docs.aws.amazon.com/iot/latest/developerguide/query-syntax.html Query Syntax> for information about query string syntax.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgQueryString :: Lens.Lens' CreateDynamicThingGroup Types.QueryString
cdtgQueryString = Lens.field @"queryString"
{-# DEPRECATED cdtgQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

-- | The dynamic thing group index name.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgIndexName :: Lens.Lens' CreateDynamicThingGroup (Core.Maybe Types.IndexName)
cdtgIndexName = Lens.field @"indexName"
{-# DEPRECATED cdtgIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The dynamic thing group query version.
--
-- /Note:/ Consider using 'queryVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgQueryVersion :: Lens.Lens' CreateDynamicThingGroup (Core.Maybe Types.QueryVersion)
cdtgQueryVersion = Lens.field @"queryVersion"
{-# DEPRECATED cdtgQueryVersion "Use generic-lens or generic-optics with 'queryVersion' instead." #-}

-- | Metadata which can be used to manage the dynamic thing group.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgTags :: Lens.Lens' CreateDynamicThingGroup (Core.Maybe [Types.Tag])
cdtgTags = Lens.field @"tags"
{-# DEPRECATED cdtgTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The dynamic thing group properties.
--
-- /Note:/ Consider using 'thingGroupProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgThingGroupProperties :: Lens.Lens' CreateDynamicThingGroup (Core.Maybe Types.ThingGroupProperties)
cdtgThingGroupProperties = Lens.field @"thingGroupProperties"
{-# DEPRECATED cdtgThingGroupProperties "Use generic-lens or generic-optics with 'thingGroupProperties' instead." #-}

instance Core.FromJSON CreateDynamicThingGroup where
  toJSON CreateDynamicThingGroup {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("queryString" Core..= queryString),
            ("indexName" Core..=) Core.<$> indexName,
            ("queryVersion" Core..=) Core.<$> queryVersion,
            ("tags" Core..=) Core.<$> tags,
            ("thingGroupProperties" Core..=) Core.<$> thingGroupProperties
          ]
      )

instance Core.AWSRequest CreateDynamicThingGroup where
  type Rs CreateDynamicThingGroup = CreateDynamicThingGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ("/dynamic-thing-groups/" Core.<> (Core.toText thingGroupName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDynamicThingGroupResponse'
            Core.<$> (x Core..:? "indexName")
            Core.<*> (x Core..:? "queryString")
            Core.<*> (x Core..:? "queryVersion")
            Core.<*> (x Core..:? "thingGroupArn")
            Core.<*> (x Core..:? "thingGroupId")
            Core.<*> (x Core..:? "thingGroupName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateDynamicThingGroupResponse' smart constructor.
data CreateDynamicThingGroupResponse = CreateDynamicThingGroupResponse'
  { -- | The dynamic thing group index name.
    indexName :: Core.Maybe Types.IndexName,
    -- | The dynamic thing group search query string.
    queryString :: Core.Maybe Types.QueryString,
    -- | The dynamic thing group query version.
    queryVersion :: Core.Maybe Types.QueryVersion,
    -- | The dynamic thing group ARN.
    thingGroupArn :: Core.Maybe Types.ThingGroupArn,
    -- | The dynamic thing group ID.
    thingGroupId :: Core.Maybe Types.ThingGroupId,
    -- | The dynamic thing group name.
    thingGroupName :: Core.Maybe Types.ThingGroupName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDynamicThingGroupResponse' value with any optional fields omitted.
mkCreateDynamicThingGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateDynamicThingGroupResponse
mkCreateDynamicThingGroupResponse responseStatus =
  CreateDynamicThingGroupResponse'
    { indexName = Core.Nothing,
      queryString = Core.Nothing,
      queryVersion = Core.Nothing,
      thingGroupArn = Core.Nothing,
      thingGroupId = Core.Nothing,
      thingGroupName = Core.Nothing,
      responseStatus
    }

-- | The dynamic thing group index name.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgrrsIndexName :: Lens.Lens' CreateDynamicThingGroupResponse (Core.Maybe Types.IndexName)
cdtgrrsIndexName = Lens.field @"indexName"
{-# DEPRECATED cdtgrrsIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

-- | The dynamic thing group search query string.
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgrrsQueryString :: Lens.Lens' CreateDynamicThingGroupResponse (Core.Maybe Types.QueryString)
cdtgrrsQueryString = Lens.field @"queryString"
{-# DEPRECATED cdtgrrsQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

-- | The dynamic thing group query version.
--
-- /Note:/ Consider using 'queryVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgrrsQueryVersion :: Lens.Lens' CreateDynamicThingGroupResponse (Core.Maybe Types.QueryVersion)
cdtgrrsQueryVersion = Lens.field @"queryVersion"
{-# DEPRECATED cdtgrrsQueryVersion "Use generic-lens or generic-optics with 'queryVersion' instead." #-}

-- | The dynamic thing group ARN.
--
-- /Note:/ Consider using 'thingGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgrrsThingGroupArn :: Lens.Lens' CreateDynamicThingGroupResponse (Core.Maybe Types.ThingGroupArn)
cdtgrrsThingGroupArn = Lens.field @"thingGroupArn"
{-# DEPRECATED cdtgrrsThingGroupArn "Use generic-lens or generic-optics with 'thingGroupArn' instead." #-}

-- | The dynamic thing group ID.
--
-- /Note:/ Consider using 'thingGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgrrsThingGroupId :: Lens.Lens' CreateDynamicThingGroupResponse (Core.Maybe Types.ThingGroupId)
cdtgrrsThingGroupId = Lens.field @"thingGroupId"
{-# DEPRECATED cdtgrrsThingGroupId "Use generic-lens or generic-optics with 'thingGroupId' instead." #-}

-- | The dynamic thing group name.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgrrsThingGroupName :: Lens.Lens' CreateDynamicThingGroupResponse (Core.Maybe Types.ThingGroupName)
cdtgrrsThingGroupName = Lens.field @"thingGroupName"
{-# DEPRECATED cdtgrrsThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdtgrrsResponseStatus :: Lens.Lens' CreateDynamicThingGroupResponse Core.Int
cdtgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdtgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
