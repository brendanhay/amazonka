{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetGroupVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a group version.
module Network.AWS.Greengrass.GetGroupVersion
  ( -- * Creating a request
    GetGroupVersion (..),
    mkGetGroupVersion,

    -- ** Request lenses
    ggvGroupVersionId,
    ggvGroupId,

    -- * Destructuring the response
    GetGroupVersionResponse (..),
    mkGetGroupVersionResponse,

    -- ** Response lenses
    ggvrrsArn,
    ggvrrsCreationTimestamp,
    ggvrrsDefinition,
    ggvrrsId,
    ggvrrsVersion,
    ggvrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetGroupVersion' smart constructor.
data GetGroupVersion = GetGroupVersion'
  { -- | The ID of the group version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListGroupVersions'' requests. If the version is the last one that was associated with a group, the value also maps to the ''LatestVersion'' property of the corresponding ''GroupInformation'' object.
    groupVersionId :: Core.Text,
    -- | The ID of the Greengrass group.
    groupId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGroupVersion' value with any optional fields omitted.
mkGetGroupVersion ::
  -- | 'groupVersionId'
  Core.Text ->
  -- | 'groupId'
  Core.Text ->
  GetGroupVersion
mkGetGroupVersion groupVersionId groupId =
  GetGroupVersion' {groupVersionId, groupId}

-- | The ID of the group version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListGroupVersions'' requests. If the version is the last one that was associated with a group, the value also maps to the ''LatestVersion'' property of the corresponding ''GroupInformation'' object.
--
-- /Note:/ Consider using 'groupVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggvGroupVersionId :: Lens.Lens' GetGroupVersion Core.Text
ggvGroupVersionId = Lens.field @"groupVersionId"
{-# DEPRECATED ggvGroupVersionId "Use generic-lens or generic-optics with 'groupVersionId' instead." #-}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggvGroupId :: Lens.Lens' GetGroupVersion Core.Text
ggvGroupId = Lens.field @"groupId"
{-# DEPRECATED ggvGroupId "Use generic-lens or generic-optics with 'groupId' instead." #-}

instance Core.AWSRequest GetGroupVersion where
  type Rs GetGroupVersion = GetGroupVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/groups/" Core.<> (Core.toText groupId)
                Core.<> ("/versions/")
                Core.<> (Core.toText groupVersionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGroupVersionResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "Definition")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetGroupVersionResponse' smart constructor.
data GetGroupVersionResponse = GetGroupVersionResponse'
  { -- | The ARN of the group version.
    arn :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the group version was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | Information about the group version definition.
    definition :: Core.Maybe Types.GroupVersion,
    -- | The ID of the group that the version is associated with.
    id :: Core.Maybe Core.Text,
    -- | The ID of the group version.
    version :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGroupVersionResponse' value with any optional fields omitted.
mkGetGroupVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetGroupVersionResponse
mkGetGroupVersionResponse responseStatus =
  GetGroupVersionResponse'
    { arn = Core.Nothing,
      creationTimestamp = Core.Nothing,
      definition = Core.Nothing,
      id = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | The ARN of the group version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggvrrsArn :: Lens.Lens' GetGroupVersionResponse (Core.Maybe Core.Text)
ggvrrsArn = Lens.field @"arn"
{-# DEPRECATED ggvrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the group version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggvrrsCreationTimestamp :: Lens.Lens' GetGroupVersionResponse (Core.Maybe Core.Text)
ggvrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED ggvrrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | Information about the group version definition.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggvrrsDefinition :: Lens.Lens' GetGroupVersionResponse (Core.Maybe Types.GroupVersion)
ggvrrsDefinition = Lens.field @"definition"
{-# DEPRECATED ggvrrsDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The ID of the group that the version is associated with.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggvrrsId :: Lens.Lens' GetGroupVersionResponse (Core.Maybe Core.Text)
ggvrrsId = Lens.field @"id"
{-# DEPRECATED ggvrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the group version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggvrrsVersion :: Lens.Lens' GetGroupVersionResponse (Core.Maybe Core.Text)
ggvrrsVersion = Lens.field @"version"
{-# DEPRECATED ggvrrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ggvrrsResponseStatus :: Lens.Lens' GetGroupVersionResponse Core.Int
ggvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ggvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
