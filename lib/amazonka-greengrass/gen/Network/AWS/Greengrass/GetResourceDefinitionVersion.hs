{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetResourceDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a resource definition version, including which resources are included in the version.
module Network.AWS.Greengrass.GetResourceDefinitionVersion
  ( -- * Creating a request
    GetResourceDefinitionVersion (..),
    mkGetResourceDefinitionVersion,

    -- ** Request lenses
    grdvResourceDefinitionVersionId,
    grdvResourceDefinitionId,

    -- * Destructuring the response
    GetResourceDefinitionVersionResponse (..),
    mkGetResourceDefinitionVersionResponse,

    -- ** Response lenses
    grdvrrsArn,
    grdvrrsCreationTimestamp,
    grdvrrsDefinition,
    grdvrrsId,
    grdvrrsVersion,
    grdvrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetResourceDefinitionVersion' smart constructor.
data GetResourceDefinitionVersion = GetResourceDefinitionVersion'
  { -- | The ID of the resource definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListResourceDefinitionVersions'' requests. If the version is the last one that was associated with a resource definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
    resourceDefinitionVersionId :: Core.Text,
    -- | The ID of the resource definition.
    resourceDefinitionId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetResourceDefinitionVersion' value with any optional fields omitted.
mkGetResourceDefinitionVersion ::
  -- | 'resourceDefinitionVersionId'
  Core.Text ->
  -- | 'resourceDefinitionId'
  Core.Text ->
  GetResourceDefinitionVersion
mkGetResourceDefinitionVersion
  resourceDefinitionVersionId
  resourceDefinitionId =
    GetResourceDefinitionVersion'
      { resourceDefinitionVersionId,
        resourceDefinitionId
      }

-- | The ID of the resource definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListResourceDefinitionVersions'' requests. If the version is the last one that was associated with a resource definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
--
-- /Note:/ Consider using 'resourceDefinitionVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdvResourceDefinitionVersionId :: Lens.Lens' GetResourceDefinitionVersion Core.Text
grdvResourceDefinitionVersionId = Lens.field @"resourceDefinitionVersionId"
{-# DEPRECATED grdvResourceDefinitionVersionId "Use generic-lens or generic-optics with 'resourceDefinitionVersionId' instead." #-}

-- | The ID of the resource definition.
--
-- /Note:/ Consider using 'resourceDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdvResourceDefinitionId :: Lens.Lens' GetResourceDefinitionVersion Core.Text
grdvResourceDefinitionId = Lens.field @"resourceDefinitionId"
{-# DEPRECATED grdvResourceDefinitionId "Use generic-lens or generic-optics with 'resourceDefinitionId' instead." #-}

instance Core.AWSRequest GetResourceDefinitionVersion where
  type
    Rs GetResourceDefinitionVersion =
      GetResourceDefinitionVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/resources/"
                Core.<> (Core.toText resourceDefinitionId)
                Core.<> ("/versions/")
                Core.<> (Core.toText resourceDefinitionVersionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceDefinitionVersionResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "Definition")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetResourceDefinitionVersionResponse' smart constructor.
data GetResourceDefinitionVersionResponse = GetResourceDefinitionVersionResponse'
  { -- | Arn of the resource definition version.
    arn :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the resource definition version was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | Information about the definition.
    definition :: Core.Maybe Types.ResourceDefinitionVersion,
    -- | The ID of the resource definition version.
    id :: Core.Maybe Core.Text,
    -- | The version of the resource definition version.
    version :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetResourceDefinitionVersionResponse' value with any optional fields omitted.
mkGetResourceDefinitionVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetResourceDefinitionVersionResponse
mkGetResourceDefinitionVersionResponse responseStatus =
  GetResourceDefinitionVersionResponse'
    { arn = Core.Nothing,
      creationTimestamp = Core.Nothing,
      definition = Core.Nothing,
      id = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | Arn of the resource definition version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdvrrsArn :: Lens.Lens' GetResourceDefinitionVersionResponse (Core.Maybe Core.Text)
grdvrrsArn = Lens.field @"arn"
{-# DEPRECATED grdvrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the resource definition version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdvrrsCreationTimestamp :: Lens.Lens' GetResourceDefinitionVersionResponse (Core.Maybe Core.Text)
grdvrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED grdvrrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | Information about the definition.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdvrrsDefinition :: Lens.Lens' GetResourceDefinitionVersionResponse (Core.Maybe Types.ResourceDefinitionVersion)
grdvrrsDefinition = Lens.field @"definition"
{-# DEPRECATED grdvrrsDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The ID of the resource definition version.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdvrrsId :: Lens.Lens' GetResourceDefinitionVersionResponse (Core.Maybe Core.Text)
grdvrrsId = Lens.field @"id"
{-# DEPRECATED grdvrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The version of the resource definition version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdvrrsVersion :: Lens.Lens' GetResourceDefinitionVersionResponse (Core.Maybe Core.Text)
grdvrrsVersion = Lens.field @"version"
{-# DEPRECATED grdvrrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grdvrrsResponseStatus :: Lens.Lens' GetResourceDefinitionVersionResponse Core.Int
grdvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grdvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
