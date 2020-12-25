{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetCoreDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a core definition version.
module Network.AWS.Greengrass.GetCoreDefinitionVersion
  ( -- * Creating a request
    GetCoreDefinitionVersion (..),
    mkGetCoreDefinitionVersion,

    -- ** Request lenses
    gcdvCoreDefinitionId,
    gcdvCoreDefinitionVersionId,

    -- * Destructuring the response
    GetCoreDefinitionVersionResponse (..),
    mkGetCoreDefinitionVersionResponse,

    -- ** Response lenses
    gcdvrfrsArn,
    gcdvrfrsCreationTimestamp,
    gcdvrfrsDefinition,
    gcdvrfrsId,
    gcdvrfrsNextToken,
    gcdvrfrsVersion,
    gcdvrfrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCoreDefinitionVersion' smart constructor.
data GetCoreDefinitionVersion = GetCoreDefinitionVersion'
  { -- | The ID of the core definition.
    coreDefinitionId :: Core.Text,
    -- | The ID of the core definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListCoreDefinitionVersions'' requests. If the version is the last one that was associated with a core definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
    coreDefinitionVersionId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCoreDefinitionVersion' value with any optional fields omitted.
mkGetCoreDefinitionVersion ::
  -- | 'coreDefinitionId'
  Core.Text ->
  -- | 'coreDefinitionVersionId'
  Core.Text ->
  GetCoreDefinitionVersion
mkGetCoreDefinitionVersion coreDefinitionId coreDefinitionVersionId =
  GetCoreDefinitionVersion'
    { coreDefinitionId,
      coreDefinitionVersionId
    }

-- | The ID of the core definition.
--
-- /Note:/ Consider using 'coreDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvCoreDefinitionId :: Lens.Lens' GetCoreDefinitionVersion Core.Text
gcdvCoreDefinitionId = Lens.field @"coreDefinitionId"
{-# DEPRECATED gcdvCoreDefinitionId "Use generic-lens or generic-optics with 'coreDefinitionId' instead." #-}

-- | The ID of the core definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListCoreDefinitionVersions'' requests. If the version is the last one that was associated with a core definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
--
-- /Note:/ Consider using 'coreDefinitionVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvCoreDefinitionVersionId :: Lens.Lens' GetCoreDefinitionVersion Core.Text
gcdvCoreDefinitionVersionId = Lens.field @"coreDefinitionVersionId"
{-# DEPRECATED gcdvCoreDefinitionVersionId "Use generic-lens or generic-optics with 'coreDefinitionVersionId' instead." #-}

instance Core.AWSRequest GetCoreDefinitionVersion where
  type Rs GetCoreDefinitionVersion = GetCoreDefinitionVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/cores/"
                Core.<> (Core.toText coreDefinitionId)
                Core.<> ("/versions/")
                Core.<> (Core.toText coreDefinitionVersionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCoreDefinitionVersionResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "Definition")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCoreDefinitionVersionResponse' smart constructor.
data GetCoreDefinitionVersionResponse = GetCoreDefinitionVersionResponse'
  { -- | The ARN of the core definition version.
    arn :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the core definition version was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | Information about the core definition version.
    definition :: Core.Maybe Types.CoreDefinitionVersion,
    -- | The ID of the core definition version.
    id :: Core.Maybe Core.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The version of the core definition version.
    version :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCoreDefinitionVersionResponse' value with any optional fields omitted.
mkGetCoreDefinitionVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCoreDefinitionVersionResponse
mkGetCoreDefinitionVersionResponse responseStatus =
  GetCoreDefinitionVersionResponse'
    { arn = Core.Nothing,
      creationTimestamp = Core.Nothing,
      definition = Core.Nothing,
      id = Core.Nothing,
      nextToken = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | The ARN of the core definition version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrfrsArn :: Lens.Lens' GetCoreDefinitionVersionResponse (Core.Maybe Core.Text)
gcdvrfrsArn = Lens.field @"arn"
{-# DEPRECATED gcdvrfrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the core definition version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrfrsCreationTimestamp :: Lens.Lens' GetCoreDefinitionVersionResponse (Core.Maybe Core.Text)
gcdvrfrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED gcdvrfrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | Information about the core definition version.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrfrsDefinition :: Lens.Lens' GetCoreDefinitionVersionResponse (Core.Maybe Types.CoreDefinitionVersion)
gcdvrfrsDefinition = Lens.field @"definition"
{-# DEPRECATED gcdvrfrsDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The ID of the core definition version.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrfrsId :: Lens.Lens' GetCoreDefinitionVersionResponse (Core.Maybe Core.Text)
gcdvrfrsId = Lens.field @"id"
{-# DEPRECATED gcdvrfrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrfrsNextToken :: Lens.Lens' GetCoreDefinitionVersionResponse (Core.Maybe Core.Text)
gcdvrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcdvrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The version of the core definition version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrfrsVersion :: Lens.Lens' GetCoreDefinitionVersionResponse (Core.Maybe Core.Text)
gcdvrfrsVersion = Lens.field @"version"
{-# DEPRECATED gcdvrfrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdvrfrsResponseStatus :: Lens.Lens' GetCoreDefinitionVersionResponse Core.Int
gcdvrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcdvrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
