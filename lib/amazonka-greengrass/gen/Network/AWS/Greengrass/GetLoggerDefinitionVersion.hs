{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetLoggerDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a logger definition version.
module Network.AWS.Greengrass.GetLoggerDefinitionVersion
  ( -- * Creating a request
    GetLoggerDefinitionVersion (..),
    mkGetLoggerDefinitionVersion,

    -- ** Request lenses
    gldvLoggerDefinitionVersionId,
    gldvLoggerDefinitionId,
    gldvNextToken,

    -- * Destructuring the response
    GetLoggerDefinitionVersionResponse (..),
    mkGetLoggerDefinitionVersionResponse,

    -- ** Response lenses
    gldvrrsArn,
    gldvrrsCreationTimestamp,
    gldvrrsDefinition,
    gldvrrsId,
    gldvrrsVersion,
    gldvrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLoggerDefinitionVersion' smart constructor.
data GetLoggerDefinitionVersion = GetLoggerDefinitionVersion'
  { -- | The ID of the logger definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListLoggerDefinitionVersions'' requests. If the version is the last one that was associated with a logger definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
    loggerDefinitionVersionId :: Core.Text,
    -- | The ID of the logger definition.
    loggerDefinitionId :: Core.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLoggerDefinitionVersion' value with any optional fields omitted.
mkGetLoggerDefinitionVersion ::
  -- | 'loggerDefinitionVersionId'
  Core.Text ->
  -- | 'loggerDefinitionId'
  Core.Text ->
  GetLoggerDefinitionVersion
mkGetLoggerDefinitionVersion
  loggerDefinitionVersionId
  loggerDefinitionId =
    GetLoggerDefinitionVersion'
      { loggerDefinitionVersionId,
        loggerDefinitionId,
        nextToken = Core.Nothing
      }

-- | The ID of the logger definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListLoggerDefinitionVersions'' requests. If the version is the last one that was associated with a logger definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
--
-- /Note:/ Consider using 'loggerDefinitionVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvLoggerDefinitionVersionId :: Lens.Lens' GetLoggerDefinitionVersion Core.Text
gldvLoggerDefinitionVersionId = Lens.field @"loggerDefinitionVersionId"
{-# DEPRECATED gldvLoggerDefinitionVersionId "Use generic-lens or generic-optics with 'loggerDefinitionVersionId' instead." #-}

-- | The ID of the logger definition.
--
-- /Note:/ Consider using 'loggerDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvLoggerDefinitionId :: Lens.Lens' GetLoggerDefinitionVersion Core.Text
gldvLoggerDefinitionId = Lens.field @"loggerDefinitionId"
{-# DEPRECATED gldvLoggerDefinitionId "Use generic-lens or generic-optics with 'loggerDefinitionId' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvNextToken :: Lens.Lens' GetLoggerDefinitionVersion (Core.Maybe Core.Text)
gldvNextToken = Lens.field @"nextToken"
{-# DEPRECATED gldvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest GetLoggerDefinitionVersion where
  type
    Rs GetLoggerDefinitionVersion =
      GetLoggerDefinitionVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/loggers/"
                Core.<> (Core.toText loggerDefinitionId)
                Core.<> ("/versions/")
                Core.<> (Core.toText loggerDefinitionVersionId)
            ),
        Core._rqQuery = Core.toQueryValue "NextToken" Core.<$> nextToken,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLoggerDefinitionVersionResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "Definition")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetLoggerDefinitionVersionResponse' smart constructor.
data GetLoggerDefinitionVersionResponse = GetLoggerDefinitionVersionResponse'
  { -- | The ARN of the logger definition version.
    arn :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the logger definition version was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | Information about the logger definition version.
    definition :: Core.Maybe Types.LoggerDefinitionVersion,
    -- | The ID of the logger definition version.
    id :: Core.Maybe Core.Text,
    -- | The version of the logger definition version.
    version :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLoggerDefinitionVersionResponse' value with any optional fields omitted.
mkGetLoggerDefinitionVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetLoggerDefinitionVersionResponse
mkGetLoggerDefinitionVersionResponse responseStatus =
  GetLoggerDefinitionVersionResponse'
    { arn = Core.Nothing,
      creationTimestamp = Core.Nothing,
      definition = Core.Nothing,
      id = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | The ARN of the logger definition version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvrrsArn :: Lens.Lens' GetLoggerDefinitionVersionResponse (Core.Maybe Core.Text)
gldvrrsArn = Lens.field @"arn"
{-# DEPRECATED gldvrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the logger definition version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvrrsCreationTimestamp :: Lens.Lens' GetLoggerDefinitionVersionResponse (Core.Maybe Core.Text)
gldvrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED gldvrrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | Information about the logger definition version.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvrrsDefinition :: Lens.Lens' GetLoggerDefinitionVersionResponse (Core.Maybe Types.LoggerDefinitionVersion)
gldvrrsDefinition = Lens.field @"definition"
{-# DEPRECATED gldvrrsDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The ID of the logger definition version.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvrrsId :: Lens.Lens' GetLoggerDefinitionVersionResponse (Core.Maybe Core.Text)
gldvrrsId = Lens.field @"id"
{-# DEPRECATED gldvrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The version of the logger definition version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvrrsVersion :: Lens.Lens' GetLoggerDefinitionVersionResponse (Core.Maybe Core.Text)
gldvrrsVersion = Lens.field @"version"
{-# DEPRECATED gldvrrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvrrsResponseStatus :: Lens.Lens' GetLoggerDefinitionVersionResponse Core.Int
gldvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gldvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
