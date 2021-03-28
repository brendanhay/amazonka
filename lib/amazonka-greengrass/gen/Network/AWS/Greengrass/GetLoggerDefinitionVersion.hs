{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetLoggerDefinitionVersion (..)
    , mkGetLoggerDefinitionVersion
    -- ** Request lenses
    , gldvLoggerDefinitionVersionId
    , gldvLoggerDefinitionId
    , gldvNextToken

    -- * Destructuring the response
    , GetLoggerDefinitionVersionResponse (..)
    , mkGetLoggerDefinitionVersionResponse
    -- ** Response lenses
    , gldvrrsArn
    , gldvrrsCreationTimestamp
    , gldvrrsDefinition
    , gldvrrsId
    , gldvrrsVersion
    , gldvrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetLoggerDefinitionVersion' smart constructor.
data GetLoggerDefinitionVersion = GetLoggerDefinitionVersion'
  { loggerDefinitionVersionId :: Core.Text
    -- ^ The ID of the logger definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListLoggerDefinitionVersions'' requests. If the version is the last one that was associated with a logger definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
  , loggerDefinitionId :: Core.Text
    -- ^ The ID of the logger definition.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLoggerDefinitionVersion' value with any optional fields omitted.
mkGetLoggerDefinitionVersion
    :: Core.Text -- ^ 'loggerDefinitionVersionId'
    -> Core.Text -- ^ 'loggerDefinitionId'
    -> GetLoggerDefinitionVersion
mkGetLoggerDefinitionVersion loggerDefinitionVersionId
  loggerDefinitionId
  = GetLoggerDefinitionVersion'{loggerDefinitionVersionId,
                                loggerDefinitionId, nextToken = Core.Nothing}

-- | The ID of the logger definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListLoggerDefinitionVersions'' requests. If the version is the last one that was associated with a logger definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
--
-- /Note:/ Consider using 'loggerDefinitionVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvLoggerDefinitionVersionId :: Lens.Lens' GetLoggerDefinitionVersion Core.Text
gldvLoggerDefinitionVersionId = Lens.field @"loggerDefinitionVersionId"
{-# INLINEABLE gldvLoggerDefinitionVersionId #-}
{-# DEPRECATED loggerDefinitionVersionId "Use generic-lens or generic-optics with 'loggerDefinitionVersionId' instead"  #-}

-- | The ID of the logger definition.
--
-- /Note:/ Consider using 'loggerDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvLoggerDefinitionId :: Lens.Lens' GetLoggerDefinitionVersion Core.Text
gldvLoggerDefinitionId = Lens.field @"loggerDefinitionId"
{-# INLINEABLE gldvLoggerDefinitionId #-}
{-# DEPRECATED loggerDefinitionId "Use generic-lens or generic-optics with 'loggerDefinitionId' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvNextToken :: Lens.Lens' GetLoggerDefinitionVersion (Core.Maybe Core.Text)
gldvNextToken = Lens.field @"nextToken"
{-# INLINEABLE gldvNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetLoggerDefinitionVersion where
        toQuery GetLoggerDefinitionVersion{..}
          = Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders GetLoggerDefinitionVersion where
        toHeaders GetLoggerDefinitionVersion{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetLoggerDefinitionVersion where
        type Rs GetLoggerDefinitionVersion =
             GetLoggerDefinitionVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/greengrass/definition/loggers/" Core.<>
                             Core.toText loggerDefinitionId
                             Core.<> "/versions/"
                             Core.<> Core.toText loggerDefinitionVersionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetLoggerDefinitionVersionResponse' Core.<$>
                   (x Core..:? "Arn") Core.<*> x Core..:? "CreationTimestamp" Core.<*>
                     x Core..:? "Definition"
                     Core.<*> x Core..:? "Id"
                     Core.<*> x Core..:? "Version"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetLoggerDefinitionVersionResponse' smart constructor.
data GetLoggerDefinitionVersionResponse = GetLoggerDefinitionVersionResponse'
  { arn :: Core.Maybe Core.Text
    -- ^ The ARN of the logger definition version.
  , creationTimestamp :: Core.Maybe Core.Text
    -- ^ The time, in milliseconds since the epoch, when the logger definition version was created.
  , definition :: Core.Maybe Types.LoggerDefinitionVersion
    -- ^ Information about the logger definition version.
  , id :: Core.Maybe Core.Text
    -- ^ The ID of the logger definition version.
  , version :: Core.Maybe Core.Text
    -- ^ The version of the logger definition version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLoggerDefinitionVersionResponse' value with any optional fields omitted.
mkGetLoggerDefinitionVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetLoggerDefinitionVersionResponse
mkGetLoggerDefinitionVersionResponse responseStatus
  = GetLoggerDefinitionVersionResponse'{arn = Core.Nothing,
                                        creationTimestamp = Core.Nothing, definition = Core.Nothing,
                                        id = Core.Nothing, version = Core.Nothing, responseStatus}

-- | The ARN of the logger definition version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvrrsArn :: Lens.Lens' GetLoggerDefinitionVersionResponse (Core.Maybe Core.Text)
gldvrrsArn = Lens.field @"arn"
{-# INLINEABLE gldvrrsArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The time, in milliseconds since the epoch, when the logger definition version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvrrsCreationTimestamp :: Lens.Lens' GetLoggerDefinitionVersionResponse (Core.Maybe Core.Text)
gldvrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# INLINEABLE gldvrrsCreationTimestamp #-}
{-# DEPRECATED creationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead"  #-}

-- | Information about the logger definition version.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvrrsDefinition :: Lens.Lens' GetLoggerDefinitionVersionResponse (Core.Maybe Types.LoggerDefinitionVersion)
gldvrrsDefinition = Lens.field @"definition"
{-# INLINEABLE gldvrrsDefinition #-}
{-# DEPRECATED definition "Use generic-lens or generic-optics with 'definition' instead"  #-}

-- | The ID of the logger definition version.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvrrsId :: Lens.Lens' GetLoggerDefinitionVersionResponse (Core.Maybe Core.Text)
gldvrrsId = Lens.field @"id"
{-# INLINEABLE gldvrrsId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The version of the logger definition version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvrrsVersion :: Lens.Lens' GetLoggerDefinitionVersionResponse (Core.Maybe Core.Text)
gldvrrsVersion = Lens.field @"version"
{-# INLINEABLE gldvrrsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gldvrrsResponseStatus :: Lens.Lens' GetLoggerDefinitionVersionResponse Core.Int
gldvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gldvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
