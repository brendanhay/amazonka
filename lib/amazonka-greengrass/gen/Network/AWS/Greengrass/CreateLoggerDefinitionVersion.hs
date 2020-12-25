{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateLoggerDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a logger definition that has already been defined.
module Network.AWS.Greengrass.CreateLoggerDefinitionVersion
  ( -- * Creating a request
    CreateLoggerDefinitionVersion (..),
    mkCreateLoggerDefinitionVersion,

    -- ** Request lenses
    cldvLoggerDefinitionId,
    cldvAmznClientToken,
    cldvLoggers,

    -- * Destructuring the response
    CreateLoggerDefinitionVersionResponse (..),
    mkCreateLoggerDefinitionVersionResponse,

    -- ** Response lenses
    cldvrrsArn,
    cldvrrsCreationTimestamp,
    cldvrrsId,
    cldvrrsVersion,
    cldvrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateLoggerDefinitionVersion' smart constructor.
data CreateLoggerDefinitionVersion = CreateLoggerDefinitionVersion'
  { -- | The ID of the logger definition.
    loggerDefinitionId :: Core.Text,
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text,
    -- | A list of loggers.
    loggers :: Core.Maybe [Types.GreengrassLogger]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLoggerDefinitionVersion' value with any optional fields omitted.
mkCreateLoggerDefinitionVersion ::
  -- | 'loggerDefinitionId'
  Core.Text ->
  CreateLoggerDefinitionVersion
mkCreateLoggerDefinitionVersion loggerDefinitionId =
  CreateLoggerDefinitionVersion'
    { loggerDefinitionId,
      amznClientToken = Core.Nothing,
      loggers = Core.Nothing
    }

-- | The ID of the logger definition.
--
-- /Note:/ Consider using 'loggerDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldvLoggerDefinitionId :: Lens.Lens' CreateLoggerDefinitionVersion Core.Text
cldvLoggerDefinitionId = Lens.field @"loggerDefinitionId"
{-# DEPRECATED cldvLoggerDefinitionId "Use generic-lens or generic-optics with 'loggerDefinitionId' instead." #-}

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldvAmznClientToken :: Lens.Lens' CreateLoggerDefinitionVersion (Core.Maybe Core.Text)
cldvAmznClientToken = Lens.field @"amznClientToken"
{-# DEPRECATED cldvAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | A list of loggers.
--
-- /Note:/ Consider using 'loggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldvLoggers :: Lens.Lens' CreateLoggerDefinitionVersion (Core.Maybe [Types.GreengrassLogger])
cldvLoggers = Lens.field @"loggers"
{-# DEPRECATED cldvLoggers "Use generic-lens or generic-optics with 'loggers' instead." #-}

instance Core.FromJSON CreateLoggerDefinitionVersion where
  toJSON CreateLoggerDefinitionVersion {..} =
    Core.object
      (Core.catMaybes [("Loggers" Core..=) Core.<$> loggers])

instance Core.AWSRequest CreateLoggerDefinitionVersion where
  type
    Rs CreateLoggerDefinitionVersion =
      CreateLoggerDefinitionVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/loggers/"
                Core.<> (Core.toText loggerDefinitionId)
                Core.<> ("/versions")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "X-Amzn-Client-Token" amznClientToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLoggerDefinitionVersionResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateLoggerDefinitionVersionResponse' smart constructor.
data CreateLoggerDefinitionVersionResponse = CreateLoggerDefinitionVersionResponse'
  { -- | The ARN of the version.
    arn :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the version was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | The ID of the parent definition that the version is associated with.
    id :: Core.Maybe Core.Text,
    -- | The ID of the version.
    version :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLoggerDefinitionVersionResponse' value with any optional fields omitted.
mkCreateLoggerDefinitionVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateLoggerDefinitionVersionResponse
mkCreateLoggerDefinitionVersionResponse responseStatus =
  CreateLoggerDefinitionVersionResponse'
    { arn = Core.Nothing,
      creationTimestamp = Core.Nothing,
      id = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | The ARN of the version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldvrrsArn :: Lens.Lens' CreateLoggerDefinitionVersionResponse (Core.Maybe Core.Text)
cldvrrsArn = Lens.field @"arn"
{-# DEPRECATED cldvrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldvrrsCreationTimestamp :: Lens.Lens' CreateLoggerDefinitionVersionResponse (Core.Maybe Core.Text)
cldvrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED cldvrrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the parent definition that the version is associated with.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldvrrsId :: Lens.Lens' CreateLoggerDefinitionVersionResponse (Core.Maybe Core.Text)
cldvrrsId = Lens.field @"id"
{-# DEPRECATED cldvrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldvrrsVersion :: Lens.Lens' CreateLoggerDefinitionVersionResponse (Core.Maybe Core.Text)
cldvrrsVersion = Lens.field @"version"
{-# DEPRECATED cldvrrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldvrrsResponseStatus :: Lens.Lens' CreateLoggerDefinitionVersionResponse Core.Int
cldvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cldvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
