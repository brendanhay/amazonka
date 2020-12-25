{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateLoggerDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a logger definition. You may provide the initial version of the logger definition now or use ''CreateLoggerDefinitionVersion'' at a later time.
module Network.AWS.Greengrass.CreateLoggerDefinition
  ( -- * Creating a request
    CreateLoggerDefinition (..),
    mkCreateLoggerDefinition,

    -- ** Request lenses
    cldAmznClientToken,
    cldInitialVersion,
    cldName,
    cldTags,

    -- * Destructuring the response
    CreateLoggerDefinitionResponse (..),
    mkCreateLoggerDefinitionResponse,

    -- ** Response lenses
    cldrrsArn,
    cldrrsCreationTimestamp,
    cldrrsId,
    cldrrsLastUpdatedTimestamp,
    cldrrsLatestVersion,
    cldrrsLatestVersionArn,
    cldrrsName,
    cldrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateLoggerDefinition' smart constructor.
data CreateLoggerDefinition = CreateLoggerDefinition'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text,
    -- | Information about the initial version of the logger definition.
    initialVersion :: Core.Maybe Types.LoggerDefinitionVersion,
    -- | The name of the logger definition.
    name :: Core.Maybe Core.Text,
    -- | Tag(s) to add to the new resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLoggerDefinition' value with any optional fields omitted.
mkCreateLoggerDefinition ::
  CreateLoggerDefinition
mkCreateLoggerDefinition =
  CreateLoggerDefinition'
    { amznClientToken = Core.Nothing,
      initialVersion = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing
    }

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldAmznClientToken :: Lens.Lens' CreateLoggerDefinition (Core.Maybe Core.Text)
cldAmznClientToken = Lens.field @"amznClientToken"
{-# DEPRECATED cldAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | Information about the initial version of the logger definition.
--
-- /Note:/ Consider using 'initialVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldInitialVersion :: Lens.Lens' CreateLoggerDefinition (Core.Maybe Types.LoggerDefinitionVersion)
cldInitialVersion = Lens.field @"initialVersion"
{-# DEPRECATED cldInitialVersion "Use generic-lens or generic-optics with 'initialVersion' instead." #-}

-- | The name of the logger definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldName :: Lens.Lens' CreateLoggerDefinition (Core.Maybe Core.Text)
cldName = Lens.field @"name"
{-# DEPRECATED cldName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tag(s) to add to the new resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldTags :: Lens.Lens' CreateLoggerDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
cldTags = Lens.field @"tags"
{-# DEPRECATED cldTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateLoggerDefinition where
  toJSON CreateLoggerDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ ("InitialVersion" Core..=) Core.<$> initialVersion,
            ("Name" Core..=) Core.<$> name,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateLoggerDefinition where
  type Rs CreateLoggerDefinition = CreateLoggerDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/greengrass/definition/loggers",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "X-Amzn-Client-Token" amznClientToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLoggerDefinitionResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "LastUpdatedTimestamp")
            Core.<*> (x Core..:? "LatestVersion")
            Core.<*> (x Core..:? "LatestVersionArn")
            Core.<*> (x Core..:? "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateLoggerDefinitionResponse' smart constructor.
data CreateLoggerDefinitionResponse = CreateLoggerDefinitionResponse'
  { -- | The ARN of the definition.
    arn :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the definition was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | The ID of the definition.
    id :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the definition was last updated.
    lastUpdatedTimestamp :: Core.Maybe Core.Text,
    -- | The ID of the latest version associated with the definition.
    latestVersion :: Core.Maybe Core.Text,
    -- | The ARN of the latest version associated with the definition.
    latestVersionArn :: Core.Maybe Core.Text,
    -- | The name of the definition.
    name :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLoggerDefinitionResponse' value with any optional fields omitted.
mkCreateLoggerDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateLoggerDefinitionResponse
mkCreateLoggerDefinitionResponse responseStatus =
  CreateLoggerDefinitionResponse'
    { arn = Core.Nothing,
      creationTimestamp = Core.Nothing,
      id = Core.Nothing,
      lastUpdatedTimestamp = Core.Nothing,
      latestVersion = Core.Nothing,
      latestVersionArn = Core.Nothing,
      name = Core.Nothing,
      responseStatus
    }

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldrrsArn :: Lens.Lens' CreateLoggerDefinitionResponse (Core.Maybe Core.Text)
cldrrsArn = Lens.field @"arn"
{-# DEPRECATED cldrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldrrsCreationTimestamp :: Lens.Lens' CreateLoggerDefinitionResponse (Core.Maybe Core.Text)
cldrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED cldrrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldrrsId :: Lens.Lens' CreateLoggerDefinitionResponse (Core.Maybe Core.Text)
cldrrsId = Lens.field @"id"
{-# DEPRECATED cldrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldrrsLastUpdatedTimestamp :: Lens.Lens' CreateLoggerDefinitionResponse (Core.Maybe Core.Text)
cldrrsLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# DEPRECATED cldrrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldrrsLatestVersion :: Lens.Lens' CreateLoggerDefinitionResponse (Core.Maybe Core.Text)
cldrrsLatestVersion = Lens.field @"latestVersion"
{-# DEPRECATED cldrrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldrrsLatestVersionArn :: Lens.Lens' CreateLoggerDefinitionResponse (Core.Maybe Core.Text)
cldrrsLatestVersionArn = Lens.field @"latestVersionArn"
{-# DEPRECATED cldrrsLatestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldrrsName :: Lens.Lens' CreateLoggerDefinitionResponse (Core.Maybe Core.Text)
cldrrsName = Lens.field @"name"
{-# DEPRECATED cldrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cldrrsResponseStatus :: Lens.Lens' CreateLoggerDefinitionResponse Core.Int
cldrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cldrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
