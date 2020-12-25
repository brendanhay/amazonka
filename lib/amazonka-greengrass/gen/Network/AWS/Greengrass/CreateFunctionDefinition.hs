{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateFunctionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Lambda function definition which contains a list of Lambda functions and their configurations to be used in a group. You can create an initial version of the definition by providing a list of Lambda functions and their configurations now, or use ''CreateFunctionDefinitionVersion'' later.
module Network.AWS.Greengrass.CreateFunctionDefinition
  ( -- * Creating a request
    CreateFunctionDefinition (..),
    mkCreateFunctionDefinition,

    -- ** Request lenses
    cfdAmznClientToken,
    cfdInitialVersion,
    cfdName,
    cfdTags,

    -- * Destructuring the response
    CreateFunctionDefinitionResponse (..),
    mkCreateFunctionDefinitionResponse,

    -- ** Response lenses
    cfdrrsArn,
    cfdrrsCreationTimestamp,
    cfdrrsId,
    cfdrrsLastUpdatedTimestamp,
    cfdrrsLatestVersion,
    cfdrrsLatestVersionArn,
    cfdrrsName,
    cfdrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateFunctionDefinition' smart constructor.
data CreateFunctionDefinition = CreateFunctionDefinition'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text,
    -- | Information about the initial version of the function definition.
    initialVersion :: Core.Maybe Types.FunctionDefinitionVersion,
    -- | The name of the function definition.
    name :: Core.Maybe Core.Text,
    -- | Tag(s) to add to the new resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFunctionDefinition' value with any optional fields omitted.
mkCreateFunctionDefinition ::
  CreateFunctionDefinition
mkCreateFunctionDefinition =
  CreateFunctionDefinition'
    { amznClientToken = Core.Nothing,
      initialVersion = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing
    }

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdAmznClientToken :: Lens.Lens' CreateFunctionDefinition (Core.Maybe Core.Text)
cfdAmznClientToken = Lens.field @"amznClientToken"
{-# DEPRECATED cfdAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | Information about the initial version of the function definition.
--
-- /Note:/ Consider using 'initialVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdInitialVersion :: Lens.Lens' CreateFunctionDefinition (Core.Maybe Types.FunctionDefinitionVersion)
cfdInitialVersion = Lens.field @"initialVersion"
{-# DEPRECATED cfdInitialVersion "Use generic-lens or generic-optics with 'initialVersion' instead." #-}

-- | The name of the function definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdName :: Lens.Lens' CreateFunctionDefinition (Core.Maybe Core.Text)
cfdName = Lens.field @"name"
{-# DEPRECATED cfdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tag(s) to add to the new resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdTags :: Lens.Lens' CreateFunctionDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
cfdTags = Lens.field @"tags"
{-# DEPRECATED cfdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateFunctionDefinition where
  toJSON CreateFunctionDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ ("InitialVersion" Core..=) Core.<$> initialVersion,
            ("Name" Core..=) Core.<$> name,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateFunctionDefinition where
  type Rs CreateFunctionDefinition = CreateFunctionDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/greengrass/definition/functions",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "X-Amzn-Client-Token" amznClientToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFunctionDefinitionResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "LastUpdatedTimestamp")
            Core.<*> (x Core..:? "LatestVersion")
            Core.<*> (x Core..:? "LatestVersionArn")
            Core.<*> (x Core..:? "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateFunctionDefinitionResponse' smart constructor.
data CreateFunctionDefinitionResponse = CreateFunctionDefinitionResponse'
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

-- | Creates a 'CreateFunctionDefinitionResponse' value with any optional fields omitted.
mkCreateFunctionDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateFunctionDefinitionResponse
mkCreateFunctionDefinitionResponse responseStatus =
  CreateFunctionDefinitionResponse'
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
cfdrrsArn :: Lens.Lens' CreateFunctionDefinitionResponse (Core.Maybe Core.Text)
cfdrrsArn = Lens.field @"arn"
{-# DEPRECATED cfdrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrrsCreationTimestamp :: Lens.Lens' CreateFunctionDefinitionResponse (Core.Maybe Core.Text)
cfdrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED cfdrrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrrsId :: Lens.Lens' CreateFunctionDefinitionResponse (Core.Maybe Core.Text)
cfdrrsId = Lens.field @"id"
{-# DEPRECATED cfdrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrrsLastUpdatedTimestamp :: Lens.Lens' CreateFunctionDefinitionResponse (Core.Maybe Core.Text)
cfdrrsLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# DEPRECATED cfdrrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrrsLatestVersion :: Lens.Lens' CreateFunctionDefinitionResponse (Core.Maybe Core.Text)
cfdrrsLatestVersion = Lens.field @"latestVersion"
{-# DEPRECATED cfdrrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrrsLatestVersionArn :: Lens.Lens' CreateFunctionDefinitionResponse (Core.Maybe Core.Text)
cfdrrsLatestVersionArn = Lens.field @"latestVersionArn"
{-# DEPRECATED cfdrrsLatestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrrsName :: Lens.Lens' CreateFunctionDefinitionResponse (Core.Maybe Core.Text)
cfdrrsName = Lens.field @"name"
{-# DEPRECATED cfdrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfdrrsResponseStatus :: Lens.Lens' CreateFunctionDefinitionResponse Core.Int
cfdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cfdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
