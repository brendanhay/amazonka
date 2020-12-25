{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateResourceDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a resource definition which contains a list of resources to be used in a group. You can create an initial version of the definition by providing a list of resources now, or use ''CreateResourceDefinitionVersion'' later.
module Network.AWS.Greengrass.CreateResourceDefinition
  ( -- * Creating a request
    CreateResourceDefinition (..),
    mkCreateResourceDefinition,

    -- ** Request lenses
    crdAmznClientToken,
    crdInitialVersion,
    crdName,
    crdTags,

    -- * Destructuring the response
    CreateResourceDefinitionResponse (..),
    mkCreateResourceDefinitionResponse,

    -- ** Response lenses
    crdrrsArn,
    crdrrsCreationTimestamp,
    crdrrsId,
    crdrrsLastUpdatedTimestamp,
    crdrrsLatestVersion,
    crdrrsLatestVersionArn,
    crdrrsName,
    crdrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateResourceDefinition' smart constructor.
data CreateResourceDefinition = CreateResourceDefinition'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text,
    -- | Information about the initial version of the resource definition.
    initialVersion :: Core.Maybe Types.ResourceDefinitionVersion,
    -- | The name of the resource definition.
    name :: Core.Maybe Core.Text,
    -- | Tag(s) to add to the new resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateResourceDefinition' value with any optional fields omitted.
mkCreateResourceDefinition ::
  CreateResourceDefinition
mkCreateResourceDefinition =
  CreateResourceDefinition'
    { amznClientToken = Core.Nothing,
      initialVersion = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing
    }

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdAmznClientToken :: Lens.Lens' CreateResourceDefinition (Core.Maybe Core.Text)
crdAmznClientToken = Lens.field @"amznClientToken"
{-# DEPRECATED crdAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | Information about the initial version of the resource definition.
--
-- /Note:/ Consider using 'initialVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdInitialVersion :: Lens.Lens' CreateResourceDefinition (Core.Maybe Types.ResourceDefinitionVersion)
crdInitialVersion = Lens.field @"initialVersion"
{-# DEPRECATED crdInitialVersion "Use generic-lens or generic-optics with 'initialVersion' instead." #-}

-- | The name of the resource definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdName :: Lens.Lens' CreateResourceDefinition (Core.Maybe Core.Text)
crdName = Lens.field @"name"
{-# DEPRECATED crdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tag(s) to add to the new resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdTags :: Lens.Lens' CreateResourceDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
crdTags = Lens.field @"tags"
{-# DEPRECATED crdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateResourceDefinition where
  toJSON CreateResourceDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ ("InitialVersion" Core..=) Core.<$> initialVersion,
            ("Name" Core..=) Core.<$> name,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateResourceDefinition where
  type Rs CreateResourceDefinition = CreateResourceDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/greengrass/definition/resources",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "X-Amzn-Client-Token" amznClientToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResourceDefinitionResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "LastUpdatedTimestamp")
            Core.<*> (x Core..:? "LatestVersion")
            Core.<*> (x Core..:? "LatestVersionArn")
            Core.<*> (x Core..:? "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateResourceDefinitionResponse' smart constructor.
data CreateResourceDefinitionResponse = CreateResourceDefinitionResponse'
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

-- | Creates a 'CreateResourceDefinitionResponse' value with any optional fields omitted.
mkCreateResourceDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateResourceDefinitionResponse
mkCreateResourceDefinitionResponse responseStatus =
  CreateResourceDefinitionResponse'
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
crdrrsArn :: Lens.Lens' CreateResourceDefinitionResponse (Core.Maybe Core.Text)
crdrrsArn = Lens.field @"arn"
{-# DEPRECATED crdrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdrrsCreationTimestamp :: Lens.Lens' CreateResourceDefinitionResponse (Core.Maybe Core.Text)
crdrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED crdrrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdrrsId :: Lens.Lens' CreateResourceDefinitionResponse (Core.Maybe Core.Text)
crdrrsId = Lens.field @"id"
{-# DEPRECATED crdrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdrrsLastUpdatedTimestamp :: Lens.Lens' CreateResourceDefinitionResponse (Core.Maybe Core.Text)
crdrrsLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# DEPRECATED crdrrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdrrsLatestVersion :: Lens.Lens' CreateResourceDefinitionResponse (Core.Maybe Core.Text)
crdrrsLatestVersion = Lens.field @"latestVersion"
{-# DEPRECATED crdrrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdrrsLatestVersionArn :: Lens.Lens' CreateResourceDefinitionResponse (Core.Maybe Core.Text)
crdrrsLatestVersionArn = Lens.field @"latestVersionArn"
{-# DEPRECATED crdrrsLatestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdrrsName :: Lens.Lens' CreateResourceDefinitionResponse (Core.Maybe Core.Text)
crdrrsName = Lens.field @"name"
{-# DEPRECATED crdrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crdrrsResponseStatus :: Lens.Lens' CreateResourceDefinitionResponse Core.Int
crdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED crdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
