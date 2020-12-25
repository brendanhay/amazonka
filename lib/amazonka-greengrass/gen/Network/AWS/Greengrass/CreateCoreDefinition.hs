{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateCoreDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a core definition. You may provide the initial version of the core definition now or use ''CreateCoreDefinitionVersion'' at a later time. Greengrass groups must each contain exactly one Greengrass core.
module Network.AWS.Greengrass.CreateCoreDefinition
  ( -- * Creating a request
    CreateCoreDefinition (..),
    mkCreateCoreDefinition,

    -- ** Request lenses
    ccdAmznClientToken,
    ccdInitialVersion,
    ccdName,
    ccdTags,

    -- * Destructuring the response
    CreateCoreDefinitionResponse (..),
    mkCreateCoreDefinitionResponse,

    -- ** Response lenses
    ccdrrsArn,
    ccdrrsCreationTimestamp,
    ccdrrsId,
    ccdrrsLastUpdatedTimestamp,
    ccdrrsLatestVersion,
    ccdrrsLatestVersionArn,
    ccdrrsName,
    ccdrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Information needed to create a core definition.
--
-- /See:/ 'mkCreateCoreDefinition' smart constructor.
data CreateCoreDefinition = CreateCoreDefinition'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text,
    -- | Information about the initial version of the core definition.
    initialVersion :: Core.Maybe Types.CoreDefinitionVersion,
    -- | The name of the core definition.
    name :: Core.Maybe Core.Text,
    -- | Tag(s) to add to the new resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCoreDefinition' value with any optional fields omitted.
mkCreateCoreDefinition ::
  CreateCoreDefinition
mkCreateCoreDefinition =
  CreateCoreDefinition'
    { amznClientToken = Core.Nothing,
      initialVersion = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing
    }

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdAmznClientToken :: Lens.Lens' CreateCoreDefinition (Core.Maybe Core.Text)
ccdAmznClientToken = Lens.field @"amznClientToken"
{-# DEPRECATED ccdAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | Information about the initial version of the core definition.
--
-- /Note:/ Consider using 'initialVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdInitialVersion :: Lens.Lens' CreateCoreDefinition (Core.Maybe Types.CoreDefinitionVersion)
ccdInitialVersion = Lens.field @"initialVersion"
{-# DEPRECATED ccdInitialVersion "Use generic-lens or generic-optics with 'initialVersion' instead." #-}

-- | The name of the core definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdName :: Lens.Lens' CreateCoreDefinition (Core.Maybe Core.Text)
ccdName = Lens.field @"name"
{-# DEPRECATED ccdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tag(s) to add to the new resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdTags :: Lens.Lens' CreateCoreDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
ccdTags = Lens.field @"tags"
{-# DEPRECATED ccdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateCoreDefinition where
  toJSON CreateCoreDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ ("InitialVersion" Core..=) Core.<$> initialVersion,
            ("Name" Core..=) Core.<$> name,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateCoreDefinition where
  type Rs CreateCoreDefinition = CreateCoreDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/greengrass/definition/cores",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "X-Amzn-Client-Token" amznClientToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCoreDefinitionResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "LastUpdatedTimestamp")
            Core.<*> (x Core..:? "LatestVersion")
            Core.<*> (x Core..:? "LatestVersionArn")
            Core.<*> (x Core..:? "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateCoreDefinitionResponse' smart constructor.
data CreateCoreDefinitionResponse = CreateCoreDefinitionResponse'
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

-- | Creates a 'CreateCoreDefinitionResponse' value with any optional fields omitted.
mkCreateCoreDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateCoreDefinitionResponse
mkCreateCoreDefinitionResponse responseStatus =
  CreateCoreDefinitionResponse'
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
ccdrrsArn :: Lens.Lens' CreateCoreDefinitionResponse (Core.Maybe Core.Text)
ccdrrsArn = Lens.field @"arn"
{-# DEPRECATED ccdrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrrsCreationTimestamp :: Lens.Lens' CreateCoreDefinitionResponse (Core.Maybe Core.Text)
ccdrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED ccdrrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrrsId :: Lens.Lens' CreateCoreDefinitionResponse (Core.Maybe Core.Text)
ccdrrsId = Lens.field @"id"
{-# DEPRECATED ccdrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrrsLastUpdatedTimestamp :: Lens.Lens' CreateCoreDefinitionResponse (Core.Maybe Core.Text)
ccdrrsLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# DEPRECATED ccdrrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrrsLatestVersion :: Lens.Lens' CreateCoreDefinitionResponse (Core.Maybe Core.Text)
ccdrrsLatestVersion = Lens.field @"latestVersion"
{-# DEPRECATED ccdrrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrrsLatestVersionArn :: Lens.Lens' CreateCoreDefinitionResponse (Core.Maybe Core.Text)
ccdrrsLatestVersionArn = Lens.field @"latestVersionArn"
{-# DEPRECATED ccdrrsLatestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrrsName :: Lens.Lens' CreateCoreDefinitionResponse (Core.Maybe Core.Text)
ccdrrsName = Lens.field @"name"
{-# DEPRECATED ccdrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccdrrsResponseStatus :: Lens.Lens' CreateCoreDefinitionResponse Core.Int
ccdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
