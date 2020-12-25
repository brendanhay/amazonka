{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateSubscriptionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a subscription definition. You may provide the initial version of the subscription definition now or use ''CreateSubscriptionDefinitionVersion'' at a later time.
module Network.AWS.Greengrass.CreateSubscriptionDefinition
  ( -- * Creating a request
    CreateSubscriptionDefinition (..),
    mkCreateSubscriptionDefinition,

    -- ** Request lenses
    csdAmznClientToken,
    csdInitialVersion,
    csdName,
    csdTags,

    -- * Destructuring the response
    CreateSubscriptionDefinitionResponse (..),
    mkCreateSubscriptionDefinitionResponse,

    -- ** Response lenses
    csdrrsArn,
    csdrrsCreationTimestamp,
    csdrrsId,
    csdrrsLastUpdatedTimestamp,
    csdrrsLatestVersion,
    csdrrsLatestVersionArn,
    csdrrsName,
    csdrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSubscriptionDefinition' smart constructor.
data CreateSubscriptionDefinition = CreateSubscriptionDefinition'
  { -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text,
    -- | Information about the initial version of the subscription definition.
    initialVersion :: Core.Maybe Types.SubscriptionDefinitionVersion,
    -- | The name of the subscription definition.
    name :: Core.Maybe Core.Text,
    -- | Tag(s) to add to the new resource.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSubscriptionDefinition' value with any optional fields omitted.
mkCreateSubscriptionDefinition ::
  CreateSubscriptionDefinition
mkCreateSubscriptionDefinition =
  CreateSubscriptionDefinition'
    { amznClientToken = Core.Nothing,
      initialVersion = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing
    }

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdAmznClientToken :: Lens.Lens' CreateSubscriptionDefinition (Core.Maybe Core.Text)
csdAmznClientToken = Lens.field @"amznClientToken"
{-# DEPRECATED csdAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | Information about the initial version of the subscription definition.
--
-- /Note:/ Consider using 'initialVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdInitialVersion :: Lens.Lens' CreateSubscriptionDefinition (Core.Maybe Types.SubscriptionDefinitionVersion)
csdInitialVersion = Lens.field @"initialVersion"
{-# DEPRECATED csdInitialVersion "Use generic-lens or generic-optics with 'initialVersion' instead." #-}

-- | The name of the subscription definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdName :: Lens.Lens' CreateSubscriptionDefinition (Core.Maybe Core.Text)
csdName = Lens.field @"name"
{-# DEPRECATED csdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tag(s) to add to the new resource.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdTags :: Lens.Lens' CreateSubscriptionDefinition (Core.Maybe (Core.HashMap Core.Text Core.Text))
csdTags = Lens.field @"tags"
{-# DEPRECATED csdTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateSubscriptionDefinition where
  toJSON CreateSubscriptionDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ ("InitialVersion" Core..=) Core.<$> initialVersion,
            ("Name" Core..=) Core.<$> name,
            ("tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateSubscriptionDefinition where
  type
    Rs CreateSubscriptionDefinition =
      CreateSubscriptionDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/greengrass/definition/subscriptions",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "X-Amzn-Client-Token" amznClientToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSubscriptionDefinitionResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "LastUpdatedTimestamp")
            Core.<*> (x Core..:? "LatestVersion")
            Core.<*> (x Core..:? "LatestVersionArn")
            Core.<*> (x Core..:? "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateSubscriptionDefinitionResponse' smart constructor.
data CreateSubscriptionDefinitionResponse = CreateSubscriptionDefinitionResponse'
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

-- | Creates a 'CreateSubscriptionDefinitionResponse' value with any optional fields omitted.
mkCreateSubscriptionDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateSubscriptionDefinitionResponse
mkCreateSubscriptionDefinitionResponse responseStatus =
  CreateSubscriptionDefinitionResponse'
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
csdrrsArn :: Lens.Lens' CreateSubscriptionDefinitionResponse (Core.Maybe Core.Text)
csdrrsArn = Lens.field @"arn"
{-# DEPRECATED csdrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrrsCreationTimestamp :: Lens.Lens' CreateSubscriptionDefinitionResponse (Core.Maybe Core.Text)
csdrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED csdrrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrrsId :: Lens.Lens' CreateSubscriptionDefinitionResponse (Core.Maybe Core.Text)
csdrrsId = Lens.field @"id"
{-# DEPRECATED csdrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrrsLastUpdatedTimestamp :: Lens.Lens' CreateSubscriptionDefinitionResponse (Core.Maybe Core.Text)
csdrrsLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# DEPRECATED csdrrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrrsLatestVersion :: Lens.Lens' CreateSubscriptionDefinitionResponse (Core.Maybe Core.Text)
csdrrsLatestVersion = Lens.field @"latestVersion"
{-# DEPRECATED csdrrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrrsLatestVersionArn :: Lens.Lens' CreateSubscriptionDefinitionResponse (Core.Maybe Core.Text)
csdrrsLatestVersionArn = Lens.field @"latestVersionArn"
{-# DEPRECATED csdrrsLatestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrrsName :: Lens.Lens' CreateSubscriptionDefinitionResponse (Core.Maybe Core.Text)
csdrrsName = Lens.field @"name"
{-# DEPRECATED csdrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdrrsResponseStatus :: Lens.Lens' CreateSubscriptionDefinitionResponse Core.Int
csdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
