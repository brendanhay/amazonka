{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetSubscriptionDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a subscription definition.
module Network.AWS.Greengrass.GetSubscriptionDefinition
  ( -- * Creating a request
    GetSubscriptionDefinition (..),
    mkGetSubscriptionDefinition,

    -- ** Request lenses
    gsdSubscriptionDefinitionId,

    -- * Destructuring the response
    GetSubscriptionDefinitionResponse (..),
    mkGetSubscriptionDefinitionResponse,

    -- ** Response lenses
    gsdrrsArn,
    gsdrrsCreationTimestamp,
    gsdrrsId,
    gsdrrsLastUpdatedTimestamp,
    gsdrrsLatestVersion,
    gsdrrsLatestVersionArn,
    gsdrrsName,
    gsdrrsTags,
    gsdrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSubscriptionDefinition' smart constructor.
newtype GetSubscriptionDefinition = GetSubscriptionDefinition'
  { -- | The ID of the subscription definition.
    subscriptionDefinitionId :: Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetSubscriptionDefinition' value with any optional fields omitted.
mkGetSubscriptionDefinition ::
  -- | 'subscriptionDefinitionId'
  Core.Text ->
  GetSubscriptionDefinition
mkGetSubscriptionDefinition subscriptionDefinitionId =
  GetSubscriptionDefinition' {subscriptionDefinitionId}

-- | The ID of the subscription definition.
--
-- /Note:/ Consider using 'subscriptionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdSubscriptionDefinitionId :: Lens.Lens' GetSubscriptionDefinition Core.Text
gsdSubscriptionDefinitionId = Lens.field @"subscriptionDefinitionId"
{-# DEPRECATED gsdSubscriptionDefinitionId "Use generic-lens or generic-optics with 'subscriptionDefinitionId' instead." #-}

instance Core.AWSRequest GetSubscriptionDefinition where
  type
    Rs GetSubscriptionDefinition =
      GetSubscriptionDefinitionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/subscriptions/"
                Core.<> (Core.toText subscriptionDefinitionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSubscriptionDefinitionResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "LastUpdatedTimestamp")
            Core.<*> (x Core..:? "LatestVersion")
            Core.<*> (x Core..:? "LatestVersionArn")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "tags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetSubscriptionDefinitionResponse' smart constructor.
data GetSubscriptionDefinitionResponse = GetSubscriptionDefinitionResponse'
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
    -- | Tag(s) attached to the resource arn.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSubscriptionDefinitionResponse' value with any optional fields omitted.
mkGetSubscriptionDefinitionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetSubscriptionDefinitionResponse
mkGetSubscriptionDefinitionResponse responseStatus =
  GetSubscriptionDefinitionResponse'
    { arn = Core.Nothing,
      creationTimestamp = Core.Nothing,
      id = Core.Nothing,
      lastUpdatedTimestamp = Core.Nothing,
      latestVersion = Core.Nothing,
      latestVersionArn = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      responseStatus
    }

-- | The ARN of the definition.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsArn :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe Core.Text)
gsdrrsArn = Lens.field @"arn"
{-# DEPRECATED gsdrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsCreationTimestamp :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe Core.Text)
gsdrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED gsdrrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the definition.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsId :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe Core.Text)
gsdrrsId = Lens.field @"id"
{-# DEPRECATED gsdrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The time, in milliseconds since the epoch, when the definition was last updated.
--
-- /Note:/ Consider using 'lastUpdatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsLastUpdatedTimestamp :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe Core.Text)
gsdrrsLastUpdatedTimestamp = Lens.field @"lastUpdatedTimestamp"
{-# DEPRECATED gsdrrsLastUpdatedTimestamp "Use generic-lens or generic-optics with 'lastUpdatedTimestamp' instead." #-}

-- | The ID of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsLatestVersion :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe Core.Text)
gsdrrsLatestVersion = Lens.field @"latestVersion"
{-# DEPRECATED gsdrrsLatestVersion "Use generic-lens or generic-optics with 'latestVersion' instead." #-}

-- | The ARN of the latest version associated with the definition.
--
-- /Note:/ Consider using 'latestVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsLatestVersionArn :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe Core.Text)
gsdrrsLatestVersionArn = Lens.field @"latestVersionArn"
{-# DEPRECATED gsdrrsLatestVersionArn "Use generic-lens or generic-optics with 'latestVersionArn' instead." #-}

-- | The name of the definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsName :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe Core.Text)
gsdrrsName = Lens.field @"name"
{-# DEPRECATED gsdrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Tag(s) attached to the resource arn.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsTags :: Lens.Lens' GetSubscriptionDefinitionResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
gsdrrsTags = Lens.field @"tags"
{-# DEPRECATED gsdrrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdrrsResponseStatus :: Lens.Lens' GetSubscriptionDefinitionResponse Core.Int
gsdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gsdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
