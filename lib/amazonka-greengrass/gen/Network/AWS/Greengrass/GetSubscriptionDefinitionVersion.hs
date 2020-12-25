{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.GetSubscriptionDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a subscription definition version.
module Network.AWS.Greengrass.GetSubscriptionDefinitionVersion
  ( -- * Creating a request
    GetSubscriptionDefinitionVersion (..),
    mkGetSubscriptionDefinitionVersion,

    -- ** Request lenses
    gsdvSubscriptionDefinitionId,
    gsdvSubscriptionDefinitionVersionId,
    gsdvNextToken,

    -- * Destructuring the response
    GetSubscriptionDefinitionVersionResponse (..),
    mkGetSubscriptionDefinitionVersionResponse,

    -- ** Response lenses
    gsdvrrsArn,
    gsdvrrsCreationTimestamp,
    gsdvrrsDefinition,
    gsdvrrsId,
    gsdvrrsNextToken,
    gsdvrrsVersion,
    gsdvrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSubscriptionDefinitionVersion' smart constructor.
data GetSubscriptionDefinitionVersion = GetSubscriptionDefinitionVersion'
  { -- | The ID of the subscription definition.
    subscriptionDefinitionId :: Core.Text,
    -- | The ID of the subscription definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListSubscriptionDefinitionVersions'' requests. If the version is the last one that was associated with a subscription definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
    subscriptionDefinitionVersionId :: Core.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSubscriptionDefinitionVersion' value with any optional fields omitted.
mkGetSubscriptionDefinitionVersion ::
  -- | 'subscriptionDefinitionId'
  Core.Text ->
  -- | 'subscriptionDefinitionVersionId'
  Core.Text ->
  GetSubscriptionDefinitionVersion
mkGetSubscriptionDefinitionVersion
  subscriptionDefinitionId
  subscriptionDefinitionVersionId =
    GetSubscriptionDefinitionVersion'
      { subscriptionDefinitionId,
        subscriptionDefinitionVersionId,
        nextToken = Core.Nothing
      }

-- | The ID of the subscription definition.
--
-- /Note:/ Consider using 'subscriptionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdvSubscriptionDefinitionId :: Lens.Lens' GetSubscriptionDefinitionVersion Core.Text
gsdvSubscriptionDefinitionId = Lens.field @"subscriptionDefinitionId"
{-# DEPRECATED gsdvSubscriptionDefinitionId "Use generic-lens or generic-optics with 'subscriptionDefinitionId' instead." #-}

-- | The ID of the subscription definition version. This value maps to the ''Version'' property of the corresponding ''VersionInformation'' object, which is returned by ''ListSubscriptionDefinitionVersions'' requests. If the version is the last one that was associated with a subscription definition, the value also maps to the ''LatestVersion'' property of the corresponding ''DefinitionInformation'' object.
--
-- /Note:/ Consider using 'subscriptionDefinitionVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdvSubscriptionDefinitionVersionId :: Lens.Lens' GetSubscriptionDefinitionVersion Core.Text
gsdvSubscriptionDefinitionVersionId = Lens.field @"subscriptionDefinitionVersionId"
{-# DEPRECATED gsdvSubscriptionDefinitionVersionId "Use generic-lens or generic-optics with 'subscriptionDefinitionVersionId' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdvNextToken :: Lens.Lens' GetSubscriptionDefinitionVersion (Core.Maybe Core.Text)
gsdvNextToken = Lens.field @"nextToken"
{-# DEPRECATED gsdvNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest GetSubscriptionDefinitionVersion where
  type
    Rs GetSubscriptionDefinitionVersion =
      GetSubscriptionDefinitionVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/subscriptions/"
                Core.<> (Core.toText subscriptionDefinitionId)
                Core.<> ("/versions/")
                Core.<> (Core.toText subscriptionDefinitionVersionId)
            ),
        Core._rqQuery = Core.toQueryValue "NextToken" Core.<$> nextToken,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSubscriptionDefinitionVersionResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "Definition")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetSubscriptionDefinitionVersionResponse' smart constructor.
data GetSubscriptionDefinitionVersionResponse = GetSubscriptionDefinitionVersionResponse'
  { -- | The ARN of the subscription definition version.
    arn :: Core.Maybe Core.Text,
    -- | The time, in milliseconds since the epoch, when the subscription definition version was created.
    creationTimestamp :: Core.Maybe Core.Text,
    -- | Information about the subscription definition version.
    definition :: Core.Maybe Types.SubscriptionDefinitionVersion,
    -- | The ID of the subscription definition version.
    id :: Core.Maybe Core.Text,
    -- | The token for the next set of results, or ''null'' if there are no additional results.
    nextToken :: Core.Maybe Core.Text,
    -- | The version of the subscription definition version.
    version :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSubscriptionDefinitionVersionResponse' value with any optional fields omitted.
mkGetSubscriptionDefinitionVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetSubscriptionDefinitionVersionResponse
mkGetSubscriptionDefinitionVersionResponse responseStatus =
  GetSubscriptionDefinitionVersionResponse'
    { arn = Core.Nothing,
      creationTimestamp = Core.Nothing,
      definition = Core.Nothing,
      id = Core.Nothing,
      nextToken = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | The ARN of the subscription definition version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdvrrsArn :: Lens.Lens' GetSubscriptionDefinitionVersionResponse (Core.Maybe Core.Text)
gsdvrrsArn = Lens.field @"arn"
{-# DEPRECATED gsdvrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the subscription definition version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdvrrsCreationTimestamp :: Lens.Lens' GetSubscriptionDefinitionVersionResponse (Core.Maybe Core.Text)
gsdvrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED gsdvrrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | Information about the subscription definition version.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdvrrsDefinition :: Lens.Lens' GetSubscriptionDefinitionVersionResponse (Core.Maybe Types.SubscriptionDefinitionVersion)
gsdvrrsDefinition = Lens.field @"definition"
{-# DEPRECATED gsdvrrsDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The ID of the subscription definition version.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdvrrsId :: Lens.Lens' GetSubscriptionDefinitionVersionResponse (Core.Maybe Core.Text)
gsdvrrsId = Lens.field @"id"
{-# DEPRECATED gsdvrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdvrrsNextToken :: Lens.Lens' GetSubscriptionDefinitionVersionResponse (Core.Maybe Core.Text)
gsdvrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gsdvrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The version of the subscription definition version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdvrrsVersion :: Lens.Lens' GetSubscriptionDefinitionVersionResponse (Core.Maybe Core.Text)
gsdvrrsVersion = Lens.field @"version"
{-# DEPRECATED gsdvrrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsdvrrsResponseStatus :: Lens.Lens' GetSubscriptionDefinitionVersionResponse Core.Int
gsdvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gsdvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
