{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.CreateSubscriptionDefinitionVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of a subscription definition which has already been defined.
module Network.AWS.Greengrass.CreateSubscriptionDefinitionVersion
  ( -- * Creating a request
    CreateSubscriptionDefinitionVersion (..),
    mkCreateSubscriptionDefinitionVersion,

    -- ** Request lenses
    csdvSubscriptionDefinitionId,
    csdvAmznClientToken,
    csdvSubscriptions,

    -- * Destructuring the response
    CreateSubscriptionDefinitionVersionResponse (..),
    mkCreateSubscriptionDefinitionVersionResponse,

    -- ** Response lenses
    csdvrrsArn,
    csdvrrsCreationTimestamp,
    csdvrrsId,
    csdvrrsVersion,
    csdvrrsResponseStatus,
  )
where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateSubscriptionDefinitionVersion' smart constructor.
data CreateSubscriptionDefinitionVersion = CreateSubscriptionDefinitionVersion'
  { -- | The ID of the subscription definition.
    subscriptionDefinitionId :: Core.Text,
    -- | A client token used to correlate requests and responses.
    amznClientToken :: Core.Maybe Core.Text,
    -- | A list of subscriptions.
    subscriptions :: Core.Maybe [Types.Subscription]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateSubscriptionDefinitionVersion' value with any optional fields omitted.
mkCreateSubscriptionDefinitionVersion ::
  -- | 'subscriptionDefinitionId'
  Core.Text ->
  CreateSubscriptionDefinitionVersion
mkCreateSubscriptionDefinitionVersion subscriptionDefinitionId =
  CreateSubscriptionDefinitionVersion'
    { subscriptionDefinitionId,
      amznClientToken = Core.Nothing,
      subscriptions = Core.Nothing
    }

-- | The ID of the subscription definition.
--
-- /Note:/ Consider using 'subscriptionDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdvSubscriptionDefinitionId :: Lens.Lens' CreateSubscriptionDefinitionVersion Core.Text
csdvSubscriptionDefinitionId = Lens.field @"subscriptionDefinitionId"
{-# DEPRECATED csdvSubscriptionDefinitionId "Use generic-lens or generic-optics with 'subscriptionDefinitionId' instead." #-}

-- | A client token used to correlate requests and responses.
--
-- /Note:/ Consider using 'amznClientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdvAmznClientToken :: Lens.Lens' CreateSubscriptionDefinitionVersion (Core.Maybe Core.Text)
csdvAmznClientToken = Lens.field @"amznClientToken"
{-# DEPRECATED csdvAmznClientToken "Use generic-lens or generic-optics with 'amznClientToken' instead." #-}

-- | A list of subscriptions.
--
-- /Note:/ Consider using 'subscriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdvSubscriptions :: Lens.Lens' CreateSubscriptionDefinitionVersion (Core.Maybe [Types.Subscription])
csdvSubscriptions = Lens.field @"subscriptions"
{-# DEPRECATED csdvSubscriptions "Use generic-lens or generic-optics with 'subscriptions' instead." #-}

instance Core.FromJSON CreateSubscriptionDefinitionVersion where
  toJSON CreateSubscriptionDefinitionVersion {..} =
    Core.object
      (Core.catMaybes [("Subscriptions" Core..=) Core.<$> subscriptions])

instance Core.AWSRequest CreateSubscriptionDefinitionVersion where
  type
    Rs CreateSubscriptionDefinitionVersion =
      CreateSubscriptionDefinitionVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/greengrass/definition/subscriptions/"
                Core.<> (Core.toText subscriptionDefinitionId)
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
          CreateSubscriptionDefinitionVersionResponse'
            Core.<$> (x Core..:? "Arn")
            Core.<*> (x Core..:? "CreationTimestamp")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateSubscriptionDefinitionVersionResponse' smart constructor.
data CreateSubscriptionDefinitionVersionResponse = CreateSubscriptionDefinitionVersionResponse'
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

-- | Creates a 'CreateSubscriptionDefinitionVersionResponse' value with any optional fields omitted.
mkCreateSubscriptionDefinitionVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateSubscriptionDefinitionVersionResponse
mkCreateSubscriptionDefinitionVersionResponse responseStatus =
  CreateSubscriptionDefinitionVersionResponse'
    { arn = Core.Nothing,
      creationTimestamp = Core.Nothing,
      id = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | The ARN of the version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdvrrsArn :: Lens.Lens' CreateSubscriptionDefinitionVersionResponse (Core.Maybe Core.Text)
csdvrrsArn = Lens.field @"arn"
{-# DEPRECATED csdvrrsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The time, in milliseconds since the epoch, when the version was created.
--
-- /Note:/ Consider using 'creationTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdvrrsCreationTimestamp :: Lens.Lens' CreateSubscriptionDefinitionVersionResponse (Core.Maybe Core.Text)
csdvrrsCreationTimestamp = Lens.field @"creationTimestamp"
{-# DEPRECATED csdvrrsCreationTimestamp "Use generic-lens or generic-optics with 'creationTimestamp' instead." #-}

-- | The ID of the parent definition that the version is associated with.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdvrrsId :: Lens.Lens' CreateSubscriptionDefinitionVersionResponse (Core.Maybe Core.Text)
csdvrrsId = Lens.field @"id"
{-# DEPRECATED csdvrrsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdvrrsVersion :: Lens.Lens' CreateSubscriptionDefinitionVersionResponse (Core.Maybe Core.Text)
csdvrrsVersion = Lens.field @"version"
{-# DEPRECATED csdvrrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csdvrrsResponseStatus :: Lens.Lens' CreateSubscriptionDefinitionVersionResponse Core.Int
csdvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED csdvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
