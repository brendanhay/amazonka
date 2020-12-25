{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeThing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified thing.
module Network.AWS.IoT.DescribeThing
  ( -- * Creating a request
    DescribeThing (..),
    mkDescribeThing,

    -- ** Request lenses
    dtfThingName,

    -- * Destructuring the response
    DescribeThingResponse (..),
    mkDescribeThingResponse,

    -- ** Response lenses
    dtrrsAttributes,
    dtrrsBillingGroupName,
    dtrrsDefaultClientId,
    dtrrsThingArn,
    dtrrsThingId,
    dtrrsThingName,
    dtrrsThingTypeName,
    dtrrsVersion,
    dtrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DescribeThing operation.
--
-- /See:/ 'mkDescribeThing' smart constructor.
newtype DescribeThing = DescribeThing'
  { -- | The name of the thing.
    thingName :: Types.ThingName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeThing' value with any optional fields omitted.
mkDescribeThing ::
  -- | 'thingName'
  Types.ThingName ->
  DescribeThing
mkDescribeThing thingName = DescribeThing' {thingName}

-- | The name of the thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfThingName :: Lens.Lens' DescribeThing Types.ThingName
dtfThingName = Lens.field @"thingName"
{-# DEPRECATED dtfThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

instance Core.AWSRequest DescribeThing where
  type Rs DescribeThing = DescribeThingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath ("/things/" Core.<> (Core.toText thingName)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeThingResponse'
            Core.<$> (x Core..:? "attributes")
            Core.<*> (x Core..:? "billingGroupName")
            Core.<*> (x Core..:? "defaultClientId")
            Core.<*> (x Core..:? "thingArn")
            Core.<*> (x Core..:? "thingId")
            Core.<*> (x Core..:? "thingName")
            Core.<*> (x Core..:? "thingTypeName")
            Core.<*> (x Core..:? "version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The output from the DescribeThing operation.
--
-- /See:/ 'mkDescribeThingResponse' smart constructor.
data DescribeThingResponse = DescribeThingResponse'
  { -- | The thing attributes.
    attributes :: Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue),
    -- | The name of the billing group the thing belongs to.
    billingGroupName :: Core.Maybe Types.BillingGroupName,
    -- | The default MQTT client ID. For a typical device, the thing name is also used as the default MQTT client ID. Although we don’t require a mapping between a thing's registry name and its use of MQTT client IDs, certificates, or shadow state, we recommend that you choose a thing name and use it as the MQTT client ID for the registry and the Device Shadow service.
    --
    -- This lets you better organize your AWS IoT fleet without removing the flexibility of the underlying device certificate model or shadows.
    defaultClientId :: Core.Maybe Types.ClientId,
    -- | The ARN of the thing to describe.
    thingArn :: Core.Maybe Types.ThingArn,
    -- | The ID of the thing to describe.
    thingId :: Core.Maybe Types.ThingId,
    -- | The name of the thing.
    thingName :: Core.Maybe Types.ThingName,
    -- | The thing type name.
    thingTypeName :: Core.Maybe Types.ThingTypeName,
    -- | The current version of the thing record in the registry.
    version :: Core.Maybe Core.Integer,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeThingResponse' value with any optional fields omitted.
mkDescribeThingResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeThingResponse
mkDescribeThingResponse responseStatus =
  DescribeThingResponse'
    { attributes = Core.Nothing,
      billingGroupName = Core.Nothing,
      defaultClientId = Core.Nothing,
      thingArn = Core.Nothing,
      thingId = Core.Nothing,
      thingName = Core.Nothing,
      thingTypeName = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | The thing attributes.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsAttributes :: Lens.Lens' DescribeThingResponse (Core.Maybe (Core.HashMap Types.AttributeName Types.AttributeValue))
dtrrsAttributes = Lens.field @"attributes"
{-# DEPRECATED dtrrsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The name of the billing group the thing belongs to.
--
-- /Note:/ Consider using 'billingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsBillingGroupName :: Lens.Lens' DescribeThingResponse (Core.Maybe Types.BillingGroupName)
dtrrsBillingGroupName = Lens.field @"billingGroupName"
{-# DEPRECATED dtrrsBillingGroupName "Use generic-lens or generic-optics with 'billingGroupName' instead." #-}

-- | The default MQTT client ID. For a typical device, the thing name is also used as the default MQTT client ID. Although we don’t require a mapping between a thing's registry name and its use of MQTT client IDs, certificates, or shadow state, we recommend that you choose a thing name and use it as the MQTT client ID for the registry and the Device Shadow service.
--
-- This lets you better organize your AWS IoT fleet without removing the flexibility of the underlying device certificate model or shadows.
--
-- /Note:/ Consider using 'defaultClientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsDefaultClientId :: Lens.Lens' DescribeThingResponse (Core.Maybe Types.ClientId)
dtrrsDefaultClientId = Lens.field @"defaultClientId"
{-# DEPRECATED dtrrsDefaultClientId "Use generic-lens or generic-optics with 'defaultClientId' instead." #-}

-- | The ARN of the thing to describe.
--
-- /Note:/ Consider using 'thingArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsThingArn :: Lens.Lens' DescribeThingResponse (Core.Maybe Types.ThingArn)
dtrrsThingArn = Lens.field @"thingArn"
{-# DEPRECATED dtrrsThingArn "Use generic-lens or generic-optics with 'thingArn' instead." #-}

-- | The ID of the thing to describe.
--
-- /Note:/ Consider using 'thingId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsThingId :: Lens.Lens' DescribeThingResponse (Core.Maybe Types.ThingId)
dtrrsThingId = Lens.field @"thingId"
{-# DEPRECATED dtrrsThingId "Use generic-lens or generic-optics with 'thingId' instead." #-}

-- | The name of the thing.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsThingName :: Lens.Lens' DescribeThingResponse (Core.Maybe Types.ThingName)
dtrrsThingName = Lens.field @"thingName"
{-# DEPRECATED dtrrsThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | The thing type name.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsThingTypeName :: Lens.Lens' DescribeThingResponse (Core.Maybe Types.ThingTypeName)
dtrrsThingTypeName = Lens.field @"thingTypeName"
{-# DEPRECATED dtrrsThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

-- | The current version of the thing record in the registry.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsVersion :: Lens.Lens' DescribeThingResponse (Core.Maybe Core.Integer)
dtrrsVersion = Lens.field @"version"
{-# DEPRECATED dtrrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrrsResponseStatus :: Lens.Lens' DescribeThingResponse Core.Int
dtrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
