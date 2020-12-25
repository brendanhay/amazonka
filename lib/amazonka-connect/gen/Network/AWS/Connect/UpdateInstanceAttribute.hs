{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.UpdateInstanceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the value for the specified attribute type.
module Network.AWS.Connect.UpdateInstanceAttribute
  ( -- * Creating a request
    UpdateInstanceAttribute (..),
    mkUpdateInstanceAttribute,

    -- ** Request lenses
    uiaInstanceId,
    uiaAttributeType,
    uiaValue,

    -- * Destructuring the response
    UpdateInstanceAttributeResponse (..),
    mkUpdateInstanceAttributeResponse,
  )
where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateInstanceAttribute' smart constructor.
data UpdateInstanceAttribute = UpdateInstanceAttribute'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Types.InstanceId,
    -- | The type of attribute.
    attributeType :: Types.InstanceAttributeType,
    -- | The value for the attribute. Maximum character limit is 100.
    value :: Types.InstanceAttributeValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateInstanceAttribute' value with any optional fields omitted.
mkUpdateInstanceAttribute ::
  -- | 'instanceId'
  Types.InstanceId ->
  -- | 'attributeType'
  Types.InstanceAttributeType ->
  -- | 'value'
  Types.InstanceAttributeValue ->
  UpdateInstanceAttribute
mkUpdateInstanceAttribute instanceId attributeType value =
  UpdateInstanceAttribute' {instanceId, attributeType, value}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiaInstanceId :: Lens.Lens' UpdateInstanceAttribute Types.InstanceId
uiaInstanceId = Lens.field @"instanceId"
{-# DEPRECATED uiaInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The type of attribute.
--
-- /Note:/ Consider using 'attributeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiaAttributeType :: Lens.Lens' UpdateInstanceAttribute Types.InstanceAttributeType
uiaAttributeType = Lens.field @"attributeType"
{-# DEPRECATED uiaAttributeType "Use generic-lens or generic-optics with 'attributeType' instead." #-}

-- | The value for the attribute. Maximum character limit is 100.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uiaValue :: Lens.Lens' UpdateInstanceAttribute Types.InstanceAttributeValue
uiaValue = Lens.field @"value"
{-# DEPRECATED uiaValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON UpdateInstanceAttribute where
  toJSON UpdateInstanceAttribute {..} =
    Core.object (Core.catMaybes [Core.Just ("Value" Core..= value)])

instance Core.AWSRequest UpdateInstanceAttribute where
  type Rs UpdateInstanceAttribute = UpdateInstanceAttributeResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/instance/" Core.<> (Core.toText instanceId)
                Core.<> ("/attribute/")
                Core.<> (Core.toText attributeType)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UpdateInstanceAttributeResponse'

-- | /See:/ 'mkUpdateInstanceAttributeResponse' smart constructor.
data UpdateInstanceAttributeResponse = UpdateInstanceAttributeResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateInstanceAttributeResponse' value with any optional fields omitted.
mkUpdateInstanceAttributeResponse ::
  UpdateInstanceAttributeResponse
mkUpdateInstanceAttributeResponse =
  UpdateInstanceAttributeResponse'
