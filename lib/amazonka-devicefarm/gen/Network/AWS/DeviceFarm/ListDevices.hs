{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.ListDevices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about unique device types.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListDevices
  ( -- * Creating a request
    ListDevices (..),
    mkListDevices,

    -- ** Request lenses
    ldArn,
    ldFilters,
    ldNextToken,

    -- * Destructuring the response
    ListDevicesResponse (..),
    mkListDevicesResponse,

    -- ** Response lenses
    ldrrsDevices,
    ldrrsNextToken,
    ldrrsResponseStatus,
  )
where

import qualified Network.AWS.DeviceFarm.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the result of a list devices request.
--
-- /See:/ 'mkListDevices' smart constructor.
data ListDevices = ListDevices'
  { -- | The Amazon Resource Name (ARN) of the project.
    arn :: Core.Maybe Types.Arn,
    -- | Used to select a set of devices. A filter is made up of an attribute, an operator, and one or more values.
    --
    --
    --     * Attribute: The aspect of a device such as platform or model used as the selection criteria in a device filter.
    -- Allowed values include:
    --
    --     * ARN: The Amazon Resource Name (ARN) of the device (for example, @arn:aws:devicefarm:us-west-2::device:12345Example@ ).
    --
    --
    --     * PLATFORM: The device platform. Valid values are ANDROID or IOS.
    --
    --
    --     * OS_VERSION: The operating system version (for example, 10.3.2).
    --
    --
    --     * MODEL: The device model (for example, iPad 5th Gen).
    --
    --
    --     * AVAILABILITY: The current availability of the device. Valid values are AVAILABLE, HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE.
    --
    --
    --     * FORM_FACTOR: The device form factor. Valid values are PHONE or TABLET.
    --
    --
    --     * MANUFACTURER: The device manufacturer (for example, Apple).
    --
    --
    --     * REMOTE_ACCESS_ENABLED: Whether the device is enabled for remote access. Valid values are TRUE or FALSE.
    --
    --
    --     * REMOTE_DEBUG_ENABLED: Whether the device is enabled for remote debugging. Valid values are TRUE or FALSE. Because remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> , this attribute is ignored.
    --
    --
    --     * INSTANCE_ARN: The Amazon Resource Name (ARN) of the device instance.
    --
    --
    --     * INSTANCE_LABELS: The label of the device instance.
    --
    --
    --     * FLEET_TYPE: The fleet type. Valid values are PUBLIC or PRIVATE.
    --
    --
    --
    --
    --     * Operator: The filter operator.
    --
    --     * The EQUALS operator is available for every attribute except INSTANCE_LABELS.
    --
    --
    --     * The CONTAINS operator is available for the INSTANCE_LABELS and MODEL attributes.
    --
    --
    --     * The IN and NOT_IN operators are available for the ARN, OS_VERSION, MODEL, MANUFACTURER, and INSTANCE_ARN attributes.
    --
    --
    --     * The LESS_THAN, GREATER_THAN, LESS_THAN_OR_EQUALS, and GREATER_THAN_OR_EQUALS operators are also available for the OS_VERSION attribute.
    --
    --
    --
    --
    --     * Values: An array of one or more filter values.
    --
    --     * The IN and NOT_IN operators take a values array that has one or more elements.
    --
    --
    --     * The other operators require an array with a single element.
    --
    --
    --     * In a request, the AVAILABILITY attribute takes the following values: AVAILABLE, HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE.
    filters :: Core.Maybe [Types.DeviceFilter],
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDevices' value with any optional fields omitted.
mkListDevices ::
  ListDevices
mkListDevices =
  ListDevices'
    { arn = Core.Nothing,
      filters = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the project.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldArn :: Lens.Lens' ListDevices (Core.Maybe Types.Arn)
ldArn = Lens.field @"arn"
{-# DEPRECATED ldArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Used to select a set of devices. A filter is made up of an attribute, an operator, and one or more values.
--
--
--     * Attribute: The aspect of a device such as platform or model used as the selection criteria in a device filter.
-- Allowed values include:
--
--     * ARN: The Amazon Resource Name (ARN) of the device (for example, @arn:aws:devicefarm:us-west-2::device:12345Example@ ).
--
--
--     * PLATFORM: The device platform. Valid values are ANDROID or IOS.
--
--
--     * OS_VERSION: The operating system version (for example, 10.3.2).
--
--
--     * MODEL: The device model (for example, iPad 5th Gen).
--
--
--     * AVAILABILITY: The current availability of the device. Valid values are AVAILABLE, HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE.
--
--
--     * FORM_FACTOR: The device form factor. Valid values are PHONE or TABLET.
--
--
--     * MANUFACTURER: The device manufacturer (for example, Apple).
--
--
--     * REMOTE_ACCESS_ENABLED: Whether the device is enabled for remote access. Valid values are TRUE or FALSE.
--
--
--     * REMOTE_DEBUG_ENABLED: Whether the device is enabled for remote debugging. Valid values are TRUE or FALSE. Because remote debugging is <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported> , this attribute is ignored.
--
--
--     * INSTANCE_ARN: The Amazon Resource Name (ARN) of the device instance.
--
--
--     * INSTANCE_LABELS: The label of the device instance.
--
--
--     * FLEET_TYPE: The fleet type. Valid values are PUBLIC or PRIVATE.
--
--
--
--
--     * Operator: The filter operator.
--
--     * The EQUALS operator is available for every attribute except INSTANCE_LABELS.
--
--
--     * The CONTAINS operator is available for the INSTANCE_LABELS and MODEL attributes.
--
--
--     * The IN and NOT_IN operators are available for the ARN, OS_VERSION, MODEL, MANUFACTURER, and INSTANCE_ARN attributes.
--
--
--     * The LESS_THAN, GREATER_THAN, LESS_THAN_OR_EQUALS, and GREATER_THAN_OR_EQUALS operators are also available for the OS_VERSION attribute.
--
--
--
--
--     * Values: An array of one or more filter values.
--
--     * The IN and NOT_IN operators take a values array that has one or more elements.
--
--
--     * The other operators require an array with a single element.
--
--
--     * In a request, the AVAILABILITY attribute takes the following values: AVAILABLE, HIGHLY_AVAILABLE, BUSY, or TEMPORARY_NOT_AVAILABLE.
--
--
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldFilters :: Lens.Lens' ListDevices (Core.Maybe [Types.DeviceFilter])
ldFilters = Lens.field @"filters"
{-# DEPRECATED ldFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextToken :: Lens.Lens' ListDevices (Core.Maybe Types.PaginationToken)
ldNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListDevices where
  toJSON ListDevices {..} =
    Core.object
      ( Core.catMaybes
          [ ("arn" Core..=) Core.<$> arn,
            ("filters" Core..=) Core.<$> filters,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListDevices where
  type Rs ListDevices = ListDevicesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "DeviceFarm_20150623.ListDevices")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDevicesResponse'
            Core.<$> (x Core..:? "devices")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListDevices where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"devices" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Represents the result of a list devices operation.
--
-- /See:/ 'mkListDevicesResponse' smart constructor.
data ListDevicesResponse = ListDevicesResponse'
  { -- | Information about the devices.
    devices :: Core.Maybe [Types.Device],
    -- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDevicesResponse' value with any optional fields omitted.
mkListDevicesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListDevicesResponse
mkListDevicesResponse responseStatus =
  ListDevicesResponse'
    { devices = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Information about the devices.
--
-- /Note:/ Consider using 'devices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsDevices :: Lens.Lens' ListDevicesResponse (Core.Maybe [Types.Device])
ldrrsDevices = Lens.field @"devices"
{-# DEPRECATED ldrrsDevices "Use generic-lens or generic-optics with 'devices' instead." #-}

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsNextToken :: Lens.Lens' ListDevicesResponse (Core.Maybe Types.PaginationToken)
ldrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ldrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrrsResponseStatus :: Lens.Lens' ListDevicesResponse Core.Int
ldrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ldrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
