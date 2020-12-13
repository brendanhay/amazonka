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
    ldrsNextToken,
    ldrsDevices,
    ldrsResponseStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the result of a list devices request.
--
-- /See:/ 'mkListDevices' smart constructor.
data ListDevices = ListDevices'
  { -- | The Amazon Resource Name (ARN) of the project.
    arn :: Lude.Maybe Lude.Text,
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
    filters :: Lude.Maybe [DeviceFilter],
    -- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDevices' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the project.
-- * 'filters' - Used to select a set of devices. A filter is made up of an attribute, an operator, and one or more values.
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
-- * 'nextToken' - An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
mkListDevices ::
  ListDevices
mkListDevices =
  ListDevices'
    { arn = Lude.Nothing,
      filters = Lude.Nothing,
      nextToken = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the project.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldArn :: Lens.Lens' ListDevices (Lude.Maybe Lude.Text)
ldArn = Lens.lens (arn :: ListDevices -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: ListDevices)
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
ldFilters :: Lens.Lens' ListDevices (Lude.Maybe [DeviceFilter])
ldFilters = Lens.lens (filters :: ListDevices -> Lude.Maybe [DeviceFilter]) (\s a -> s {filters = a} :: ListDevices)
{-# DEPRECATED ldFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | An identifier that was returned from the previous call to this operation, which can be used to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldNextToken :: Lens.Lens' ListDevices (Lude.Maybe Lude.Text)
ldNextToken = Lens.lens (nextToken :: ListDevices -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDevices)
{-# DEPRECATED ldNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Page.AWSPager ListDevices where
  page rq rs
    | Page.stop (rs Lens.^. ldrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ldrsDevices) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ldNextToken Lens..~ rs Lens.^. ldrsNextToken

instance Lude.AWSRequest ListDevices where
  type Rs ListDevices = ListDevicesResponse
  request = Req.postJSON deviceFarmService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListDevicesResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "devices" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListDevices where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DeviceFarm_20150623.ListDevices" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListDevices where
  toJSON ListDevices' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("arn" Lude..=) Lude.<$> arn,
            ("filters" Lude..=) Lude.<$> filters,
            ("nextToken" Lude..=) Lude.<$> nextToken
          ]
      )

instance Lude.ToPath ListDevices where
  toPath = Lude.const "/"

instance Lude.ToQuery ListDevices where
  toQuery = Lude.const Lude.mempty

-- | Represents the result of a list devices operation.
--
-- /See:/ 'mkListDevicesResponse' smart constructor.
data ListDevicesResponse = ListDevicesResponse'
  { -- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
    nextToken :: Lude.Maybe Lude.Text,
    -- | Information about the devices.
    devices :: Lude.Maybe [Device],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListDevicesResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
-- * 'devices' - Information about the devices.
-- * 'responseStatus' - The response status code.
mkListDevicesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListDevicesResponse
mkListDevicesResponse pResponseStatus_ =
  ListDevicesResponse'
    { nextToken = Lude.Nothing,
      devices = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | If the number of items that are returned is significantly large, this is an identifier that is also returned. It can be used in a subsequent call to this operation to return the next set of items in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsNextToken :: Lens.Lens' ListDevicesResponse (Lude.Maybe Lude.Text)
ldrsNextToken = Lens.lens (nextToken :: ListDevicesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListDevicesResponse)
{-# DEPRECATED ldrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Information about the devices.
--
-- /Note:/ Consider using 'devices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsDevices :: Lens.Lens' ListDevicesResponse (Lude.Maybe [Device])
ldrsDevices = Lens.lens (devices :: ListDevicesResponse -> Lude.Maybe [Device]) (\s a -> s {devices = a} :: ListDevicesResponse)
{-# DEPRECATED ldrsDevices "Use generic-lens or generic-optics with 'devices' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldrsResponseStatus :: Lens.Lens' ListDevicesResponse Lude.Int
ldrsResponseStatus = Lens.lens (responseStatus :: ListDevicesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListDevicesResponse)
{-# DEPRECATED ldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
