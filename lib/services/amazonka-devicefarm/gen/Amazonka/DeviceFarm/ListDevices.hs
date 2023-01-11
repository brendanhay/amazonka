{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DeviceFarm.ListDevices
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about unique device types.
--
-- This operation returns paginated results.
module Amazonka.DeviceFarm.ListDevices
  ( -- * Creating a Request
    ListDevices (..),
    newListDevices,

    -- * Request Lenses
    listDevices_arn,
    listDevices_filters,
    listDevices_nextToken,

    -- * Destructuring the Response
    ListDevicesResponse (..),
    newListDevicesResponse,

    -- * Response Lenses
    listDevicesResponse_devices,
    listDevicesResponse_nextToken,
    listDevicesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the result of a list devices request.
--
-- /See:/ 'newListDevices' smart constructor.
data ListDevices = ListDevices'
  { -- | The Amazon Resource Name (ARN) of the project.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Used to select a set of devices. A filter is made up of an attribute, an
    -- operator, and one or more values.
    --
    -- -   Attribute: The aspect of a device such as platform or model used as
    --     the selection criteria in a device filter.
    --
    --     Allowed values include:
    --
    --     -   ARN: The Amazon Resource Name (ARN) of the device (for example,
    --         @arn:aws:devicefarm:us-west-2::device:12345Example@).
    --
    --     -   PLATFORM: The device platform. Valid values are ANDROID or IOS.
    --
    --     -   OS_VERSION: The operating system version (for example, 10.3.2).
    --
    --     -   MODEL: The device model (for example, iPad 5th Gen).
    --
    --     -   AVAILABILITY: The current availability of the device. Valid
    --         values are AVAILABLE, HIGHLY_AVAILABLE, BUSY, or
    --         TEMPORARY_NOT_AVAILABLE.
    --
    --     -   FORM_FACTOR: The device form factor. Valid values are PHONE or
    --         TABLET.
    --
    --     -   MANUFACTURER: The device manufacturer (for example, Apple).
    --
    --     -   REMOTE_ACCESS_ENABLED: Whether the device is enabled for remote
    --         access. Valid values are TRUE or FALSE.
    --
    --     -   REMOTE_DEBUG_ENABLED: Whether the device is enabled for remote
    --         debugging. Valid values are TRUE or FALSE. Because remote
    --         debugging is
    --         <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>,
    --         this attribute is ignored.
    --
    --     -   INSTANCE_ARN: The Amazon Resource Name (ARN) of the device
    --         instance.
    --
    --     -   INSTANCE_LABELS: The label of the device instance.
    --
    --     -   FLEET_TYPE: The fleet type. Valid values are PUBLIC or PRIVATE.
    --
    -- -   Operator: The filter operator.
    --
    --     -   The EQUALS operator is available for every attribute except
    --         INSTANCE_LABELS.
    --
    --     -   The CONTAINS operator is available for the INSTANCE_LABELS and
    --         MODEL attributes.
    --
    --     -   The IN and NOT_IN operators are available for the ARN,
    --         OS_VERSION, MODEL, MANUFACTURER, and INSTANCE_ARN attributes.
    --
    --     -   The LESS_THAN, GREATER_THAN, LESS_THAN_OR_EQUALS, and
    --         GREATER_THAN_OR_EQUALS operators are also available for the
    --         OS_VERSION attribute.
    --
    -- -   Values: An array of one or more filter values.
    --
    --     -   The IN and NOT_IN operators take a values array that has one or
    --         more elements.
    --
    --     -   The other operators require an array with a single element.
    --
    --     -   In a request, the AVAILABILITY attribute takes the following
    --         values: AVAILABLE, HIGHLY_AVAILABLE, BUSY, or
    --         TEMPORARY_NOT_AVAILABLE.
    filters :: Prelude.Maybe [DeviceFilter],
    -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDevices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'listDevices_arn' - The Amazon Resource Name (ARN) of the project.
--
-- 'filters', 'listDevices_filters' - Used to select a set of devices. A filter is made up of an attribute, an
-- operator, and one or more values.
--
-- -   Attribute: The aspect of a device such as platform or model used as
--     the selection criteria in a device filter.
--
--     Allowed values include:
--
--     -   ARN: The Amazon Resource Name (ARN) of the device (for example,
--         @arn:aws:devicefarm:us-west-2::device:12345Example@).
--
--     -   PLATFORM: The device platform. Valid values are ANDROID or IOS.
--
--     -   OS_VERSION: The operating system version (for example, 10.3.2).
--
--     -   MODEL: The device model (for example, iPad 5th Gen).
--
--     -   AVAILABILITY: The current availability of the device. Valid
--         values are AVAILABLE, HIGHLY_AVAILABLE, BUSY, or
--         TEMPORARY_NOT_AVAILABLE.
--
--     -   FORM_FACTOR: The device form factor. Valid values are PHONE or
--         TABLET.
--
--     -   MANUFACTURER: The device manufacturer (for example, Apple).
--
--     -   REMOTE_ACCESS_ENABLED: Whether the device is enabled for remote
--         access. Valid values are TRUE or FALSE.
--
--     -   REMOTE_DEBUG_ENABLED: Whether the device is enabled for remote
--         debugging. Valid values are TRUE or FALSE. Because remote
--         debugging is
--         <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>,
--         this attribute is ignored.
--
--     -   INSTANCE_ARN: The Amazon Resource Name (ARN) of the device
--         instance.
--
--     -   INSTANCE_LABELS: The label of the device instance.
--
--     -   FLEET_TYPE: The fleet type. Valid values are PUBLIC or PRIVATE.
--
-- -   Operator: The filter operator.
--
--     -   The EQUALS operator is available for every attribute except
--         INSTANCE_LABELS.
--
--     -   The CONTAINS operator is available for the INSTANCE_LABELS and
--         MODEL attributes.
--
--     -   The IN and NOT_IN operators are available for the ARN,
--         OS_VERSION, MODEL, MANUFACTURER, and INSTANCE_ARN attributes.
--
--     -   The LESS_THAN, GREATER_THAN, LESS_THAN_OR_EQUALS, and
--         GREATER_THAN_OR_EQUALS operators are also available for the
--         OS_VERSION attribute.
--
-- -   Values: An array of one or more filter values.
--
--     -   The IN and NOT_IN operators take a values array that has one or
--         more elements.
--
--     -   The other operators require an array with a single element.
--
--     -   In a request, the AVAILABILITY attribute takes the following
--         values: AVAILABLE, HIGHLY_AVAILABLE, BUSY, or
--         TEMPORARY_NOT_AVAILABLE.
--
-- 'nextToken', 'listDevices_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
newListDevices ::
  ListDevices
newListDevices =
  ListDevices'
    { arn = Prelude.Nothing,
      filters = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the project.
listDevices_arn :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.Text)
listDevices_arn = Lens.lens (\ListDevices' {arn} -> arn) (\s@ListDevices' {} a -> s {arn = a} :: ListDevices)

-- | Used to select a set of devices. A filter is made up of an attribute, an
-- operator, and one or more values.
--
-- -   Attribute: The aspect of a device such as platform or model used as
--     the selection criteria in a device filter.
--
--     Allowed values include:
--
--     -   ARN: The Amazon Resource Name (ARN) of the device (for example,
--         @arn:aws:devicefarm:us-west-2::device:12345Example@).
--
--     -   PLATFORM: The device platform. Valid values are ANDROID or IOS.
--
--     -   OS_VERSION: The operating system version (for example, 10.3.2).
--
--     -   MODEL: The device model (for example, iPad 5th Gen).
--
--     -   AVAILABILITY: The current availability of the device. Valid
--         values are AVAILABLE, HIGHLY_AVAILABLE, BUSY, or
--         TEMPORARY_NOT_AVAILABLE.
--
--     -   FORM_FACTOR: The device form factor. Valid values are PHONE or
--         TABLET.
--
--     -   MANUFACTURER: The device manufacturer (for example, Apple).
--
--     -   REMOTE_ACCESS_ENABLED: Whether the device is enabled for remote
--         access. Valid values are TRUE or FALSE.
--
--     -   REMOTE_DEBUG_ENABLED: Whether the device is enabled for remote
--         debugging. Valid values are TRUE or FALSE. Because remote
--         debugging is
--         <https://docs.aws.amazon.com/devicefarm/latest/developerguide/history.html no longer supported>,
--         this attribute is ignored.
--
--     -   INSTANCE_ARN: The Amazon Resource Name (ARN) of the device
--         instance.
--
--     -   INSTANCE_LABELS: The label of the device instance.
--
--     -   FLEET_TYPE: The fleet type. Valid values are PUBLIC or PRIVATE.
--
-- -   Operator: The filter operator.
--
--     -   The EQUALS operator is available for every attribute except
--         INSTANCE_LABELS.
--
--     -   The CONTAINS operator is available for the INSTANCE_LABELS and
--         MODEL attributes.
--
--     -   The IN and NOT_IN operators are available for the ARN,
--         OS_VERSION, MODEL, MANUFACTURER, and INSTANCE_ARN attributes.
--
--     -   The LESS_THAN, GREATER_THAN, LESS_THAN_OR_EQUALS, and
--         GREATER_THAN_OR_EQUALS operators are also available for the
--         OS_VERSION attribute.
--
-- -   Values: An array of one or more filter values.
--
--     -   The IN and NOT_IN operators take a values array that has one or
--         more elements.
--
--     -   The other operators require an array with a single element.
--
--     -   In a request, the AVAILABILITY attribute takes the following
--         values: AVAILABLE, HIGHLY_AVAILABLE, BUSY, or
--         TEMPORARY_NOT_AVAILABLE.
listDevices_filters :: Lens.Lens' ListDevices (Prelude.Maybe [DeviceFilter])
listDevices_filters = Lens.lens (\ListDevices' {filters} -> filters) (\s@ListDevices' {} a -> s {filters = a} :: ListDevices) Prelude.. Lens.mapping Lens.coerced

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listDevices_nextToken :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.Text)
listDevices_nextToken = Lens.lens (\ListDevices' {nextToken} -> nextToken) (\s@ListDevices' {} a -> s {nextToken = a} :: ListDevices)

instance Core.AWSPager ListDevices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDevicesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDevicesResponse_devices Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDevices_nextToken
          Lens..~ rs
          Lens.^? listDevicesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListDevices where
  type AWSResponse ListDevices = ListDevicesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDevicesResponse'
            Prelude.<$> (x Data..?> "devices" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDevices where
  hashWithSalt _salt ListDevices' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDevices where
  rnf ListDevices' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListDevices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.ListDevices" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDevices where
  toJSON ListDevices' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("arn" Data..=) Prelude.<$> arn,
            ("filters" Data..=) Prelude.<$> filters,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListDevices where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDevices where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of a list devices operation.
--
-- /See:/ 'newListDevicesResponse' smart constructor.
data ListDevicesResponse = ListDevicesResponse'
  { -- | Information about the devices.
    devices :: Prelude.Maybe [Device],
    -- | If the number of items that are returned is significantly large, this is
    -- an identifier that is also returned. It can be used in a subsequent call
    -- to this operation to return the next set of items in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'devices', 'listDevicesResponse_devices' - Information about the devices.
--
-- 'nextToken', 'listDevicesResponse_nextToken' - If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
--
-- 'httpStatus', 'listDevicesResponse_httpStatus' - The response's http status code.
newListDevicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDevicesResponse
newListDevicesResponse pHttpStatus_ =
  ListDevicesResponse'
    { devices = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the devices.
listDevicesResponse_devices :: Lens.Lens' ListDevicesResponse (Prelude.Maybe [Device])
listDevicesResponse_devices = Lens.lens (\ListDevicesResponse' {devices} -> devices) (\s@ListDevicesResponse' {} a -> s {devices = a} :: ListDevicesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
listDevicesResponse_nextToken :: Lens.Lens' ListDevicesResponse (Prelude.Maybe Prelude.Text)
listDevicesResponse_nextToken = Lens.lens (\ListDevicesResponse' {nextToken} -> nextToken) (\s@ListDevicesResponse' {} a -> s {nextToken = a} :: ListDevicesResponse)

-- | The response's http status code.
listDevicesResponse_httpStatus :: Lens.Lens' ListDevicesResponse Prelude.Int
listDevicesResponse_httpStatus = Lens.lens (\ListDevicesResponse' {httpStatus} -> httpStatus) (\s@ListDevicesResponse' {} a -> s {httpStatus = a} :: ListDevicesResponse)

instance Prelude.NFData ListDevicesResponse where
  rnf ListDevicesResponse' {..} =
    Prelude.rnf devices
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
