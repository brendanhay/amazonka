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
-- Module      : Amazonka.GreengrassV2.ListCoreDevices
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a paginated list of Greengrass core devices.
--
-- IoT Greengrass relies on individual devices to send status updates to
-- the Amazon Web Services Cloud. If the IoT Greengrass Core software
-- isn\'t running on the device, or if device isn\'t connected to the
-- Amazon Web Services Cloud, then the reported status of that device might
-- not reflect its current status. The status timestamp indicates when the
-- device status was last updated.
--
-- Core devices send status updates at the following times:
--
-- -   When the IoT Greengrass Core software starts
--
-- -   When the core device receives a deployment from the Amazon Web
--     Services Cloud
--
-- -   When the status of any component on the core device becomes @BROKEN@
--
-- -   At a
--     <https://docs.aws.amazon.com/greengrass/v2/developerguide/greengrass-nucleus-component.html#greengrass-nucleus-component-configuration-fss regular interval that you can configure>,
--     which defaults to 24 hours
--
-- -   For IoT Greengrass Core v2.7.0, the core device sends status updates
--     upon local deployment and cloud deployment
--
-- This operation returns paginated results.
module Amazonka.GreengrassV2.ListCoreDevices
  ( -- * Creating a Request
    ListCoreDevices (..),
    newListCoreDevices,

    -- * Request Lenses
    listCoreDevices_maxResults,
    listCoreDevices_nextToken,
    listCoreDevices_status,
    listCoreDevices_thingGroupArn,

    -- * Destructuring the Response
    ListCoreDevicesResponse (..),
    newListCoreDevicesResponse,

    -- * Response Lenses
    listCoreDevicesResponse_coreDevices,
    listCoreDevicesResponse_nextToken,
    listCoreDevicesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCoreDevices' smart constructor.
data ListCoreDevices = ListCoreDevices'
  { -- | The maximum number of results to be returned per paginated request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The core device status by which to filter. If you specify this
    -- parameter, the list includes only core devices that have this status.
    -- Choose one of the following options:
    --
    -- -   @HEALTHY@ – The IoT Greengrass Core software and all components run
    --     on the core device without issue.
    --
    -- -   @UNHEALTHY@ – The IoT Greengrass Core software or a component is in
    --     a failed state on the core device.
    status :: Prelude.Maybe CoreDeviceStatus,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the IoT thing group by which to filter. If you specify this
    -- parameter, the list includes only core devices that have successfully
    -- deployed a deployment that targets the thing group. When you remove a
    -- core device from a thing group, the list continues to include that core
    -- device.
    thingGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCoreDevices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listCoreDevices_maxResults' - The maximum number of results to be returned per paginated request.
--
-- 'nextToken', 'listCoreDevices_nextToken' - The token to be used for the next set of paginated results.
--
-- 'status', 'listCoreDevices_status' - The core device status by which to filter. If you specify this
-- parameter, the list includes only core devices that have this status.
-- Choose one of the following options:
--
-- -   @HEALTHY@ – The IoT Greengrass Core software and all components run
--     on the core device without issue.
--
-- -   @UNHEALTHY@ – The IoT Greengrass Core software or a component is in
--     a failed state on the core device.
--
-- 'thingGroupArn', 'listCoreDevices_thingGroupArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the IoT thing group by which to filter. If you specify this
-- parameter, the list includes only core devices that have successfully
-- deployed a deployment that targets the thing group. When you remove a
-- core device from a thing group, the list continues to include that core
-- device.
newListCoreDevices ::
  ListCoreDevices
newListCoreDevices =
  ListCoreDevices'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      status = Prelude.Nothing,
      thingGroupArn = Prelude.Nothing
    }

-- | The maximum number of results to be returned per paginated request.
listCoreDevices_maxResults :: Lens.Lens' ListCoreDevices (Prelude.Maybe Prelude.Natural)
listCoreDevices_maxResults = Lens.lens (\ListCoreDevices' {maxResults} -> maxResults) (\s@ListCoreDevices' {} a -> s {maxResults = a} :: ListCoreDevices)

-- | The token to be used for the next set of paginated results.
listCoreDevices_nextToken :: Lens.Lens' ListCoreDevices (Prelude.Maybe Prelude.Text)
listCoreDevices_nextToken = Lens.lens (\ListCoreDevices' {nextToken} -> nextToken) (\s@ListCoreDevices' {} a -> s {nextToken = a} :: ListCoreDevices)

-- | The core device status by which to filter. If you specify this
-- parameter, the list includes only core devices that have this status.
-- Choose one of the following options:
--
-- -   @HEALTHY@ – The IoT Greengrass Core software and all components run
--     on the core device without issue.
--
-- -   @UNHEALTHY@ – The IoT Greengrass Core software or a component is in
--     a failed state on the core device.
listCoreDevices_status :: Lens.Lens' ListCoreDevices (Prelude.Maybe CoreDeviceStatus)
listCoreDevices_status = Lens.lens (\ListCoreDevices' {status} -> status) (\s@ListCoreDevices' {} a -> s {status = a} :: ListCoreDevices)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the IoT thing group by which to filter. If you specify this
-- parameter, the list includes only core devices that have successfully
-- deployed a deployment that targets the thing group. When you remove a
-- core device from a thing group, the list continues to include that core
-- device.
listCoreDevices_thingGroupArn :: Lens.Lens' ListCoreDevices (Prelude.Maybe Prelude.Text)
listCoreDevices_thingGroupArn = Lens.lens (\ListCoreDevices' {thingGroupArn} -> thingGroupArn) (\s@ListCoreDevices' {} a -> s {thingGroupArn = a} :: ListCoreDevices)

instance Core.AWSPager ListCoreDevices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCoreDevicesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCoreDevicesResponse_coreDevices
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCoreDevices_nextToken
          Lens..~ rs
          Lens.^? listCoreDevicesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListCoreDevices where
  type
    AWSResponse ListCoreDevices =
      ListCoreDevicesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCoreDevicesResponse'
            Prelude.<$> (x Data..?> "coreDevices" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCoreDevices where
  hashWithSalt _salt ListCoreDevices' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` thingGroupArn

instance Prelude.NFData ListCoreDevices where
  rnf ListCoreDevices' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf thingGroupArn

instance Data.ToHeaders ListCoreDevices where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListCoreDevices where
  toPath = Prelude.const "/greengrass/v2/coreDevices"

instance Data.ToQuery ListCoreDevices where
  toQuery ListCoreDevices' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "status" Data.=: status,
        "thingGroupArn" Data.=: thingGroupArn
      ]

-- | /See:/ 'newListCoreDevicesResponse' smart constructor.
data ListCoreDevicesResponse = ListCoreDevicesResponse'
  { -- | A list that summarizes each core device.
    coreDevices :: Prelude.Maybe [CoreDevice],
    -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCoreDevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreDevices', 'listCoreDevicesResponse_coreDevices' - A list that summarizes each core device.
--
-- 'nextToken', 'listCoreDevicesResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'httpStatus', 'listCoreDevicesResponse_httpStatus' - The response's http status code.
newListCoreDevicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCoreDevicesResponse
newListCoreDevicesResponse pHttpStatus_ =
  ListCoreDevicesResponse'
    { coreDevices =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list that summarizes each core device.
listCoreDevicesResponse_coreDevices :: Lens.Lens' ListCoreDevicesResponse (Prelude.Maybe [CoreDevice])
listCoreDevicesResponse_coreDevices = Lens.lens (\ListCoreDevicesResponse' {coreDevices} -> coreDevices) (\s@ListCoreDevicesResponse' {} a -> s {coreDevices = a} :: ListCoreDevicesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no
-- additional results.
listCoreDevicesResponse_nextToken :: Lens.Lens' ListCoreDevicesResponse (Prelude.Maybe Prelude.Text)
listCoreDevicesResponse_nextToken = Lens.lens (\ListCoreDevicesResponse' {nextToken} -> nextToken) (\s@ListCoreDevicesResponse' {} a -> s {nextToken = a} :: ListCoreDevicesResponse)

-- | The response's http status code.
listCoreDevicesResponse_httpStatus :: Lens.Lens' ListCoreDevicesResponse Prelude.Int
listCoreDevicesResponse_httpStatus = Lens.lens (\ListCoreDevicesResponse' {httpStatus} -> httpStatus) (\s@ListCoreDevicesResponse' {} a -> s {httpStatus = a} :: ListCoreDevicesResponse)

instance Prelude.NFData ListCoreDevicesResponse where
  rnf ListCoreDevicesResponse' {..} =
    Prelude.rnf coreDevices
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
