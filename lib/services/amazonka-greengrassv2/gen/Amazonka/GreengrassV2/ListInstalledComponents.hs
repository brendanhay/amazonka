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
-- Module      : Amazonka.GreengrassV2.ListInstalledComponents
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a paginated list of the components that a Greengrass core
-- device runs. By default, this list doesn\'t include components that are
-- deployed as dependencies of other components. To include dependencies in
-- the response, set the @topologyFilter@ parameter to @ALL@.
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
module Amazonka.GreengrassV2.ListInstalledComponents
  ( -- * Creating a Request
    ListInstalledComponents (..),
    newListInstalledComponents,

    -- * Request Lenses
    listInstalledComponents_nextToken,
    listInstalledComponents_topologyFilter,
    listInstalledComponents_maxResults,
    listInstalledComponents_coreDeviceThingName,

    -- * Destructuring the Response
    ListInstalledComponentsResponse (..),
    newListInstalledComponentsResponse,

    -- * Response Lenses
    listInstalledComponentsResponse_nextToken,
    listInstalledComponentsResponse_installedComponents,
    listInstalledComponentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListInstalledComponents' smart constructor.
data ListInstalledComponents = ListInstalledComponents'
  { -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The filter for the list of components. Choose from the following
    -- options:
    --
    -- -   @ALL@ – The list includes all components installed on the core
    --     device.
    --
    -- -   @ROOT@ – The list includes only /root/ components, which are
    --     components that you specify in a deployment. When you choose this
    --     option, the list doesn\'t include components that the core device
    --     installs as dependencies of other components.
    --
    -- Default: @ROOT@
    topologyFilter :: Prelude.Maybe InstalledComponentTopologyFilter,
    -- | The maximum number of results to be returned per paginated request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the core device. This is also the name of the IoT thing.
    coreDeviceThingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInstalledComponents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInstalledComponents_nextToken' - The token to be used for the next set of paginated results.
--
-- 'topologyFilter', 'listInstalledComponents_topologyFilter' - The filter for the list of components. Choose from the following
-- options:
--
-- -   @ALL@ – The list includes all components installed on the core
--     device.
--
-- -   @ROOT@ – The list includes only /root/ components, which are
--     components that you specify in a deployment. When you choose this
--     option, the list doesn\'t include components that the core device
--     installs as dependencies of other components.
--
-- Default: @ROOT@
--
-- 'maxResults', 'listInstalledComponents_maxResults' - The maximum number of results to be returned per paginated request.
--
-- 'coreDeviceThingName', 'listInstalledComponents_coreDeviceThingName' - The name of the core device. This is also the name of the IoT thing.
newListInstalledComponents ::
  -- | 'coreDeviceThingName'
  Prelude.Text ->
  ListInstalledComponents
newListInstalledComponents pCoreDeviceThingName_ =
  ListInstalledComponents'
    { nextToken =
        Prelude.Nothing,
      topologyFilter = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      coreDeviceThingName = pCoreDeviceThingName_
    }

-- | The token to be used for the next set of paginated results.
listInstalledComponents_nextToken :: Lens.Lens' ListInstalledComponents (Prelude.Maybe Prelude.Text)
listInstalledComponents_nextToken = Lens.lens (\ListInstalledComponents' {nextToken} -> nextToken) (\s@ListInstalledComponents' {} a -> s {nextToken = a} :: ListInstalledComponents)

-- | The filter for the list of components. Choose from the following
-- options:
--
-- -   @ALL@ – The list includes all components installed on the core
--     device.
--
-- -   @ROOT@ – The list includes only /root/ components, which are
--     components that you specify in a deployment. When you choose this
--     option, the list doesn\'t include components that the core device
--     installs as dependencies of other components.
--
-- Default: @ROOT@
listInstalledComponents_topologyFilter :: Lens.Lens' ListInstalledComponents (Prelude.Maybe InstalledComponentTopologyFilter)
listInstalledComponents_topologyFilter = Lens.lens (\ListInstalledComponents' {topologyFilter} -> topologyFilter) (\s@ListInstalledComponents' {} a -> s {topologyFilter = a} :: ListInstalledComponents)

-- | The maximum number of results to be returned per paginated request.
listInstalledComponents_maxResults :: Lens.Lens' ListInstalledComponents (Prelude.Maybe Prelude.Natural)
listInstalledComponents_maxResults = Lens.lens (\ListInstalledComponents' {maxResults} -> maxResults) (\s@ListInstalledComponents' {} a -> s {maxResults = a} :: ListInstalledComponents)

-- | The name of the core device. This is also the name of the IoT thing.
listInstalledComponents_coreDeviceThingName :: Lens.Lens' ListInstalledComponents Prelude.Text
listInstalledComponents_coreDeviceThingName = Lens.lens (\ListInstalledComponents' {coreDeviceThingName} -> coreDeviceThingName) (\s@ListInstalledComponents' {} a -> s {coreDeviceThingName = a} :: ListInstalledComponents)

instance Core.AWSPager ListInstalledComponents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInstalledComponentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listInstalledComponentsResponse_installedComponents
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listInstalledComponents_nextToken
          Lens..~ rs
          Lens.^? listInstalledComponentsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListInstalledComponents where
  type
    AWSResponse ListInstalledComponents =
      ListInstalledComponentsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstalledComponentsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "installedComponents"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInstalledComponents where
  hashWithSalt _salt ListInstalledComponents' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` topologyFilter
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` coreDeviceThingName

instance Prelude.NFData ListInstalledComponents where
  rnf ListInstalledComponents' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf topologyFilter
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf coreDeviceThingName

instance Data.ToHeaders ListInstalledComponents where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListInstalledComponents where
  toPath ListInstalledComponents' {..} =
    Prelude.mconcat
      [ "/greengrass/v2/coreDevices/",
        Data.toBS coreDeviceThingName,
        "/installedComponents"
      ]

instance Data.ToQuery ListInstalledComponents where
  toQuery ListInstalledComponents' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "topologyFilter" Data.=: topologyFilter,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListInstalledComponentsResponse' smart constructor.
data ListInstalledComponentsResponse = ListInstalledComponentsResponse'
  { -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list that summarizes each component on the core device.
    --
    -- Greengrass nucleus v2.7.0 or later is required to get an accurate
    -- @lastStatusChangeTimestamp@ response. This response can be inaccurate in
    -- earlier Greengrass nucleus versions.
    --
    -- Greengrass nucleus v2.8.0 or later is required to get an accurate
    -- @lastInstallationSource@ and @lastReportedTimestamp@ response. This
    -- response can be inaccurate or null in earlier Greengrass nucleus
    -- versions.
    installedComponents :: Prelude.Maybe [InstalledComponent],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInstalledComponentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listInstalledComponentsResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'installedComponents', 'listInstalledComponentsResponse_installedComponents' - A list that summarizes each component on the core device.
--
-- Greengrass nucleus v2.7.0 or later is required to get an accurate
-- @lastStatusChangeTimestamp@ response. This response can be inaccurate in
-- earlier Greengrass nucleus versions.
--
-- Greengrass nucleus v2.8.0 or later is required to get an accurate
-- @lastInstallationSource@ and @lastReportedTimestamp@ response. This
-- response can be inaccurate or null in earlier Greengrass nucleus
-- versions.
--
-- 'httpStatus', 'listInstalledComponentsResponse_httpStatus' - The response's http status code.
newListInstalledComponentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInstalledComponentsResponse
newListInstalledComponentsResponse pHttpStatus_ =
  ListInstalledComponentsResponse'
    { nextToken =
        Prelude.Nothing,
      installedComponents = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or null if there are no
-- additional results.
listInstalledComponentsResponse_nextToken :: Lens.Lens' ListInstalledComponentsResponse (Prelude.Maybe Prelude.Text)
listInstalledComponentsResponse_nextToken = Lens.lens (\ListInstalledComponentsResponse' {nextToken} -> nextToken) (\s@ListInstalledComponentsResponse' {} a -> s {nextToken = a} :: ListInstalledComponentsResponse)

-- | A list that summarizes each component on the core device.
--
-- Greengrass nucleus v2.7.0 or later is required to get an accurate
-- @lastStatusChangeTimestamp@ response. This response can be inaccurate in
-- earlier Greengrass nucleus versions.
--
-- Greengrass nucleus v2.8.0 or later is required to get an accurate
-- @lastInstallationSource@ and @lastReportedTimestamp@ response. This
-- response can be inaccurate or null in earlier Greengrass nucleus
-- versions.
listInstalledComponentsResponse_installedComponents :: Lens.Lens' ListInstalledComponentsResponse (Prelude.Maybe [InstalledComponent])
listInstalledComponentsResponse_installedComponents = Lens.lens (\ListInstalledComponentsResponse' {installedComponents} -> installedComponents) (\s@ListInstalledComponentsResponse' {} a -> s {installedComponents = a} :: ListInstalledComponentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listInstalledComponentsResponse_httpStatus :: Lens.Lens' ListInstalledComponentsResponse Prelude.Int
listInstalledComponentsResponse_httpStatus = Lens.lens (\ListInstalledComponentsResponse' {httpStatus} -> httpStatus) (\s@ListInstalledComponentsResponse' {} a -> s {httpStatus = a} :: ListInstalledComponentsResponse)

instance
  Prelude.NFData
    ListInstalledComponentsResponse
  where
  rnf ListInstalledComponentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf installedComponents
      `Prelude.seq` Prelude.rnf httpStatus
