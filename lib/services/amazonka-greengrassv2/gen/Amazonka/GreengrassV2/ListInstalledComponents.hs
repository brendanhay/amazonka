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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a paginated list of the components that a Greengrass core
-- device runs. This list doesn\'t include components that are deployed
-- from local deployments or components that are deployed as dependencies
-- of other components.
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
-- This operation returns paginated results.
module Amazonka.GreengrassV2.ListInstalledComponents
  ( -- * Creating a Request
    ListInstalledComponents (..),
    newListInstalledComponents,

    -- * Request Lenses
    listInstalledComponents_nextToken,
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
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListInstalledComponents' smart constructor.
data ListInstalledComponents = ListInstalledComponents'
  { -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
      maxResults = Prelude.Nothing,
      coreDeviceThingName = pCoreDeviceThingName_
    }

-- | The token to be used for the next set of paginated results.
listInstalledComponents_nextToken :: Lens.Lens' ListInstalledComponents (Prelude.Maybe Prelude.Text)
listInstalledComponents_nextToken = Lens.lens (\ListInstalledComponents' {nextToken} -> nextToken) (\s@ListInstalledComponents' {} a -> s {nextToken = a} :: ListInstalledComponents)

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInstalledComponentsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "installedComponents"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInstalledComponents where
  hashWithSalt _salt ListInstalledComponents' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` coreDeviceThingName

instance Prelude.NFData ListInstalledComponents where
  rnf ListInstalledComponents' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf coreDeviceThingName

instance Core.ToHeaders ListInstalledComponents where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListInstalledComponents where
  toPath ListInstalledComponents' {..} =
    Prelude.mconcat
      [ "/greengrass/v2/coreDevices/",
        Core.toBS coreDeviceThingName,
        "/installedComponents"
      ]

instance Core.ToQuery ListInstalledComponents where
  toQuery ListInstalledComponents' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListInstalledComponentsResponse' smart constructor.
data ListInstalledComponentsResponse = ListInstalledComponentsResponse'
  { -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list that summarizes each component on the core device.
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
