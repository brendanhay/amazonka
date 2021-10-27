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
-- Module      : Network.AWS.GreengrassV2.ListInstalledComponents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a paginated list of the components that a Greengrass core
-- device runs.
--
-- This operation returns paginated results.
module Network.AWS.GreengrassV2.ListInstalledComponents
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
    listInstalledComponentsResponse_installedComponents,
    listInstalledComponentsResponse_nextToken,
    listInstalledComponentsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GreengrassV2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
            Prelude.<$> ( x Core..?> "installedComponents"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInstalledComponents

instance Prelude.NFData ListInstalledComponents

instance Core.ToHeaders ListInstalledComponents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

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
  { -- | A list that summarizes each component on the core device.
    installedComponents :: Prelude.Maybe [InstalledComponent],
    -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'installedComponents', 'listInstalledComponentsResponse_installedComponents' - A list that summarizes each component on the core device.
--
-- 'nextToken', 'listInstalledComponentsResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'httpStatus', 'listInstalledComponentsResponse_httpStatus' - The response's http status code.
newListInstalledComponentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInstalledComponentsResponse
newListInstalledComponentsResponse pHttpStatus_ =
  ListInstalledComponentsResponse'
    { installedComponents =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list that summarizes each component on the core device.
listInstalledComponentsResponse_installedComponents :: Lens.Lens' ListInstalledComponentsResponse (Prelude.Maybe [InstalledComponent])
listInstalledComponentsResponse_installedComponents = Lens.lens (\ListInstalledComponentsResponse' {installedComponents} -> installedComponents) (\s@ListInstalledComponentsResponse' {} a -> s {installedComponents = a} :: ListInstalledComponentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no
-- additional results.
listInstalledComponentsResponse_nextToken :: Lens.Lens' ListInstalledComponentsResponse (Prelude.Maybe Prelude.Text)
listInstalledComponentsResponse_nextToken = Lens.lens (\ListInstalledComponentsResponse' {nextToken} -> nextToken) (\s@ListInstalledComponentsResponse' {} a -> s {nextToken = a} :: ListInstalledComponentsResponse)

-- | The response's http status code.
listInstalledComponentsResponse_httpStatus :: Lens.Lens' ListInstalledComponentsResponse Prelude.Int
listInstalledComponentsResponse_httpStatus = Lens.lens (\ListInstalledComponentsResponse' {httpStatus} -> httpStatus) (\s@ListInstalledComponentsResponse' {} a -> s {httpStatus = a} :: ListInstalledComponentsResponse)

instance
  Prelude.NFData
    ListInstalledComponentsResponse
