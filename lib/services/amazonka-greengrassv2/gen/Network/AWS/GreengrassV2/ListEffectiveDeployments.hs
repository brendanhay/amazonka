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
-- Module      : Network.AWS.GreengrassV2.ListEffectiveDeployments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a paginated list of deployment jobs that IoT Greengrass sends
-- to Greengrass core devices.
--
-- This operation returns paginated results.
module Network.AWS.GreengrassV2.ListEffectiveDeployments
  ( -- * Creating a Request
    ListEffectiveDeployments (..),
    newListEffectiveDeployments,

    -- * Request Lenses
    listEffectiveDeployments_nextToken,
    listEffectiveDeployments_maxResults,
    listEffectiveDeployments_coreDeviceThingName,

    -- * Destructuring the Response
    ListEffectiveDeploymentsResponse (..),
    newListEffectiveDeploymentsResponse,

    -- * Response Lenses
    listEffectiveDeploymentsResponse_nextToken,
    listEffectiveDeploymentsResponse_effectiveDeployments,
    listEffectiveDeploymentsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GreengrassV2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListEffectiveDeployments' smart constructor.
data ListEffectiveDeployments = ListEffectiveDeployments'
  { -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per paginated request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the core device. This is also the name of the IoT thing.
    coreDeviceThingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEffectiveDeployments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEffectiveDeployments_nextToken' - The token to be used for the next set of paginated results.
--
-- 'maxResults', 'listEffectiveDeployments_maxResults' - The maximum number of results to be returned per paginated request.
--
-- 'coreDeviceThingName', 'listEffectiveDeployments_coreDeviceThingName' - The name of the core device. This is also the name of the IoT thing.
newListEffectiveDeployments ::
  -- | 'coreDeviceThingName'
  Prelude.Text ->
  ListEffectiveDeployments
newListEffectiveDeployments pCoreDeviceThingName_ =
  ListEffectiveDeployments'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      coreDeviceThingName = pCoreDeviceThingName_
    }

-- | The token to be used for the next set of paginated results.
listEffectiveDeployments_nextToken :: Lens.Lens' ListEffectiveDeployments (Prelude.Maybe Prelude.Text)
listEffectiveDeployments_nextToken = Lens.lens (\ListEffectiveDeployments' {nextToken} -> nextToken) (\s@ListEffectiveDeployments' {} a -> s {nextToken = a} :: ListEffectiveDeployments)

-- | The maximum number of results to be returned per paginated request.
listEffectiveDeployments_maxResults :: Lens.Lens' ListEffectiveDeployments (Prelude.Maybe Prelude.Natural)
listEffectiveDeployments_maxResults = Lens.lens (\ListEffectiveDeployments' {maxResults} -> maxResults) (\s@ListEffectiveDeployments' {} a -> s {maxResults = a} :: ListEffectiveDeployments)

-- | The name of the core device. This is also the name of the IoT thing.
listEffectiveDeployments_coreDeviceThingName :: Lens.Lens' ListEffectiveDeployments Prelude.Text
listEffectiveDeployments_coreDeviceThingName = Lens.lens (\ListEffectiveDeployments' {coreDeviceThingName} -> coreDeviceThingName) (\s@ListEffectiveDeployments' {} a -> s {coreDeviceThingName = a} :: ListEffectiveDeployments)

instance Core.AWSPager ListEffectiveDeployments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEffectiveDeploymentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listEffectiveDeploymentsResponse_effectiveDeployments
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listEffectiveDeployments_nextToken
          Lens..~ rs
          Lens.^? listEffectiveDeploymentsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListEffectiveDeployments where
  type
    AWSResponse ListEffectiveDeployments =
      ListEffectiveDeploymentsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEffectiveDeploymentsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "effectiveDeployments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEffectiveDeployments

instance Prelude.NFData ListEffectiveDeployments

instance Core.ToHeaders ListEffectiveDeployments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListEffectiveDeployments where
  toPath ListEffectiveDeployments' {..} =
    Prelude.mconcat
      [ "/greengrass/v2/coreDevices/",
        Core.toBS coreDeviceThingName,
        "/effectiveDeployments"
      ]

instance Core.ToQuery ListEffectiveDeployments where
  toQuery ListEffectiveDeployments' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListEffectiveDeploymentsResponse' smart constructor.
data ListEffectiveDeploymentsResponse = ListEffectiveDeploymentsResponse'
  { -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list that summarizes each deployment on the core device.
    effectiveDeployments :: Prelude.Maybe [EffectiveDeployment],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEffectiveDeploymentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEffectiveDeploymentsResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'effectiveDeployments', 'listEffectiveDeploymentsResponse_effectiveDeployments' - A list that summarizes each deployment on the core device.
--
-- 'httpStatus', 'listEffectiveDeploymentsResponse_httpStatus' - The response's http status code.
newListEffectiveDeploymentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEffectiveDeploymentsResponse
newListEffectiveDeploymentsResponse pHttpStatus_ =
  ListEffectiveDeploymentsResponse'
    { nextToken =
        Prelude.Nothing,
      effectiveDeployments = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or null if there are no
-- additional results.
listEffectiveDeploymentsResponse_nextToken :: Lens.Lens' ListEffectiveDeploymentsResponse (Prelude.Maybe Prelude.Text)
listEffectiveDeploymentsResponse_nextToken = Lens.lens (\ListEffectiveDeploymentsResponse' {nextToken} -> nextToken) (\s@ListEffectiveDeploymentsResponse' {} a -> s {nextToken = a} :: ListEffectiveDeploymentsResponse)

-- | A list that summarizes each deployment on the core device.
listEffectiveDeploymentsResponse_effectiveDeployments :: Lens.Lens' ListEffectiveDeploymentsResponse (Prelude.Maybe [EffectiveDeployment])
listEffectiveDeploymentsResponse_effectiveDeployments = Lens.lens (\ListEffectiveDeploymentsResponse' {effectiveDeployments} -> effectiveDeployments) (\s@ListEffectiveDeploymentsResponse' {} a -> s {effectiveDeployments = a} :: ListEffectiveDeploymentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listEffectiveDeploymentsResponse_httpStatus :: Lens.Lens' ListEffectiveDeploymentsResponse Prelude.Int
listEffectiveDeploymentsResponse_httpStatus = Lens.lens (\ListEffectiveDeploymentsResponse' {httpStatus} -> httpStatus) (\s@ListEffectiveDeploymentsResponse' {} a -> s {httpStatus = a} :: ListEffectiveDeploymentsResponse)

instance
  Prelude.NFData
    ListEffectiveDeploymentsResponse
