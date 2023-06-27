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
-- Module      : Amazonka.GreengrassV2.ListEffectiveDeployments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a paginated list of deployment jobs that IoT Greengrass sends
-- to Greengrass core devices.
--
-- This operation returns paginated results.
module Amazonka.GreengrassV2.ListEffectiveDeployments
  ( -- * Creating a Request
    ListEffectiveDeployments (..),
    newListEffectiveDeployments,

    -- * Request Lenses
    listEffectiveDeployments_maxResults,
    listEffectiveDeployments_nextToken,
    listEffectiveDeployments_coreDeviceThingName,

    -- * Destructuring the Response
    ListEffectiveDeploymentsResponse (..),
    newListEffectiveDeploymentsResponse,

    -- * Response Lenses
    listEffectiveDeploymentsResponse_effectiveDeployments,
    listEffectiveDeploymentsResponse_nextToken,
    listEffectiveDeploymentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEffectiveDeployments' smart constructor.
data ListEffectiveDeployments = ListEffectiveDeployments'
  { -- | The maximum number of results to be returned per paginated request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'maxResults', 'listEffectiveDeployments_maxResults' - The maximum number of results to be returned per paginated request.
--
-- 'nextToken', 'listEffectiveDeployments_nextToken' - The token to be used for the next set of paginated results.
--
-- 'coreDeviceThingName', 'listEffectiveDeployments_coreDeviceThingName' - The name of the core device. This is also the name of the IoT thing.
newListEffectiveDeployments ::
  -- | 'coreDeviceThingName'
  Prelude.Text ->
  ListEffectiveDeployments
newListEffectiveDeployments pCoreDeviceThingName_ =
  ListEffectiveDeployments'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      coreDeviceThingName = pCoreDeviceThingName_
    }

-- | The maximum number of results to be returned per paginated request.
listEffectiveDeployments_maxResults :: Lens.Lens' ListEffectiveDeployments (Prelude.Maybe Prelude.Natural)
listEffectiveDeployments_maxResults = Lens.lens (\ListEffectiveDeployments' {maxResults} -> maxResults) (\s@ListEffectiveDeployments' {} a -> s {maxResults = a} :: ListEffectiveDeployments)

-- | The token to be used for the next set of paginated results.
listEffectiveDeployments_nextToken :: Lens.Lens' ListEffectiveDeployments (Prelude.Maybe Prelude.Text)
listEffectiveDeployments_nextToken = Lens.lens (\ListEffectiveDeployments' {nextToken} -> nextToken) (\s@ListEffectiveDeployments' {} a -> s {nextToken = a} :: ListEffectiveDeployments)

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
        Prelude.Just
          Prelude.$ rq
          Prelude.& listEffectiveDeployments_nextToken
          Lens..~ rs
          Lens.^? listEffectiveDeploymentsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListEffectiveDeployments where
  type
    AWSResponse ListEffectiveDeployments =
      ListEffectiveDeploymentsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEffectiveDeploymentsResponse'
            Prelude.<$> ( x
                            Data..?> "effectiveDeployments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListEffectiveDeployments where
  hashWithSalt _salt ListEffectiveDeployments' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` coreDeviceThingName

instance Prelude.NFData ListEffectiveDeployments where
  rnf ListEffectiveDeployments' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf coreDeviceThingName

instance Data.ToHeaders ListEffectiveDeployments where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListEffectiveDeployments where
  toPath ListEffectiveDeployments' {..} =
    Prelude.mconcat
      [ "/greengrass/v2/coreDevices/",
        Data.toBS coreDeviceThingName,
        "/effectiveDeployments"
      ]

instance Data.ToQuery ListEffectiveDeployments where
  toQuery ListEffectiveDeployments' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListEffectiveDeploymentsResponse' smart constructor.
data ListEffectiveDeploymentsResponse = ListEffectiveDeploymentsResponse'
  { -- | A list that summarizes each deployment on the core device.
    effectiveDeployments :: Prelude.Maybe [EffectiveDeployment],
    -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'effectiveDeployments', 'listEffectiveDeploymentsResponse_effectiveDeployments' - A list that summarizes each deployment on the core device.
--
-- 'nextToken', 'listEffectiveDeploymentsResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'httpStatus', 'listEffectiveDeploymentsResponse_httpStatus' - The response's http status code.
newListEffectiveDeploymentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEffectiveDeploymentsResponse
newListEffectiveDeploymentsResponse pHttpStatus_ =
  ListEffectiveDeploymentsResponse'
    { effectiveDeployments =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list that summarizes each deployment on the core device.
listEffectiveDeploymentsResponse_effectiveDeployments :: Lens.Lens' ListEffectiveDeploymentsResponse (Prelude.Maybe [EffectiveDeployment])
listEffectiveDeploymentsResponse_effectiveDeployments = Lens.lens (\ListEffectiveDeploymentsResponse' {effectiveDeployments} -> effectiveDeployments) (\s@ListEffectiveDeploymentsResponse' {} a -> s {effectiveDeployments = a} :: ListEffectiveDeploymentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no
-- additional results.
listEffectiveDeploymentsResponse_nextToken :: Lens.Lens' ListEffectiveDeploymentsResponse (Prelude.Maybe Prelude.Text)
listEffectiveDeploymentsResponse_nextToken = Lens.lens (\ListEffectiveDeploymentsResponse' {nextToken} -> nextToken) (\s@ListEffectiveDeploymentsResponse' {} a -> s {nextToken = a} :: ListEffectiveDeploymentsResponse)

-- | The response's http status code.
listEffectiveDeploymentsResponse_httpStatus :: Lens.Lens' ListEffectiveDeploymentsResponse Prelude.Int
listEffectiveDeploymentsResponse_httpStatus = Lens.lens (\ListEffectiveDeploymentsResponse' {httpStatus} -> httpStatus) (\s@ListEffectiveDeploymentsResponse' {} a -> s {httpStatus = a} :: ListEffectiveDeploymentsResponse)

instance
  Prelude.NFData
    ListEffectiveDeploymentsResponse
  where
  rnf ListEffectiveDeploymentsResponse' {..} =
    Prelude.rnf effectiveDeployments
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
