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
-- Module      : Amazonka.SageMaker.ListStageDevices
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists devices allocated to the stage, containing detailed device
-- information and deployment status.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListStageDevices
  ( -- * Creating a Request
    ListStageDevices (..),
    newListStageDevices,

    -- * Request Lenses
    listStageDevices_excludeDevicesDeployedInOtherStage,
    listStageDevices_maxResults,
    listStageDevices_nextToken,
    listStageDevices_edgeDeploymentPlanName,
    listStageDevices_stageName,

    -- * Destructuring the Response
    ListStageDevicesResponse (..),
    newListStageDevicesResponse,

    -- * Response Lenses
    listStageDevicesResponse_nextToken,
    listStageDevicesResponse_httpStatus,
    listStageDevicesResponse_deviceDeploymentSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListStageDevices' smart constructor.
data ListStageDevices = ListStageDevices'
  { -- | Toggle for excluding devices deployed in other stages.
    excludeDevicesDeployedInOtherStage :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of requests to select.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The response from the last list when returning a list large enough to
    -- neeed tokening.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the edge deployment plan.
    edgeDeploymentPlanName :: Prelude.Text,
    -- | The name of the stage in the deployment.
    stageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStageDevices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludeDevicesDeployedInOtherStage', 'listStageDevices_excludeDevicesDeployedInOtherStage' - Toggle for excluding devices deployed in other stages.
--
-- 'maxResults', 'listStageDevices_maxResults' - The maximum number of requests to select.
--
-- 'nextToken', 'listStageDevices_nextToken' - The response from the last list when returning a list large enough to
-- neeed tokening.
--
-- 'edgeDeploymentPlanName', 'listStageDevices_edgeDeploymentPlanName' - The name of the edge deployment plan.
--
-- 'stageName', 'listStageDevices_stageName' - The name of the stage in the deployment.
newListStageDevices ::
  -- | 'edgeDeploymentPlanName'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  ListStageDevices
newListStageDevices
  pEdgeDeploymentPlanName_
  pStageName_ =
    ListStageDevices'
      { excludeDevicesDeployedInOtherStage =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        edgeDeploymentPlanName = pEdgeDeploymentPlanName_,
        stageName = pStageName_
      }

-- | Toggle for excluding devices deployed in other stages.
listStageDevices_excludeDevicesDeployedInOtherStage :: Lens.Lens' ListStageDevices (Prelude.Maybe Prelude.Bool)
listStageDevices_excludeDevicesDeployedInOtherStage = Lens.lens (\ListStageDevices' {excludeDevicesDeployedInOtherStage} -> excludeDevicesDeployedInOtherStage) (\s@ListStageDevices' {} a -> s {excludeDevicesDeployedInOtherStage = a} :: ListStageDevices)

-- | The maximum number of requests to select.
listStageDevices_maxResults :: Lens.Lens' ListStageDevices (Prelude.Maybe Prelude.Int)
listStageDevices_maxResults = Lens.lens (\ListStageDevices' {maxResults} -> maxResults) (\s@ListStageDevices' {} a -> s {maxResults = a} :: ListStageDevices)

-- | The response from the last list when returning a list large enough to
-- neeed tokening.
listStageDevices_nextToken :: Lens.Lens' ListStageDevices (Prelude.Maybe Prelude.Text)
listStageDevices_nextToken = Lens.lens (\ListStageDevices' {nextToken} -> nextToken) (\s@ListStageDevices' {} a -> s {nextToken = a} :: ListStageDevices)

-- | The name of the edge deployment plan.
listStageDevices_edgeDeploymentPlanName :: Lens.Lens' ListStageDevices Prelude.Text
listStageDevices_edgeDeploymentPlanName = Lens.lens (\ListStageDevices' {edgeDeploymentPlanName} -> edgeDeploymentPlanName) (\s@ListStageDevices' {} a -> s {edgeDeploymentPlanName = a} :: ListStageDevices)

-- | The name of the stage in the deployment.
listStageDevices_stageName :: Lens.Lens' ListStageDevices Prelude.Text
listStageDevices_stageName = Lens.lens (\ListStageDevices' {stageName} -> stageName) (\s@ListStageDevices' {} a -> s {stageName = a} :: ListStageDevices)

instance Core.AWSPager ListStageDevices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStageDevicesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listStageDevicesResponse_deviceDeploymentSummaries
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listStageDevices_nextToken
          Lens..~ rs
          Lens.^? listStageDevicesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListStageDevices where
  type
    AWSResponse ListStageDevices =
      ListStageDevicesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStageDevicesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "DeviceDeploymentSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListStageDevices where
  hashWithSalt _salt ListStageDevices' {..} =
    _salt
      `Prelude.hashWithSalt` excludeDevicesDeployedInOtherStage
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` edgeDeploymentPlanName
      `Prelude.hashWithSalt` stageName

instance Prelude.NFData ListStageDevices where
  rnf ListStageDevices' {..} =
    Prelude.rnf excludeDevicesDeployedInOtherStage
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf edgeDeploymentPlanName
      `Prelude.seq` Prelude.rnf stageName

instance Data.ToHeaders ListStageDevices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.ListStageDevices" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListStageDevices where
  toJSON ListStageDevices' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExcludeDevicesDeployedInOtherStage" Data..=)
              Prelude.<$> excludeDevicesDeployedInOtherStage,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ( "EdgeDeploymentPlanName"
                  Data..= edgeDeploymentPlanName
              ),
            Prelude.Just ("StageName" Data..= stageName)
          ]
      )

instance Data.ToPath ListStageDevices where
  toPath = Prelude.const "/"

instance Data.ToQuery ListStageDevices where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListStageDevicesResponse' smart constructor.
data ListStageDevicesResponse = ListStageDevicesResponse'
  { -- | The token to use when calling the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | List of summaries of devices allocated to the stage.
    deviceDeploymentSummaries :: [DeviceDeploymentSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStageDevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStageDevicesResponse_nextToken' - The token to use when calling the next page of results.
--
-- 'httpStatus', 'listStageDevicesResponse_httpStatus' - The response's http status code.
--
-- 'deviceDeploymentSummaries', 'listStageDevicesResponse_deviceDeploymentSummaries' - List of summaries of devices allocated to the stage.
newListStageDevicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStageDevicesResponse
newListStageDevicesResponse pHttpStatus_ =
  ListStageDevicesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      deviceDeploymentSummaries = Prelude.mempty
    }

-- | The token to use when calling the next page of results.
listStageDevicesResponse_nextToken :: Lens.Lens' ListStageDevicesResponse (Prelude.Maybe Prelude.Text)
listStageDevicesResponse_nextToken = Lens.lens (\ListStageDevicesResponse' {nextToken} -> nextToken) (\s@ListStageDevicesResponse' {} a -> s {nextToken = a} :: ListStageDevicesResponse)

-- | The response's http status code.
listStageDevicesResponse_httpStatus :: Lens.Lens' ListStageDevicesResponse Prelude.Int
listStageDevicesResponse_httpStatus = Lens.lens (\ListStageDevicesResponse' {httpStatus} -> httpStatus) (\s@ListStageDevicesResponse' {} a -> s {httpStatus = a} :: ListStageDevicesResponse)

-- | List of summaries of devices allocated to the stage.
listStageDevicesResponse_deviceDeploymentSummaries :: Lens.Lens' ListStageDevicesResponse [DeviceDeploymentSummary]
listStageDevicesResponse_deviceDeploymentSummaries = Lens.lens (\ListStageDevicesResponse' {deviceDeploymentSummaries} -> deviceDeploymentSummaries) (\s@ListStageDevicesResponse' {} a -> s {deviceDeploymentSummaries = a} :: ListStageDevicesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListStageDevicesResponse where
  rnf ListStageDevicesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf deviceDeploymentSummaries
