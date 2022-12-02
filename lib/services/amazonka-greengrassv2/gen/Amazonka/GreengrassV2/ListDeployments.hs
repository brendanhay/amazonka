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
-- Module      : Amazonka.GreengrassV2.ListDeployments
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a paginated list of deployments.
--
-- This operation returns paginated results.
module Amazonka.GreengrassV2.ListDeployments
  ( -- * Creating a Request
    ListDeployments (..),
    newListDeployments,

    -- * Request Lenses
    listDeployments_nextToken,
    listDeployments_targetArn,
    listDeployments_parentTargetArn,
    listDeployments_maxResults,
    listDeployments_historyFilter,

    -- * Destructuring the Response
    ListDeploymentsResponse (..),
    newListDeploymentsResponse,

    -- * Response Lenses
    listDeploymentsResponse_nextToken,
    listDeploymentsResponse_deployments,
    listDeploymentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDeployments' smart constructor.
data ListDeployments = ListDeployments'
  { -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the target IoT thing or thing group.
    targetArn :: Prelude.Maybe Prelude.Text,
    -- | The parent deployment\'s target
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- within a subdeployment.
    parentTargetArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per paginated request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The filter for the list of deployments. Choose one of the following
    -- options:
    --
    -- -   @ALL@ – The list includes all deployments.
    --
    -- -   @LATEST_ONLY@ – The list includes only the latest revision of each
    --     deployment.
    --
    -- Default: @LATEST_ONLY@
    historyFilter :: Prelude.Maybe DeploymentHistoryFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeployments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeployments_nextToken' - The token to be used for the next set of paginated results.
--
-- 'targetArn', 'listDeployments_targetArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the target IoT thing or thing group.
--
-- 'parentTargetArn', 'listDeployments_parentTargetArn' - The parent deployment\'s target
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- within a subdeployment.
--
-- 'maxResults', 'listDeployments_maxResults' - The maximum number of results to be returned per paginated request.
--
-- 'historyFilter', 'listDeployments_historyFilter' - The filter for the list of deployments. Choose one of the following
-- options:
--
-- -   @ALL@ – The list includes all deployments.
--
-- -   @LATEST_ONLY@ – The list includes only the latest revision of each
--     deployment.
--
-- Default: @LATEST_ONLY@
newListDeployments ::
  ListDeployments
newListDeployments =
  ListDeployments'
    { nextToken = Prelude.Nothing,
      targetArn = Prelude.Nothing,
      parentTargetArn = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      historyFilter = Prelude.Nothing
    }

-- | The token to be used for the next set of paginated results.
listDeployments_nextToken :: Lens.Lens' ListDeployments (Prelude.Maybe Prelude.Text)
listDeployments_nextToken = Lens.lens (\ListDeployments' {nextToken} -> nextToken) (\s@ListDeployments' {} a -> s {nextToken = a} :: ListDeployments)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the target IoT thing or thing group.
listDeployments_targetArn :: Lens.Lens' ListDeployments (Prelude.Maybe Prelude.Text)
listDeployments_targetArn = Lens.lens (\ListDeployments' {targetArn} -> targetArn) (\s@ListDeployments' {} a -> s {targetArn = a} :: ListDeployments)

-- | The parent deployment\'s target
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- within a subdeployment.
listDeployments_parentTargetArn :: Lens.Lens' ListDeployments (Prelude.Maybe Prelude.Text)
listDeployments_parentTargetArn = Lens.lens (\ListDeployments' {parentTargetArn} -> parentTargetArn) (\s@ListDeployments' {} a -> s {parentTargetArn = a} :: ListDeployments)

-- | The maximum number of results to be returned per paginated request.
listDeployments_maxResults :: Lens.Lens' ListDeployments (Prelude.Maybe Prelude.Natural)
listDeployments_maxResults = Lens.lens (\ListDeployments' {maxResults} -> maxResults) (\s@ListDeployments' {} a -> s {maxResults = a} :: ListDeployments)

-- | The filter for the list of deployments. Choose one of the following
-- options:
--
-- -   @ALL@ – The list includes all deployments.
--
-- -   @LATEST_ONLY@ – The list includes only the latest revision of each
--     deployment.
--
-- Default: @LATEST_ONLY@
listDeployments_historyFilter :: Lens.Lens' ListDeployments (Prelude.Maybe DeploymentHistoryFilter)
listDeployments_historyFilter = Lens.lens (\ListDeployments' {historyFilter} -> historyFilter) (\s@ListDeployments' {} a -> s {historyFilter = a} :: ListDeployments)

instance Core.AWSPager ListDeployments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeploymentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDeploymentsResponse_deployments
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDeployments_nextToken
          Lens..~ rs
          Lens.^? listDeploymentsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDeployments where
  type
    AWSResponse ListDeployments =
      ListDeploymentsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeploymentsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "deployments" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDeployments where
  hashWithSalt _salt ListDeployments' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` targetArn
      `Prelude.hashWithSalt` parentTargetArn
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` historyFilter

instance Prelude.NFData ListDeployments where
  rnf ListDeployments' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf targetArn
      `Prelude.seq` Prelude.rnf parentTargetArn
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf historyFilter

instance Data.ToHeaders ListDeployments where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListDeployments where
  toPath = Prelude.const "/greengrass/v2/deployments"

instance Data.ToQuery ListDeployments where
  toQuery ListDeployments' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "targetArn" Data.=: targetArn,
        "parentTargetArn" Data.=: parentTargetArn,
        "maxResults" Data.=: maxResults,
        "historyFilter" Data.=: historyFilter
      ]

-- | /See:/ 'newListDeploymentsResponse' smart constructor.
data ListDeploymentsResponse = ListDeploymentsResponse'
  { -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list that summarizes each deployment.
    deployments :: Prelude.Maybe [Deployment],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDeploymentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeploymentsResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'deployments', 'listDeploymentsResponse_deployments' - A list that summarizes each deployment.
--
-- 'httpStatus', 'listDeploymentsResponse_httpStatus' - The response's http status code.
newListDeploymentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDeploymentsResponse
newListDeploymentsResponse pHttpStatus_ =
  ListDeploymentsResponse'
    { nextToken =
        Prelude.Nothing,
      deployments = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or null if there are no
-- additional results.
listDeploymentsResponse_nextToken :: Lens.Lens' ListDeploymentsResponse (Prelude.Maybe Prelude.Text)
listDeploymentsResponse_nextToken = Lens.lens (\ListDeploymentsResponse' {nextToken} -> nextToken) (\s@ListDeploymentsResponse' {} a -> s {nextToken = a} :: ListDeploymentsResponse)

-- | A list that summarizes each deployment.
listDeploymentsResponse_deployments :: Lens.Lens' ListDeploymentsResponse (Prelude.Maybe [Deployment])
listDeploymentsResponse_deployments = Lens.lens (\ListDeploymentsResponse' {deployments} -> deployments) (\s@ListDeploymentsResponse' {} a -> s {deployments = a} :: ListDeploymentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDeploymentsResponse_httpStatus :: Lens.Lens' ListDeploymentsResponse Prelude.Int
listDeploymentsResponse_httpStatus = Lens.lens (\ListDeploymentsResponse' {httpStatus} -> httpStatus) (\s@ListDeploymentsResponse' {} a -> s {httpStatus = a} :: ListDeploymentsResponse)

instance Prelude.NFData ListDeploymentsResponse where
  rnf ListDeploymentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf deployments
      `Prelude.seq` Prelude.rnf httpStatus
