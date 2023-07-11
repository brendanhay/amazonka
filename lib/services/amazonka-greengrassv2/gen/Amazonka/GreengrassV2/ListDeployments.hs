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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    listDeployments_historyFilter,
    listDeployments_maxResults,
    listDeployments_nextToken,
    listDeployments_parentTargetArn,
    listDeployments_targetArn,

    -- * Destructuring the Response
    ListDeploymentsResponse (..),
    newListDeploymentsResponse,

    -- * Response Lenses
    listDeploymentsResponse_deployments,
    listDeploymentsResponse_nextToken,
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
  { -- | The filter for the list of deployments. Choose one of the following
    -- options:
    --
    -- -   @ALL@ – The list includes all deployments.
    --
    -- -   @LATEST_ONLY@ – The list includes only the latest revision of each
    --     deployment.
    --
    -- Default: @LATEST_ONLY@
    historyFilter :: Prelude.Maybe DeploymentHistoryFilter,
    -- | The maximum number of results to be returned per paginated request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The parent deployment\'s target
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- within a subdeployment.
    parentTargetArn :: Prelude.Maybe Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the target IoT thing or thing group.
    targetArn :: Prelude.Maybe Prelude.Text
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
-- 'historyFilter', 'listDeployments_historyFilter' - The filter for the list of deployments. Choose one of the following
-- options:
--
-- -   @ALL@ – The list includes all deployments.
--
-- -   @LATEST_ONLY@ – The list includes only the latest revision of each
--     deployment.
--
-- Default: @LATEST_ONLY@
--
-- 'maxResults', 'listDeployments_maxResults' - The maximum number of results to be returned per paginated request.
--
-- 'nextToken', 'listDeployments_nextToken' - The token to be used for the next set of paginated results.
--
-- 'parentTargetArn', 'listDeployments_parentTargetArn' - The parent deployment\'s target
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- within a subdeployment.
--
-- 'targetArn', 'listDeployments_targetArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the target IoT thing or thing group.
newListDeployments ::
  ListDeployments
newListDeployments =
  ListDeployments'
    { historyFilter = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      parentTargetArn = Prelude.Nothing,
      targetArn = Prelude.Nothing
    }

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

-- | The maximum number of results to be returned per paginated request.
listDeployments_maxResults :: Lens.Lens' ListDeployments (Prelude.Maybe Prelude.Natural)
listDeployments_maxResults = Lens.lens (\ListDeployments' {maxResults} -> maxResults) (\s@ListDeployments' {} a -> s {maxResults = a} :: ListDeployments)

-- | The token to be used for the next set of paginated results.
listDeployments_nextToken :: Lens.Lens' ListDeployments (Prelude.Maybe Prelude.Text)
listDeployments_nextToken = Lens.lens (\ListDeployments' {nextToken} -> nextToken) (\s@ListDeployments' {} a -> s {nextToken = a} :: ListDeployments)

-- | The parent deployment\'s target
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- within a subdeployment.
listDeployments_parentTargetArn :: Lens.Lens' ListDeployments (Prelude.Maybe Prelude.Text)
listDeployments_parentTargetArn = Lens.lens (\ListDeployments' {parentTargetArn} -> parentTargetArn) (\s@ListDeployments' {} a -> s {parentTargetArn = a} :: ListDeployments)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the target IoT thing or thing group.
listDeployments_targetArn :: Lens.Lens' ListDeployments (Prelude.Maybe Prelude.Text)
listDeployments_targetArn = Lens.lens (\ListDeployments' {targetArn} -> targetArn) (\s@ListDeployments' {} a -> s {targetArn = a} :: ListDeployments)

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
        Prelude.Just
          Prelude.$ rq
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
            Prelude.<$> (x Data..?> "deployments" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDeployments where
  hashWithSalt _salt ListDeployments' {..} =
    _salt
      `Prelude.hashWithSalt` historyFilter
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` parentTargetArn
      `Prelude.hashWithSalt` targetArn

instance Prelude.NFData ListDeployments where
  rnf ListDeployments' {..} =
    Prelude.rnf historyFilter
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf parentTargetArn
      `Prelude.seq` Prelude.rnf targetArn

instance Data.ToHeaders ListDeployments where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListDeployments where
  toPath = Prelude.const "/greengrass/v2/deployments"

instance Data.ToQuery ListDeployments where
  toQuery ListDeployments' {..} =
    Prelude.mconcat
      [ "historyFilter" Data.=: historyFilter,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "parentTargetArn" Data.=: parentTargetArn,
        "targetArn" Data.=: targetArn
      ]

-- | /See:/ 'newListDeploymentsResponse' smart constructor.
data ListDeploymentsResponse = ListDeploymentsResponse'
  { -- | A list that summarizes each deployment.
    deployments :: Prelude.Maybe [Deployment],
    -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'deployments', 'listDeploymentsResponse_deployments' - A list that summarizes each deployment.
--
-- 'nextToken', 'listDeploymentsResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'httpStatus', 'listDeploymentsResponse_httpStatus' - The response's http status code.
newListDeploymentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDeploymentsResponse
newListDeploymentsResponse pHttpStatus_ =
  ListDeploymentsResponse'
    { deployments =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list that summarizes each deployment.
listDeploymentsResponse_deployments :: Lens.Lens' ListDeploymentsResponse (Prelude.Maybe [Deployment])
listDeploymentsResponse_deployments = Lens.lens (\ListDeploymentsResponse' {deployments} -> deployments) (\s@ListDeploymentsResponse' {} a -> s {deployments = a} :: ListDeploymentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or null if there are no
-- additional results.
listDeploymentsResponse_nextToken :: Lens.Lens' ListDeploymentsResponse (Prelude.Maybe Prelude.Text)
listDeploymentsResponse_nextToken = Lens.lens (\ListDeploymentsResponse' {nextToken} -> nextToken) (\s@ListDeploymentsResponse' {} a -> s {nextToken = a} :: ListDeploymentsResponse)

-- | The response's http status code.
listDeploymentsResponse_httpStatus :: Lens.Lens' ListDeploymentsResponse Prelude.Int
listDeploymentsResponse_httpStatus = Lens.lens (\ListDeploymentsResponse' {httpStatus} -> httpStatus) (\s@ListDeploymentsResponse' {} a -> s {httpStatus = a} :: ListDeploymentsResponse)

instance Prelude.NFData ListDeploymentsResponse where
  rnf ListDeploymentsResponse' {..} =
    Prelude.rnf deployments
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
