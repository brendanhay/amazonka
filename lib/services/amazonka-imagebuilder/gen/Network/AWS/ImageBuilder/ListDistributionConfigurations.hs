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
-- Module      : Network.AWS.ImageBuilder.ListDistributionConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of distribution configurations.
module Network.AWS.ImageBuilder.ListDistributionConfigurations
  ( -- * Creating a Request
    ListDistributionConfigurations (..),
    newListDistributionConfigurations,

    -- * Request Lenses
    listDistributionConfigurations_filters,
    listDistributionConfigurations_nextToken,
    listDistributionConfigurations_maxResults,

    -- * Destructuring the Response
    ListDistributionConfigurationsResponse (..),
    newListDistributionConfigurationsResponse,

    -- * Response Lenses
    listDistributionConfigurationsResponse_requestId,
    listDistributionConfigurationsResponse_distributionConfigurationSummaryList,
    listDistributionConfigurationsResponse_nextToken,
    listDistributionConfigurationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ImageBuilder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDistributionConfigurations' smart constructor.
data ListDistributionConfigurations = ListDistributionConfigurations'
  { -- | You can filter on @name@ to streamline results.
    filters :: Prelude.Maybe (Prelude.NonEmpty Filter),
    -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum items to return in a request.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDistributionConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listDistributionConfigurations_filters' - You can filter on @name@ to streamline results.
--
-- 'nextToken', 'listDistributionConfigurations_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
--
-- 'maxResults', 'listDistributionConfigurations_maxResults' - The maximum items to return in a request.
newListDistributionConfigurations ::
  ListDistributionConfigurations
newListDistributionConfigurations =
  ListDistributionConfigurations'
    { filters =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | You can filter on @name@ to streamline results.
listDistributionConfigurations_filters :: Lens.Lens' ListDistributionConfigurations (Prelude.Maybe (Prelude.NonEmpty Filter))
listDistributionConfigurations_filters = Lens.lens (\ListDistributionConfigurations' {filters} -> filters) (\s@ListDistributionConfigurations' {} a -> s {filters = a} :: ListDistributionConfigurations) Prelude.. Lens.mapping Lens.coerced

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
listDistributionConfigurations_nextToken :: Lens.Lens' ListDistributionConfigurations (Prelude.Maybe Prelude.Text)
listDistributionConfigurations_nextToken = Lens.lens (\ListDistributionConfigurations' {nextToken} -> nextToken) (\s@ListDistributionConfigurations' {} a -> s {nextToken = a} :: ListDistributionConfigurations)

-- | The maximum items to return in a request.
listDistributionConfigurations_maxResults :: Lens.Lens' ListDistributionConfigurations (Prelude.Maybe Prelude.Natural)
listDistributionConfigurations_maxResults = Lens.lens (\ListDistributionConfigurations' {maxResults} -> maxResults) (\s@ListDistributionConfigurations' {} a -> s {maxResults = a} :: ListDistributionConfigurations)

instance
  Core.AWSRequest
    ListDistributionConfigurations
  where
  type
    AWSResponse ListDistributionConfigurations =
      ListDistributionConfigurationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDistributionConfigurationsResponse'
            Prelude.<$> (x Core..?> "requestId")
            Prelude.<*> ( x Core..?> "distributionConfigurationSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDistributionConfigurations

instance
  Prelude.NFData
    ListDistributionConfigurations

instance
  Core.ToHeaders
    ListDistributionConfigurations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListDistributionConfigurations where
  toJSON ListDistributionConfigurations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("filters" Core..=) Prelude.<$> filters,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListDistributionConfigurations where
  toPath =
    Prelude.const "/ListDistributionConfigurations"

instance Core.ToQuery ListDistributionConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDistributionConfigurationsResponse' smart constructor.
data ListDistributionConfigurationsResponse = ListDistributionConfigurationsResponse'
  { -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The list of distributions.
    distributionConfigurationSummaryList :: Prelude.Maybe [DistributionConfigurationSummary],
    -- | The next token used for paginated responses. When this is not empty,
    -- there are additional elements that the service has not included in this
    -- request. Use this token with the next request to retrieve additional
    -- objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDistributionConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'listDistributionConfigurationsResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'distributionConfigurationSummaryList', 'listDistributionConfigurationsResponse_distributionConfigurationSummaryList' - The list of distributions.
--
-- 'nextToken', 'listDistributionConfigurationsResponse_nextToken' - The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
--
-- 'httpStatus', 'listDistributionConfigurationsResponse_httpStatus' - The response's http status code.
newListDistributionConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDistributionConfigurationsResponse
newListDistributionConfigurationsResponse
  pHttpStatus_ =
    ListDistributionConfigurationsResponse'
      { requestId =
          Prelude.Nothing,
        distributionConfigurationSummaryList =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The request ID that uniquely identifies this request.
listDistributionConfigurationsResponse_requestId :: Lens.Lens' ListDistributionConfigurationsResponse (Prelude.Maybe Prelude.Text)
listDistributionConfigurationsResponse_requestId = Lens.lens (\ListDistributionConfigurationsResponse' {requestId} -> requestId) (\s@ListDistributionConfigurationsResponse' {} a -> s {requestId = a} :: ListDistributionConfigurationsResponse)

-- | The list of distributions.
listDistributionConfigurationsResponse_distributionConfigurationSummaryList :: Lens.Lens' ListDistributionConfigurationsResponse (Prelude.Maybe [DistributionConfigurationSummary])
listDistributionConfigurationsResponse_distributionConfigurationSummaryList = Lens.lens (\ListDistributionConfigurationsResponse' {distributionConfigurationSummaryList} -> distributionConfigurationSummaryList) (\s@ListDistributionConfigurationsResponse' {} a -> s {distributionConfigurationSummaryList = a} :: ListDistributionConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
listDistributionConfigurationsResponse_nextToken :: Lens.Lens' ListDistributionConfigurationsResponse (Prelude.Maybe Prelude.Text)
listDistributionConfigurationsResponse_nextToken = Lens.lens (\ListDistributionConfigurationsResponse' {nextToken} -> nextToken) (\s@ListDistributionConfigurationsResponse' {} a -> s {nextToken = a} :: ListDistributionConfigurationsResponse)

-- | The response's http status code.
listDistributionConfigurationsResponse_httpStatus :: Lens.Lens' ListDistributionConfigurationsResponse Prelude.Int
listDistributionConfigurationsResponse_httpStatus = Lens.lens (\ListDistributionConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListDistributionConfigurationsResponse' {} a -> s {httpStatus = a} :: ListDistributionConfigurationsResponse)

instance
  Prelude.NFData
    ListDistributionConfigurationsResponse
