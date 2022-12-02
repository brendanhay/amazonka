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
-- Module      : Amazonka.ImageBuilder.ListDistributionConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of distribution configurations.
module Amazonka.ImageBuilder.ListDistributionConfigurations
  ( -- * Creating a Request
    ListDistributionConfigurations (..),
    newListDistributionConfigurations,

    -- * Request Lenses
    listDistributionConfigurations_nextToken,
    listDistributionConfigurations_filters,
    listDistributionConfigurations_maxResults,

    -- * Destructuring the Response
    ListDistributionConfigurationsResponse (..),
    newListDistributionConfigurationsResponse,

    -- * Response Lenses
    listDistributionConfigurationsResponse_nextToken,
    listDistributionConfigurationsResponse_distributionConfigurationSummaryList,
    listDistributionConfigurationsResponse_requestId,
    listDistributionConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDistributionConfigurations' smart constructor.
data ListDistributionConfigurations = ListDistributionConfigurations'
  { -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | You can filter on @name@ to streamline results.
    filters :: Prelude.Maybe (Prelude.NonEmpty Filter),
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
-- 'nextToken', 'listDistributionConfigurations_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
--
-- 'filters', 'listDistributionConfigurations_filters' - You can filter on @name@ to streamline results.
--
-- 'maxResults', 'listDistributionConfigurations_maxResults' - The maximum items to return in a request.
newListDistributionConfigurations ::
  ListDistributionConfigurations
newListDistributionConfigurations =
  ListDistributionConfigurations'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
listDistributionConfigurations_nextToken :: Lens.Lens' ListDistributionConfigurations (Prelude.Maybe Prelude.Text)
listDistributionConfigurations_nextToken = Lens.lens (\ListDistributionConfigurations' {nextToken} -> nextToken) (\s@ListDistributionConfigurations' {} a -> s {nextToken = a} :: ListDistributionConfigurations)

-- | You can filter on @name@ to streamline results.
listDistributionConfigurations_filters :: Lens.Lens' ListDistributionConfigurations (Prelude.Maybe (Prelude.NonEmpty Filter))
listDistributionConfigurations_filters = Lens.lens (\ListDistributionConfigurations' {filters} -> filters) (\s@ListDistributionConfigurations' {} a -> s {filters = a} :: ListDistributionConfigurations) Prelude.. Lens.mapping Lens.coerced

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDistributionConfigurationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "distributionConfigurationSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListDistributionConfigurations
  where
  hashWithSalt
    _salt
    ListDistributionConfigurations' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults

instance
  Prelude.NFData
    ListDistributionConfigurations
  where
  rnf ListDistributionConfigurations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance
  Data.ToHeaders
    ListDistributionConfigurations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDistributionConfigurations where
  toJSON ListDistributionConfigurations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults
          ]
      )

instance Data.ToPath ListDistributionConfigurations where
  toPath =
    Prelude.const "/ListDistributionConfigurations"

instance Data.ToQuery ListDistributionConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDistributionConfigurationsResponse' smart constructor.
data ListDistributionConfigurationsResponse = ListDistributionConfigurationsResponse'
  { -- | The next token used for paginated responses. When this is not empty,
    -- there are additional elements that the service has not included in this
    -- request. Use this token with the next request to retrieve additional
    -- objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of distributions.
    distributionConfigurationSummaryList :: Prelude.Maybe [DistributionConfigurationSummary],
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
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
-- 'nextToken', 'listDistributionConfigurationsResponse_nextToken' - The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
--
-- 'distributionConfigurationSummaryList', 'listDistributionConfigurationsResponse_distributionConfigurationSummaryList' - The list of distributions.
--
-- 'requestId', 'listDistributionConfigurationsResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'listDistributionConfigurationsResponse_httpStatus' - The response's http status code.
newListDistributionConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDistributionConfigurationsResponse
newListDistributionConfigurationsResponse
  pHttpStatus_ =
    ListDistributionConfigurationsResponse'
      { nextToken =
          Prelude.Nothing,
        distributionConfigurationSummaryList =
          Prelude.Nothing,
        requestId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
listDistributionConfigurationsResponse_nextToken :: Lens.Lens' ListDistributionConfigurationsResponse (Prelude.Maybe Prelude.Text)
listDistributionConfigurationsResponse_nextToken = Lens.lens (\ListDistributionConfigurationsResponse' {nextToken} -> nextToken) (\s@ListDistributionConfigurationsResponse' {} a -> s {nextToken = a} :: ListDistributionConfigurationsResponse)

-- | The list of distributions.
listDistributionConfigurationsResponse_distributionConfigurationSummaryList :: Lens.Lens' ListDistributionConfigurationsResponse (Prelude.Maybe [DistributionConfigurationSummary])
listDistributionConfigurationsResponse_distributionConfigurationSummaryList = Lens.lens (\ListDistributionConfigurationsResponse' {distributionConfigurationSummaryList} -> distributionConfigurationSummaryList) (\s@ListDistributionConfigurationsResponse' {} a -> s {distributionConfigurationSummaryList = a} :: ListDistributionConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The request ID that uniquely identifies this request.
listDistributionConfigurationsResponse_requestId :: Lens.Lens' ListDistributionConfigurationsResponse (Prelude.Maybe Prelude.Text)
listDistributionConfigurationsResponse_requestId = Lens.lens (\ListDistributionConfigurationsResponse' {requestId} -> requestId) (\s@ListDistributionConfigurationsResponse' {} a -> s {requestId = a} :: ListDistributionConfigurationsResponse)

-- | The response's http status code.
listDistributionConfigurationsResponse_httpStatus :: Lens.Lens' ListDistributionConfigurationsResponse Prelude.Int
listDistributionConfigurationsResponse_httpStatus = Lens.lens (\ListDistributionConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListDistributionConfigurationsResponse' {} a -> s {httpStatus = a} :: ListDistributionConfigurationsResponse)

instance
  Prelude.NFData
    ListDistributionConfigurationsResponse
  where
  rnf ListDistributionConfigurationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf distributionConfigurationSummaryList
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
