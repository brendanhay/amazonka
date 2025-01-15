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
-- Module      : Amazonka.ImageBuilder.ListInfrastructureConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of infrastructure configurations.
module Amazonka.ImageBuilder.ListInfrastructureConfigurations
  ( -- * Creating a Request
    ListInfrastructureConfigurations (..),
    newListInfrastructureConfigurations,

    -- * Request Lenses
    listInfrastructureConfigurations_filters,
    listInfrastructureConfigurations_maxResults,
    listInfrastructureConfigurations_nextToken,

    -- * Destructuring the Response
    ListInfrastructureConfigurationsResponse (..),
    newListInfrastructureConfigurationsResponse,

    -- * Response Lenses
    listInfrastructureConfigurationsResponse_infrastructureConfigurationSummaryList,
    listInfrastructureConfigurationsResponse_nextToken,
    listInfrastructureConfigurationsResponse_requestId,
    listInfrastructureConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListInfrastructureConfigurations' smart constructor.
data ListInfrastructureConfigurations = ListInfrastructureConfigurations'
  { -- | You can filter on @name@ to streamline results.
    filters :: Prelude.Maybe (Prelude.NonEmpty Filter),
    -- | The maximum items to return in a request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInfrastructureConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listInfrastructureConfigurations_filters' - You can filter on @name@ to streamline results.
--
-- 'maxResults', 'listInfrastructureConfigurations_maxResults' - The maximum items to return in a request.
--
-- 'nextToken', 'listInfrastructureConfigurations_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
newListInfrastructureConfigurations ::
  ListInfrastructureConfigurations
newListInfrastructureConfigurations =
  ListInfrastructureConfigurations'
    { filters =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | You can filter on @name@ to streamline results.
listInfrastructureConfigurations_filters :: Lens.Lens' ListInfrastructureConfigurations (Prelude.Maybe (Prelude.NonEmpty Filter))
listInfrastructureConfigurations_filters = Lens.lens (\ListInfrastructureConfigurations' {filters} -> filters) (\s@ListInfrastructureConfigurations' {} a -> s {filters = a} :: ListInfrastructureConfigurations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum items to return in a request.
listInfrastructureConfigurations_maxResults :: Lens.Lens' ListInfrastructureConfigurations (Prelude.Maybe Prelude.Natural)
listInfrastructureConfigurations_maxResults = Lens.lens (\ListInfrastructureConfigurations' {maxResults} -> maxResults) (\s@ListInfrastructureConfigurations' {} a -> s {maxResults = a} :: ListInfrastructureConfigurations)

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
listInfrastructureConfigurations_nextToken :: Lens.Lens' ListInfrastructureConfigurations (Prelude.Maybe Prelude.Text)
listInfrastructureConfigurations_nextToken = Lens.lens (\ListInfrastructureConfigurations' {nextToken} -> nextToken) (\s@ListInfrastructureConfigurations' {} a -> s {nextToken = a} :: ListInfrastructureConfigurations)

instance
  Core.AWSRequest
    ListInfrastructureConfigurations
  where
  type
    AWSResponse ListInfrastructureConfigurations =
      ListInfrastructureConfigurationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInfrastructureConfigurationsResponse'
            Prelude.<$> ( x
                            Data..?> "infrastructureConfigurationSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListInfrastructureConfigurations
  where
  hashWithSalt
    _salt
    ListInfrastructureConfigurations' {..} =
      _salt
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    ListInfrastructureConfigurations
  where
  rnf ListInfrastructureConfigurations' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken

instance
  Data.ToHeaders
    ListInfrastructureConfigurations
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

instance Data.ToJSON ListInfrastructureConfigurations where
  toJSON ListInfrastructureConfigurations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filters" Data..=) Prelude.<$> filters,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListInfrastructureConfigurations where
  toPath =
    Prelude.const "/ListInfrastructureConfigurations"

instance
  Data.ToQuery
    ListInfrastructureConfigurations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListInfrastructureConfigurationsResponse' smart constructor.
data ListInfrastructureConfigurationsResponse = ListInfrastructureConfigurationsResponse'
  { -- | The list of infrastructure configurations.
    infrastructureConfigurationSummaryList :: Prelude.Maybe [InfrastructureConfigurationSummary],
    -- | The next token used for paginated responses. When this is not empty,
    -- there are additional elements that the service has not included in this
    -- request. Use this token with the next request to retrieve additional
    -- objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInfrastructureConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'infrastructureConfigurationSummaryList', 'listInfrastructureConfigurationsResponse_infrastructureConfigurationSummaryList' - The list of infrastructure configurations.
--
-- 'nextToken', 'listInfrastructureConfigurationsResponse_nextToken' - The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
--
-- 'requestId', 'listInfrastructureConfigurationsResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'listInfrastructureConfigurationsResponse_httpStatus' - The response's http status code.
newListInfrastructureConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInfrastructureConfigurationsResponse
newListInfrastructureConfigurationsResponse
  pHttpStatus_ =
    ListInfrastructureConfigurationsResponse'
      { infrastructureConfigurationSummaryList =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        requestId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The list of infrastructure configurations.
listInfrastructureConfigurationsResponse_infrastructureConfigurationSummaryList :: Lens.Lens' ListInfrastructureConfigurationsResponse (Prelude.Maybe [InfrastructureConfigurationSummary])
listInfrastructureConfigurationsResponse_infrastructureConfigurationSummaryList = Lens.lens (\ListInfrastructureConfigurationsResponse' {infrastructureConfigurationSummaryList} -> infrastructureConfigurationSummaryList) (\s@ListInfrastructureConfigurationsResponse' {} a -> s {infrastructureConfigurationSummaryList = a} :: ListInfrastructureConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next token used for paginated responses. When this is not empty,
-- there are additional elements that the service has not included in this
-- request. Use this token with the next request to retrieve additional
-- objects.
listInfrastructureConfigurationsResponse_nextToken :: Lens.Lens' ListInfrastructureConfigurationsResponse (Prelude.Maybe Prelude.Text)
listInfrastructureConfigurationsResponse_nextToken = Lens.lens (\ListInfrastructureConfigurationsResponse' {nextToken} -> nextToken) (\s@ListInfrastructureConfigurationsResponse' {} a -> s {nextToken = a} :: ListInfrastructureConfigurationsResponse)

-- | The request ID that uniquely identifies this request.
listInfrastructureConfigurationsResponse_requestId :: Lens.Lens' ListInfrastructureConfigurationsResponse (Prelude.Maybe Prelude.Text)
listInfrastructureConfigurationsResponse_requestId = Lens.lens (\ListInfrastructureConfigurationsResponse' {requestId} -> requestId) (\s@ListInfrastructureConfigurationsResponse' {} a -> s {requestId = a} :: ListInfrastructureConfigurationsResponse)

-- | The response's http status code.
listInfrastructureConfigurationsResponse_httpStatus :: Lens.Lens' ListInfrastructureConfigurationsResponse Prelude.Int
listInfrastructureConfigurationsResponse_httpStatus = Lens.lens (\ListInfrastructureConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListInfrastructureConfigurationsResponse' {} a -> s {httpStatus = a} :: ListInfrastructureConfigurationsResponse)

instance
  Prelude.NFData
    ListInfrastructureConfigurationsResponse
  where
  rnf ListInfrastructureConfigurationsResponse' {..} =
    Prelude.rnf infrastructureConfigurationSummaryList `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf requestId `Prelude.seq`
          Prelude.rnf httpStatus
