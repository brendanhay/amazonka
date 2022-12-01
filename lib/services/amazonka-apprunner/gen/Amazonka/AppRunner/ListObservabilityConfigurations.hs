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
-- Module      : Amazonka.AppRunner.ListObservabilityConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of active App Runner observability configurations in your
-- Amazon Web Services account. You can query the revisions for a specific
-- configuration name or the revisions for all active configurations in
-- your account. You can optionally query only the latest revision of each
-- requested name.
--
-- To retrieve a full description of a particular configuration revision,
-- call and provide one of the ARNs returned by
-- @ListObservabilityConfigurations@.
module Amazonka.AppRunner.ListObservabilityConfigurations
  ( -- * Creating a Request
    ListObservabilityConfigurations (..),
    newListObservabilityConfigurations,

    -- * Request Lenses
    listObservabilityConfigurations_nextToken,
    listObservabilityConfigurations_observabilityConfigurationName,
    listObservabilityConfigurations_latestOnly,
    listObservabilityConfigurations_maxResults,

    -- * Destructuring the Response
    ListObservabilityConfigurationsResponse (..),
    newListObservabilityConfigurationsResponse,

    -- * Response Lenses
    listObservabilityConfigurationsResponse_nextToken,
    listObservabilityConfigurationsResponse_httpStatus,
    listObservabilityConfigurationsResponse_observabilityConfigurationSummaryList,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListObservabilityConfigurations' smart constructor.
data ListObservabilityConfigurations = ListObservabilityConfigurations'
  { -- | A token from a previous result page. It\'s used for a paginated request.
    -- The request retrieves the next result page. All other parameter values
    -- must be identical to the ones that are specified in the initial request.
    --
    -- If you don\'t specify @NextToken@, the request retrieves the first
    -- result page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the App Runner observability configuration that you want to
    -- list. If specified, App Runner lists revisions that share this name. If
    -- not specified, App Runner returns revisions of all active
    -- configurations.
    observabilityConfigurationName :: Prelude.Maybe Prelude.Text,
    -- | Set to @true@ to list only the latest revision for each requested
    -- configuration name.
    --
    -- Set to @false@ to list all revisions for each requested configuration
    -- name.
    --
    -- Default: @true@
    latestOnly :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to include in each response (result page).
    -- It\'s used for a paginated request.
    --
    -- If you don\'t specify @MaxResults@, the request retrieves all available
    -- results in a single response.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListObservabilityConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listObservabilityConfigurations_nextToken' - A token from a previous result page. It\'s used for a paginated request.
-- The request retrieves the next result page. All other parameter values
-- must be identical to the ones that are specified in the initial request.
--
-- If you don\'t specify @NextToken@, the request retrieves the first
-- result page.
--
-- 'observabilityConfigurationName', 'listObservabilityConfigurations_observabilityConfigurationName' - The name of the App Runner observability configuration that you want to
-- list. If specified, App Runner lists revisions that share this name. If
-- not specified, App Runner returns revisions of all active
-- configurations.
--
-- 'latestOnly', 'listObservabilityConfigurations_latestOnly' - Set to @true@ to list only the latest revision for each requested
-- configuration name.
--
-- Set to @false@ to list all revisions for each requested configuration
-- name.
--
-- Default: @true@
--
-- 'maxResults', 'listObservabilityConfigurations_maxResults' - The maximum number of results to include in each response (result page).
-- It\'s used for a paginated request.
--
-- If you don\'t specify @MaxResults@, the request retrieves all available
-- results in a single response.
newListObservabilityConfigurations ::
  ListObservabilityConfigurations
newListObservabilityConfigurations =
  ListObservabilityConfigurations'
    { nextToken =
        Prelude.Nothing,
      observabilityConfigurationName =
        Prelude.Nothing,
      latestOnly = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token from a previous result page. It\'s used for a paginated request.
-- The request retrieves the next result page. All other parameter values
-- must be identical to the ones that are specified in the initial request.
--
-- If you don\'t specify @NextToken@, the request retrieves the first
-- result page.
listObservabilityConfigurations_nextToken :: Lens.Lens' ListObservabilityConfigurations (Prelude.Maybe Prelude.Text)
listObservabilityConfigurations_nextToken = Lens.lens (\ListObservabilityConfigurations' {nextToken} -> nextToken) (\s@ListObservabilityConfigurations' {} a -> s {nextToken = a} :: ListObservabilityConfigurations)

-- | The name of the App Runner observability configuration that you want to
-- list. If specified, App Runner lists revisions that share this name. If
-- not specified, App Runner returns revisions of all active
-- configurations.
listObservabilityConfigurations_observabilityConfigurationName :: Lens.Lens' ListObservabilityConfigurations (Prelude.Maybe Prelude.Text)
listObservabilityConfigurations_observabilityConfigurationName = Lens.lens (\ListObservabilityConfigurations' {observabilityConfigurationName} -> observabilityConfigurationName) (\s@ListObservabilityConfigurations' {} a -> s {observabilityConfigurationName = a} :: ListObservabilityConfigurations)

-- | Set to @true@ to list only the latest revision for each requested
-- configuration name.
--
-- Set to @false@ to list all revisions for each requested configuration
-- name.
--
-- Default: @true@
listObservabilityConfigurations_latestOnly :: Lens.Lens' ListObservabilityConfigurations (Prelude.Maybe Prelude.Bool)
listObservabilityConfigurations_latestOnly = Lens.lens (\ListObservabilityConfigurations' {latestOnly} -> latestOnly) (\s@ListObservabilityConfigurations' {} a -> s {latestOnly = a} :: ListObservabilityConfigurations)

-- | The maximum number of results to include in each response (result page).
-- It\'s used for a paginated request.
--
-- If you don\'t specify @MaxResults@, the request retrieves all available
-- results in a single response.
listObservabilityConfigurations_maxResults :: Lens.Lens' ListObservabilityConfigurations (Prelude.Maybe Prelude.Natural)
listObservabilityConfigurations_maxResults = Lens.lens (\ListObservabilityConfigurations' {maxResults} -> maxResults) (\s@ListObservabilityConfigurations' {} a -> s {maxResults = a} :: ListObservabilityConfigurations)

instance
  Core.AWSRequest
    ListObservabilityConfigurations
  where
  type
    AWSResponse ListObservabilityConfigurations =
      ListObservabilityConfigurationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListObservabilityConfigurationsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "ObservabilityConfigurationSummaryList"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListObservabilityConfigurations
  where
  hashWithSalt
    _salt
    ListObservabilityConfigurations' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` observabilityConfigurationName
        `Prelude.hashWithSalt` latestOnly
        `Prelude.hashWithSalt` maxResults

instance
  Prelude.NFData
    ListObservabilityConfigurations
  where
  rnf ListObservabilityConfigurations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf observabilityConfigurationName
      `Prelude.seq` Prelude.rnf latestOnly
      `Prelude.seq` Prelude.rnf maxResults

instance
  Core.ToHeaders
    ListObservabilityConfigurations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AppRunner.ListObservabilityConfigurations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListObservabilityConfigurations where
  toJSON ListObservabilityConfigurations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("ObservabilityConfigurationName" Core..=)
              Prelude.<$> observabilityConfigurationName,
            ("LatestOnly" Core..=) Prelude.<$> latestOnly,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListObservabilityConfigurations where
  toPath = Prelude.const "/"

instance Core.ToQuery ListObservabilityConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListObservabilityConfigurationsResponse' smart constructor.
data ListObservabilityConfigurationsResponse = ListObservabilityConfigurationsResponse'
  { -- | The token that you can pass in a subsequent request to get the next
    -- result page. It\'s returned in a paginated request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of summary information records for observability configurations.
    -- In a paginated request, the request returns up to @MaxResults@ records
    -- for each call.
    observabilityConfigurationSummaryList :: [ObservabilityConfigurationSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListObservabilityConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listObservabilityConfigurationsResponse_nextToken' - The token that you can pass in a subsequent request to get the next
-- result page. It\'s returned in a paginated request.
--
-- 'httpStatus', 'listObservabilityConfigurationsResponse_httpStatus' - The response's http status code.
--
-- 'observabilityConfigurationSummaryList', 'listObservabilityConfigurationsResponse_observabilityConfigurationSummaryList' - A list of summary information records for observability configurations.
-- In a paginated request, the request returns up to @MaxResults@ records
-- for each call.
newListObservabilityConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListObservabilityConfigurationsResponse
newListObservabilityConfigurationsResponse
  pHttpStatus_ =
    ListObservabilityConfigurationsResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        observabilityConfigurationSummaryList =
          Prelude.mempty
      }

-- | The token that you can pass in a subsequent request to get the next
-- result page. It\'s returned in a paginated request.
listObservabilityConfigurationsResponse_nextToken :: Lens.Lens' ListObservabilityConfigurationsResponse (Prelude.Maybe Prelude.Text)
listObservabilityConfigurationsResponse_nextToken = Lens.lens (\ListObservabilityConfigurationsResponse' {nextToken} -> nextToken) (\s@ListObservabilityConfigurationsResponse' {} a -> s {nextToken = a} :: ListObservabilityConfigurationsResponse)

-- | The response's http status code.
listObservabilityConfigurationsResponse_httpStatus :: Lens.Lens' ListObservabilityConfigurationsResponse Prelude.Int
listObservabilityConfigurationsResponse_httpStatus = Lens.lens (\ListObservabilityConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListObservabilityConfigurationsResponse' {} a -> s {httpStatus = a} :: ListObservabilityConfigurationsResponse)

-- | A list of summary information records for observability configurations.
-- In a paginated request, the request returns up to @MaxResults@ records
-- for each call.
listObservabilityConfigurationsResponse_observabilityConfigurationSummaryList :: Lens.Lens' ListObservabilityConfigurationsResponse [ObservabilityConfigurationSummary]
listObservabilityConfigurationsResponse_observabilityConfigurationSummaryList = Lens.lens (\ListObservabilityConfigurationsResponse' {observabilityConfigurationSummaryList} -> observabilityConfigurationSummaryList) (\s@ListObservabilityConfigurationsResponse' {} a -> s {observabilityConfigurationSummaryList = a} :: ListObservabilityConfigurationsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListObservabilityConfigurationsResponse
  where
  rnf ListObservabilityConfigurationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf observabilityConfigurationSummaryList
