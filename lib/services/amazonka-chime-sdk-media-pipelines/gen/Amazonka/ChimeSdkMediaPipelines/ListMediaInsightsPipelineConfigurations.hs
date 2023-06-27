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
-- Module      : Amazonka.ChimeSdkMediaPipelines.ListMediaInsightsPipelineConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the available media insights pipeline configurations.
module Amazonka.ChimeSdkMediaPipelines.ListMediaInsightsPipelineConfigurations
  ( -- * Creating a Request
    ListMediaInsightsPipelineConfigurations (..),
    newListMediaInsightsPipelineConfigurations,

    -- * Request Lenses
    listMediaInsightsPipelineConfigurations_maxResults,
    listMediaInsightsPipelineConfigurations_nextToken,

    -- * Destructuring the Response
    ListMediaInsightsPipelineConfigurationsResponse (..),
    newListMediaInsightsPipelineConfigurationsResponse,

    -- * Response Lenses
    listMediaInsightsPipelineConfigurationsResponse_mediaInsightsPipelineConfigurations,
    listMediaInsightsPipelineConfigurationsResponse_nextToken,
    listMediaInsightsPipelineConfigurationsResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkMediaPipelines.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListMediaInsightsPipelineConfigurations' smart constructor.
data ListMediaInsightsPipelineConfigurations = ListMediaInsightsPipelineConfigurations'
  { -- | The maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token used to return the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMediaInsightsPipelineConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listMediaInsightsPipelineConfigurations_maxResults' - The maximum number of results to return in a single call.
--
-- 'nextToken', 'listMediaInsightsPipelineConfigurations_nextToken' - The token used to return the next page of results.
newListMediaInsightsPipelineConfigurations ::
  ListMediaInsightsPipelineConfigurations
newListMediaInsightsPipelineConfigurations =
  ListMediaInsightsPipelineConfigurations'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return in a single call.
listMediaInsightsPipelineConfigurations_maxResults :: Lens.Lens' ListMediaInsightsPipelineConfigurations (Prelude.Maybe Prelude.Natural)
listMediaInsightsPipelineConfigurations_maxResults = Lens.lens (\ListMediaInsightsPipelineConfigurations' {maxResults} -> maxResults) (\s@ListMediaInsightsPipelineConfigurations' {} a -> s {maxResults = a} :: ListMediaInsightsPipelineConfigurations)

-- | The token used to return the next page of results.
listMediaInsightsPipelineConfigurations_nextToken :: Lens.Lens' ListMediaInsightsPipelineConfigurations (Prelude.Maybe Prelude.Text)
listMediaInsightsPipelineConfigurations_nextToken = Lens.lens (\ListMediaInsightsPipelineConfigurations' {nextToken} -> nextToken) (\s@ListMediaInsightsPipelineConfigurations' {} a -> s {nextToken = a} :: ListMediaInsightsPipelineConfigurations)

instance
  Core.AWSRequest
    ListMediaInsightsPipelineConfigurations
  where
  type
    AWSResponse
      ListMediaInsightsPipelineConfigurations =
      ListMediaInsightsPipelineConfigurationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListMediaInsightsPipelineConfigurationsResponse'
            Prelude.<$> ( x
                            Data..?> "MediaInsightsPipelineConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListMediaInsightsPipelineConfigurations
  where
  hashWithSalt
    _salt
    ListMediaInsightsPipelineConfigurations' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    ListMediaInsightsPipelineConfigurations
  where
  rnf ListMediaInsightsPipelineConfigurations' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    ListMediaInsightsPipelineConfigurations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ListMediaInsightsPipelineConfigurations
  where
  toPath =
    Prelude.const
      "/media-insights-pipeline-configurations"

instance
  Data.ToQuery
    ListMediaInsightsPipelineConfigurations
  where
  toQuery ListMediaInsightsPipelineConfigurations' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListMediaInsightsPipelineConfigurationsResponse' smart constructor.
data ListMediaInsightsPipelineConfigurationsResponse = ListMediaInsightsPipelineConfigurationsResponse'
  { -- | The requested list of media insights pipeline configurations.
    mediaInsightsPipelineConfigurations :: Prelude.Maybe [MediaInsightsPipelineConfigurationSummary],
    -- | The token used to return the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListMediaInsightsPipelineConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaInsightsPipelineConfigurations', 'listMediaInsightsPipelineConfigurationsResponse_mediaInsightsPipelineConfigurations' - The requested list of media insights pipeline configurations.
--
-- 'nextToken', 'listMediaInsightsPipelineConfigurationsResponse_nextToken' - The token used to return the next page of results.
--
-- 'httpStatus', 'listMediaInsightsPipelineConfigurationsResponse_httpStatus' - The response's http status code.
newListMediaInsightsPipelineConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListMediaInsightsPipelineConfigurationsResponse
newListMediaInsightsPipelineConfigurationsResponse
  pHttpStatus_ =
    ListMediaInsightsPipelineConfigurationsResponse'
      { mediaInsightsPipelineConfigurations =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The requested list of media insights pipeline configurations.
listMediaInsightsPipelineConfigurationsResponse_mediaInsightsPipelineConfigurations :: Lens.Lens' ListMediaInsightsPipelineConfigurationsResponse (Prelude.Maybe [MediaInsightsPipelineConfigurationSummary])
listMediaInsightsPipelineConfigurationsResponse_mediaInsightsPipelineConfigurations = Lens.lens (\ListMediaInsightsPipelineConfigurationsResponse' {mediaInsightsPipelineConfigurations} -> mediaInsightsPipelineConfigurations) (\s@ListMediaInsightsPipelineConfigurationsResponse' {} a -> s {mediaInsightsPipelineConfigurations = a} :: ListMediaInsightsPipelineConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token used to return the next page of results.
listMediaInsightsPipelineConfigurationsResponse_nextToken :: Lens.Lens' ListMediaInsightsPipelineConfigurationsResponse (Prelude.Maybe Prelude.Text)
listMediaInsightsPipelineConfigurationsResponse_nextToken = Lens.lens (\ListMediaInsightsPipelineConfigurationsResponse' {nextToken} -> nextToken) (\s@ListMediaInsightsPipelineConfigurationsResponse' {} a -> s {nextToken = a} :: ListMediaInsightsPipelineConfigurationsResponse)

-- | The response's http status code.
listMediaInsightsPipelineConfigurationsResponse_httpStatus :: Lens.Lens' ListMediaInsightsPipelineConfigurationsResponse Prelude.Int
listMediaInsightsPipelineConfigurationsResponse_httpStatus = Lens.lens (\ListMediaInsightsPipelineConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListMediaInsightsPipelineConfigurationsResponse' {} a -> s {httpStatus = a} :: ListMediaInsightsPipelineConfigurationsResponse)

instance
  Prelude.NFData
    ListMediaInsightsPipelineConfigurationsResponse
  where
  rnf
    ListMediaInsightsPipelineConfigurationsResponse' {..} =
      Prelude.rnf mediaInsightsPipelineConfigurations
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
