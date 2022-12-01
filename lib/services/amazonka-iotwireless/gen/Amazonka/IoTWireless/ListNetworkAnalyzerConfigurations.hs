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
-- Module      : Amazonka.IoTWireless.ListNetworkAnalyzerConfigurations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the network analyzer configurations.
module Amazonka.IoTWireless.ListNetworkAnalyzerConfigurations
  ( -- * Creating a Request
    ListNetworkAnalyzerConfigurations (..),
    newListNetworkAnalyzerConfigurations,

    -- * Request Lenses
    listNetworkAnalyzerConfigurations_nextToken,
    listNetworkAnalyzerConfigurations_maxResults,

    -- * Destructuring the Response
    ListNetworkAnalyzerConfigurationsResponse (..),
    newListNetworkAnalyzerConfigurationsResponse,

    -- * Response Lenses
    listNetworkAnalyzerConfigurationsResponse_nextToken,
    listNetworkAnalyzerConfigurationsResponse_networkAnalyzerConfigurationList,
    listNetworkAnalyzerConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListNetworkAnalyzerConfigurations' smart constructor.
data ListNetworkAnalyzerConfigurations = ListNetworkAnalyzerConfigurations'
  { -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNetworkAnalyzerConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNetworkAnalyzerConfigurations_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'maxResults', 'listNetworkAnalyzerConfigurations_maxResults' - Undocumented member.
newListNetworkAnalyzerConfigurations ::
  ListNetworkAnalyzerConfigurations
newListNetworkAnalyzerConfigurations =
  ListNetworkAnalyzerConfigurations'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listNetworkAnalyzerConfigurations_nextToken :: Lens.Lens' ListNetworkAnalyzerConfigurations (Prelude.Maybe Prelude.Text)
listNetworkAnalyzerConfigurations_nextToken = Lens.lens (\ListNetworkAnalyzerConfigurations' {nextToken} -> nextToken) (\s@ListNetworkAnalyzerConfigurations' {} a -> s {nextToken = a} :: ListNetworkAnalyzerConfigurations)

-- | Undocumented member.
listNetworkAnalyzerConfigurations_maxResults :: Lens.Lens' ListNetworkAnalyzerConfigurations (Prelude.Maybe Prelude.Natural)
listNetworkAnalyzerConfigurations_maxResults = Lens.lens (\ListNetworkAnalyzerConfigurations' {maxResults} -> maxResults) (\s@ListNetworkAnalyzerConfigurations' {} a -> s {maxResults = a} :: ListNetworkAnalyzerConfigurations)

instance
  Core.AWSRequest
    ListNetworkAnalyzerConfigurations
  where
  type
    AWSResponse ListNetworkAnalyzerConfigurations =
      ListNetworkAnalyzerConfigurationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNetworkAnalyzerConfigurationsResponse'
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> ( x Core..?> "NetworkAnalyzerConfigurationList"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListNetworkAnalyzerConfigurations
  where
  hashWithSalt
    _salt
    ListNetworkAnalyzerConfigurations' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults

instance
  Prelude.NFData
    ListNetworkAnalyzerConfigurations
  where
  rnf ListNetworkAnalyzerConfigurations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance
  Core.ToHeaders
    ListNetworkAnalyzerConfigurations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    ListNetworkAnalyzerConfigurations
  where
  toPath =
    Prelude.const "/network-analyzer-configurations"

instance
  Core.ToQuery
    ListNetworkAnalyzerConfigurations
  where
  toQuery ListNetworkAnalyzerConfigurations' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListNetworkAnalyzerConfigurationsResponse' smart constructor.
data ListNetworkAnalyzerConfigurationsResponse = ListNetworkAnalyzerConfigurationsResponse'
  { -- | The token to use to get the next set of results, or __null__ if there
    -- are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of network analyzer configurations.
    networkAnalyzerConfigurationList :: Prelude.Maybe [NetworkAnalyzerConfigurations],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNetworkAnalyzerConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNetworkAnalyzerConfigurationsResponse_nextToken' - The token to use to get the next set of results, or __null__ if there
-- are no additional results.
--
-- 'networkAnalyzerConfigurationList', 'listNetworkAnalyzerConfigurationsResponse_networkAnalyzerConfigurationList' - The list of network analyzer configurations.
--
-- 'httpStatus', 'listNetworkAnalyzerConfigurationsResponse_httpStatus' - The response's http status code.
newListNetworkAnalyzerConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListNetworkAnalyzerConfigurationsResponse
newListNetworkAnalyzerConfigurationsResponse
  pHttpStatus_ =
    ListNetworkAnalyzerConfigurationsResponse'
      { nextToken =
          Prelude.Nothing,
        networkAnalyzerConfigurationList =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to get the next set of results, or __null__ if there
-- are no additional results.
listNetworkAnalyzerConfigurationsResponse_nextToken :: Lens.Lens' ListNetworkAnalyzerConfigurationsResponse (Prelude.Maybe Prelude.Text)
listNetworkAnalyzerConfigurationsResponse_nextToken = Lens.lens (\ListNetworkAnalyzerConfigurationsResponse' {nextToken} -> nextToken) (\s@ListNetworkAnalyzerConfigurationsResponse' {} a -> s {nextToken = a} :: ListNetworkAnalyzerConfigurationsResponse)

-- | The list of network analyzer configurations.
listNetworkAnalyzerConfigurationsResponse_networkAnalyzerConfigurationList :: Lens.Lens' ListNetworkAnalyzerConfigurationsResponse (Prelude.Maybe [NetworkAnalyzerConfigurations])
listNetworkAnalyzerConfigurationsResponse_networkAnalyzerConfigurationList = Lens.lens (\ListNetworkAnalyzerConfigurationsResponse' {networkAnalyzerConfigurationList} -> networkAnalyzerConfigurationList) (\s@ListNetworkAnalyzerConfigurationsResponse' {} a -> s {networkAnalyzerConfigurationList = a} :: ListNetworkAnalyzerConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listNetworkAnalyzerConfigurationsResponse_httpStatus :: Lens.Lens' ListNetworkAnalyzerConfigurationsResponse Prelude.Int
listNetworkAnalyzerConfigurationsResponse_httpStatus = Lens.lens (\ListNetworkAnalyzerConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListNetworkAnalyzerConfigurationsResponse' {} a -> s {httpStatus = a} :: ListNetworkAnalyzerConfigurationsResponse)

instance
  Prelude.NFData
    ListNetworkAnalyzerConfigurationsResponse
  where
  rnf ListNetworkAnalyzerConfigurationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf networkAnalyzerConfigurationList
      `Prelude.seq` Prelude.rnf httpStatus
