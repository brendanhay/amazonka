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
-- Module      : Amazonka.SupportApp.ListSlackWorkspaceConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Slack workspace configurations for an Amazon Web Services
-- account.
module Amazonka.SupportApp.ListSlackWorkspaceConfigurations
  ( -- * Creating a Request
    ListSlackWorkspaceConfigurations (..),
    newListSlackWorkspaceConfigurations,

    -- * Request Lenses
    listSlackWorkspaceConfigurations_nextToken,

    -- * Destructuring the Response
    ListSlackWorkspaceConfigurationsResponse (..),
    newListSlackWorkspaceConfigurationsResponse,

    -- * Response Lenses
    listSlackWorkspaceConfigurationsResponse_nextToken,
    listSlackWorkspaceConfigurationsResponse_slackWorkspaceConfigurations,
    listSlackWorkspaceConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SupportApp.Types

-- | /See:/ 'newListSlackWorkspaceConfigurations' smart constructor.
data ListSlackWorkspaceConfigurations = ListSlackWorkspaceConfigurations'
  { -- | If the results of a search are large, the API only returns a portion of
    -- the results and includes a @nextToken@ pagination token in the response.
    -- To retrieve the next batch of results, reissue the search request and
    -- include the returned token. When the API returns the last set of
    -- results, the response doesn\'t include a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSlackWorkspaceConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSlackWorkspaceConfigurations_nextToken' - If the results of a search are large, the API only returns a portion of
-- the results and includes a @nextToken@ pagination token in the response.
-- To retrieve the next batch of results, reissue the search request and
-- include the returned token. When the API returns the last set of
-- results, the response doesn\'t include a pagination token value.
newListSlackWorkspaceConfigurations ::
  ListSlackWorkspaceConfigurations
newListSlackWorkspaceConfigurations =
  ListSlackWorkspaceConfigurations'
    { nextToken =
        Prelude.Nothing
    }

-- | If the results of a search are large, the API only returns a portion of
-- the results and includes a @nextToken@ pagination token in the response.
-- To retrieve the next batch of results, reissue the search request and
-- include the returned token. When the API returns the last set of
-- results, the response doesn\'t include a pagination token value.
listSlackWorkspaceConfigurations_nextToken :: Lens.Lens' ListSlackWorkspaceConfigurations (Prelude.Maybe Prelude.Text)
listSlackWorkspaceConfigurations_nextToken = Lens.lens (\ListSlackWorkspaceConfigurations' {nextToken} -> nextToken) (\s@ListSlackWorkspaceConfigurations' {} a -> s {nextToken = a} :: ListSlackWorkspaceConfigurations)

instance
  Core.AWSRequest
    ListSlackWorkspaceConfigurations
  where
  type
    AWSResponse ListSlackWorkspaceConfigurations =
      ListSlackWorkspaceConfigurationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSlackWorkspaceConfigurationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x
                            Data..?> "slackWorkspaceConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListSlackWorkspaceConfigurations
  where
  hashWithSalt
    _salt
    ListSlackWorkspaceConfigurations' {..} =
      _salt `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    ListSlackWorkspaceConfigurations
  where
  rnf ListSlackWorkspaceConfigurations' {..} =
    Prelude.rnf nextToken

instance
  Data.ToHeaders
    ListSlackWorkspaceConfigurations
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

instance Data.ToJSON ListSlackWorkspaceConfigurations where
  toJSON ListSlackWorkspaceConfigurations' {..} =
    Data.object
      ( Prelude.catMaybes
          [("nextToken" Data..=) Prelude.<$> nextToken]
      )

instance Data.ToPath ListSlackWorkspaceConfigurations where
  toPath =
    Prelude.const
      "/control/list-slack-workspace-configurations"

instance
  Data.ToQuery
    ListSlackWorkspaceConfigurations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSlackWorkspaceConfigurationsResponse' smart constructor.
data ListSlackWorkspaceConfigurationsResponse = ListSlackWorkspaceConfigurationsResponse'
  { -- | The point where pagination should resume when the response returns only
    -- partial results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The configurations for a Slack workspace.
    slackWorkspaceConfigurations :: Prelude.Maybe [SlackWorkspaceConfiguration],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSlackWorkspaceConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSlackWorkspaceConfigurationsResponse_nextToken' - The point where pagination should resume when the response returns only
-- partial results.
--
-- 'slackWorkspaceConfigurations', 'listSlackWorkspaceConfigurationsResponse_slackWorkspaceConfigurations' - The configurations for a Slack workspace.
--
-- 'httpStatus', 'listSlackWorkspaceConfigurationsResponse_httpStatus' - The response's http status code.
newListSlackWorkspaceConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSlackWorkspaceConfigurationsResponse
newListSlackWorkspaceConfigurationsResponse
  pHttpStatus_ =
    ListSlackWorkspaceConfigurationsResponse'
      { nextToken =
          Prelude.Nothing,
        slackWorkspaceConfigurations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The point where pagination should resume when the response returns only
-- partial results.
listSlackWorkspaceConfigurationsResponse_nextToken :: Lens.Lens' ListSlackWorkspaceConfigurationsResponse (Prelude.Maybe Prelude.Text)
listSlackWorkspaceConfigurationsResponse_nextToken = Lens.lens (\ListSlackWorkspaceConfigurationsResponse' {nextToken} -> nextToken) (\s@ListSlackWorkspaceConfigurationsResponse' {} a -> s {nextToken = a} :: ListSlackWorkspaceConfigurationsResponse)

-- | The configurations for a Slack workspace.
listSlackWorkspaceConfigurationsResponse_slackWorkspaceConfigurations :: Lens.Lens' ListSlackWorkspaceConfigurationsResponse (Prelude.Maybe [SlackWorkspaceConfiguration])
listSlackWorkspaceConfigurationsResponse_slackWorkspaceConfigurations = Lens.lens (\ListSlackWorkspaceConfigurationsResponse' {slackWorkspaceConfigurations} -> slackWorkspaceConfigurations) (\s@ListSlackWorkspaceConfigurationsResponse' {} a -> s {slackWorkspaceConfigurations = a} :: ListSlackWorkspaceConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSlackWorkspaceConfigurationsResponse_httpStatus :: Lens.Lens' ListSlackWorkspaceConfigurationsResponse Prelude.Int
listSlackWorkspaceConfigurationsResponse_httpStatus = Lens.lens (\ListSlackWorkspaceConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListSlackWorkspaceConfigurationsResponse' {} a -> s {httpStatus = a} :: ListSlackWorkspaceConfigurationsResponse)

instance
  Prelude.NFData
    ListSlackWorkspaceConfigurationsResponse
  where
  rnf ListSlackWorkspaceConfigurationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf slackWorkspaceConfigurations
      `Prelude.seq` Prelude.rnf httpStatus
