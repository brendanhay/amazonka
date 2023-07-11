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
-- Module      : Amazonka.SupportApp.ListSlackChannelConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Slack channel configurations for an Amazon Web Services
-- account.
module Amazonka.SupportApp.ListSlackChannelConfigurations
  ( -- * Creating a Request
    ListSlackChannelConfigurations (..),
    newListSlackChannelConfigurations,

    -- * Request Lenses
    listSlackChannelConfigurations_nextToken,

    -- * Destructuring the Response
    ListSlackChannelConfigurationsResponse (..),
    newListSlackChannelConfigurationsResponse,

    -- * Response Lenses
    listSlackChannelConfigurationsResponse_nextToken,
    listSlackChannelConfigurationsResponse_httpStatus,
    listSlackChannelConfigurationsResponse_slackChannelConfigurations,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SupportApp.Types

-- | /See:/ 'newListSlackChannelConfigurations' smart constructor.
data ListSlackChannelConfigurations = ListSlackChannelConfigurations'
  { -- | If the results of a search are large, the API only returns a portion of
    -- the results and includes a @nextToken@ pagination token in the response.
    -- To retrieve the next batch of results, reissue the search request and
    -- include the returned token. When the API returns the last set of
    -- results, the response doesn\'t include a pagination token value.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSlackChannelConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSlackChannelConfigurations_nextToken' - If the results of a search are large, the API only returns a portion of
-- the results and includes a @nextToken@ pagination token in the response.
-- To retrieve the next batch of results, reissue the search request and
-- include the returned token. When the API returns the last set of
-- results, the response doesn\'t include a pagination token value.
newListSlackChannelConfigurations ::
  ListSlackChannelConfigurations
newListSlackChannelConfigurations =
  ListSlackChannelConfigurations'
    { nextToken =
        Prelude.Nothing
    }

-- | If the results of a search are large, the API only returns a portion of
-- the results and includes a @nextToken@ pagination token in the response.
-- To retrieve the next batch of results, reissue the search request and
-- include the returned token. When the API returns the last set of
-- results, the response doesn\'t include a pagination token value.
listSlackChannelConfigurations_nextToken :: Lens.Lens' ListSlackChannelConfigurations (Prelude.Maybe Prelude.Text)
listSlackChannelConfigurations_nextToken = Lens.lens (\ListSlackChannelConfigurations' {nextToken} -> nextToken) (\s@ListSlackChannelConfigurations' {} a -> s {nextToken = a} :: ListSlackChannelConfigurations)

instance
  Core.AWSRequest
    ListSlackChannelConfigurations
  where
  type
    AWSResponse ListSlackChannelConfigurations =
      ListSlackChannelConfigurationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSlackChannelConfigurationsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "slackChannelConfigurations"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListSlackChannelConfigurations
  where
  hashWithSalt
    _salt
    ListSlackChannelConfigurations' {..} =
      _salt `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    ListSlackChannelConfigurations
  where
  rnf ListSlackChannelConfigurations' {..} =
    Prelude.rnf nextToken

instance
  Data.ToHeaders
    ListSlackChannelConfigurations
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

instance Data.ToJSON ListSlackChannelConfigurations where
  toJSON ListSlackChannelConfigurations' {..} =
    Data.object
      ( Prelude.catMaybes
          [("nextToken" Data..=) Prelude.<$> nextToken]
      )

instance Data.ToPath ListSlackChannelConfigurations where
  toPath =
    Prelude.const
      "/control/list-slack-channel-configurations"

instance Data.ToQuery ListSlackChannelConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSlackChannelConfigurationsResponse' smart constructor.
data ListSlackChannelConfigurationsResponse = ListSlackChannelConfigurationsResponse'
  { -- | The point where pagination should resume when the response returns only
    -- partial results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The configurations for a Slack channel.
    slackChannelConfigurations :: [SlackChannelConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSlackChannelConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSlackChannelConfigurationsResponse_nextToken' - The point where pagination should resume when the response returns only
-- partial results.
--
-- 'httpStatus', 'listSlackChannelConfigurationsResponse_httpStatus' - The response's http status code.
--
-- 'slackChannelConfigurations', 'listSlackChannelConfigurationsResponse_slackChannelConfigurations' - The configurations for a Slack channel.
newListSlackChannelConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSlackChannelConfigurationsResponse
newListSlackChannelConfigurationsResponse
  pHttpStatus_ =
    ListSlackChannelConfigurationsResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        slackChannelConfigurations =
          Prelude.mempty
      }

-- | The point where pagination should resume when the response returns only
-- partial results.
listSlackChannelConfigurationsResponse_nextToken :: Lens.Lens' ListSlackChannelConfigurationsResponse (Prelude.Maybe Prelude.Text)
listSlackChannelConfigurationsResponse_nextToken = Lens.lens (\ListSlackChannelConfigurationsResponse' {nextToken} -> nextToken) (\s@ListSlackChannelConfigurationsResponse' {} a -> s {nextToken = a} :: ListSlackChannelConfigurationsResponse)

-- | The response's http status code.
listSlackChannelConfigurationsResponse_httpStatus :: Lens.Lens' ListSlackChannelConfigurationsResponse Prelude.Int
listSlackChannelConfigurationsResponse_httpStatus = Lens.lens (\ListSlackChannelConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListSlackChannelConfigurationsResponse' {} a -> s {httpStatus = a} :: ListSlackChannelConfigurationsResponse)

-- | The configurations for a Slack channel.
listSlackChannelConfigurationsResponse_slackChannelConfigurations :: Lens.Lens' ListSlackChannelConfigurationsResponse [SlackChannelConfiguration]
listSlackChannelConfigurationsResponse_slackChannelConfigurations = Lens.lens (\ListSlackChannelConfigurationsResponse' {slackChannelConfigurations} -> slackChannelConfigurations) (\s@ListSlackChannelConfigurationsResponse' {} a -> s {slackChannelConfigurations = a} :: ListSlackChannelConfigurationsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListSlackChannelConfigurationsResponse
  where
  rnf ListSlackChannelConfigurationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf slackChannelConfigurations
