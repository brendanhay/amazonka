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
-- Module      : Amazonka.ChimeSDKMessaging.ListChannelFlows
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated lists of all the channel flows created under a
-- single Chime. This is a developer API.
module Amazonka.ChimeSDKMessaging.ListChannelFlows
  ( -- * Creating a Request
    ListChannelFlows (..),
    newListChannelFlows,

    -- * Request Lenses
    listChannelFlows_maxResults,
    listChannelFlows_nextToken,
    listChannelFlows_appInstanceArn,

    -- * Destructuring the Response
    ListChannelFlowsResponse (..),
    newListChannelFlowsResponse,

    -- * Response Lenses
    listChannelFlowsResponse_channelFlows,
    listChannelFlowsResponse_nextToken,
    listChannelFlowsResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListChannelFlows' smart constructor.
data ListChannelFlows = ListChannelFlows'
  { -- | The maximum number of channel flows that you want to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token passed by previous API calls until all requested channel flows
    -- are returned.
    nextToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ARN of the app instance.
    appInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChannelFlows' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listChannelFlows_maxResults' - The maximum number of channel flows that you want to return.
--
-- 'nextToken', 'listChannelFlows_nextToken' - The token passed by previous API calls until all requested channel flows
-- are returned.
--
-- 'appInstanceArn', 'listChannelFlows_appInstanceArn' - The ARN of the app instance.
newListChannelFlows ::
  -- | 'appInstanceArn'
  Prelude.Text ->
  ListChannelFlows
newListChannelFlows pAppInstanceArn_ =
  ListChannelFlows'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      appInstanceArn = pAppInstanceArn_
    }

-- | The maximum number of channel flows that you want to return.
listChannelFlows_maxResults :: Lens.Lens' ListChannelFlows (Prelude.Maybe Prelude.Natural)
listChannelFlows_maxResults = Lens.lens (\ListChannelFlows' {maxResults} -> maxResults) (\s@ListChannelFlows' {} a -> s {maxResults = a} :: ListChannelFlows)

-- | The token passed by previous API calls until all requested channel flows
-- are returned.
listChannelFlows_nextToken :: Lens.Lens' ListChannelFlows (Prelude.Maybe Prelude.Text)
listChannelFlows_nextToken = Lens.lens (\ListChannelFlows' {nextToken} -> nextToken) (\s@ListChannelFlows' {} a -> s {nextToken = a} :: ListChannelFlows) Prelude.. Lens.mapping Data._Sensitive

-- | The ARN of the app instance.
listChannelFlows_appInstanceArn :: Lens.Lens' ListChannelFlows Prelude.Text
listChannelFlows_appInstanceArn = Lens.lens (\ListChannelFlows' {appInstanceArn} -> appInstanceArn) (\s@ListChannelFlows' {} a -> s {appInstanceArn = a} :: ListChannelFlows)

instance Core.AWSRequest ListChannelFlows where
  type
    AWSResponse ListChannelFlows =
      ListChannelFlowsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListChannelFlowsResponse'
            Prelude.<$> (x Data..?> "ChannelFlows" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListChannelFlows where
  hashWithSalt _salt ListChannelFlows' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` appInstanceArn

instance Prelude.NFData ListChannelFlows where
  rnf ListChannelFlows' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf appInstanceArn

instance Data.ToHeaders ListChannelFlows where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListChannelFlows where
  toPath = Prelude.const "/channel-flows"

instance Data.ToQuery ListChannelFlows where
  toQuery ListChannelFlows' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken,
        "app-instance-arn" Data.=: appInstanceArn
      ]

-- | /See:/ 'newListChannelFlowsResponse' smart constructor.
data ListChannelFlowsResponse = ListChannelFlowsResponse'
  { -- | The information about each channel flow.
    channelFlows :: Prelude.Maybe [ChannelFlowSummary],
    -- | The token passed by previous API calls until all requested channels are
    -- returned.
    nextToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListChannelFlowsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'channelFlows', 'listChannelFlowsResponse_channelFlows' - The information about each channel flow.
--
-- 'nextToken', 'listChannelFlowsResponse_nextToken' - The token passed by previous API calls until all requested channels are
-- returned.
--
-- 'httpStatus', 'listChannelFlowsResponse_httpStatus' - The response's http status code.
newListChannelFlowsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListChannelFlowsResponse
newListChannelFlowsResponse pHttpStatus_ =
  ListChannelFlowsResponse'
    { channelFlows =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The information about each channel flow.
listChannelFlowsResponse_channelFlows :: Lens.Lens' ListChannelFlowsResponse (Prelude.Maybe [ChannelFlowSummary])
listChannelFlowsResponse_channelFlows = Lens.lens (\ListChannelFlowsResponse' {channelFlows} -> channelFlows) (\s@ListChannelFlowsResponse' {} a -> s {channelFlows = a} :: ListChannelFlowsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token passed by previous API calls until all requested channels are
-- returned.
listChannelFlowsResponse_nextToken :: Lens.Lens' ListChannelFlowsResponse (Prelude.Maybe Prelude.Text)
listChannelFlowsResponse_nextToken = Lens.lens (\ListChannelFlowsResponse' {nextToken} -> nextToken) (\s@ListChannelFlowsResponse' {} a -> s {nextToken = a} :: ListChannelFlowsResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The response's http status code.
listChannelFlowsResponse_httpStatus :: Lens.Lens' ListChannelFlowsResponse Prelude.Int
listChannelFlowsResponse_httpStatus = Lens.lens (\ListChannelFlowsResponse' {httpStatus} -> httpStatus) (\s@ListChannelFlowsResponse' {} a -> s {httpStatus = a} :: ListChannelFlowsResponse)

instance Prelude.NFData ListChannelFlowsResponse where
  rnf ListChannelFlowsResponse' {..} =
    Prelude.rnf channelFlows
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
