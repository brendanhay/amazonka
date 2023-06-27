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
-- Module      : Amazonka.ChimeSDKIdentity.ListAppInstanceBots
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all @AppInstanceBots@ created under a single @AppInstance@.
module Amazonka.ChimeSDKIdentity.ListAppInstanceBots
  ( -- * Creating a Request
    ListAppInstanceBots (..),
    newListAppInstanceBots,

    -- * Request Lenses
    listAppInstanceBots_maxResults,
    listAppInstanceBots_nextToken,
    listAppInstanceBots_appInstanceArn,

    -- * Destructuring the Response
    ListAppInstanceBotsResponse (..),
    newListAppInstanceBotsResponse,

    -- * Response Lenses
    listAppInstanceBotsResponse_appInstanceArn,
    listAppInstanceBotsResponse_appInstanceBots,
    listAppInstanceBotsResponse_nextToken,
    listAppInstanceBotsResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAppInstanceBots' smart constructor.
data ListAppInstanceBots = ListAppInstanceBots'
  { -- | The maximum number of requests to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token passed by previous API calls until all requested bots are
    -- returned.
    nextToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ARN of the @AppInstance@.
    appInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppInstanceBots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAppInstanceBots_maxResults' - The maximum number of requests to return.
--
-- 'nextToken', 'listAppInstanceBots_nextToken' - The token passed by previous API calls until all requested bots are
-- returned.
--
-- 'appInstanceArn', 'listAppInstanceBots_appInstanceArn' - The ARN of the @AppInstance@.
newListAppInstanceBots ::
  -- | 'appInstanceArn'
  Prelude.Text ->
  ListAppInstanceBots
newListAppInstanceBots pAppInstanceArn_ =
  ListAppInstanceBots'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      appInstanceArn = pAppInstanceArn_
    }

-- | The maximum number of requests to return.
listAppInstanceBots_maxResults :: Lens.Lens' ListAppInstanceBots (Prelude.Maybe Prelude.Natural)
listAppInstanceBots_maxResults = Lens.lens (\ListAppInstanceBots' {maxResults} -> maxResults) (\s@ListAppInstanceBots' {} a -> s {maxResults = a} :: ListAppInstanceBots)

-- | The token passed by previous API calls until all requested bots are
-- returned.
listAppInstanceBots_nextToken :: Lens.Lens' ListAppInstanceBots (Prelude.Maybe Prelude.Text)
listAppInstanceBots_nextToken = Lens.lens (\ListAppInstanceBots' {nextToken} -> nextToken) (\s@ListAppInstanceBots' {} a -> s {nextToken = a} :: ListAppInstanceBots) Prelude.. Lens.mapping Data._Sensitive

-- | The ARN of the @AppInstance@.
listAppInstanceBots_appInstanceArn :: Lens.Lens' ListAppInstanceBots Prelude.Text
listAppInstanceBots_appInstanceArn = Lens.lens (\ListAppInstanceBots' {appInstanceArn} -> appInstanceArn) (\s@ListAppInstanceBots' {} a -> s {appInstanceArn = a} :: ListAppInstanceBots)

instance Core.AWSRequest ListAppInstanceBots where
  type
    AWSResponse ListAppInstanceBots =
      ListAppInstanceBotsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppInstanceBotsResponse'
            Prelude.<$> (x Data..?> "AppInstanceArn")
            Prelude.<*> ( x
                            Data..?> "AppInstanceBots"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAppInstanceBots where
  hashWithSalt _salt ListAppInstanceBots' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` appInstanceArn

instance Prelude.NFData ListAppInstanceBots where
  rnf ListAppInstanceBots' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf appInstanceArn

instance Data.ToHeaders ListAppInstanceBots where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListAppInstanceBots where
  toPath = Prelude.const "/app-instance-bots"

instance Data.ToQuery ListAppInstanceBots where
  toQuery ListAppInstanceBots' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken,
        "app-instance-arn" Data.=: appInstanceArn
      ]

-- | /See:/ 'newListAppInstanceBotsResponse' smart constructor.
data ListAppInstanceBotsResponse = ListAppInstanceBotsResponse'
  { -- | The ARN of the AppInstance.
    appInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The information for each requested @AppInstanceBot@.
    appInstanceBots :: Prelude.Maybe [AppInstanceBotSummary],
    -- | The token passed by previous API calls until all requested bots are
    -- returned.
    nextToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppInstanceBotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceArn', 'listAppInstanceBotsResponse_appInstanceArn' - The ARN of the AppInstance.
--
-- 'appInstanceBots', 'listAppInstanceBotsResponse_appInstanceBots' - The information for each requested @AppInstanceBot@.
--
-- 'nextToken', 'listAppInstanceBotsResponse_nextToken' - The token passed by previous API calls until all requested bots are
-- returned.
--
-- 'httpStatus', 'listAppInstanceBotsResponse_httpStatus' - The response's http status code.
newListAppInstanceBotsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAppInstanceBotsResponse
newListAppInstanceBotsResponse pHttpStatus_ =
  ListAppInstanceBotsResponse'
    { appInstanceArn =
        Prelude.Nothing,
      appInstanceBots = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the AppInstance.
listAppInstanceBotsResponse_appInstanceArn :: Lens.Lens' ListAppInstanceBotsResponse (Prelude.Maybe Prelude.Text)
listAppInstanceBotsResponse_appInstanceArn = Lens.lens (\ListAppInstanceBotsResponse' {appInstanceArn} -> appInstanceArn) (\s@ListAppInstanceBotsResponse' {} a -> s {appInstanceArn = a} :: ListAppInstanceBotsResponse)

-- | The information for each requested @AppInstanceBot@.
listAppInstanceBotsResponse_appInstanceBots :: Lens.Lens' ListAppInstanceBotsResponse (Prelude.Maybe [AppInstanceBotSummary])
listAppInstanceBotsResponse_appInstanceBots = Lens.lens (\ListAppInstanceBotsResponse' {appInstanceBots} -> appInstanceBots) (\s@ListAppInstanceBotsResponse' {} a -> s {appInstanceBots = a} :: ListAppInstanceBotsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token passed by previous API calls until all requested bots are
-- returned.
listAppInstanceBotsResponse_nextToken :: Lens.Lens' ListAppInstanceBotsResponse (Prelude.Maybe Prelude.Text)
listAppInstanceBotsResponse_nextToken = Lens.lens (\ListAppInstanceBotsResponse' {nextToken} -> nextToken) (\s@ListAppInstanceBotsResponse' {} a -> s {nextToken = a} :: ListAppInstanceBotsResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The response's http status code.
listAppInstanceBotsResponse_httpStatus :: Lens.Lens' ListAppInstanceBotsResponse Prelude.Int
listAppInstanceBotsResponse_httpStatus = Lens.lens (\ListAppInstanceBotsResponse' {httpStatus} -> httpStatus) (\s@ListAppInstanceBotsResponse' {} a -> s {httpStatus = a} :: ListAppInstanceBotsResponse)

instance Prelude.NFData ListAppInstanceBotsResponse where
  rnf ListAppInstanceBotsResponse' {..} =
    Prelude.rnf appInstanceArn
      `Prelude.seq` Prelude.rnf appInstanceBots
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
