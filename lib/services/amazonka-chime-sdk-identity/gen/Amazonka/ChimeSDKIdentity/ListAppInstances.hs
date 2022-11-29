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
-- Module      : Amazonka.ChimeSDKIdentity.ListAppInstances
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all Amazon Chime @AppInstance@s created under a single AWS
-- account.
module Amazonka.ChimeSDKIdentity.ListAppInstances
  ( -- * Creating a Request
    ListAppInstances (..),
    newListAppInstances,

    -- * Request Lenses
    listAppInstances_nextToken,
    listAppInstances_maxResults,

    -- * Destructuring the Response
    ListAppInstancesResponse (..),
    newListAppInstancesResponse,

    -- * Response Lenses
    listAppInstancesResponse_nextToken,
    listAppInstancesResponse_appInstances,
    listAppInstancesResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAppInstances' smart constructor.
data ListAppInstances = ListAppInstances'
  { -- | The token passed by previous API requests until you reach the maximum
    -- number of @AppInstances@.
    nextToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The maximum number of @AppInstance@s that you want to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAppInstances_nextToken' - The token passed by previous API requests until you reach the maximum
-- number of @AppInstances@.
--
-- 'maxResults', 'listAppInstances_maxResults' - The maximum number of @AppInstance@s that you want to return.
newListAppInstances ::
  ListAppInstances
newListAppInstances =
  ListAppInstances'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token passed by previous API requests until you reach the maximum
-- number of @AppInstances@.
listAppInstances_nextToken :: Lens.Lens' ListAppInstances (Prelude.Maybe Prelude.Text)
listAppInstances_nextToken = Lens.lens (\ListAppInstances' {nextToken} -> nextToken) (\s@ListAppInstances' {} a -> s {nextToken = a} :: ListAppInstances) Prelude.. Lens.mapping Core._Sensitive

-- | The maximum number of @AppInstance@s that you want to return.
listAppInstances_maxResults :: Lens.Lens' ListAppInstances (Prelude.Maybe Prelude.Natural)
listAppInstances_maxResults = Lens.lens (\ListAppInstances' {maxResults} -> maxResults) (\s@ListAppInstances' {} a -> s {maxResults = a} :: ListAppInstances)

instance Core.AWSRequest ListAppInstances where
  type
    AWSResponse ListAppInstances =
      ListAppInstancesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppInstancesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "AppInstances" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAppInstances where
  hashWithSalt _salt ListAppInstances' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListAppInstances where
  rnf ListAppInstances' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListAppInstances where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListAppInstances where
  toPath = Prelude.const "/app-instances"

instance Core.ToQuery ListAppInstances where
  toQuery ListAppInstances' {..} =
    Prelude.mconcat
      [ "next-token" Core.=: nextToken,
        "max-results" Core.=: maxResults
      ]

-- | /See:/ 'newListAppInstancesResponse' smart constructor.
data ListAppInstancesResponse = ListAppInstancesResponse'
  { -- | The token passed by previous API requests until the maximum number of
    -- @AppInstance@s is reached.
    nextToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The information for each @AppInstance@.
    appInstances :: Prelude.Maybe [AppInstanceSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAppInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAppInstancesResponse_nextToken' - The token passed by previous API requests until the maximum number of
-- @AppInstance@s is reached.
--
-- 'appInstances', 'listAppInstancesResponse_appInstances' - The information for each @AppInstance@.
--
-- 'httpStatus', 'listAppInstancesResponse_httpStatus' - The response's http status code.
newListAppInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAppInstancesResponse
newListAppInstancesResponse pHttpStatus_ =
  ListAppInstancesResponse'
    { nextToken =
        Prelude.Nothing,
      appInstances = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token passed by previous API requests until the maximum number of
-- @AppInstance@s is reached.
listAppInstancesResponse_nextToken :: Lens.Lens' ListAppInstancesResponse (Prelude.Maybe Prelude.Text)
listAppInstancesResponse_nextToken = Lens.lens (\ListAppInstancesResponse' {nextToken} -> nextToken) (\s@ListAppInstancesResponse' {} a -> s {nextToken = a} :: ListAppInstancesResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The information for each @AppInstance@.
listAppInstancesResponse_appInstances :: Lens.Lens' ListAppInstancesResponse (Prelude.Maybe [AppInstanceSummary])
listAppInstancesResponse_appInstances = Lens.lens (\ListAppInstancesResponse' {appInstances} -> appInstances) (\s@ListAppInstancesResponse' {} a -> s {appInstances = a} :: ListAppInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAppInstancesResponse_httpStatus :: Lens.Lens' ListAppInstancesResponse Prelude.Int
listAppInstancesResponse_httpStatus = Lens.lens (\ListAppInstancesResponse' {httpStatus} -> httpStatus) (\s@ListAppInstancesResponse' {} a -> s {httpStatus = a} :: ListAppInstancesResponse)

instance Prelude.NFData ListAppInstancesResponse where
  rnf ListAppInstancesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf appInstances
      `Prelude.seq` Prelude.rnf httpStatus
