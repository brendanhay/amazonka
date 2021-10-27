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
-- Module      : Network.AWS.Chime.ListAppInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all Amazon Chime @AppInstance@s created under a single AWS
-- account.
module Network.AWS.Chime.ListAppInstances
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
    listAppInstancesResponse_appInstances,
    listAppInstancesResponse_nextToken,
    listAppInstancesResponse_httpStatus,
  )
where

import Network.AWS.Chime.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListAppInstances' smart constructor.
data ListAppInstances = ListAppInstances'
  { -- | The token passed by previous API requests until you reach the maximum
    -- number of @AppInstance@s.
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
-- number of @AppInstance@s.
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
-- number of @AppInstance@s.
listAppInstances_nextToken :: Lens.Lens' ListAppInstances (Prelude.Maybe Prelude.Text)
listAppInstances_nextToken = Lens.lens (\ListAppInstances' {nextToken} -> nextToken) (\s@ListAppInstances' {} a -> s {nextToken = a} :: ListAppInstances) Prelude.. Lens.mapping Core._Sensitive

-- | The maximum number of @AppInstance@s that you want to return.
listAppInstances_maxResults :: Lens.Lens' ListAppInstances (Prelude.Maybe Prelude.Natural)
listAppInstances_maxResults = Lens.lens (\ListAppInstances' {maxResults} -> maxResults) (\s@ListAppInstances' {} a -> s {maxResults = a} :: ListAppInstances)

instance Core.AWSRequest ListAppInstances where
  type
    AWSResponse ListAppInstances =
      ListAppInstancesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAppInstancesResponse'
            Prelude.<$> (x Core..?> "AppInstances" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAppInstances

instance Prelude.NFData ListAppInstances

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
  { -- | The information for each @AppInstance@.
    appInstances :: Prelude.Maybe [AppInstanceSummary],
    -- | The token passed by previous API requests until the maximum number of
    -- @AppInstance@s is reached.
    nextToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
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
-- 'appInstances', 'listAppInstancesResponse_appInstances' - The information for each @AppInstance@.
--
-- 'nextToken', 'listAppInstancesResponse_nextToken' - The token passed by previous API requests until the maximum number of
-- @AppInstance@s is reached.
--
-- 'httpStatus', 'listAppInstancesResponse_httpStatus' - The response's http status code.
newListAppInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAppInstancesResponse
newListAppInstancesResponse pHttpStatus_ =
  ListAppInstancesResponse'
    { appInstances =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The information for each @AppInstance@.
listAppInstancesResponse_appInstances :: Lens.Lens' ListAppInstancesResponse (Prelude.Maybe [AppInstanceSummary])
listAppInstancesResponse_appInstances = Lens.lens (\ListAppInstancesResponse' {appInstances} -> appInstances) (\s@ListAppInstancesResponse' {} a -> s {appInstances = a} :: ListAppInstancesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token passed by previous API requests until the maximum number of
-- @AppInstance@s is reached.
listAppInstancesResponse_nextToken :: Lens.Lens' ListAppInstancesResponse (Prelude.Maybe Prelude.Text)
listAppInstancesResponse_nextToken = Lens.lens (\ListAppInstancesResponse' {nextToken} -> nextToken) (\s@ListAppInstancesResponse' {} a -> s {nextToken = a} :: ListAppInstancesResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The response's http status code.
listAppInstancesResponse_httpStatus :: Lens.Lens' ListAppInstancesResponse Prelude.Int
listAppInstancesResponse_httpStatus = Lens.lens (\ListAppInstancesResponse' {httpStatus} -> httpStatus) (\s@ListAppInstancesResponse' {} a -> s {httpStatus = a} :: ListAppInstancesResponse)

instance Prelude.NFData ListAppInstancesResponse
