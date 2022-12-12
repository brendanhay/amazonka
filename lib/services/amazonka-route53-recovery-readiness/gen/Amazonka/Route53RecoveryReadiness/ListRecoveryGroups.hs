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
-- Module      : Amazonka.Route53RecoveryReadiness.ListRecoveryGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the recovery groups in an account.
--
-- This operation returns paginated results.
module Amazonka.Route53RecoveryReadiness.ListRecoveryGroups
  ( -- * Creating a Request
    ListRecoveryGroups (..),
    newListRecoveryGroups,

    -- * Request Lenses
    listRecoveryGroups_maxResults,
    listRecoveryGroups_nextToken,

    -- * Destructuring the Response
    ListRecoveryGroupsResponse (..),
    newListRecoveryGroupsResponse,

    -- * Response Lenses
    listRecoveryGroupsResponse_nextToken,
    listRecoveryGroupsResponse_recoveryGroups,
    listRecoveryGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newListRecoveryGroups' smart constructor.
data ListRecoveryGroups = ListRecoveryGroups'
  { -- | The number of objects that you want to return with this call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecoveryGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRecoveryGroups_maxResults' - The number of objects that you want to return with this call.
--
-- 'nextToken', 'listRecoveryGroups_nextToken' - The token that identifies which batch of results you want to see.
newListRecoveryGroups ::
  ListRecoveryGroups
newListRecoveryGroups =
  ListRecoveryGroups'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The number of objects that you want to return with this call.
listRecoveryGroups_maxResults :: Lens.Lens' ListRecoveryGroups (Prelude.Maybe Prelude.Natural)
listRecoveryGroups_maxResults = Lens.lens (\ListRecoveryGroups' {maxResults} -> maxResults) (\s@ListRecoveryGroups' {} a -> s {maxResults = a} :: ListRecoveryGroups)

-- | The token that identifies which batch of results you want to see.
listRecoveryGroups_nextToken :: Lens.Lens' ListRecoveryGroups (Prelude.Maybe Prelude.Text)
listRecoveryGroups_nextToken = Lens.lens (\ListRecoveryGroups' {nextToken} -> nextToken) (\s@ListRecoveryGroups' {} a -> s {nextToken = a} :: ListRecoveryGroups)

instance Core.AWSPager ListRecoveryGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRecoveryGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRecoveryGroupsResponse_recoveryGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRecoveryGroups_nextToken
          Lens..~ rs
          Lens.^? listRecoveryGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListRecoveryGroups where
  type
    AWSResponse ListRecoveryGroups =
      ListRecoveryGroupsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRecoveryGroupsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "recoveryGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRecoveryGroups where
  hashWithSalt _salt ListRecoveryGroups' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListRecoveryGroups where
  rnf ListRecoveryGroups' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListRecoveryGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListRecoveryGroups where
  toPath = Prelude.const "/recoverygroups"

instance Data.ToQuery ListRecoveryGroups where
  toQuery ListRecoveryGroups' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListRecoveryGroupsResponse' smart constructor.
data ListRecoveryGroupsResponse = ListRecoveryGroupsResponse'
  { -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of recovery groups.
    recoveryGroups :: Prelude.Maybe [RecoveryGroupOutput],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecoveryGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRecoveryGroupsResponse_nextToken' - The token that identifies which batch of results you want to see.
--
-- 'recoveryGroups', 'listRecoveryGroupsResponse_recoveryGroups' - A list of recovery groups.
--
-- 'httpStatus', 'listRecoveryGroupsResponse_httpStatus' - The response's http status code.
newListRecoveryGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRecoveryGroupsResponse
newListRecoveryGroupsResponse pHttpStatus_ =
  ListRecoveryGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      recoveryGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that identifies which batch of results you want to see.
listRecoveryGroupsResponse_nextToken :: Lens.Lens' ListRecoveryGroupsResponse (Prelude.Maybe Prelude.Text)
listRecoveryGroupsResponse_nextToken = Lens.lens (\ListRecoveryGroupsResponse' {nextToken} -> nextToken) (\s@ListRecoveryGroupsResponse' {} a -> s {nextToken = a} :: ListRecoveryGroupsResponse)

-- | A list of recovery groups.
listRecoveryGroupsResponse_recoveryGroups :: Lens.Lens' ListRecoveryGroupsResponse (Prelude.Maybe [RecoveryGroupOutput])
listRecoveryGroupsResponse_recoveryGroups = Lens.lens (\ListRecoveryGroupsResponse' {recoveryGroups} -> recoveryGroups) (\s@ListRecoveryGroupsResponse' {} a -> s {recoveryGroups = a} :: ListRecoveryGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRecoveryGroupsResponse_httpStatus :: Lens.Lens' ListRecoveryGroupsResponse Prelude.Int
listRecoveryGroupsResponse_httpStatus = Lens.lens (\ListRecoveryGroupsResponse' {httpStatus} -> httpStatus) (\s@ListRecoveryGroupsResponse' {} a -> s {httpStatus = a} :: ListRecoveryGroupsResponse)

instance Prelude.NFData ListRecoveryGroupsResponse where
  rnf ListRecoveryGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf recoveryGroups
      `Prelude.seq` Prelude.rnf httpStatus
