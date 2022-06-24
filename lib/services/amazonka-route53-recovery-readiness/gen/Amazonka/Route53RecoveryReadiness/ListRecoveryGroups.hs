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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a collection of Recovery Groups.
--
-- This operation returns paginated results.
module Amazonka.Route53RecoveryReadiness.ListRecoveryGroups
  ( -- * Creating a Request
    ListRecoveryGroups (..),
    newListRecoveryGroups,

    -- * Request Lenses
    listRecoveryGroups_nextToken,
    listRecoveryGroups_maxResults,

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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newListRecoveryGroups' smart constructor.
data ListRecoveryGroups = ListRecoveryGroups'
  { -- | A token used to resume pagination from the end of a previous request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Upper bound on number of records to return.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'nextToken', 'listRecoveryGroups_nextToken' - A token used to resume pagination from the end of a previous request.
--
-- 'maxResults', 'listRecoveryGroups_maxResults' - Upper bound on number of records to return.
newListRecoveryGroups ::
  ListRecoveryGroups
newListRecoveryGroups =
  ListRecoveryGroups'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token used to resume pagination from the end of a previous request.
listRecoveryGroups_nextToken :: Lens.Lens' ListRecoveryGroups (Prelude.Maybe Prelude.Text)
listRecoveryGroups_nextToken = Lens.lens (\ListRecoveryGroups' {nextToken} -> nextToken) (\s@ListRecoveryGroups' {} a -> s {nextToken = a} :: ListRecoveryGroups)

-- | Upper bound on number of records to return.
listRecoveryGroups_maxResults :: Lens.Lens' ListRecoveryGroups (Prelude.Maybe Prelude.Natural)
listRecoveryGroups_maxResults = Lens.lens (\ListRecoveryGroups' {maxResults} -> maxResults) (\s@ListRecoveryGroups' {} a -> s {maxResults = a} :: ListRecoveryGroups)

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRecoveryGroupsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "recoveryGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRecoveryGroups where
  hashWithSalt _salt ListRecoveryGroups' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListRecoveryGroups where
  rnf ListRecoveryGroups' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListRecoveryGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListRecoveryGroups where
  toPath = Prelude.const "/recoverygroups"

instance Core.ToQuery ListRecoveryGroups where
  toQuery ListRecoveryGroups' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListRecoveryGroupsResponse' smart constructor.
data ListRecoveryGroupsResponse = ListRecoveryGroupsResponse'
  { -- | A token that can be used to resume pagination from the end of the
    -- collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of RecoveryGroups
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
-- 'nextToken', 'listRecoveryGroupsResponse_nextToken' - A token that can be used to resume pagination from the end of the
-- collection.
--
-- 'recoveryGroups', 'listRecoveryGroupsResponse_recoveryGroups' - A list of RecoveryGroups
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

-- | A token that can be used to resume pagination from the end of the
-- collection.
listRecoveryGroupsResponse_nextToken :: Lens.Lens' ListRecoveryGroupsResponse (Prelude.Maybe Prelude.Text)
listRecoveryGroupsResponse_nextToken = Lens.lens (\ListRecoveryGroupsResponse' {nextToken} -> nextToken) (\s@ListRecoveryGroupsResponse' {} a -> s {nextToken = a} :: ListRecoveryGroupsResponse)

-- | A list of RecoveryGroups
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
