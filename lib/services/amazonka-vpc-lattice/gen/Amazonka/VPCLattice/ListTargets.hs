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
-- Module      : Amazonka.VPCLattice.ListTargets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the targets for the target group. By default, all targets are
-- included. You can use this API to check the health status of targets.
-- You can also ﬁlter the results by target.
--
-- This operation returns paginated results.
module Amazonka.VPCLattice.ListTargets
  ( -- * Creating a Request
    ListTargets (..),
    newListTargets,

    -- * Request Lenses
    listTargets_maxResults,
    listTargets_nextToken,
    listTargets_targets,
    listTargets_targetGroupIdentifier,

    -- * Destructuring the Response
    ListTargetsResponse (..),
    newListTargetsResponse,

    -- * Response Lenses
    listTargetsResponse_nextToken,
    listTargetsResponse_httpStatus,
    listTargetsResponse_items,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newListTargets' smart constructor.
data ListTargets = ListTargets'
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The targets to list.
    targets :: Prelude.Maybe [Target],
    -- | The ID or Amazon Resource Name (ARN) of the target group.
    targetGroupIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listTargets_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'listTargets_nextToken' - A pagination token for the next page of results.
--
-- 'targets', 'listTargets_targets' - The targets to list.
--
-- 'targetGroupIdentifier', 'listTargets_targetGroupIdentifier' - The ID or Amazon Resource Name (ARN) of the target group.
newListTargets ::
  -- | 'targetGroupIdentifier'
  Prelude.Text ->
  ListTargets
newListTargets pTargetGroupIdentifier_ =
  ListTargets'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      targets = Prelude.Nothing,
      targetGroupIdentifier = pTargetGroupIdentifier_
    }

-- | The maximum number of results to return.
listTargets_maxResults :: Lens.Lens' ListTargets (Prelude.Maybe Prelude.Natural)
listTargets_maxResults = Lens.lens (\ListTargets' {maxResults} -> maxResults) (\s@ListTargets' {} a -> s {maxResults = a} :: ListTargets)

-- | A pagination token for the next page of results.
listTargets_nextToken :: Lens.Lens' ListTargets (Prelude.Maybe Prelude.Text)
listTargets_nextToken = Lens.lens (\ListTargets' {nextToken} -> nextToken) (\s@ListTargets' {} a -> s {nextToken = a} :: ListTargets)

-- | The targets to list.
listTargets_targets :: Lens.Lens' ListTargets (Prelude.Maybe [Target])
listTargets_targets = Lens.lens (\ListTargets' {targets} -> targets) (\s@ListTargets' {} a -> s {targets = a} :: ListTargets) Prelude.. Lens.mapping Lens.coerced

-- | The ID or Amazon Resource Name (ARN) of the target group.
listTargets_targetGroupIdentifier :: Lens.Lens' ListTargets Prelude.Text
listTargets_targetGroupIdentifier = Lens.lens (\ListTargets' {targetGroupIdentifier} -> targetGroupIdentifier) (\s@ListTargets' {} a -> s {targetGroupIdentifier = a} :: ListTargets)

instance Core.AWSPager ListTargets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTargetsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop (rs Lens.^. listTargetsResponse_items) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTargets_nextToken
          Lens..~ rs
          Lens.^? listTargetsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListTargets where
  type AWSResponse ListTargets = ListTargetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTargetsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "items" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListTargets where
  hashWithSalt _salt ListTargets' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` targetGroupIdentifier

instance Prelude.NFData ListTargets where
  rnf ListTargets' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf targetGroupIdentifier

instance Data.ToHeaders ListTargets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTargets where
  toJSON ListTargets' {..} =
    Data.object
      ( Prelude.catMaybes
          [("targets" Data..=) Prelude.<$> targets]
      )

instance Data.ToPath ListTargets where
  toPath ListTargets' {..} =
    Prelude.mconcat
      [ "/targetgroups/",
        Data.toBS targetGroupIdentifier,
        "/listtargets"
      ]

instance Data.ToQuery ListTargets where
  toQuery ListTargets' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListTargetsResponse' smart constructor.
data ListTargetsResponse = ListTargetsResponse'
  { -- | If there are additional results, a pagination token for the next page of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the targets.
    items :: [TargetSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTargetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTargetsResponse_nextToken' - If there are additional results, a pagination token for the next page of
-- results.
--
-- 'httpStatus', 'listTargetsResponse_httpStatus' - The response's http status code.
--
-- 'items', 'listTargetsResponse_items' - Information about the targets.
newListTargetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTargetsResponse
newListTargetsResponse pHttpStatus_ =
  ListTargetsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      items = Prelude.mempty
    }

-- | If there are additional results, a pagination token for the next page of
-- results.
listTargetsResponse_nextToken :: Lens.Lens' ListTargetsResponse (Prelude.Maybe Prelude.Text)
listTargetsResponse_nextToken = Lens.lens (\ListTargetsResponse' {nextToken} -> nextToken) (\s@ListTargetsResponse' {} a -> s {nextToken = a} :: ListTargetsResponse)

-- | The response's http status code.
listTargetsResponse_httpStatus :: Lens.Lens' ListTargetsResponse Prelude.Int
listTargetsResponse_httpStatus = Lens.lens (\ListTargetsResponse' {httpStatus} -> httpStatus) (\s@ListTargetsResponse' {} a -> s {httpStatus = a} :: ListTargetsResponse)

-- | Information about the targets.
listTargetsResponse_items :: Lens.Lens' ListTargetsResponse [TargetSummary]
listTargetsResponse_items = Lens.lens (\ListTargetsResponse' {items} -> items) (\s@ListTargetsResponse' {} a -> s {items = a} :: ListTargetsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListTargetsResponse where
  rnf ListTargetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf items
