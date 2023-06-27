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
-- Module      : Amazonka.VPCLattice.ListTargetGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your target groups. You can narrow your search by using the
-- filters below in your request.
--
-- This operation returns paginated results.
module Amazonka.VPCLattice.ListTargetGroups
  ( -- * Creating a Request
    ListTargetGroups (..),
    newListTargetGroups,

    -- * Request Lenses
    listTargetGroups_maxResults,
    listTargetGroups_nextToken,
    listTargetGroups_targetGroupType,
    listTargetGroups_vpcIdentifier,

    -- * Destructuring the Response
    ListTargetGroupsResponse (..),
    newListTargetGroupsResponse,

    -- * Response Lenses
    listTargetGroupsResponse_items,
    listTargetGroupsResponse_nextToken,
    listTargetGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.VPCLattice.Types

-- | /See:/ 'newListTargetGroups' smart constructor.
data ListTargetGroups = ListTargetGroups'
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The target group type.
    targetGroupType :: Prelude.Maybe TargetGroupType,
    -- | The ID or Amazon Resource Name (ARN) of the service.
    vpcIdentifier :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTargetGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listTargetGroups_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'listTargetGroups_nextToken' - A pagination token for the next page of results.
--
-- 'targetGroupType', 'listTargetGroups_targetGroupType' - The target group type.
--
-- 'vpcIdentifier', 'listTargetGroups_vpcIdentifier' - The ID or Amazon Resource Name (ARN) of the service.
newListTargetGroups ::
  ListTargetGroups
newListTargetGroups =
  ListTargetGroups'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      targetGroupType = Prelude.Nothing,
      vpcIdentifier = Prelude.Nothing
    }

-- | The maximum number of results to return.
listTargetGroups_maxResults :: Lens.Lens' ListTargetGroups (Prelude.Maybe Prelude.Natural)
listTargetGroups_maxResults = Lens.lens (\ListTargetGroups' {maxResults} -> maxResults) (\s@ListTargetGroups' {} a -> s {maxResults = a} :: ListTargetGroups)

-- | A pagination token for the next page of results.
listTargetGroups_nextToken :: Lens.Lens' ListTargetGroups (Prelude.Maybe Prelude.Text)
listTargetGroups_nextToken = Lens.lens (\ListTargetGroups' {nextToken} -> nextToken) (\s@ListTargetGroups' {} a -> s {nextToken = a} :: ListTargetGroups)

-- | The target group type.
listTargetGroups_targetGroupType :: Lens.Lens' ListTargetGroups (Prelude.Maybe TargetGroupType)
listTargetGroups_targetGroupType = Lens.lens (\ListTargetGroups' {targetGroupType} -> targetGroupType) (\s@ListTargetGroups' {} a -> s {targetGroupType = a} :: ListTargetGroups)

-- | The ID or Amazon Resource Name (ARN) of the service.
listTargetGroups_vpcIdentifier :: Lens.Lens' ListTargetGroups (Prelude.Maybe Prelude.Text)
listTargetGroups_vpcIdentifier = Lens.lens (\ListTargetGroups' {vpcIdentifier} -> vpcIdentifier) (\s@ListTargetGroups' {} a -> s {vpcIdentifier = a} :: ListTargetGroups)

instance Core.AWSPager ListTargetGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTargetGroupsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTargetGroupsResponse_items
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTargetGroups_nextToken
          Lens..~ rs
          Lens.^? listTargetGroupsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListTargetGroups where
  type
    AWSResponse ListTargetGroups =
      ListTargetGroupsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTargetGroupsResponse'
            Prelude.<$> (x Data..?> "items" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTargetGroups where
  hashWithSalt _salt ListTargetGroups' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` targetGroupType
      `Prelude.hashWithSalt` vpcIdentifier

instance Prelude.NFData ListTargetGroups where
  rnf ListTargetGroups' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf targetGroupType
      `Prelude.seq` Prelude.rnf vpcIdentifier

instance Data.ToHeaders ListTargetGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListTargetGroups where
  toPath = Prelude.const "/targetgroups"

instance Data.ToQuery ListTargetGroups where
  toQuery ListTargetGroups' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "targetGroupType" Data.=: targetGroupType,
        "vpcIdentifier" Data.=: vpcIdentifier
      ]

-- | /See:/ 'newListTargetGroupsResponse' smart constructor.
data ListTargetGroupsResponse = ListTargetGroupsResponse'
  { -- | Information about the target groups.
    items :: Prelude.Maybe [TargetGroupSummary],
    -- | If there are additional results, a pagination token for the next page of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTargetGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'listTargetGroupsResponse_items' - Information about the target groups.
--
-- 'nextToken', 'listTargetGroupsResponse_nextToken' - If there are additional results, a pagination token for the next page of
-- results.
--
-- 'httpStatus', 'listTargetGroupsResponse_httpStatus' - The response's http status code.
newListTargetGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTargetGroupsResponse
newListTargetGroupsResponse pHttpStatus_ =
  ListTargetGroupsResponse'
    { items = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the target groups.
listTargetGroupsResponse_items :: Lens.Lens' ListTargetGroupsResponse (Prelude.Maybe [TargetGroupSummary])
listTargetGroupsResponse_items = Lens.lens (\ListTargetGroupsResponse' {items} -> items) (\s@ListTargetGroupsResponse' {} a -> s {items = a} :: ListTargetGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are additional results, a pagination token for the next page of
-- results.
listTargetGroupsResponse_nextToken :: Lens.Lens' ListTargetGroupsResponse (Prelude.Maybe Prelude.Text)
listTargetGroupsResponse_nextToken = Lens.lens (\ListTargetGroupsResponse' {nextToken} -> nextToken) (\s@ListTargetGroupsResponse' {} a -> s {nextToken = a} :: ListTargetGroupsResponse)

-- | The response's http status code.
listTargetGroupsResponse_httpStatus :: Lens.Lens' ListTargetGroupsResponse Prelude.Int
listTargetGroupsResponse_httpStatus = Lens.lens (\ListTargetGroupsResponse' {httpStatus} -> httpStatus) (\s@ListTargetGroupsResponse' {} a -> s {httpStatus = a} :: ListTargetGroupsResponse)

instance Prelude.NFData ListTargetGroupsResponse where
  rnf ListTargetGroupsResponse' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
