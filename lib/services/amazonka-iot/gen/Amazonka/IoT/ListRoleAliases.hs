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
-- Module      : Amazonka.IoT.ListRoleAliases
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the role aliases registered in your account.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListRoleAliases>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListRoleAliases
  ( -- * Creating a Request
    ListRoleAliases (..),
    newListRoleAliases,

    -- * Request Lenses
    listRoleAliases_ascendingOrder,
    listRoleAliases_marker,
    listRoleAliases_pageSize,

    -- * Destructuring the Response
    ListRoleAliasesResponse (..),
    newListRoleAliasesResponse,

    -- * Response Lenses
    listRoleAliasesResponse_nextMarker,
    listRoleAliasesResponse_roleAliases,
    listRoleAliasesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRoleAliases' smart constructor.
data ListRoleAliases = ListRoleAliases'
  { -- | Return the list of role aliases in ascending alphabetical order.
    ascendingOrder :: Prelude.Maybe Prelude.Bool,
    -- | A marker used to get the next set of results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return at one time.
    pageSize :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRoleAliases' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ascendingOrder', 'listRoleAliases_ascendingOrder' - Return the list of role aliases in ascending alphabetical order.
--
-- 'marker', 'listRoleAliases_marker' - A marker used to get the next set of results.
--
-- 'pageSize', 'listRoleAliases_pageSize' - The maximum number of results to return at one time.
newListRoleAliases ::
  ListRoleAliases
newListRoleAliases =
  ListRoleAliases'
    { ascendingOrder = Prelude.Nothing,
      marker = Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | Return the list of role aliases in ascending alphabetical order.
listRoleAliases_ascendingOrder :: Lens.Lens' ListRoleAliases (Prelude.Maybe Prelude.Bool)
listRoleAliases_ascendingOrder = Lens.lens (\ListRoleAliases' {ascendingOrder} -> ascendingOrder) (\s@ListRoleAliases' {} a -> s {ascendingOrder = a} :: ListRoleAliases)

-- | A marker used to get the next set of results.
listRoleAliases_marker :: Lens.Lens' ListRoleAliases (Prelude.Maybe Prelude.Text)
listRoleAliases_marker = Lens.lens (\ListRoleAliases' {marker} -> marker) (\s@ListRoleAliases' {} a -> s {marker = a} :: ListRoleAliases)

-- | The maximum number of results to return at one time.
listRoleAliases_pageSize :: Lens.Lens' ListRoleAliases (Prelude.Maybe Prelude.Natural)
listRoleAliases_pageSize = Lens.lens (\ListRoleAliases' {pageSize} -> pageSize) (\s@ListRoleAliases' {} a -> s {pageSize = a} :: ListRoleAliases)

instance Core.AWSPager ListRoleAliases where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRoleAliasesResponse_nextMarker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRoleAliasesResponse_roleAliases
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listRoleAliases_marker
          Lens..~ rs
          Lens.^? listRoleAliasesResponse_nextMarker
          Prelude.. Lens._Just

instance Core.AWSRequest ListRoleAliases where
  type
    AWSResponse ListRoleAliases =
      ListRoleAliasesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRoleAliasesResponse'
            Prelude.<$> (x Data..?> "nextMarker")
            Prelude.<*> (x Data..?> "roleAliases" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRoleAliases where
  hashWithSalt _salt ListRoleAliases' {..} =
    _salt
      `Prelude.hashWithSalt` ascendingOrder
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` pageSize

instance Prelude.NFData ListRoleAliases where
  rnf ListRoleAliases' {..} =
    Prelude.rnf ascendingOrder
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf pageSize

instance Data.ToHeaders ListRoleAliases where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListRoleAliases where
  toPath = Prelude.const "/role-aliases"

instance Data.ToQuery ListRoleAliases where
  toQuery ListRoleAliases' {..} =
    Prelude.mconcat
      [ "isAscendingOrder" Data.=: ascendingOrder,
        "marker" Data.=: marker,
        "pageSize" Data.=: pageSize
      ]

-- | /See:/ 'newListRoleAliasesResponse' smart constructor.
data ListRoleAliasesResponse = ListRoleAliasesResponse'
  { -- | A marker used to get the next set of results.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The role aliases.
    roleAliases :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRoleAliasesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listRoleAliasesResponse_nextMarker' - A marker used to get the next set of results.
--
-- 'roleAliases', 'listRoleAliasesResponse_roleAliases' - The role aliases.
--
-- 'httpStatus', 'listRoleAliasesResponse_httpStatus' - The response's http status code.
newListRoleAliasesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRoleAliasesResponse
newListRoleAliasesResponse pHttpStatus_ =
  ListRoleAliasesResponse'
    { nextMarker =
        Prelude.Nothing,
      roleAliases = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A marker used to get the next set of results.
listRoleAliasesResponse_nextMarker :: Lens.Lens' ListRoleAliasesResponse (Prelude.Maybe Prelude.Text)
listRoleAliasesResponse_nextMarker = Lens.lens (\ListRoleAliasesResponse' {nextMarker} -> nextMarker) (\s@ListRoleAliasesResponse' {} a -> s {nextMarker = a} :: ListRoleAliasesResponse)

-- | The role aliases.
listRoleAliasesResponse_roleAliases :: Lens.Lens' ListRoleAliasesResponse (Prelude.Maybe [Prelude.Text])
listRoleAliasesResponse_roleAliases = Lens.lens (\ListRoleAliasesResponse' {roleAliases} -> roleAliases) (\s@ListRoleAliasesResponse' {} a -> s {roleAliases = a} :: ListRoleAliasesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRoleAliasesResponse_httpStatus :: Lens.Lens' ListRoleAliasesResponse Prelude.Int
listRoleAliasesResponse_httpStatus = Lens.lens (\ListRoleAliasesResponse' {httpStatus} -> httpStatus) (\s@ListRoleAliasesResponse' {} a -> s {httpStatus = a} :: ListRoleAliasesResponse)

instance Prelude.NFData ListRoleAliasesResponse where
  rnf ListRoleAliasesResponse' {..} =
    Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf roleAliases
      `Prelude.seq` Prelude.rnf httpStatus
