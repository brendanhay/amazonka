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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    listRoleAliases_marker,
    listRoleAliases_pageSize,
    listRoleAliases_ascendingOrder,

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
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRoleAliases' smart constructor.
data ListRoleAliases = ListRoleAliases'
  { -- | A marker used to get the next set of results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return at one time.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | Return the list of role aliases in ascending alphabetical order.
    ascendingOrder :: Prelude.Maybe Prelude.Bool
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
-- 'marker', 'listRoleAliases_marker' - A marker used to get the next set of results.
--
-- 'pageSize', 'listRoleAliases_pageSize' - The maximum number of results to return at one time.
--
-- 'ascendingOrder', 'listRoleAliases_ascendingOrder' - Return the list of role aliases in ascending alphabetical order.
newListRoleAliases ::
  ListRoleAliases
newListRoleAliases =
  ListRoleAliases'
    { marker = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      ascendingOrder = Prelude.Nothing
    }

-- | A marker used to get the next set of results.
listRoleAliases_marker :: Lens.Lens' ListRoleAliases (Prelude.Maybe Prelude.Text)
listRoleAliases_marker = Lens.lens (\ListRoleAliases' {marker} -> marker) (\s@ListRoleAliases' {} a -> s {marker = a} :: ListRoleAliases)

-- | The maximum number of results to return at one time.
listRoleAliases_pageSize :: Lens.Lens' ListRoleAliases (Prelude.Maybe Prelude.Natural)
listRoleAliases_pageSize = Lens.lens (\ListRoleAliases' {pageSize} -> pageSize) (\s@ListRoleAliases' {} a -> s {pageSize = a} :: ListRoleAliases)

-- | Return the list of role aliases in ascending alphabetical order.
listRoleAliases_ascendingOrder :: Lens.Lens' ListRoleAliases (Prelude.Maybe Prelude.Bool)
listRoleAliases_ascendingOrder = Lens.lens (\ListRoleAliases' {ascendingOrder} -> ascendingOrder) (\s@ListRoleAliases' {} a -> s {ascendingOrder = a} :: ListRoleAliases)

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
      Prelude.Just Prelude.$
        rq
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
            Prelude.<$> (x Core..?> "nextMarker")
            Prelude.<*> (x Core..?> "roleAliases" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRoleAliases where
  hashWithSalt _salt ListRoleAliases' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` ascendingOrder

instance Prelude.NFData ListRoleAliases where
  rnf ListRoleAliases' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf ascendingOrder

instance Core.ToHeaders ListRoleAliases where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListRoleAliases where
  toPath = Prelude.const "/role-aliases"

instance Core.ToQuery ListRoleAliases where
  toQuery ListRoleAliases' {..} =
    Prelude.mconcat
      [ "marker" Core.=: marker,
        "pageSize" Core.=: pageSize,
        "isAscendingOrder" Core.=: ascendingOrder
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
