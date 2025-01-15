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
-- Module      : Amazonka.SESV2.ListDedicatedIpPools
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all of the dedicated IP pools that exist in your Amazon Web
-- Services account in the current Region.
module Amazonka.SESV2.ListDedicatedIpPools
  ( -- * Creating a Request
    ListDedicatedIpPools (..),
    newListDedicatedIpPools,

    -- * Request Lenses
    listDedicatedIpPools_nextToken,
    listDedicatedIpPools_pageSize,

    -- * Destructuring the Response
    ListDedicatedIpPoolsResponse (..),
    newListDedicatedIpPoolsResponse,

    -- * Response Lenses
    listDedicatedIpPoolsResponse_dedicatedIpPools,
    listDedicatedIpPoolsResponse_nextToken,
    listDedicatedIpPoolsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SESV2.Types

-- | A request to obtain a list of dedicated IP pools.
--
-- /See:/ 'newListDedicatedIpPools' smart constructor.
data ListDedicatedIpPools = ListDedicatedIpPools'
  { -- | A token returned from a previous call to @ListDedicatedIpPools@ to
    -- indicate the position in the list of dedicated IP pools.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of results to show in a single call to
    -- @ListDedicatedIpPools@. If the number of results is larger than the
    -- number you specified in this parameter, then the response includes a
    -- @NextToken@ element, which you can use to obtain additional results.
    pageSize :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDedicatedIpPools' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDedicatedIpPools_nextToken' - A token returned from a previous call to @ListDedicatedIpPools@ to
-- indicate the position in the list of dedicated IP pools.
--
-- 'pageSize', 'listDedicatedIpPools_pageSize' - The number of results to show in a single call to
-- @ListDedicatedIpPools@. If the number of results is larger than the
-- number you specified in this parameter, then the response includes a
-- @NextToken@ element, which you can use to obtain additional results.
newListDedicatedIpPools ::
  ListDedicatedIpPools
newListDedicatedIpPools =
  ListDedicatedIpPools'
    { nextToken = Prelude.Nothing,
      pageSize = Prelude.Nothing
    }

-- | A token returned from a previous call to @ListDedicatedIpPools@ to
-- indicate the position in the list of dedicated IP pools.
listDedicatedIpPools_nextToken :: Lens.Lens' ListDedicatedIpPools (Prelude.Maybe Prelude.Text)
listDedicatedIpPools_nextToken = Lens.lens (\ListDedicatedIpPools' {nextToken} -> nextToken) (\s@ListDedicatedIpPools' {} a -> s {nextToken = a} :: ListDedicatedIpPools)

-- | The number of results to show in a single call to
-- @ListDedicatedIpPools@. If the number of results is larger than the
-- number you specified in this parameter, then the response includes a
-- @NextToken@ element, which you can use to obtain additional results.
listDedicatedIpPools_pageSize :: Lens.Lens' ListDedicatedIpPools (Prelude.Maybe Prelude.Int)
listDedicatedIpPools_pageSize = Lens.lens (\ListDedicatedIpPools' {pageSize} -> pageSize) (\s@ListDedicatedIpPools' {} a -> s {pageSize = a} :: ListDedicatedIpPools)

instance Core.AWSRequest ListDedicatedIpPools where
  type
    AWSResponse ListDedicatedIpPools =
      ListDedicatedIpPoolsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDedicatedIpPoolsResponse'
            Prelude.<$> ( x
                            Data..?> "DedicatedIpPools"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDedicatedIpPools where
  hashWithSalt _salt ListDedicatedIpPools' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` pageSize

instance Prelude.NFData ListDedicatedIpPools where
  rnf ListDedicatedIpPools' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf pageSize

instance Data.ToHeaders ListDedicatedIpPools where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDedicatedIpPools where
  toPath = Prelude.const "/v2/email/dedicated-ip-pools"

instance Data.ToQuery ListDedicatedIpPools where
  toQuery ListDedicatedIpPools' {..} =
    Prelude.mconcat
      [ "NextToken" Data.=: nextToken,
        "PageSize" Data.=: pageSize
      ]

-- | A list of dedicated IP pools.
--
-- /See:/ 'newListDedicatedIpPoolsResponse' smart constructor.
data ListDedicatedIpPoolsResponse = ListDedicatedIpPoolsResponse'
  { -- | A list of all of the dedicated IP pools that are associated with your
    -- Amazon Web Services account in the current Region.
    dedicatedIpPools :: Prelude.Maybe [Prelude.Text],
    -- | A token that indicates that there are additional IP pools to list. To
    -- view additional IP pools, issue another request to
    -- @ListDedicatedIpPools@, passing this token in the @NextToken@ parameter.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDedicatedIpPoolsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dedicatedIpPools', 'listDedicatedIpPoolsResponse_dedicatedIpPools' - A list of all of the dedicated IP pools that are associated with your
-- Amazon Web Services account in the current Region.
--
-- 'nextToken', 'listDedicatedIpPoolsResponse_nextToken' - A token that indicates that there are additional IP pools to list. To
-- view additional IP pools, issue another request to
-- @ListDedicatedIpPools@, passing this token in the @NextToken@ parameter.
--
-- 'httpStatus', 'listDedicatedIpPoolsResponse_httpStatus' - The response's http status code.
newListDedicatedIpPoolsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDedicatedIpPoolsResponse
newListDedicatedIpPoolsResponse pHttpStatus_ =
  ListDedicatedIpPoolsResponse'
    { dedicatedIpPools =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of all of the dedicated IP pools that are associated with your
-- Amazon Web Services account in the current Region.
listDedicatedIpPoolsResponse_dedicatedIpPools :: Lens.Lens' ListDedicatedIpPoolsResponse (Prelude.Maybe [Prelude.Text])
listDedicatedIpPoolsResponse_dedicatedIpPools = Lens.lens (\ListDedicatedIpPoolsResponse' {dedicatedIpPools} -> dedicatedIpPools) (\s@ListDedicatedIpPoolsResponse' {} a -> s {dedicatedIpPools = a} :: ListDedicatedIpPoolsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that indicates that there are additional IP pools to list. To
-- view additional IP pools, issue another request to
-- @ListDedicatedIpPools@, passing this token in the @NextToken@ parameter.
listDedicatedIpPoolsResponse_nextToken :: Lens.Lens' ListDedicatedIpPoolsResponse (Prelude.Maybe Prelude.Text)
listDedicatedIpPoolsResponse_nextToken = Lens.lens (\ListDedicatedIpPoolsResponse' {nextToken} -> nextToken) (\s@ListDedicatedIpPoolsResponse' {} a -> s {nextToken = a} :: ListDedicatedIpPoolsResponse)

-- | The response's http status code.
listDedicatedIpPoolsResponse_httpStatus :: Lens.Lens' ListDedicatedIpPoolsResponse Prelude.Int
listDedicatedIpPoolsResponse_httpStatus = Lens.lens (\ListDedicatedIpPoolsResponse' {httpStatus} -> httpStatus) (\s@ListDedicatedIpPoolsResponse' {} a -> s {httpStatus = a} :: ListDedicatedIpPoolsResponse)

instance Prelude.NFData ListDedicatedIpPoolsResponse where
  rnf ListDedicatedIpPoolsResponse' {..} =
    Prelude.rnf dedicatedIpPools `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
