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
-- Module      : Amazonka.Schemas.ListDiscoverers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the discoverers.
--
-- This operation returns paginated results.
module Amazonka.Schemas.ListDiscoverers
  ( -- * Creating a Request
    ListDiscoverers (..),
    newListDiscoverers,

    -- * Request Lenses
    listDiscoverers_discovererIdPrefix,
    listDiscoverers_limit,
    listDiscoverers_nextToken,
    listDiscoverers_sourceArnPrefix,

    -- * Destructuring the Response
    ListDiscoverersResponse (..),
    newListDiscoverersResponse,

    -- * Response Lenses
    listDiscoverersResponse_discoverers,
    listDiscoverersResponse_nextToken,
    listDiscoverersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Schemas.Types

-- | /See:/ 'newListDiscoverers' smart constructor.
data ListDiscoverers = ListDiscoverers'
  { -- | Specifying this limits the results to only those discoverer IDs that
    -- start with the specified prefix.
    discovererIdPrefix :: Prelude.Maybe Prelude.Text,
    limit :: Prelude.Maybe Prelude.Int,
    -- | The token that specifies the next page of results to return. To request
    -- the first page, leave NextToken empty. The token will expire in 24
    -- hours, and cannot be shared with other accounts.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Specifying this limits the results to only those ARNs that start with
    -- the specified prefix.
    sourceArnPrefix :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDiscoverers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'discovererIdPrefix', 'listDiscoverers_discovererIdPrefix' - Specifying this limits the results to only those discoverer IDs that
-- start with the specified prefix.
--
-- 'limit', 'listDiscoverers_limit' - Undocumented member.
--
-- 'nextToken', 'listDiscoverers_nextToken' - The token that specifies the next page of results to return. To request
-- the first page, leave NextToken empty. The token will expire in 24
-- hours, and cannot be shared with other accounts.
--
-- 'sourceArnPrefix', 'listDiscoverers_sourceArnPrefix' - Specifying this limits the results to only those ARNs that start with
-- the specified prefix.
newListDiscoverers ::
  ListDiscoverers
newListDiscoverers =
  ListDiscoverers'
    { discovererIdPrefix =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sourceArnPrefix = Prelude.Nothing
    }

-- | Specifying this limits the results to only those discoverer IDs that
-- start with the specified prefix.
listDiscoverers_discovererIdPrefix :: Lens.Lens' ListDiscoverers (Prelude.Maybe Prelude.Text)
listDiscoverers_discovererIdPrefix = Lens.lens (\ListDiscoverers' {discovererIdPrefix} -> discovererIdPrefix) (\s@ListDiscoverers' {} a -> s {discovererIdPrefix = a} :: ListDiscoverers)

-- | Undocumented member.
listDiscoverers_limit :: Lens.Lens' ListDiscoverers (Prelude.Maybe Prelude.Int)
listDiscoverers_limit = Lens.lens (\ListDiscoverers' {limit} -> limit) (\s@ListDiscoverers' {} a -> s {limit = a} :: ListDiscoverers)

-- | The token that specifies the next page of results to return. To request
-- the first page, leave NextToken empty. The token will expire in 24
-- hours, and cannot be shared with other accounts.
listDiscoverers_nextToken :: Lens.Lens' ListDiscoverers (Prelude.Maybe Prelude.Text)
listDiscoverers_nextToken = Lens.lens (\ListDiscoverers' {nextToken} -> nextToken) (\s@ListDiscoverers' {} a -> s {nextToken = a} :: ListDiscoverers)

-- | Specifying this limits the results to only those ARNs that start with
-- the specified prefix.
listDiscoverers_sourceArnPrefix :: Lens.Lens' ListDiscoverers (Prelude.Maybe Prelude.Text)
listDiscoverers_sourceArnPrefix = Lens.lens (\ListDiscoverers' {sourceArnPrefix} -> sourceArnPrefix) (\s@ListDiscoverers' {} a -> s {sourceArnPrefix = a} :: ListDiscoverers)

instance Core.AWSPager ListDiscoverers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDiscoverersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDiscoverersResponse_discoverers
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDiscoverers_nextToken
          Lens..~ rs
          Lens.^? listDiscoverersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDiscoverers where
  type
    AWSResponse ListDiscoverers =
      ListDiscoverersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDiscoverersResponse'
            Prelude.<$> (x Data..?> "Discoverers" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDiscoverers where
  hashWithSalt _salt ListDiscoverers' {..} =
    _salt `Prelude.hashWithSalt` discovererIdPrefix
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sourceArnPrefix

instance Prelude.NFData ListDiscoverers where
  rnf ListDiscoverers' {..} =
    Prelude.rnf discovererIdPrefix
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sourceArnPrefix

instance Data.ToHeaders ListDiscoverers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDiscoverers where
  toPath = Prelude.const "/v1/discoverers"

instance Data.ToQuery ListDiscoverers where
  toQuery ListDiscoverers' {..} =
    Prelude.mconcat
      [ "discovererIdPrefix" Data.=: discovererIdPrefix,
        "limit" Data.=: limit,
        "nextToken" Data.=: nextToken,
        "sourceArnPrefix" Data.=: sourceArnPrefix
      ]

-- | /See:/ 'newListDiscoverersResponse' smart constructor.
data ListDiscoverersResponse = ListDiscoverersResponse'
  { -- | An array of DiscovererSummary information.
    discoverers :: Prelude.Maybe [DiscovererSummary],
    -- | The token that specifies the next page of results to return. To request
    -- the first page, leave NextToken empty. The token will expire in 24
    -- hours, and cannot be shared with other accounts.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDiscoverersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'discoverers', 'listDiscoverersResponse_discoverers' - An array of DiscovererSummary information.
--
-- 'nextToken', 'listDiscoverersResponse_nextToken' - The token that specifies the next page of results to return. To request
-- the first page, leave NextToken empty. The token will expire in 24
-- hours, and cannot be shared with other accounts.
--
-- 'httpStatus', 'listDiscoverersResponse_httpStatus' - The response's http status code.
newListDiscoverersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDiscoverersResponse
newListDiscoverersResponse pHttpStatus_ =
  ListDiscoverersResponse'
    { discoverers =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of DiscovererSummary information.
listDiscoverersResponse_discoverers :: Lens.Lens' ListDiscoverersResponse (Prelude.Maybe [DiscovererSummary])
listDiscoverersResponse_discoverers = Lens.lens (\ListDiscoverersResponse' {discoverers} -> discoverers) (\s@ListDiscoverersResponse' {} a -> s {discoverers = a} :: ListDiscoverersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token that specifies the next page of results to return. To request
-- the first page, leave NextToken empty. The token will expire in 24
-- hours, and cannot be shared with other accounts.
listDiscoverersResponse_nextToken :: Lens.Lens' ListDiscoverersResponse (Prelude.Maybe Prelude.Text)
listDiscoverersResponse_nextToken = Lens.lens (\ListDiscoverersResponse' {nextToken} -> nextToken) (\s@ListDiscoverersResponse' {} a -> s {nextToken = a} :: ListDiscoverersResponse)

-- | The response's http status code.
listDiscoverersResponse_httpStatus :: Lens.Lens' ListDiscoverersResponse Prelude.Int
listDiscoverersResponse_httpStatus = Lens.lens (\ListDiscoverersResponse' {httpStatus} -> httpStatus) (\s@ListDiscoverersResponse' {} a -> s {httpStatus = a} :: ListDiscoverersResponse)

instance Prelude.NFData ListDiscoverersResponse where
  rnf ListDiscoverersResponse' {..} =
    Prelude.rnf discoverers
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
