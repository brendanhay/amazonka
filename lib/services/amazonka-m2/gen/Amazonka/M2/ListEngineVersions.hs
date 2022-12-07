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
-- Module      : Amazonka.M2.ListEngineVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the available engine versions.
--
-- This operation returns paginated results.
module Amazonka.M2.ListEngineVersions
  ( -- * Creating a Request
    ListEngineVersions (..),
    newListEngineVersions,

    -- * Request Lenses
    listEngineVersions_nextToken,
    listEngineVersions_engineType,
    listEngineVersions_maxResults,

    -- * Destructuring the Response
    ListEngineVersionsResponse (..),
    newListEngineVersionsResponse,

    -- * Response Lenses
    listEngineVersionsResponse_nextToken,
    listEngineVersionsResponse_httpStatus,
    listEngineVersionsResponse_engineVersions,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEngineVersions' smart constructor.
data ListEngineVersions = ListEngineVersions'
  { -- | A pagination token returned from a previous call to this operation. This
    -- specifies the next item to return. To return to the beginning of the
    -- list, exclude this parameter.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type of target platform.
    engineType :: Prelude.Maybe EngineType,
    -- | The maximum number of objects to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEngineVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEngineVersions_nextToken' - A pagination token returned from a previous call to this operation. This
-- specifies the next item to return. To return to the beginning of the
-- list, exclude this parameter.
--
-- 'engineType', 'listEngineVersions_engineType' - The type of target platform.
--
-- 'maxResults', 'listEngineVersions_maxResults' - The maximum number of objects to return.
newListEngineVersions ::
  ListEngineVersions
newListEngineVersions =
  ListEngineVersions'
    { nextToken = Prelude.Nothing,
      engineType = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A pagination token returned from a previous call to this operation. This
-- specifies the next item to return. To return to the beginning of the
-- list, exclude this parameter.
listEngineVersions_nextToken :: Lens.Lens' ListEngineVersions (Prelude.Maybe Prelude.Text)
listEngineVersions_nextToken = Lens.lens (\ListEngineVersions' {nextToken} -> nextToken) (\s@ListEngineVersions' {} a -> s {nextToken = a} :: ListEngineVersions)

-- | The type of target platform.
listEngineVersions_engineType :: Lens.Lens' ListEngineVersions (Prelude.Maybe EngineType)
listEngineVersions_engineType = Lens.lens (\ListEngineVersions' {engineType} -> engineType) (\s@ListEngineVersions' {} a -> s {engineType = a} :: ListEngineVersions)

-- | The maximum number of objects to return.
listEngineVersions_maxResults :: Lens.Lens' ListEngineVersions (Prelude.Maybe Prelude.Natural)
listEngineVersions_maxResults = Lens.lens (\ListEngineVersions' {maxResults} -> maxResults) (\s@ListEngineVersions' {} a -> s {maxResults = a} :: ListEngineVersions)

instance Core.AWSPager ListEngineVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEngineVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listEngineVersionsResponse_engineVersions
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listEngineVersions_nextToken
          Lens..~ rs
          Lens.^? listEngineVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListEngineVersions where
  type
    AWSResponse ListEngineVersions =
      ListEngineVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEngineVersionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "engineVersions"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListEngineVersions where
  hashWithSalt _salt ListEngineVersions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` engineType
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListEngineVersions where
  rnf ListEngineVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf engineType
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListEngineVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListEngineVersions where
  toPath = Prelude.const "/engine-versions"

instance Data.ToQuery ListEngineVersions where
  toQuery ListEngineVersions' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "engineType" Data.=: engineType,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListEngineVersionsResponse' smart constructor.
data ListEngineVersionsResponse = ListEngineVersionsResponse'
  { -- | If there are more items to return, this contains a token that is passed
    -- to a subsequent call to this operation to retrieve the next set of
    -- items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns the engine versions.
    engineVersions :: [EngineVersionsSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEngineVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEngineVersionsResponse_nextToken' - If there are more items to return, this contains a token that is passed
-- to a subsequent call to this operation to retrieve the next set of
-- items.
--
-- 'httpStatus', 'listEngineVersionsResponse_httpStatus' - The response's http status code.
--
-- 'engineVersions', 'listEngineVersionsResponse_engineVersions' - Returns the engine versions.
newListEngineVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEngineVersionsResponse
newListEngineVersionsResponse pHttpStatus_ =
  ListEngineVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      engineVersions = Prelude.mempty
    }

-- | If there are more items to return, this contains a token that is passed
-- to a subsequent call to this operation to retrieve the next set of
-- items.
listEngineVersionsResponse_nextToken :: Lens.Lens' ListEngineVersionsResponse (Prelude.Maybe Prelude.Text)
listEngineVersionsResponse_nextToken = Lens.lens (\ListEngineVersionsResponse' {nextToken} -> nextToken) (\s@ListEngineVersionsResponse' {} a -> s {nextToken = a} :: ListEngineVersionsResponse)

-- | The response's http status code.
listEngineVersionsResponse_httpStatus :: Lens.Lens' ListEngineVersionsResponse Prelude.Int
listEngineVersionsResponse_httpStatus = Lens.lens (\ListEngineVersionsResponse' {httpStatus} -> httpStatus) (\s@ListEngineVersionsResponse' {} a -> s {httpStatus = a} :: ListEngineVersionsResponse)

-- | Returns the engine versions.
listEngineVersionsResponse_engineVersions :: Lens.Lens' ListEngineVersionsResponse [EngineVersionsSummary]
listEngineVersionsResponse_engineVersions = Lens.lens (\ListEngineVersionsResponse' {engineVersions} -> engineVersions) (\s@ListEngineVersionsResponse' {} a -> s {engineVersions = a} :: ListEngineVersionsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListEngineVersionsResponse where
  rnf ListEngineVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf engineVersions
