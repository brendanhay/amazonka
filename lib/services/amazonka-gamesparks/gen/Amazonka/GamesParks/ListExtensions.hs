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
-- Module      : Amazonka.GamesParks.ListExtensions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a paginated list of available extensions.
--
-- Extensions provide features that games can use from scripts.
--
-- This operation returns paginated results.
module Amazonka.GamesParks.ListExtensions
  ( -- * Creating a Request
    ListExtensions (..),
    newListExtensions,

    -- * Request Lenses
    listExtensions_maxResults,
    listExtensions_nextToken,

    -- * Destructuring the Response
    ListExtensionsResponse (..),
    newListExtensionsResponse,

    -- * Response Lenses
    listExtensionsResponse_extensions,
    listExtensionsResponse_nextToken,
    listExtensionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListExtensions' smart constructor.
data ListExtensions = ListExtensions'
  { -- | The maximum number of results to return.
    --
    -- Use this parameter with NextToken to get results as a set of sequential
    -- pages.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that indicates the start of the next sequential page of
    -- results.
    --
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExtensions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listExtensions_maxResults' - The maximum number of results to return.
--
-- Use this parameter with NextToken to get results as a set of sequential
-- pages.
--
-- 'nextToken', 'listExtensions_nextToken' - The token that indicates the start of the next sequential page of
-- results.
--
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
newListExtensions ::
  ListExtensions
newListExtensions =
  ListExtensions'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return.
--
-- Use this parameter with NextToken to get results as a set of sequential
-- pages.
listExtensions_maxResults :: Lens.Lens' ListExtensions (Prelude.Maybe Prelude.Natural)
listExtensions_maxResults = Lens.lens (\ListExtensions' {maxResults} -> maxResults) (\s@ListExtensions' {} a -> s {maxResults = a} :: ListExtensions)

-- | The token that indicates the start of the next sequential page of
-- results.
--
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
listExtensions_nextToken :: Lens.Lens' ListExtensions (Prelude.Maybe Prelude.Text)
listExtensions_nextToken = Lens.lens (\ListExtensions' {nextToken} -> nextToken) (\s@ListExtensions' {} a -> s {nextToken = a} :: ListExtensions)

instance Core.AWSPager ListExtensions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listExtensionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listExtensionsResponse_extensions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listExtensions_nextToken
          Lens..~ rs
          Lens.^? listExtensionsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListExtensions where
  type
    AWSResponse ListExtensions =
      ListExtensionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListExtensionsResponse'
            Prelude.<$> (x Data..?> "Extensions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListExtensions where
  hashWithSalt _salt ListExtensions' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListExtensions where
  rnf ListExtensions' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListExtensions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListExtensions where
  toPath = Prelude.const "/extension"

instance Data.ToQuery ListExtensions where
  toQuery ListExtensions' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListExtensionsResponse' smart constructor.
data ListExtensionsResponse = ListExtensionsResponse'
  { -- | The list of extensions.
    extensions :: Prelude.Maybe [ExtensionDetails],
    -- | The token that indicates the start of the next sequential page of
    -- results.
    --
    -- Use this value when making the next call to this operation to continue
    -- where the last one finished.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExtensionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'extensions', 'listExtensionsResponse_extensions' - The list of extensions.
--
-- 'nextToken', 'listExtensionsResponse_nextToken' - The token that indicates the start of the next sequential page of
-- results.
--
-- Use this value when making the next call to this operation to continue
-- where the last one finished.
--
-- 'httpStatus', 'listExtensionsResponse_httpStatus' - The response's http status code.
newListExtensionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListExtensionsResponse
newListExtensionsResponse pHttpStatus_ =
  ListExtensionsResponse'
    { extensions =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of extensions.
listExtensionsResponse_extensions :: Lens.Lens' ListExtensionsResponse (Prelude.Maybe [ExtensionDetails])
listExtensionsResponse_extensions = Lens.lens (\ListExtensionsResponse' {extensions} -> extensions) (\s@ListExtensionsResponse' {} a -> s {extensions = a} :: ListExtensionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token that indicates the start of the next sequential page of
-- results.
--
-- Use this value when making the next call to this operation to continue
-- where the last one finished.
listExtensionsResponse_nextToken :: Lens.Lens' ListExtensionsResponse (Prelude.Maybe Prelude.Text)
listExtensionsResponse_nextToken = Lens.lens (\ListExtensionsResponse' {nextToken} -> nextToken) (\s@ListExtensionsResponse' {} a -> s {nextToken = a} :: ListExtensionsResponse)

-- | The response's http status code.
listExtensionsResponse_httpStatus :: Lens.Lens' ListExtensionsResponse Prelude.Int
listExtensionsResponse_httpStatus = Lens.lens (\ListExtensionsResponse' {httpStatus} -> httpStatus) (\s@ListExtensionsResponse' {} a -> s {httpStatus = a} :: ListExtensionsResponse)

instance Prelude.NFData ListExtensionsResponse where
  rnf ListExtensionsResponse' {..} =
    Prelude.rnf extensions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
