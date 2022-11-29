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
-- Module      : Amazonka.GamesParks.ListExtensionVersions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a paginated list of available versions for the extension.
--
-- Each time an API change is made to an extension, the version is
-- incremented. The list retrieved by this operation shows the versions
-- that are currently available.
--
-- This operation returns paginated results.
module Amazonka.GamesParks.ListExtensionVersions
  ( -- * Creating a Request
    ListExtensionVersions (..),
    newListExtensionVersions,

    -- * Request Lenses
    listExtensionVersions_nextToken,
    listExtensionVersions_maxResults,
    listExtensionVersions_name,
    listExtensionVersions_namespace,

    -- * Destructuring the Response
    ListExtensionVersionsResponse (..),
    newListExtensionVersionsResponse,

    -- * Response Lenses
    listExtensionVersionsResponse_nextToken,
    listExtensionVersionsResponse_extensionVersions,
    listExtensionVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GamesParks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListExtensionVersions' smart constructor.
data ListExtensionVersions = ListExtensionVersions'
  { -- | The token that indicates the start of the next sequential page of
    -- results.
    --
    -- Use the token that is returned with a previous call to this operation.
    -- To start at the beginning of the result set, do not specify a value.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    --
    -- Use this parameter with NextToken to get results as a set of sequential
    -- pages.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the extension.
    name :: Prelude.Text,
    -- | The namespace (qualifier) of the extension.
    namespace :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExtensionVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listExtensionVersions_nextToken' - The token that indicates the start of the next sequential page of
-- results.
--
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
--
-- 'maxResults', 'listExtensionVersions_maxResults' - The maximum number of results to return.
--
-- Use this parameter with NextToken to get results as a set of sequential
-- pages.
--
-- 'name', 'listExtensionVersions_name' - The name of the extension.
--
-- 'namespace', 'listExtensionVersions_namespace' - The namespace (qualifier) of the extension.
newListExtensionVersions ::
  -- | 'name'
  Prelude.Text ->
  -- | 'namespace'
  Prelude.Text ->
  ListExtensionVersions
newListExtensionVersions pName_ pNamespace_ =
  ListExtensionVersions'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      name = pName_,
      namespace = pNamespace_
    }

-- | The token that indicates the start of the next sequential page of
-- results.
--
-- Use the token that is returned with a previous call to this operation.
-- To start at the beginning of the result set, do not specify a value.
listExtensionVersions_nextToken :: Lens.Lens' ListExtensionVersions (Prelude.Maybe Prelude.Text)
listExtensionVersions_nextToken = Lens.lens (\ListExtensionVersions' {nextToken} -> nextToken) (\s@ListExtensionVersions' {} a -> s {nextToken = a} :: ListExtensionVersions)

-- | The maximum number of results to return.
--
-- Use this parameter with NextToken to get results as a set of sequential
-- pages.
listExtensionVersions_maxResults :: Lens.Lens' ListExtensionVersions (Prelude.Maybe Prelude.Natural)
listExtensionVersions_maxResults = Lens.lens (\ListExtensionVersions' {maxResults} -> maxResults) (\s@ListExtensionVersions' {} a -> s {maxResults = a} :: ListExtensionVersions)

-- | The name of the extension.
listExtensionVersions_name :: Lens.Lens' ListExtensionVersions Prelude.Text
listExtensionVersions_name = Lens.lens (\ListExtensionVersions' {name} -> name) (\s@ListExtensionVersions' {} a -> s {name = a} :: ListExtensionVersions)

-- | The namespace (qualifier) of the extension.
listExtensionVersions_namespace :: Lens.Lens' ListExtensionVersions Prelude.Text
listExtensionVersions_namespace = Lens.lens (\ListExtensionVersions' {namespace} -> namespace) (\s@ListExtensionVersions' {} a -> s {namespace = a} :: ListExtensionVersions)

instance Core.AWSPager ListExtensionVersions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listExtensionVersionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listExtensionVersionsResponse_extensionVersions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listExtensionVersions_nextToken
          Lens..~ rs
          Lens.^? listExtensionVersionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListExtensionVersions where
  type
    AWSResponse ListExtensionVersions =
      ListExtensionVersionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListExtensionVersionsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ExtensionVersions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListExtensionVersions where
  hashWithSalt _salt ListExtensionVersions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` namespace

instance Prelude.NFData ListExtensionVersions where
  rnf ListExtensionVersions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf namespace

instance Core.ToHeaders ListExtensionVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListExtensionVersions where
  toPath ListExtensionVersions' {..} =
    Prelude.mconcat
      [ "/extension/",
        Core.toBS namespace,
        "/",
        Core.toBS name,
        "/version"
      ]

instance Core.ToQuery ListExtensionVersions where
  toQuery ListExtensionVersions' {..} =
    Prelude.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListExtensionVersionsResponse' smart constructor.
data ListExtensionVersionsResponse = ListExtensionVersionsResponse'
  { -- | The token that indicates the start of the next sequential page of
    -- results.
    --
    -- Use this value when making the next call to this operation to continue
    -- where the last one finished.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of extension versions.
    extensionVersions :: Prelude.Maybe [ExtensionVersionDetails],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExtensionVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listExtensionVersionsResponse_nextToken' - The token that indicates the start of the next sequential page of
-- results.
--
-- Use this value when making the next call to this operation to continue
-- where the last one finished.
--
-- 'extensionVersions', 'listExtensionVersionsResponse_extensionVersions' - The list of extension versions.
--
-- 'httpStatus', 'listExtensionVersionsResponse_httpStatus' - The response's http status code.
newListExtensionVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListExtensionVersionsResponse
newListExtensionVersionsResponse pHttpStatus_ =
  ListExtensionVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      extensionVersions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that indicates the start of the next sequential page of
-- results.
--
-- Use this value when making the next call to this operation to continue
-- where the last one finished.
listExtensionVersionsResponse_nextToken :: Lens.Lens' ListExtensionVersionsResponse (Prelude.Maybe Prelude.Text)
listExtensionVersionsResponse_nextToken = Lens.lens (\ListExtensionVersionsResponse' {nextToken} -> nextToken) (\s@ListExtensionVersionsResponse' {} a -> s {nextToken = a} :: ListExtensionVersionsResponse)

-- | The list of extension versions.
listExtensionVersionsResponse_extensionVersions :: Lens.Lens' ListExtensionVersionsResponse (Prelude.Maybe [ExtensionVersionDetails])
listExtensionVersionsResponse_extensionVersions = Lens.lens (\ListExtensionVersionsResponse' {extensionVersions} -> extensionVersions) (\s@ListExtensionVersionsResponse' {} a -> s {extensionVersions = a} :: ListExtensionVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listExtensionVersionsResponse_httpStatus :: Lens.Lens' ListExtensionVersionsResponse Prelude.Int
listExtensionVersionsResponse_httpStatus = Lens.lens (\ListExtensionVersionsResponse' {httpStatus} -> httpStatus) (\s@ListExtensionVersionsResponse' {} a -> s {httpStatus = a} :: ListExtensionVersionsResponse)

instance Prelude.NFData ListExtensionVersionsResponse where
  rnf ListExtensionVersionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf extensionVersions
      `Prelude.seq` Prelude.rnf httpStatus
