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
-- Module      : Amazonka.Route53.ListCidrCollections
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of CIDR collections in the Amazon Web Services
-- account (metadata only).
--
-- This operation returns paginated results.
module Amazonka.Route53.ListCidrCollections
  ( -- * Creating a Request
    ListCidrCollections (..),
    newListCidrCollections,

    -- * Request Lenses
    listCidrCollections_nextToken,
    listCidrCollections_maxResults,

    -- * Destructuring the Response
    ListCidrCollectionsResponse (..),
    newListCidrCollectionsResponse,

    -- * Response Lenses
    listCidrCollectionsResponse_nextToken,
    listCidrCollectionsResponse_cidrCollections,
    listCidrCollectionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | /See:/ 'newListCidrCollections' smart constructor.
data ListCidrCollections = ListCidrCollections'
  { -- | An opaque pagination token to indicate where the service is to begin
    -- enumerating results.
    --
    -- If no value is provided, the listing of results starts from the
    -- beginning.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of CIDR collections to return in the response.
    maxResults :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCidrCollections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCidrCollections_nextToken' - An opaque pagination token to indicate where the service is to begin
-- enumerating results.
--
-- If no value is provided, the listing of results starts from the
-- beginning.
--
-- 'maxResults', 'listCidrCollections_maxResults' - The maximum number of CIDR collections to return in the response.
newListCidrCollections ::
  ListCidrCollections
newListCidrCollections =
  ListCidrCollections'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | An opaque pagination token to indicate where the service is to begin
-- enumerating results.
--
-- If no value is provided, the listing of results starts from the
-- beginning.
listCidrCollections_nextToken :: Lens.Lens' ListCidrCollections (Prelude.Maybe Prelude.Text)
listCidrCollections_nextToken = Lens.lens (\ListCidrCollections' {nextToken} -> nextToken) (\s@ListCidrCollections' {} a -> s {nextToken = a} :: ListCidrCollections)

-- | The maximum number of CIDR collections to return in the response.
listCidrCollections_maxResults :: Lens.Lens' ListCidrCollections (Prelude.Maybe Prelude.Text)
listCidrCollections_maxResults = Lens.lens (\ListCidrCollections' {maxResults} -> maxResults) (\s@ListCidrCollections' {} a -> s {maxResults = a} :: ListCidrCollections)

instance Core.AWSPager ListCidrCollections where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCidrCollectionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCidrCollectionsResponse_cidrCollections
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCidrCollections_nextToken
          Lens..~ rs
          Lens.^? listCidrCollectionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListCidrCollections where
  type
    AWSResponse ListCidrCollections =
      ListCidrCollectionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListCidrCollectionsResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> ( x Data..@? "CidrCollections" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCidrCollections where
  hashWithSalt _salt ListCidrCollections' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListCidrCollections where
  rnf ListCidrCollections' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Data.ToHeaders ListCidrCollections where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListCidrCollections where
  toPath = Prelude.const "/2013-04-01/cidrcollection"

instance Data.ToQuery ListCidrCollections where
  toQuery ListCidrCollections' {..} =
    Prelude.mconcat
      [ "nexttoken" Data.=: nextToken,
        "maxresults" Data.=: maxResults
      ]

-- | /See:/ 'newListCidrCollectionsResponse' smart constructor.
data ListCidrCollectionsResponse = ListCidrCollectionsResponse'
  { -- | An opaque pagination token to indicate where the service is to begin
    -- enumerating results.
    --
    -- If no value is provided, the listing of results starts from the
    -- beginning.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A complex type with information about the CIDR collection.
    cidrCollections :: Prelude.Maybe [CollectionSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCidrCollectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCidrCollectionsResponse_nextToken' - An opaque pagination token to indicate where the service is to begin
-- enumerating results.
--
-- If no value is provided, the listing of results starts from the
-- beginning.
--
-- 'cidrCollections', 'listCidrCollectionsResponse_cidrCollections' - A complex type with information about the CIDR collection.
--
-- 'httpStatus', 'listCidrCollectionsResponse_httpStatus' - The response's http status code.
newListCidrCollectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCidrCollectionsResponse
newListCidrCollectionsResponse pHttpStatus_ =
  ListCidrCollectionsResponse'
    { nextToken =
        Prelude.Nothing,
      cidrCollections = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An opaque pagination token to indicate where the service is to begin
-- enumerating results.
--
-- If no value is provided, the listing of results starts from the
-- beginning.
listCidrCollectionsResponse_nextToken :: Lens.Lens' ListCidrCollectionsResponse (Prelude.Maybe Prelude.Text)
listCidrCollectionsResponse_nextToken = Lens.lens (\ListCidrCollectionsResponse' {nextToken} -> nextToken) (\s@ListCidrCollectionsResponse' {} a -> s {nextToken = a} :: ListCidrCollectionsResponse)

-- | A complex type with information about the CIDR collection.
listCidrCollectionsResponse_cidrCollections :: Lens.Lens' ListCidrCollectionsResponse (Prelude.Maybe [CollectionSummary])
listCidrCollectionsResponse_cidrCollections = Lens.lens (\ListCidrCollectionsResponse' {cidrCollections} -> cidrCollections) (\s@ListCidrCollectionsResponse' {} a -> s {cidrCollections = a} :: ListCidrCollectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCidrCollectionsResponse_httpStatus :: Lens.Lens' ListCidrCollectionsResponse Prelude.Int
listCidrCollectionsResponse_httpStatus = Lens.lens (\ListCidrCollectionsResponse' {httpStatus} -> httpStatus) (\s@ListCidrCollectionsResponse' {} a -> s {httpStatus = a} :: ListCidrCollectionsResponse)

instance Prelude.NFData ListCidrCollectionsResponse where
  rnf ListCidrCollectionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf cidrCollections
      `Prelude.seq` Prelude.rnf httpStatus
