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
-- Module      : Amazonka.Route53.ListCidrBlocks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of location objects and their CIDR blocks.
--
-- This operation returns paginated results.
module Amazonka.Route53.ListCidrBlocks
  ( -- * Creating a Request
    ListCidrBlocks (..),
    newListCidrBlocks,

    -- * Request Lenses
    listCidrBlocks_nextToken,
    listCidrBlocks_maxResults,
    listCidrBlocks_locationName,
    listCidrBlocks_collectionId,

    -- * Destructuring the Response
    ListCidrBlocksResponse (..),
    newListCidrBlocksResponse,

    -- * Response Lenses
    listCidrBlocksResponse_nextToken,
    listCidrBlocksResponse_cidrBlocks,
    listCidrBlocksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | /See:/ 'newListCidrBlocks' smart constructor.
data ListCidrBlocks = ListCidrBlocks'
  { -- | An opaque pagination token to indicate where the service is to begin
    -- enumerating results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of results you want returned.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | The name of the CIDR collection location.
    locationName :: Prelude.Maybe Prelude.Text,
    -- | The UUID of the CIDR collection.
    collectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCidrBlocks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCidrBlocks_nextToken' - An opaque pagination token to indicate where the service is to begin
-- enumerating results.
--
-- 'maxResults', 'listCidrBlocks_maxResults' - Maximum number of results you want returned.
--
-- 'locationName', 'listCidrBlocks_locationName' - The name of the CIDR collection location.
--
-- 'collectionId', 'listCidrBlocks_collectionId' - The UUID of the CIDR collection.
newListCidrBlocks ::
  -- | 'collectionId'
  Prelude.Text ->
  ListCidrBlocks
newListCidrBlocks pCollectionId_ =
  ListCidrBlocks'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      locationName = Prelude.Nothing,
      collectionId = pCollectionId_
    }

-- | An opaque pagination token to indicate where the service is to begin
-- enumerating results.
listCidrBlocks_nextToken :: Lens.Lens' ListCidrBlocks (Prelude.Maybe Prelude.Text)
listCidrBlocks_nextToken = Lens.lens (\ListCidrBlocks' {nextToken} -> nextToken) (\s@ListCidrBlocks' {} a -> s {nextToken = a} :: ListCidrBlocks)

-- | Maximum number of results you want returned.
listCidrBlocks_maxResults :: Lens.Lens' ListCidrBlocks (Prelude.Maybe Prelude.Text)
listCidrBlocks_maxResults = Lens.lens (\ListCidrBlocks' {maxResults} -> maxResults) (\s@ListCidrBlocks' {} a -> s {maxResults = a} :: ListCidrBlocks)

-- | The name of the CIDR collection location.
listCidrBlocks_locationName :: Lens.Lens' ListCidrBlocks (Prelude.Maybe Prelude.Text)
listCidrBlocks_locationName = Lens.lens (\ListCidrBlocks' {locationName} -> locationName) (\s@ListCidrBlocks' {} a -> s {locationName = a} :: ListCidrBlocks)

-- | The UUID of the CIDR collection.
listCidrBlocks_collectionId :: Lens.Lens' ListCidrBlocks Prelude.Text
listCidrBlocks_collectionId = Lens.lens (\ListCidrBlocks' {collectionId} -> collectionId) (\s@ListCidrBlocks' {} a -> s {collectionId = a} :: ListCidrBlocks)

instance Core.AWSPager ListCidrBlocks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCidrBlocksResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCidrBlocksResponse_cidrBlocks
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCidrBlocks_nextToken
          Lens..~ rs
          Lens.^? listCidrBlocksResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListCidrBlocks where
  type
    AWSResponse ListCidrBlocks =
      ListCidrBlocksResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListCidrBlocksResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> ( x Data..@? "CidrBlocks" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCidrBlocks where
  hashWithSalt _salt ListCidrBlocks' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` locationName
      `Prelude.hashWithSalt` collectionId

instance Prelude.NFData ListCidrBlocks where
  rnf ListCidrBlocks' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf locationName
      `Prelude.seq` Prelude.rnf collectionId

instance Data.ToHeaders ListCidrBlocks where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListCidrBlocks where
  toPath ListCidrBlocks' {..} =
    Prelude.mconcat
      [ "/2013-04-01/cidrcollection/",
        Data.toBS collectionId,
        "/cidrblocks"
      ]

instance Data.ToQuery ListCidrBlocks where
  toQuery ListCidrBlocks' {..} =
    Prelude.mconcat
      [ "nexttoken" Data.=: nextToken,
        "maxresults" Data.=: maxResults,
        "location" Data.=: locationName
      ]

-- | /See:/ 'newListCidrBlocksResponse' smart constructor.
data ListCidrBlocksResponse = ListCidrBlocksResponse'
  { -- | An opaque pagination token to indicate where the service is to begin
    -- enumerating results.
    --
    -- If no value is provided, the listing of results starts from the
    -- beginning.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains information about the CIDR blocks.
    cidrBlocks :: Prelude.Maybe [CidrBlockSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCidrBlocksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCidrBlocksResponse_nextToken' - An opaque pagination token to indicate where the service is to begin
-- enumerating results.
--
-- If no value is provided, the listing of results starts from the
-- beginning.
--
-- 'cidrBlocks', 'listCidrBlocksResponse_cidrBlocks' - A complex type that contains information about the CIDR blocks.
--
-- 'httpStatus', 'listCidrBlocksResponse_httpStatus' - The response's http status code.
newListCidrBlocksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCidrBlocksResponse
newListCidrBlocksResponse pHttpStatus_ =
  ListCidrBlocksResponse'
    { nextToken =
        Prelude.Nothing,
      cidrBlocks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An opaque pagination token to indicate where the service is to begin
-- enumerating results.
--
-- If no value is provided, the listing of results starts from the
-- beginning.
listCidrBlocksResponse_nextToken :: Lens.Lens' ListCidrBlocksResponse (Prelude.Maybe Prelude.Text)
listCidrBlocksResponse_nextToken = Lens.lens (\ListCidrBlocksResponse' {nextToken} -> nextToken) (\s@ListCidrBlocksResponse' {} a -> s {nextToken = a} :: ListCidrBlocksResponse)

-- | A complex type that contains information about the CIDR blocks.
listCidrBlocksResponse_cidrBlocks :: Lens.Lens' ListCidrBlocksResponse (Prelude.Maybe [CidrBlockSummary])
listCidrBlocksResponse_cidrBlocks = Lens.lens (\ListCidrBlocksResponse' {cidrBlocks} -> cidrBlocks) (\s@ListCidrBlocksResponse' {} a -> s {cidrBlocks = a} :: ListCidrBlocksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCidrBlocksResponse_httpStatus :: Lens.Lens' ListCidrBlocksResponse Prelude.Int
listCidrBlocksResponse_httpStatus = Lens.lens (\ListCidrBlocksResponse' {httpStatus} -> httpStatus) (\s@ListCidrBlocksResponse' {} a -> s {httpStatus = a} :: ListCidrBlocksResponse)

instance Prelude.NFData ListCidrBlocksResponse where
  rnf ListCidrBlocksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf cidrBlocks
      `Prelude.seq` Prelude.rnf httpStatus
