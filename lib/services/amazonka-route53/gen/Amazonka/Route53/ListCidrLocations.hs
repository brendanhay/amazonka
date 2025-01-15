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
-- Module      : Amazonka.Route53.ListCidrLocations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of CIDR locations for the given collection
-- (metadata only, does not include CIDR blocks).
--
-- This operation returns paginated results.
module Amazonka.Route53.ListCidrLocations
  ( -- * Creating a Request
    ListCidrLocations (..),
    newListCidrLocations,

    -- * Request Lenses
    listCidrLocations_maxResults,
    listCidrLocations_nextToken,
    listCidrLocations_collectionId,

    -- * Destructuring the Response
    ListCidrLocationsResponse (..),
    newListCidrLocationsResponse,

    -- * Response Lenses
    listCidrLocationsResponse_cidrLocations,
    listCidrLocationsResponse_nextToken,
    listCidrLocationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | /See:/ 'newListCidrLocations' smart constructor.
data ListCidrLocations = ListCidrLocations'
  { -- | The maximum number of CIDR collection locations to return in the
    -- response.
    maxResults :: Prelude.Maybe Prelude.Text,
    -- | An opaque pagination token to indicate where the service is to begin
    -- enumerating results.
    --
    -- If no value is provided, the listing of results starts from the
    -- beginning.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The CIDR collection ID.
    collectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCidrLocations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listCidrLocations_maxResults' - The maximum number of CIDR collection locations to return in the
-- response.
--
-- 'nextToken', 'listCidrLocations_nextToken' - An opaque pagination token to indicate where the service is to begin
-- enumerating results.
--
-- If no value is provided, the listing of results starts from the
-- beginning.
--
-- 'collectionId', 'listCidrLocations_collectionId' - The CIDR collection ID.
newListCidrLocations ::
  -- | 'collectionId'
  Prelude.Text ->
  ListCidrLocations
newListCidrLocations pCollectionId_ =
  ListCidrLocations'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      collectionId = pCollectionId_
    }

-- | The maximum number of CIDR collection locations to return in the
-- response.
listCidrLocations_maxResults :: Lens.Lens' ListCidrLocations (Prelude.Maybe Prelude.Text)
listCidrLocations_maxResults = Lens.lens (\ListCidrLocations' {maxResults} -> maxResults) (\s@ListCidrLocations' {} a -> s {maxResults = a} :: ListCidrLocations)

-- | An opaque pagination token to indicate where the service is to begin
-- enumerating results.
--
-- If no value is provided, the listing of results starts from the
-- beginning.
listCidrLocations_nextToken :: Lens.Lens' ListCidrLocations (Prelude.Maybe Prelude.Text)
listCidrLocations_nextToken = Lens.lens (\ListCidrLocations' {nextToken} -> nextToken) (\s@ListCidrLocations' {} a -> s {nextToken = a} :: ListCidrLocations)

-- | The CIDR collection ID.
listCidrLocations_collectionId :: Lens.Lens' ListCidrLocations Prelude.Text
listCidrLocations_collectionId = Lens.lens (\ListCidrLocations' {collectionId} -> collectionId) (\s@ListCidrLocations' {} a -> s {collectionId = a} :: ListCidrLocations)

instance Core.AWSPager ListCidrLocations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCidrLocationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCidrLocationsResponse_cidrLocations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listCidrLocations_nextToken
              Lens..~ rs
              Lens.^? listCidrLocationsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListCidrLocations where
  type
    AWSResponse ListCidrLocations =
      ListCidrLocationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ListCidrLocationsResponse'
            Prelude.<$> ( x Data..@? "CidrLocations" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCidrLocations where
  hashWithSalt _salt ListCidrLocations' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` collectionId

instance Prelude.NFData ListCidrLocations where
  rnf ListCidrLocations' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf collectionId

instance Data.ToHeaders ListCidrLocations where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListCidrLocations where
  toPath ListCidrLocations' {..} =
    Prelude.mconcat
      [ "/2013-04-01/cidrcollection/",
        Data.toBS collectionId
      ]

instance Data.ToQuery ListCidrLocations where
  toQuery ListCidrLocations' {..} =
    Prelude.mconcat
      [ "maxresults" Data.=: maxResults,
        "nexttoken" Data.=: nextToken
      ]

-- | /See:/ 'newListCidrLocationsResponse' smart constructor.
data ListCidrLocationsResponse = ListCidrLocationsResponse'
  { -- | A complex type that contains information about the list of CIDR
    -- locations.
    cidrLocations :: Prelude.Maybe [LocationSummary],
    -- | An opaque pagination token to indicate where the service is to begin
    -- enumerating results.
    --
    -- If no value is provided, the listing of results starts from the
    -- beginning.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCidrLocationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrLocations', 'listCidrLocationsResponse_cidrLocations' - A complex type that contains information about the list of CIDR
-- locations.
--
-- 'nextToken', 'listCidrLocationsResponse_nextToken' - An opaque pagination token to indicate where the service is to begin
-- enumerating results.
--
-- If no value is provided, the listing of results starts from the
-- beginning.
--
-- 'httpStatus', 'listCidrLocationsResponse_httpStatus' - The response's http status code.
newListCidrLocationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCidrLocationsResponse
newListCidrLocationsResponse pHttpStatus_ =
  ListCidrLocationsResponse'
    { cidrLocations =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A complex type that contains information about the list of CIDR
-- locations.
listCidrLocationsResponse_cidrLocations :: Lens.Lens' ListCidrLocationsResponse (Prelude.Maybe [LocationSummary])
listCidrLocationsResponse_cidrLocations = Lens.lens (\ListCidrLocationsResponse' {cidrLocations} -> cidrLocations) (\s@ListCidrLocationsResponse' {} a -> s {cidrLocations = a} :: ListCidrLocationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An opaque pagination token to indicate where the service is to begin
-- enumerating results.
--
-- If no value is provided, the listing of results starts from the
-- beginning.
listCidrLocationsResponse_nextToken :: Lens.Lens' ListCidrLocationsResponse (Prelude.Maybe Prelude.Text)
listCidrLocationsResponse_nextToken = Lens.lens (\ListCidrLocationsResponse' {nextToken} -> nextToken) (\s@ListCidrLocationsResponse' {} a -> s {nextToken = a} :: ListCidrLocationsResponse)

-- | The response's http status code.
listCidrLocationsResponse_httpStatus :: Lens.Lens' ListCidrLocationsResponse Prelude.Int
listCidrLocationsResponse_httpStatus = Lens.lens (\ListCidrLocationsResponse' {httpStatus} -> httpStatus) (\s@ListCidrLocationsResponse' {} a -> s {httpStatus = a} :: ListCidrLocationsResponse)

instance Prelude.NFData ListCidrLocationsResponse where
  rnf ListCidrLocationsResponse' {..} =
    Prelude.rnf cidrLocations `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
