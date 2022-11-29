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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    listCidrLocations_nextToken,
    listCidrLocations_maxResults,
    listCidrLocations_collectionId,

    -- * Destructuring the Response
    ListCidrLocationsResponse (..),
    newListCidrLocationsResponse,

    -- * Response Lenses
    listCidrLocationsResponse_nextToken,
    listCidrLocationsResponse_cidrLocations,
    listCidrLocationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | /See:/ 'newListCidrLocations' smart constructor.
data ListCidrLocations = ListCidrLocations'
  { -- | An opaque pagination token to indicate where the service is to begin
    -- enumerating results.
    --
    -- If no value is provided, the listing of results starts from the
    -- beginning.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of CIDR collection locations to return in the
    -- response.
    maxResults :: Prelude.Maybe Prelude.Text,
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
-- 'nextToken', 'listCidrLocations_nextToken' - An opaque pagination token to indicate where the service is to begin
-- enumerating results.
--
-- If no value is provided, the listing of results starts from the
-- beginning.
--
-- 'maxResults', 'listCidrLocations_maxResults' - The maximum number of CIDR collection locations to return in the
-- response.
--
-- 'collectionId', 'listCidrLocations_collectionId' - The CIDR collection ID.
newListCidrLocations ::
  -- | 'collectionId'
  Prelude.Text ->
  ListCidrLocations
newListCidrLocations pCollectionId_ =
  ListCidrLocations'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      collectionId = pCollectionId_
    }

-- | An opaque pagination token to indicate where the service is to begin
-- enumerating results.
--
-- If no value is provided, the listing of results starts from the
-- beginning.
listCidrLocations_nextToken :: Lens.Lens' ListCidrLocations (Prelude.Maybe Prelude.Text)
listCidrLocations_nextToken = Lens.lens (\ListCidrLocations' {nextToken} -> nextToken) (\s@ListCidrLocations' {} a -> s {nextToken = a} :: ListCidrLocations)

-- | The maximum number of CIDR collection locations to return in the
-- response.
listCidrLocations_maxResults :: Lens.Lens' ListCidrLocations (Prelude.Maybe Prelude.Text)
listCidrLocations_maxResults = Lens.lens (\ListCidrLocations' {maxResults} -> maxResults) (\s@ListCidrLocations' {} a -> s {maxResults = a} :: ListCidrLocations)

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
            Prelude.<$> (x Core..@? "NextToken")
            Prelude.<*> ( x Core..@? "CidrLocations" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCidrLocations where
  hashWithSalt _salt ListCidrLocations' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` collectionId

instance Prelude.NFData ListCidrLocations where
  rnf ListCidrLocations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf collectionId

instance Core.ToHeaders ListCidrLocations where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListCidrLocations where
  toPath ListCidrLocations' {..} =
    Prelude.mconcat
      [ "/2013-04-01/cidrcollection/",
        Core.toBS collectionId
      ]

instance Core.ToQuery ListCidrLocations where
  toQuery ListCidrLocations' {..} =
    Prelude.mconcat
      [ "nexttoken" Core.=: nextToken,
        "maxresults" Core.=: maxResults
      ]

-- | /See:/ 'newListCidrLocationsResponse' smart constructor.
data ListCidrLocationsResponse = ListCidrLocationsResponse'
  { -- | An opaque pagination token to indicate where the service is to begin
    -- enumerating results.
    --
    -- If no value is provided, the listing of results starts from the
    -- beginning.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A complex type that contains information about the list of CIDR
    -- locations.
    cidrLocations :: Prelude.Maybe [LocationSummary],
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
-- 'nextToken', 'listCidrLocationsResponse_nextToken' - An opaque pagination token to indicate where the service is to begin
-- enumerating results.
--
-- If no value is provided, the listing of results starts from the
-- beginning.
--
-- 'cidrLocations', 'listCidrLocationsResponse_cidrLocations' - A complex type that contains information about the list of CIDR
-- locations.
--
-- 'httpStatus', 'listCidrLocationsResponse_httpStatus' - The response's http status code.
newListCidrLocationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCidrLocationsResponse
newListCidrLocationsResponse pHttpStatus_ =
  ListCidrLocationsResponse'
    { nextToken =
        Prelude.Nothing,
      cidrLocations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An opaque pagination token to indicate where the service is to begin
-- enumerating results.
--
-- If no value is provided, the listing of results starts from the
-- beginning.
listCidrLocationsResponse_nextToken :: Lens.Lens' ListCidrLocationsResponse (Prelude.Maybe Prelude.Text)
listCidrLocationsResponse_nextToken = Lens.lens (\ListCidrLocationsResponse' {nextToken} -> nextToken) (\s@ListCidrLocationsResponse' {} a -> s {nextToken = a} :: ListCidrLocationsResponse)

-- | A complex type that contains information about the list of CIDR
-- locations.
listCidrLocationsResponse_cidrLocations :: Lens.Lens' ListCidrLocationsResponse (Prelude.Maybe [LocationSummary])
listCidrLocationsResponse_cidrLocations = Lens.lens (\ListCidrLocationsResponse' {cidrLocations} -> cidrLocations) (\s@ListCidrLocationsResponse' {} a -> s {cidrLocations = a} :: ListCidrLocationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCidrLocationsResponse_httpStatus :: Lens.Lens' ListCidrLocationsResponse Prelude.Int
listCidrLocationsResponse_httpStatus = Lens.lens (\ListCidrLocationsResponse' {httpStatus} -> httpStatus) (\s@ListCidrLocationsResponse' {} a -> s {httpStatus = a} :: ListCidrLocationsResponse)

instance Prelude.NFData ListCidrLocationsResponse where
  rnf ListCidrLocationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf cidrLocations
      `Prelude.seq` Prelude.rnf httpStatus
