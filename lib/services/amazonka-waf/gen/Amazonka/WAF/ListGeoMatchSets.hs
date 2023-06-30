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
-- Module      : Amazonka.WAF.ListGeoMatchSets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is __AWS WAF Classic__ documentation. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Returns an array of GeoMatchSetSummary objects in the response.
--
-- This operation returns paginated results.
module Amazonka.WAF.ListGeoMatchSets
  ( -- * Creating a Request
    ListGeoMatchSets (..),
    newListGeoMatchSets,

    -- * Request Lenses
    listGeoMatchSets_limit,
    listGeoMatchSets_nextMarker,

    -- * Destructuring the Response
    ListGeoMatchSetsResponse (..),
    newListGeoMatchSetsResponse,

    -- * Response Lenses
    listGeoMatchSetsResponse_geoMatchSets,
    listGeoMatchSetsResponse_nextMarker,
    listGeoMatchSetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAF.Types

-- | /See:/ 'newListGeoMatchSets' smart constructor.
data ListGeoMatchSets = ListGeoMatchSets'
  { -- | Specifies the number of @GeoMatchSet@ objects that you want AWS WAF to
    -- return for this request. If you have more @GeoMatchSet@ objects than the
    -- number you specify for @Limit@, the response includes a @NextMarker@
    -- value that you can use to get another batch of @GeoMatchSet@ objects.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | If you specify a value for @Limit@ and you have more @GeoMatchSet@s than
    -- the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
    -- response that allows you to list another group of @GeoMatchSet@ objects.
    -- For the second and subsequent @ListGeoMatchSets@ requests, specify the
    -- value of @NextMarker@ from the previous response to get information
    -- about another batch of @GeoMatchSet@ objects.
    nextMarker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGeoMatchSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listGeoMatchSets_limit' - Specifies the number of @GeoMatchSet@ objects that you want AWS WAF to
-- return for this request. If you have more @GeoMatchSet@ objects than the
-- number you specify for @Limit@, the response includes a @NextMarker@
-- value that you can use to get another batch of @GeoMatchSet@ objects.
--
-- 'nextMarker', 'listGeoMatchSets_nextMarker' - If you specify a value for @Limit@ and you have more @GeoMatchSet@s than
-- the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
-- response that allows you to list another group of @GeoMatchSet@ objects.
-- For the second and subsequent @ListGeoMatchSets@ requests, specify the
-- value of @NextMarker@ from the previous response to get information
-- about another batch of @GeoMatchSet@ objects.
newListGeoMatchSets ::
  ListGeoMatchSets
newListGeoMatchSets =
  ListGeoMatchSets'
    { limit = Prelude.Nothing,
      nextMarker = Prelude.Nothing
    }

-- | Specifies the number of @GeoMatchSet@ objects that you want AWS WAF to
-- return for this request. If you have more @GeoMatchSet@ objects than the
-- number you specify for @Limit@, the response includes a @NextMarker@
-- value that you can use to get another batch of @GeoMatchSet@ objects.
listGeoMatchSets_limit :: Lens.Lens' ListGeoMatchSets (Prelude.Maybe Prelude.Natural)
listGeoMatchSets_limit = Lens.lens (\ListGeoMatchSets' {limit} -> limit) (\s@ListGeoMatchSets' {} a -> s {limit = a} :: ListGeoMatchSets)

-- | If you specify a value for @Limit@ and you have more @GeoMatchSet@s than
-- the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
-- response that allows you to list another group of @GeoMatchSet@ objects.
-- For the second and subsequent @ListGeoMatchSets@ requests, specify the
-- value of @NextMarker@ from the previous response to get information
-- about another batch of @GeoMatchSet@ objects.
listGeoMatchSets_nextMarker :: Lens.Lens' ListGeoMatchSets (Prelude.Maybe Prelude.Text)
listGeoMatchSets_nextMarker = Lens.lens (\ListGeoMatchSets' {nextMarker} -> nextMarker) (\s@ListGeoMatchSets' {} a -> s {nextMarker = a} :: ListGeoMatchSets)

instance Core.AWSPager ListGeoMatchSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listGeoMatchSetsResponse_nextMarker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listGeoMatchSetsResponse_geoMatchSets
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listGeoMatchSets_nextMarker
          Lens..~ rs
          Lens.^? listGeoMatchSetsResponse_nextMarker
          Prelude.. Lens._Just

instance Core.AWSRequest ListGeoMatchSets where
  type
    AWSResponse ListGeoMatchSets =
      ListGeoMatchSetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGeoMatchSetsResponse'
            Prelude.<$> (x Data..?> "GeoMatchSets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListGeoMatchSets where
  hashWithSalt _salt ListGeoMatchSets' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextMarker

instance Prelude.NFData ListGeoMatchSets where
  rnf ListGeoMatchSets' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextMarker

instance Data.ToHeaders ListGeoMatchSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20150824.ListGeoMatchSets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListGeoMatchSets where
  toJSON ListGeoMatchSets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextMarker" Data..=) Prelude.<$> nextMarker
          ]
      )

instance Data.ToPath ListGeoMatchSets where
  toPath = Prelude.const "/"

instance Data.ToQuery ListGeoMatchSets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListGeoMatchSetsResponse' smart constructor.
data ListGeoMatchSetsResponse = ListGeoMatchSetsResponse'
  { -- | An array of GeoMatchSetSummary objects.
    geoMatchSets :: Prelude.Maybe [GeoMatchSetSummary],
    -- | If you have more @GeoMatchSet@ objects than the number that you
    -- specified for @Limit@ in the request, the response includes a
    -- @NextMarker@ value. To list more @GeoMatchSet@ objects, submit another
    -- @ListGeoMatchSets@ request, and specify the @NextMarker@ value from the
    -- response in the @NextMarker@ value in the next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListGeoMatchSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'geoMatchSets', 'listGeoMatchSetsResponse_geoMatchSets' - An array of GeoMatchSetSummary objects.
--
-- 'nextMarker', 'listGeoMatchSetsResponse_nextMarker' - If you have more @GeoMatchSet@ objects than the number that you
-- specified for @Limit@ in the request, the response includes a
-- @NextMarker@ value. To list more @GeoMatchSet@ objects, submit another
-- @ListGeoMatchSets@ request, and specify the @NextMarker@ value from the
-- response in the @NextMarker@ value in the next request.
--
-- 'httpStatus', 'listGeoMatchSetsResponse_httpStatus' - The response's http status code.
newListGeoMatchSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListGeoMatchSetsResponse
newListGeoMatchSetsResponse pHttpStatus_ =
  ListGeoMatchSetsResponse'
    { geoMatchSets =
        Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of GeoMatchSetSummary objects.
listGeoMatchSetsResponse_geoMatchSets :: Lens.Lens' ListGeoMatchSetsResponse (Prelude.Maybe [GeoMatchSetSummary])
listGeoMatchSetsResponse_geoMatchSets = Lens.lens (\ListGeoMatchSetsResponse' {geoMatchSets} -> geoMatchSets) (\s@ListGeoMatchSetsResponse' {} a -> s {geoMatchSets = a} :: ListGeoMatchSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If you have more @GeoMatchSet@ objects than the number that you
-- specified for @Limit@ in the request, the response includes a
-- @NextMarker@ value. To list more @GeoMatchSet@ objects, submit another
-- @ListGeoMatchSets@ request, and specify the @NextMarker@ value from the
-- response in the @NextMarker@ value in the next request.
listGeoMatchSetsResponse_nextMarker :: Lens.Lens' ListGeoMatchSetsResponse (Prelude.Maybe Prelude.Text)
listGeoMatchSetsResponse_nextMarker = Lens.lens (\ListGeoMatchSetsResponse' {nextMarker} -> nextMarker) (\s@ListGeoMatchSetsResponse' {} a -> s {nextMarker = a} :: ListGeoMatchSetsResponse)

-- | The response's http status code.
listGeoMatchSetsResponse_httpStatus :: Lens.Lens' ListGeoMatchSetsResponse Prelude.Int
listGeoMatchSetsResponse_httpStatus = Lens.lens (\ListGeoMatchSetsResponse' {httpStatus} -> httpStatus) (\s@ListGeoMatchSetsResponse' {} a -> s {httpStatus = a} :: ListGeoMatchSetsResponse)

instance Prelude.NFData ListGeoMatchSetsResponse where
  rnf ListGeoMatchSetsResponse' {..} =
    Prelude.rnf geoMatchSets
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
