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
-- Module      : Network.AWS.WAFRegional.ListGeoMatchSets
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.WAFRegional.ListGeoMatchSets
  ( -- * Creating a Request
    ListGeoMatchSets (..),
    newListGeoMatchSets,

    -- * Request Lenses
    listGeoMatchSets_nextMarker,
    listGeoMatchSets_limit,

    -- * Destructuring the Response
    ListGeoMatchSetsResponse (..),
    newListGeoMatchSetsResponse,

    -- * Response Lenses
    listGeoMatchSetsResponse_geoMatchSets,
    listGeoMatchSetsResponse_nextMarker,
    listGeoMatchSetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAFRegional.Types

-- | /See:/ 'newListGeoMatchSets' smart constructor.
data ListGeoMatchSets = ListGeoMatchSets'
  { -- | If you specify a value for @Limit@ and you have more @GeoMatchSet@s than
    -- the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
    -- response that allows you to list another group of @GeoMatchSet@ objects.
    -- For the second and subsequent @ListGeoMatchSets@ requests, specify the
    -- value of @NextMarker@ from the previous response to get information
    -- about another batch of @GeoMatchSet@ objects.
    nextMarker :: Core.Maybe Core.Text,
    -- | Specifies the number of @GeoMatchSet@ objects that you want AWS WAF to
    -- return for this request. If you have more @GeoMatchSet@ objects than the
    -- number you specify for @Limit@, the response includes a @NextMarker@
    -- value that you can use to get another batch of @GeoMatchSet@ objects.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListGeoMatchSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listGeoMatchSets_nextMarker' - If you specify a value for @Limit@ and you have more @GeoMatchSet@s than
-- the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
-- response that allows you to list another group of @GeoMatchSet@ objects.
-- For the second and subsequent @ListGeoMatchSets@ requests, specify the
-- value of @NextMarker@ from the previous response to get information
-- about another batch of @GeoMatchSet@ objects.
--
-- 'limit', 'listGeoMatchSets_limit' - Specifies the number of @GeoMatchSet@ objects that you want AWS WAF to
-- return for this request. If you have more @GeoMatchSet@ objects than the
-- number you specify for @Limit@, the response includes a @NextMarker@
-- value that you can use to get another batch of @GeoMatchSet@ objects.
newListGeoMatchSets ::
  ListGeoMatchSets
newListGeoMatchSets =
  ListGeoMatchSets'
    { nextMarker = Core.Nothing,
      limit = Core.Nothing
    }

-- | If you specify a value for @Limit@ and you have more @GeoMatchSet@s than
-- the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
-- response that allows you to list another group of @GeoMatchSet@ objects.
-- For the second and subsequent @ListGeoMatchSets@ requests, specify the
-- value of @NextMarker@ from the previous response to get information
-- about another batch of @GeoMatchSet@ objects.
listGeoMatchSets_nextMarker :: Lens.Lens' ListGeoMatchSets (Core.Maybe Core.Text)
listGeoMatchSets_nextMarker = Lens.lens (\ListGeoMatchSets' {nextMarker} -> nextMarker) (\s@ListGeoMatchSets' {} a -> s {nextMarker = a} :: ListGeoMatchSets)

-- | Specifies the number of @GeoMatchSet@ objects that you want AWS WAF to
-- return for this request. If you have more @GeoMatchSet@ objects than the
-- number you specify for @Limit@, the response includes a @NextMarker@
-- value that you can use to get another batch of @GeoMatchSet@ objects.
listGeoMatchSets_limit :: Lens.Lens' ListGeoMatchSets (Core.Maybe Core.Natural)
listGeoMatchSets_limit = Lens.lens (\ListGeoMatchSets' {limit} -> limit) (\s@ListGeoMatchSets' {} a -> s {limit = a} :: ListGeoMatchSets)

instance Core.AWSRequest ListGeoMatchSets where
  type
    AWSResponse ListGeoMatchSets =
      ListGeoMatchSetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGeoMatchSetsResponse'
            Core.<$> (x Core..?> "GeoMatchSets" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListGeoMatchSets

instance Core.NFData ListGeoMatchSets

instance Core.ToHeaders ListGeoMatchSets where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_Regional_20161128.ListGeoMatchSets" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListGeoMatchSets where
  toJSON ListGeoMatchSets' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextMarker" Core..=) Core.<$> nextMarker,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath ListGeoMatchSets where
  toPath = Core.const "/"

instance Core.ToQuery ListGeoMatchSets where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListGeoMatchSetsResponse' smart constructor.
data ListGeoMatchSetsResponse = ListGeoMatchSetsResponse'
  { -- | An array of GeoMatchSetSummary objects.
    geoMatchSets :: Core.Maybe [GeoMatchSetSummary],
    -- | If you have more @GeoMatchSet@ objects than the number that you
    -- specified for @Limit@ in the request, the response includes a
    -- @NextMarker@ value. To list more @GeoMatchSet@ objects, submit another
    -- @ListGeoMatchSets@ request, and specify the @NextMarker@ value from the
    -- response in the @NextMarker@ value in the next request.
    nextMarker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListGeoMatchSetsResponse
newListGeoMatchSetsResponse pHttpStatus_ =
  ListGeoMatchSetsResponse'
    { geoMatchSets =
        Core.Nothing,
      nextMarker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of GeoMatchSetSummary objects.
listGeoMatchSetsResponse_geoMatchSets :: Lens.Lens' ListGeoMatchSetsResponse (Core.Maybe [GeoMatchSetSummary])
listGeoMatchSetsResponse_geoMatchSets = Lens.lens (\ListGeoMatchSetsResponse' {geoMatchSets} -> geoMatchSets) (\s@ListGeoMatchSetsResponse' {} a -> s {geoMatchSets = a} :: ListGeoMatchSetsResponse) Core.. Lens.mapping Lens._Coerce

-- | If you have more @GeoMatchSet@ objects than the number that you
-- specified for @Limit@ in the request, the response includes a
-- @NextMarker@ value. To list more @GeoMatchSet@ objects, submit another
-- @ListGeoMatchSets@ request, and specify the @NextMarker@ value from the
-- response in the @NextMarker@ value in the next request.
listGeoMatchSetsResponse_nextMarker :: Lens.Lens' ListGeoMatchSetsResponse (Core.Maybe Core.Text)
listGeoMatchSetsResponse_nextMarker = Lens.lens (\ListGeoMatchSetsResponse' {nextMarker} -> nextMarker) (\s@ListGeoMatchSetsResponse' {} a -> s {nextMarker = a} :: ListGeoMatchSetsResponse)

-- | The response's http status code.
listGeoMatchSetsResponse_httpStatus :: Lens.Lens' ListGeoMatchSetsResponse Core.Int
listGeoMatchSetsResponse_httpStatus = Lens.lens (\ListGeoMatchSetsResponse' {httpStatus} -> httpStatus) (\s@ListGeoMatchSetsResponse' {} a -> s {httpStatus = a} :: ListGeoMatchSetsResponse)

instance Core.NFData ListGeoMatchSetsResponse
