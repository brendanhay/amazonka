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
-- Module      : Network.AWS.WAF.ListXssMatchSets
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
-- Returns an array of XssMatchSet objects.
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListXssMatchSets
  ( -- * Creating a Request
    ListXssMatchSets (..),
    newListXssMatchSets,

    -- * Request Lenses
    listXssMatchSets_nextMarker,
    listXssMatchSets_limit,

    -- * Destructuring the Response
    ListXssMatchSetsResponse (..),
    newListXssMatchSetsResponse,

    -- * Response Lenses
    listXssMatchSetsResponse_nextMarker,
    listXssMatchSetsResponse_xssMatchSets,
    listXssMatchSetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | A request to list the XssMatchSet objects created by the current AWS
-- account.
--
-- /See:/ 'newListXssMatchSets' smart constructor.
data ListXssMatchSets = ListXssMatchSets'
  { -- | If you specify a value for @Limit@ and you have more XssMatchSet objects
    -- than the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
    -- response that allows you to list another group of @XssMatchSets@. For
    -- the second and subsequent @ListXssMatchSets@ requests, specify the value
    -- of @NextMarker@ from the previous response to get information about
    -- another batch of @XssMatchSets@.
    nextMarker :: Core.Maybe Core.Text,
    -- | Specifies the number of XssMatchSet objects that you want AWS WAF to
    -- return for this request. If you have more @XssMatchSet@ objects than the
    -- number you specify for @Limit@, the response includes a @NextMarker@
    -- value that you can use to get another batch of @Rules@.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListXssMatchSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listXssMatchSets_nextMarker' - If you specify a value for @Limit@ and you have more XssMatchSet objects
-- than the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
-- response that allows you to list another group of @XssMatchSets@. For
-- the second and subsequent @ListXssMatchSets@ requests, specify the value
-- of @NextMarker@ from the previous response to get information about
-- another batch of @XssMatchSets@.
--
-- 'limit', 'listXssMatchSets_limit' - Specifies the number of XssMatchSet objects that you want AWS WAF to
-- return for this request. If you have more @XssMatchSet@ objects than the
-- number you specify for @Limit@, the response includes a @NextMarker@
-- value that you can use to get another batch of @Rules@.
newListXssMatchSets ::
  ListXssMatchSets
newListXssMatchSets =
  ListXssMatchSets'
    { nextMarker = Core.Nothing,
      limit = Core.Nothing
    }

-- | If you specify a value for @Limit@ and you have more XssMatchSet objects
-- than the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
-- response that allows you to list another group of @XssMatchSets@. For
-- the second and subsequent @ListXssMatchSets@ requests, specify the value
-- of @NextMarker@ from the previous response to get information about
-- another batch of @XssMatchSets@.
listXssMatchSets_nextMarker :: Lens.Lens' ListXssMatchSets (Core.Maybe Core.Text)
listXssMatchSets_nextMarker = Lens.lens (\ListXssMatchSets' {nextMarker} -> nextMarker) (\s@ListXssMatchSets' {} a -> s {nextMarker = a} :: ListXssMatchSets)

-- | Specifies the number of XssMatchSet objects that you want AWS WAF to
-- return for this request. If you have more @XssMatchSet@ objects than the
-- number you specify for @Limit@, the response includes a @NextMarker@
-- value that you can use to get another batch of @Rules@.
listXssMatchSets_limit :: Lens.Lens' ListXssMatchSets (Core.Maybe Core.Natural)
listXssMatchSets_limit = Lens.lens (\ListXssMatchSets' {limit} -> limit) (\s@ListXssMatchSets' {} a -> s {limit = a} :: ListXssMatchSets)

instance Core.AWSPager ListXssMatchSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listXssMatchSetsResponse_nextMarker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listXssMatchSetsResponse_xssMatchSets
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listXssMatchSets_nextMarker
          Lens..~ rs
          Lens.^? listXssMatchSetsResponse_nextMarker Core.. Lens._Just

instance Core.AWSRequest ListXssMatchSets where
  type
    AWSResponse ListXssMatchSets =
      ListXssMatchSetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListXssMatchSetsResponse'
            Core.<$> (x Core..?> "NextMarker")
            Core.<*> (x Core..?> "XssMatchSets" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListXssMatchSets

instance Core.NFData ListXssMatchSets

instance Core.ToHeaders ListXssMatchSets where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.ListXssMatchSets" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListXssMatchSets where
  toJSON ListXssMatchSets' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextMarker" Core..=) Core.<$> nextMarker,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath ListXssMatchSets where
  toPath = Core.const "/"

instance Core.ToQuery ListXssMatchSets where
  toQuery = Core.const Core.mempty

-- | The response to a ListXssMatchSets request.
--
-- /See:/ 'newListXssMatchSetsResponse' smart constructor.
data ListXssMatchSetsResponse = ListXssMatchSetsResponse'
  { -- | If you have more XssMatchSet objects than the number that you specified
    -- for @Limit@ in the request, the response includes a @NextMarker@ value.
    -- To list more @XssMatchSet@ objects, submit another @ListXssMatchSets@
    -- request, and specify the @NextMarker@ value from the response in the
    -- @NextMarker@ value in the next request.
    nextMarker :: Core.Maybe Core.Text,
    -- | An array of XssMatchSetSummary objects.
    xssMatchSets :: Core.Maybe [XssMatchSetSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListXssMatchSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listXssMatchSetsResponse_nextMarker' - If you have more XssMatchSet objects than the number that you specified
-- for @Limit@ in the request, the response includes a @NextMarker@ value.
-- To list more @XssMatchSet@ objects, submit another @ListXssMatchSets@
-- request, and specify the @NextMarker@ value from the response in the
-- @NextMarker@ value in the next request.
--
-- 'xssMatchSets', 'listXssMatchSetsResponse_xssMatchSets' - An array of XssMatchSetSummary objects.
--
-- 'httpStatus', 'listXssMatchSetsResponse_httpStatus' - The response's http status code.
newListXssMatchSetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListXssMatchSetsResponse
newListXssMatchSetsResponse pHttpStatus_ =
  ListXssMatchSetsResponse'
    { nextMarker =
        Core.Nothing,
      xssMatchSets = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you have more XssMatchSet objects than the number that you specified
-- for @Limit@ in the request, the response includes a @NextMarker@ value.
-- To list more @XssMatchSet@ objects, submit another @ListXssMatchSets@
-- request, and specify the @NextMarker@ value from the response in the
-- @NextMarker@ value in the next request.
listXssMatchSetsResponse_nextMarker :: Lens.Lens' ListXssMatchSetsResponse (Core.Maybe Core.Text)
listXssMatchSetsResponse_nextMarker = Lens.lens (\ListXssMatchSetsResponse' {nextMarker} -> nextMarker) (\s@ListXssMatchSetsResponse' {} a -> s {nextMarker = a} :: ListXssMatchSetsResponse)

-- | An array of XssMatchSetSummary objects.
listXssMatchSetsResponse_xssMatchSets :: Lens.Lens' ListXssMatchSetsResponse (Core.Maybe [XssMatchSetSummary])
listXssMatchSetsResponse_xssMatchSets = Lens.lens (\ListXssMatchSetsResponse' {xssMatchSets} -> xssMatchSets) (\s@ListXssMatchSetsResponse' {} a -> s {xssMatchSets = a} :: ListXssMatchSetsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listXssMatchSetsResponse_httpStatus :: Lens.Lens' ListXssMatchSetsResponse Core.Int
listXssMatchSetsResponse_httpStatus = Lens.lens (\ListXssMatchSetsResponse' {httpStatus} -> httpStatus) (\s@ListXssMatchSetsResponse' {} a -> s {httpStatus = a} :: ListXssMatchSetsResponse)

instance Core.NFData ListXssMatchSetsResponse
