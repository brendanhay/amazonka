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
-- Module      : Amazonka.WAF.ListXssMatchSets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.WAF.ListXssMatchSets
  ( -- * Creating a Request
    ListXssMatchSets (..),
    newListXssMatchSets,

    -- * Request Lenses
    listXssMatchSets_limit,
    listXssMatchSets_nextMarker,

    -- * Destructuring the Response
    ListXssMatchSetsResponse (..),
    newListXssMatchSetsResponse,

    -- * Response Lenses
    listXssMatchSetsResponse_nextMarker,
    listXssMatchSetsResponse_xssMatchSets,
    listXssMatchSetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAF.Types

-- | A request to list the XssMatchSet objects created by the current AWS
-- account.
--
-- /See:/ 'newListXssMatchSets' smart constructor.
data ListXssMatchSets = ListXssMatchSets'
  { -- | Specifies the number of XssMatchSet objects that you want AWS WAF to
    -- return for this request. If you have more @XssMatchSet@ objects than the
    -- number you specify for @Limit@, the response includes a @NextMarker@
    -- value that you can use to get another batch of @Rules@.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | If you specify a value for @Limit@ and you have more XssMatchSet objects
    -- than the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
    -- response that allows you to list another group of @XssMatchSets@. For
    -- the second and subsequent @ListXssMatchSets@ requests, specify the value
    -- of @NextMarker@ from the previous response to get information about
    -- another batch of @XssMatchSets@.
    nextMarker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListXssMatchSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listXssMatchSets_limit' - Specifies the number of XssMatchSet objects that you want AWS WAF to
-- return for this request. If you have more @XssMatchSet@ objects than the
-- number you specify for @Limit@, the response includes a @NextMarker@
-- value that you can use to get another batch of @Rules@.
--
-- 'nextMarker', 'listXssMatchSets_nextMarker' - If you specify a value for @Limit@ and you have more XssMatchSet objects
-- than the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
-- response that allows you to list another group of @XssMatchSets@. For
-- the second and subsequent @ListXssMatchSets@ requests, specify the value
-- of @NextMarker@ from the previous response to get information about
-- another batch of @XssMatchSets@.
newListXssMatchSets ::
  ListXssMatchSets
newListXssMatchSets =
  ListXssMatchSets'
    { limit = Prelude.Nothing,
      nextMarker = Prelude.Nothing
    }

-- | Specifies the number of XssMatchSet objects that you want AWS WAF to
-- return for this request. If you have more @XssMatchSet@ objects than the
-- number you specify for @Limit@, the response includes a @NextMarker@
-- value that you can use to get another batch of @Rules@.
listXssMatchSets_limit :: Lens.Lens' ListXssMatchSets (Prelude.Maybe Prelude.Natural)
listXssMatchSets_limit = Lens.lens (\ListXssMatchSets' {limit} -> limit) (\s@ListXssMatchSets' {} a -> s {limit = a} :: ListXssMatchSets)

-- | If you specify a value for @Limit@ and you have more XssMatchSet objects
-- than the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
-- response that allows you to list another group of @XssMatchSets@. For
-- the second and subsequent @ListXssMatchSets@ requests, specify the value
-- of @NextMarker@ from the previous response to get information about
-- another batch of @XssMatchSets@.
listXssMatchSets_nextMarker :: Lens.Lens' ListXssMatchSets (Prelude.Maybe Prelude.Text)
listXssMatchSets_nextMarker = Lens.lens (\ListXssMatchSets' {nextMarker} -> nextMarker) (\s@ListXssMatchSets' {} a -> s {nextMarker = a} :: ListXssMatchSets)

instance Core.AWSPager ListXssMatchSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listXssMatchSetsResponse_nextMarker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listXssMatchSetsResponse_xssMatchSets
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listXssMatchSets_nextMarker
          Lens..~ rs
          Lens.^? listXssMatchSetsResponse_nextMarker
          Prelude.. Lens._Just

instance Core.AWSRequest ListXssMatchSets where
  type
    AWSResponse ListXssMatchSets =
      ListXssMatchSetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListXssMatchSetsResponse'
            Prelude.<$> (x Data..?> "NextMarker")
            Prelude.<*> (x Data..?> "XssMatchSets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListXssMatchSets where
  hashWithSalt _salt ListXssMatchSets' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextMarker

instance Prelude.NFData ListXssMatchSets where
  rnf ListXssMatchSets' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextMarker

instance Data.ToHeaders ListXssMatchSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20150824.ListXssMatchSets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListXssMatchSets where
  toJSON ListXssMatchSets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextMarker" Data..=) Prelude.<$> nextMarker
          ]
      )

instance Data.ToPath ListXssMatchSets where
  toPath = Prelude.const "/"

instance Data.ToQuery ListXssMatchSets where
  toQuery = Prelude.const Prelude.mempty

-- | The response to a ListXssMatchSets request.
--
-- /See:/ 'newListXssMatchSetsResponse' smart constructor.
data ListXssMatchSetsResponse = ListXssMatchSetsResponse'
  { -- | If you have more XssMatchSet objects than the number that you specified
    -- for @Limit@ in the request, the response includes a @NextMarker@ value.
    -- To list more @XssMatchSet@ objects, submit another @ListXssMatchSets@
    -- request, and specify the @NextMarker@ value from the response in the
    -- @NextMarker@ value in the next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | An array of XssMatchSetSummary objects.
    xssMatchSets :: Prelude.Maybe [XssMatchSetSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListXssMatchSetsResponse
newListXssMatchSetsResponse pHttpStatus_ =
  ListXssMatchSetsResponse'
    { nextMarker =
        Prelude.Nothing,
      xssMatchSets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you have more XssMatchSet objects than the number that you specified
-- for @Limit@ in the request, the response includes a @NextMarker@ value.
-- To list more @XssMatchSet@ objects, submit another @ListXssMatchSets@
-- request, and specify the @NextMarker@ value from the response in the
-- @NextMarker@ value in the next request.
listXssMatchSetsResponse_nextMarker :: Lens.Lens' ListXssMatchSetsResponse (Prelude.Maybe Prelude.Text)
listXssMatchSetsResponse_nextMarker = Lens.lens (\ListXssMatchSetsResponse' {nextMarker} -> nextMarker) (\s@ListXssMatchSetsResponse' {} a -> s {nextMarker = a} :: ListXssMatchSetsResponse)

-- | An array of XssMatchSetSummary objects.
listXssMatchSetsResponse_xssMatchSets :: Lens.Lens' ListXssMatchSetsResponse (Prelude.Maybe [XssMatchSetSummary])
listXssMatchSetsResponse_xssMatchSets = Lens.lens (\ListXssMatchSetsResponse' {xssMatchSets} -> xssMatchSets) (\s@ListXssMatchSetsResponse' {} a -> s {xssMatchSets = a} :: ListXssMatchSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listXssMatchSetsResponse_httpStatus :: Lens.Lens' ListXssMatchSetsResponse Prelude.Int
listXssMatchSetsResponse_httpStatus = Lens.lens (\ListXssMatchSetsResponse' {httpStatus} -> httpStatus) (\s@ListXssMatchSetsResponse' {} a -> s {httpStatus = a} :: ListXssMatchSetsResponse)

instance Prelude.NFData ListXssMatchSetsResponse where
  rnf ListXssMatchSetsResponse' {..} =
    Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf xssMatchSets
      `Prelude.seq` Prelude.rnf httpStatus
