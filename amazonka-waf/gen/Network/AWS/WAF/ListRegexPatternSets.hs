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
-- Module      : Network.AWS.WAF.ListRegexPatternSets
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
-- Returns an array of RegexPatternSetSummary objects.
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListRegexPatternSets
  ( -- * Creating a Request
    ListRegexPatternSets (..),
    newListRegexPatternSets,

    -- * Request Lenses
    listRegexPatternSets_nextMarker,
    listRegexPatternSets_limit,

    -- * Destructuring the Response
    ListRegexPatternSetsResponse (..),
    newListRegexPatternSetsResponse,

    -- * Response Lenses
    listRegexPatternSetsResponse_regexPatternSets,
    listRegexPatternSetsResponse_nextMarker,
    listRegexPatternSetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newListRegexPatternSets' smart constructor.
data ListRegexPatternSets = ListRegexPatternSets'
  { -- | If you specify a value for @Limit@ and you have more @RegexPatternSet@
    -- objects than the value of @Limit@, AWS WAF returns a @NextMarker@ value
    -- in the response that allows you to list another group of
    -- @RegexPatternSet@ objects. For the second and subsequent
    -- @ListRegexPatternSets@ requests, specify the value of @NextMarker@ from
    -- the previous response to get information about another batch of
    -- @RegexPatternSet@ objects.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | Specifies the number of @RegexPatternSet@ objects that you want AWS WAF
    -- to return for this request. If you have more @RegexPatternSet@ objects
    -- than the number you specify for @Limit@, the response includes a
    -- @NextMarker@ value that you can use to get another batch of
    -- @RegexPatternSet@ objects.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRegexPatternSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listRegexPatternSets_nextMarker' - If you specify a value for @Limit@ and you have more @RegexPatternSet@
-- objects than the value of @Limit@, AWS WAF returns a @NextMarker@ value
-- in the response that allows you to list another group of
-- @RegexPatternSet@ objects. For the second and subsequent
-- @ListRegexPatternSets@ requests, specify the value of @NextMarker@ from
-- the previous response to get information about another batch of
-- @RegexPatternSet@ objects.
--
-- 'limit', 'listRegexPatternSets_limit' - Specifies the number of @RegexPatternSet@ objects that you want AWS WAF
-- to return for this request. If you have more @RegexPatternSet@ objects
-- than the number you specify for @Limit@, the response includes a
-- @NextMarker@ value that you can use to get another batch of
-- @RegexPatternSet@ objects.
newListRegexPatternSets ::
  ListRegexPatternSets
newListRegexPatternSets =
  ListRegexPatternSets'
    { nextMarker = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | If you specify a value for @Limit@ and you have more @RegexPatternSet@
-- objects than the value of @Limit@, AWS WAF returns a @NextMarker@ value
-- in the response that allows you to list another group of
-- @RegexPatternSet@ objects. For the second and subsequent
-- @ListRegexPatternSets@ requests, specify the value of @NextMarker@ from
-- the previous response to get information about another batch of
-- @RegexPatternSet@ objects.
listRegexPatternSets_nextMarker :: Lens.Lens' ListRegexPatternSets (Prelude.Maybe Prelude.Text)
listRegexPatternSets_nextMarker = Lens.lens (\ListRegexPatternSets' {nextMarker} -> nextMarker) (\s@ListRegexPatternSets' {} a -> s {nextMarker = a} :: ListRegexPatternSets)

-- | Specifies the number of @RegexPatternSet@ objects that you want AWS WAF
-- to return for this request. If you have more @RegexPatternSet@ objects
-- than the number you specify for @Limit@, the response includes a
-- @NextMarker@ value that you can use to get another batch of
-- @RegexPatternSet@ objects.
listRegexPatternSets_limit :: Lens.Lens' ListRegexPatternSets (Prelude.Maybe Prelude.Natural)
listRegexPatternSets_limit = Lens.lens (\ListRegexPatternSets' {limit} -> limit) (\s@ListRegexPatternSets' {} a -> s {limit = a} :: ListRegexPatternSets)

instance Core.AWSPager ListRegexPatternSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRegexPatternSetsResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRegexPatternSetsResponse_regexPatternSets
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRegexPatternSets_nextMarker
          Lens..~ rs
          Lens.^? listRegexPatternSetsResponse_nextMarker
            Prelude.. Lens._Just

instance Core.AWSRequest ListRegexPatternSets where
  type
    AWSResponse ListRegexPatternSets =
      ListRegexPatternSetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRegexPatternSetsResponse'
            Prelude.<$> ( x Core..?> "RegexPatternSets"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRegexPatternSets

instance Prelude.NFData ListRegexPatternSets

instance Core.ToHeaders ListRegexPatternSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.ListRegexPatternSets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListRegexPatternSets where
  toJSON ListRegexPatternSets' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextMarker" Core..=) Prelude.<$> nextMarker,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath ListRegexPatternSets where
  toPath = Prelude.const "/"

instance Core.ToQuery ListRegexPatternSets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRegexPatternSetsResponse' smart constructor.
data ListRegexPatternSetsResponse = ListRegexPatternSetsResponse'
  { -- | An array of RegexPatternSetSummary objects.
    regexPatternSets :: Prelude.Maybe [RegexPatternSetSummary],
    -- | If you have more @RegexPatternSet@ objects than the number that you
    -- specified for @Limit@ in the request, the response includes a
    -- @NextMarker@ value. To list more @RegexPatternSet@ objects, submit
    -- another @ListRegexPatternSets@ request, and specify the @NextMarker@
    -- value from the response in the @NextMarker@ value in the next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRegexPatternSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regexPatternSets', 'listRegexPatternSetsResponse_regexPatternSets' - An array of RegexPatternSetSummary objects.
--
-- 'nextMarker', 'listRegexPatternSetsResponse_nextMarker' - If you have more @RegexPatternSet@ objects than the number that you
-- specified for @Limit@ in the request, the response includes a
-- @NextMarker@ value. To list more @RegexPatternSet@ objects, submit
-- another @ListRegexPatternSets@ request, and specify the @NextMarker@
-- value from the response in the @NextMarker@ value in the next request.
--
-- 'httpStatus', 'listRegexPatternSetsResponse_httpStatus' - The response's http status code.
newListRegexPatternSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRegexPatternSetsResponse
newListRegexPatternSetsResponse pHttpStatus_ =
  ListRegexPatternSetsResponse'
    { regexPatternSets =
        Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of RegexPatternSetSummary objects.
listRegexPatternSetsResponse_regexPatternSets :: Lens.Lens' ListRegexPatternSetsResponse (Prelude.Maybe [RegexPatternSetSummary])
listRegexPatternSetsResponse_regexPatternSets = Lens.lens (\ListRegexPatternSetsResponse' {regexPatternSets} -> regexPatternSets) (\s@ListRegexPatternSetsResponse' {} a -> s {regexPatternSets = a} :: ListRegexPatternSetsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | If you have more @RegexPatternSet@ objects than the number that you
-- specified for @Limit@ in the request, the response includes a
-- @NextMarker@ value. To list more @RegexPatternSet@ objects, submit
-- another @ListRegexPatternSets@ request, and specify the @NextMarker@
-- value from the response in the @NextMarker@ value in the next request.
listRegexPatternSetsResponse_nextMarker :: Lens.Lens' ListRegexPatternSetsResponse (Prelude.Maybe Prelude.Text)
listRegexPatternSetsResponse_nextMarker = Lens.lens (\ListRegexPatternSetsResponse' {nextMarker} -> nextMarker) (\s@ListRegexPatternSetsResponse' {} a -> s {nextMarker = a} :: ListRegexPatternSetsResponse)

-- | The response's http status code.
listRegexPatternSetsResponse_httpStatus :: Lens.Lens' ListRegexPatternSetsResponse Prelude.Int
listRegexPatternSetsResponse_httpStatus = Lens.lens (\ListRegexPatternSetsResponse' {httpStatus} -> httpStatus) (\s@ListRegexPatternSetsResponse' {} a -> s {httpStatus = a} :: ListRegexPatternSetsResponse)

instance Prelude.NFData ListRegexPatternSetsResponse
