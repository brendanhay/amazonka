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
-- Module      : Amazonka.WAF.ListRegexPatternSets
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
-- Returns an array of RegexPatternSetSummary objects.
--
-- This operation returns paginated results.
module Amazonka.WAF.ListRegexPatternSets
  ( -- * Creating a Request
    ListRegexPatternSets (..),
    newListRegexPatternSets,

    -- * Request Lenses
    listRegexPatternSets_limit,
    listRegexPatternSets_nextMarker,

    -- * Destructuring the Response
    ListRegexPatternSetsResponse (..),
    newListRegexPatternSetsResponse,

    -- * Response Lenses
    listRegexPatternSetsResponse_nextMarker,
    listRegexPatternSetsResponse_regexPatternSets,
    listRegexPatternSetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAF.Types

-- | /See:/ 'newListRegexPatternSets' smart constructor.
data ListRegexPatternSets = ListRegexPatternSets'
  { -- | Specifies the number of @RegexPatternSet@ objects that you want AWS WAF
    -- to return for this request. If you have more @RegexPatternSet@ objects
    -- than the number you specify for @Limit@, the response includes a
    -- @NextMarker@ value that you can use to get another batch of
    -- @RegexPatternSet@ objects.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | If you specify a value for @Limit@ and you have more @RegexPatternSet@
    -- objects than the value of @Limit@, AWS WAF returns a @NextMarker@ value
    -- in the response that allows you to list another group of
    -- @RegexPatternSet@ objects. For the second and subsequent
    -- @ListRegexPatternSets@ requests, specify the value of @NextMarker@ from
    -- the previous response to get information about another batch of
    -- @RegexPatternSet@ objects.
    nextMarker :: Prelude.Maybe Prelude.Text
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
-- 'limit', 'listRegexPatternSets_limit' - Specifies the number of @RegexPatternSet@ objects that you want AWS WAF
-- to return for this request. If you have more @RegexPatternSet@ objects
-- than the number you specify for @Limit@, the response includes a
-- @NextMarker@ value that you can use to get another batch of
-- @RegexPatternSet@ objects.
--
-- 'nextMarker', 'listRegexPatternSets_nextMarker' - If you specify a value for @Limit@ and you have more @RegexPatternSet@
-- objects than the value of @Limit@, AWS WAF returns a @NextMarker@ value
-- in the response that allows you to list another group of
-- @RegexPatternSet@ objects. For the second and subsequent
-- @ListRegexPatternSets@ requests, specify the value of @NextMarker@ from
-- the previous response to get information about another batch of
-- @RegexPatternSet@ objects.
newListRegexPatternSets ::
  ListRegexPatternSets
newListRegexPatternSets =
  ListRegexPatternSets'
    { limit = Prelude.Nothing,
      nextMarker = Prelude.Nothing
    }

-- | Specifies the number of @RegexPatternSet@ objects that you want AWS WAF
-- to return for this request. If you have more @RegexPatternSet@ objects
-- than the number you specify for @Limit@, the response includes a
-- @NextMarker@ value that you can use to get another batch of
-- @RegexPatternSet@ objects.
listRegexPatternSets_limit :: Lens.Lens' ListRegexPatternSets (Prelude.Maybe Prelude.Natural)
listRegexPatternSets_limit = Lens.lens (\ListRegexPatternSets' {limit} -> limit) (\s@ListRegexPatternSets' {} a -> s {limit = a} :: ListRegexPatternSets)

-- | If you specify a value for @Limit@ and you have more @RegexPatternSet@
-- objects than the value of @Limit@, AWS WAF returns a @NextMarker@ value
-- in the response that allows you to list another group of
-- @RegexPatternSet@ objects. For the second and subsequent
-- @ListRegexPatternSets@ requests, specify the value of @NextMarker@ from
-- the previous response to get information about another batch of
-- @RegexPatternSet@ objects.
listRegexPatternSets_nextMarker :: Lens.Lens' ListRegexPatternSets (Prelude.Maybe Prelude.Text)
listRegexPatternSets_nextMarker = Lens.lens (\ListRegexPatternSets' {nextMarker} -> nextMarker) (\s@ListRegexPatternSets' {} a -> s {nextMarker = a} :: ListRegexPatternSets)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRegexPatternSetsResponse'
            Prelude.<$> (x Data..?> "NextMarker")
            Prelude.<*> ( x Data..?> "RegexPatternSets"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRegexPatternSets where
  hashWithSalt _salt ListRegexPatternSets' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextMarker

instance Prelude.NFData ListRegexPatternSets where
  rnf ListRegexPatternSets' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextMarker

instance Data.ToHeaders ListRegexPatternSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20150824.ListRegexPatternSets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRegexPatternSets where
  toJSON ListRegexPatternSets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextMarker" Data..=) Prelude.<$> nextMarker
          ]
      )

instance Data.ToPath ListRegexPatternSets where
  toPath = Prelude.const "/"

instance Data.ToQuery ListRegexPatternSets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRegexPatternSetsResponse' smart constructor.
data ListRegexPatternSetsResponse = ListRegexPatternSetsResponse'
  { -- | If you have more @RegexPatternSet@ objects than the number that you
    -- specified for @Limit@ in the request, the response includes a
    -- @NextMarker@ value. To list more @RegexPatternSet@ objects, submit
    -- another @ListRegexPatternSets@ request, and specify the @NextMarker@
    -- value from the response in the @NextMarker@ value in the next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | An array of RegexPatternSetSummary objects.
    regexPatternSets :: Prelude.Maybe [RegexPatternSetSummary],
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
-- 'nextMarker', 'listRegexPatternSetsResponse_nextMarker' - If you have more @RegexPatternSet@ objects than the number that you
-- specified for @Limit@ in the request, the response includes a
-- @NextMarker@ value. To list more @RegexPatternSet@ objects, submit
-- another @ListRegexPatternSets@ request, and specify the @NextMarker@
-- value from the response in the @NextMarker@ value in the next request.
--
-- 'regexPatternSets', 'listRegexPatternSetsResponse_regexPatternSets' - An array of RegexPatternSetSummary objects.
--
-- 'httpStatus', 'listRegexPatternSetsResponse_httpStatus' - The response's http status code.
newListRegexPatternSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRegexPatternSetsResponse
newListRegexPatternSetsResponse pHttpStatus_ =
  ListRegexPatternSetsResponse'
    { nextMarker =
        Prelude.Nothing,
      regexPatternSets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you have more @RegexPatternSet@ objects than the number that you
-- specified for @Limit@ in the request, the response includes a
-- @NextMarker@ value. To list more @RegexPatternSet@ objects, submit
-- another @ListRegexPatternSets@ request, and specify the @NextMarker@
-- value from the response in the @NextMarker@ value in the next request.
listRegexPatternSetsResponse_nextMarker :: Lens.Lens' ListRegexPatternSetsResponse (Prelude.Maybe Prelude.Text)
listRegexPatternSetsResponse_nextMarker = Lens.lens (\ListRegexPatternSetsResponse' {nextMarker} -> nextMarker) (\s@ListRegexPatternSetsResponse' {} a -> s {nextMarker = a} :: ListRegexPatternSetsResponse)

-- | An array of RegexPatternSetSummary objects.
listRegexPatternSetsResponse_regexPatternSets :: Lens.Lens' ListRegexPatternSetsResponse (Prelude.Maybe [RegexPatternSetSummary])
listRegexPatternSetsResponse_regexPatternSets = Lens.lens (\ListRegexPatternSetsResponse' {regexPatternSets} -> regexPatternSets) (\s@ListRegexPatternSetsResponse' {} a -> s {regexPatternSets = a} :: ListRegexPatternSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRegexPatternSetsResponse_httpStatus :: Lens.Lens' ListRegexPatternSetsResponse Prelude.Int
listRegexPatternSetsResponse_httpStatus = Lens.lens (\ListRegexPatternSetsResponse' {httpStatus} -> httpStatus) (\s@ListRegexPatternSetsResponse' {} a -> s {httpStatus = a} :: ListRegexPatternSetsResponse)

instance Prelude.NFData ListRegexPatternSetsResponse where
  rnf ListRegexPatternSetsResponse' {..} =
    Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf regexPatternSets
      `Prelude.seq` Prelude.rnf httpStatus
