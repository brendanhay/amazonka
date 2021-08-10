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
-- Module      : Network.AWS.WAFRegional.ListRegexMatchSets
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
-- Returns an array of RegexMatchSetSummary objects.
module Network.AWS.WAFRegional.ListRegexMatchSets
  ( -- * Creating a Request
    ListRegexMatchSets (..),
    newListRegexMatchSets,

    -- * Request Lenses
    listRegexMatchSets_nextMarker,
    listRegexMatchSets_limit,

    -- * Destructuring the Response
    ListRegexMatchSetsResponse (..),
    newListRegexMatchSetsResponse,

    -- * Response Lenses
    listRegexMatchSetsResponse_nextMarker,
    listRegexMatchSetsResponse_regexMatchSets,
    listRegexMatchSetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAFRegional.Types

-- | /See:/ 'newListRegexMatchSets' smart constructor.
data ListRegexMatchSets = ListRegexMatchSets'
  { -- | If you specify a value for @Limit@ and you have more @RegexMatchSet@
    -- objects than the value of @Limit@, AWS WAF returns a @NextMarker@ value
    -- in the response that allows you to list another group of
    -- @ByteMatchSets@. For the second and subsequent @ListRegexMatchSets@
    -- requests, specify the value of @NextMarker@ from the previous response
    -- to get information about another batch of @RegexMatchSet@ objects.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | Specifies the number of @RegexMatchSet@ objects that you want AWS WAF to
    -- return for this request. If you have more @RegexMatchSet@ objects than
    -- the number you specify for @Limit@, the response includes a @NextMarker@
    -- value that you can use to get another batch of @RegexMatchSet@ objects.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRegexMatchSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listRegexMatchSets_nextMarker' - If you specify a value for @Limit@ and you have more @RegexMatchSet@
-- objects than the value of @Limit@, AWS WAF returns a @NextMarker@ value
-- in the response that allows you to list another group of
-- @ByteMatchSets@. For the second and subsequent @ListRegexMatchSets@
-- requests, specify the value of @NextMarker@ from the previous response
-- to get information about another batch of @RegexMatchSet@ objects.
--
-- 'limit', 'listRegexMatchSets_limit' - Specifies the number of @RegexMatchSet@ objects that you want AWS WAF to
-- return for this request. If you have more @RegexMatchSet@ objects than
-- the number you specify for @Limit@, the response includes a @NextMarker@
-- value that you can use to get another batch of @RegexMatchSet@ objects.
newListRegexMatchSets ::
  ListRegexMatchSets
newListRegexMatchSets =
  ListRegexMatchSets'
    { nextMarker = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | If you specify a value for @Limit@ and you have more @RegexMatchSet@
-- objects than the value of @Limit@, AWS WAF returns a @NextMarker@ value
-- in the response that allows you to list another group of
-- @ByteMatchSets@. For the second and subsequent @ListRegexMatchSets@
-- requests, specify the value of @NextMarker@ from the previous response
-- to get information about another batch of @RegexMatchSet@ objects.
listRegexMatchSets_nextMarker :: Lens.Lens' ListRegexMatchSets (Prelude.Maybe Prelude.Text)
listRegexMatchSets_nextMarker = Lens.lens (\ListRegexMatchSets' {nextMarker} -> nextMarker) (\s@ListRegexMatchSets' {} a -> s {nextMarker = a} :: ListRegexMatchSets)

-- | Specifies the number of @RegexMatchSet@ objects that you want AWS WAF to
-- return for this request. If you have more @RegexMatchSet@ objects than
-- the number you specify for @Limit@, the response includes a @NextMarker@
-- value that you can use to get another batch of @RegexMatchSet@ objects.
listRegexMatchSets_limit :: Lens.Lens' ListRegexMatchSets (Prelude.Maybe Prelude.Natural)
listRegexMatchSets_limit = Lens.lens (\ListRegexMatchSets' {limit} -> limit) (\s@ListRegexMatchSets' {} a -> s {limit = a} :: ListRegexMatchSets)

instance Core.AWSRequest ListRegexMatchSets where
  type
    AWSResponse ListRegexMatchSets =
      ListRegexMatchSetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRegexMatchSetsResponse'
            Prelude.<$> (x Core..?> "NextMarker")
            Prelude.<*> (x Core..?> "RegexMatchSets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRegexMatchSets

instance Prelude.NFData ListRegexMatchSets

instance Core.ToHeaders ListRegexMatchSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_Regional_20161128.ListRegexMatchSets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListRegexMatchSets where
  toJSON ListRegexMatchSets' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextMarker" Core..=) Prelude.<$> nextMarker,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath ListRegexMatchSets where
  toPath = Prelude.const "/"

instance Core.ToQuery ListRegexMatchSets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRegexMatchSetsResponse' smart constructor.
data ListRegexMatchSetsResponse = ListRegexMatchSetsResponse'
  { -- | If you have more @RegexMatchSet@ objects than the number that you
    -- specified for @Limit@ in the request, the response includes a
    -- @NextMarker@ value. To list more @RegexMatchSet@ objects, submit another
    -- @ListRegexMatchSets@ request, and specify the @NextMarker@ value from
    -- the response in the @NextMarker@ value in the next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | An array of RegexMatchSetSummary objects.
    regexMatchSets :: Prelude.Maybe [RegexMatchSetSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRegexMatchSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listRegexMatchSetsResponse_nextMarker' - If you have more @RegexMatchSet@ objects than the number that you
-- specified for @Limit@ in the request, the response includes a
-- @NextMarker@ value. To list more @RegexMatchSet@ objects, submit another
-- @ListRegexMatchSets@ request, and specify the @NextMarker@ value from
-- the response in the @NextMarker@ value in the next request.
--
-- 'regexMatchSets', 'listRegexMatchSetsResponse_regexMatchSets' - An array of RegexMatchSetSummary objects.
--
-- 'httpStatus', 'listRegexMatchSetsResponse_httpStatus' - The response's http status code.
newListRegexMatchSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRegexMatchSetsResponse
newListRegexMatchSetsResponse pHttpStatus_ =
  ListRegexMatchSetsResponse'
    { nextMarker =
        Prelude.Nothing,
      regexMatchSets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you have more @RegexMatchSet@ objects than the number that you
-- specified for @Limit@ in the request, the response includes a
-- @NextMarker@ value. To list more @RegexMatchSet@ objects, submit another
-- @ListRegexMatchSets@ request, and specify the @NextMarker@ value from
-- the response in the @NextMarker@ value in the next request.
listRegexMatchSetsResponse_nextMarker :: Lens.Lens' ListRegexMatchSetsResponse (Prelude.Maybe Prelude.Text)
listRegexMatchSetsResponse_nextMarker = Lens.lens (\ListRegexMatchSetsResponse' {nextMarker} -> nextMarker) (\s@ListRegexMatchSetsResponse' {} a -> s {nextMarker = a} :: ListRegexMatchSetsResponse)

-- | An array of RegexMatchSetSummary objects.
listRegexMatchSetsResponse_regexMatchSets :: Lens.Lens' ListRegexMatchSetsResponse (Prelude.Maybe [RegexMatchSetSummary])
listRegexMatchSetsResponse_regexMatchSets = Lens.lens (\ListRegexMatchSetsResponse' {regexMatchSets} -> regexMatchSets) (\s@ListRegexMatchSetsResponse' {} a -> s {regexMatchSets = a} :: ListRegexMatchSetsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listRegexMatchSetsResponse_httpStatus :: Lens.Lens' ListRegexMatchSetsResponse Prelude.Int
listRegexMatchSetsResponse_httpStatus = Lens.lens (\ListRegexMatchSetsResponse' {httpStatus} -> httpStatus) (\s@ListRegexMatchSetsResponse' {} a -> s {httpStatus = a} :: ListRegexMatchSetsResponse)

instance Prelude.NFData ListRegexMatchSetsResponse
