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
-- Module      : Network.AWS.WAF.ListByteMatchSets
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
-- Returns an array of ByteMatchSetSummary objects.
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListByteMatchSets
  ( -- * Creating a Request
    ListByteMatchSets (..),
    newListByteMatchSets,

    -- * Request Lenses
    listByteMatchSets_nextMarker,
    listByteMatchSets_limit,

    -- * Destructuring the Response
    ListByteMatchSetsResponse (..),
    newListByteMatchSetsResponse,

    -- * Response Lenses
    listByteMatchSetsResponse_nextMarker,
    listByteMatchSetsResponse_byteMatchSets,
    listByteMatchSetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newListByteMatchSets' smart constructor.
data ListByteMatchSets = ListByteMatchSets'
  { -- | If you specify a value for @Limit@ and you have more @ByteMatchSets@
    -- than the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
    -- response that allows you to list another group of @ByteMatchSets@. For
    -- the second and subsequent @ListByteMatchSets@ requests, specify the
    -- value of @NextMarker@ from the previous response to get information
    -- about another batch of @ByteMatchSets@.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | Specifies the number of @ByteMatchSet@ objects that you want AWS WAF to
    -- return for this request. If you have more @ByteMatchSets@ objects than
    -- the number you specify for @Limit@, the response includes a @NextMarker@
    -- value that you can use to get another batch of @ByteMatchSet@ objects.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListByteMatchSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listByteMatchSets_nextMarker' - If you specify a value for @Limit@ and you have more @ByteMatchSets@
-- than the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
-- response that allows you to list another group of @ByteMatchSets@. For
-- the second and subsequent @ListByteMatchSets@ requests, specify the
-- value of @NextMarker@ from the previous response to get information
-- about another batch of @ByteMatchSets@.
--
-- 'limit', 'listByteMatchSets_limit' - Specifies the number of @ByteMatchSet@ objects that you want AWS WAF to
-- return for this request. If you have more @ByteMatchSets@ objects than
-- the number you specify for @Limit@, the response includes a @NextMarker@
-- value that you can use to get another batch of @ByteMatchSet@ objects.
newListByteMatchSets ::
  ListByteMatchSets
newListByteMatchSets =
  ListByteMatchSets'
    { nextMarker = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | If you specify a value for @Limit@ and you have more @ByteMatchSets@
-- than the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
-- response that allows you to list another group of @ByteMatchSets@. For
-- the second and subsequent @ListByteMatchSets@ requests, specify the
-- value of @NextMarker@ from the previous response to get information
-- about another batch of @ByteMatchSets@.
listByteMatchSets_nextMarker :: Lens.Lens' ListByteMatchSets (Prelude.Maybe Prelude.Text)
listByteMatchSets_nextMarker = Lens.lens (\ListByteMatchSets' {nextMarker} -> nextMarker) (\s@ListByteMatchSets' {} a -> s {nextMarker = a} :: ListByteMatchSets)

-- | Specifies the number of @ByteMatchSet@ objects that you want AWS WAF to
-- return for this request. If you have more @ByteMatchSets@ objects than
-- the number you specify for @Limit@, the response includes a @NextMarker@
-- value that you can use to get another batch of @ByteMatchSet@ objects.
listByteMatchSets_limit :: Lens.Lens' ListByteMatchSets (Prelude.Maybe Prelude.Natural)
listByteMatchSets_limit = Lens.lens (\ListByteMatchSets' {limit} -> limit) (\s@ListByteMatchSets' {} a -> s {limit = a} :: ListByteMatchSets)

instance Core.AWSPager ListByteMatchSets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listByteMatchSetsResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listByteMatchSetsResponse_byteMatchSets
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listByteMatchSets_nextMarker
          Lens..~ rs
          Lens.^? listByteMatchSetsResponse_nextMarker
            Prelude.. Lens._Just

instance Core.AWSRequest ListByteMatchSets where
  type
    AWSResponse ListByteMatchSets =
      ListByteMatchSetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListByteMatchSetsResponse'
            Prelude.<$> (x Core..?> "NextMarker")
            Prelude.<*> (x Core..?> "ByteMatchSets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListByteMatchSets

instance Prelude.NFData ListByteMatchSets

instance Core.ToHeaders ListByteMatchSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.ListByteMatchSets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListByteMatchSets where
  toJSON ListByteMatchSets' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextMarker" Core..=) Prelude.<$> nextMarker,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath ListByteMatchSets where
  toPath = Prelude.const "/"

instance Core.ToQuery ListByteMatchSets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListByteMatchSetsResponse' smart constructor.
data ListByteMatchSetsResponse = ListByteMatchSetsResponse'
  { -- | If you have more @ByteMatchSet@ objects than the number that you
    -- specified for @Limit@ in the request, the response includes a
    -- @NextMarker@ value. To list more @ByteMatchSet@ objects, submit another
    -- @ListByteMatchSets@ request, and specify the @NextMarker@ value from the
    -- response in the @NextMarker@ value in the next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | An array of ByteMatchSetSummary objects.
    byteMatchSets :: Prelude.Maybe [ByteMatchSetSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListByteMatchSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listByteMatchSetsResponse_nextMarker' - If you have more @ByteMatchSet@ objects than the number that you
-- specified for @Limit@ in the request, the response includes a
-- @NextMarker@ value. To list more @ByteMatchSet@ objects, submit another
-- @ListByteMatchSets@ request, and specify the @NextMarker@ value from the
-- response in the @NextMarker@ value in the next request.
--
-- 'byteMatchSets', 'listByteMatchSetsResponse_byteMatchSets' - An array of ByteMatchSetSummary objects.
--
-- 'httpStatus', 'listByteMatchSetsResponse_httpStatus' - The response's http status code.
newListByteMatchSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListByteMatchSetsResponse
newListByteMatchSetsResponse pHttpStatus_ =
  ListByteMatchSetsResponse'
    { nextMarker =
        Prelude.Nothing,
      byteMatchSets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you have more @ByteMatchSet@ objects than the number that you
-- specified for @Limit@ in the request, the response includes a
-- @NextMarker@ value. To list more @ByteMatchSet@ objects, submit another
-- @ListByteMatchSets@ request, and specify the @NextMarker@ value from the
-- response in the @NextMarker@ value in the next request.
listByteMatchSetsResponse_nextMarker :: Lens.Lens' ListByteMatchSetsResponse (Prelude.Maybe Prelude.Text)
listByteMatchSetsResponse_nextMarker = Lens.lens (\ListByteMatchSetsResponse' {nextMarker} -> nextMarker) (\s@ListByteMatchSetsResponse' {} a -> s {nextMarker = a} :: ListByteMatchSetsResponse)

-- | An array of ByteMatchSetSummary objects.
listByteMatchSetsResponse_byteMatchSets :: Lens.Lens' ListByteMatchSetsResponse (Prelude.Maybe [ByteMatchSetSummary])
listByteMatchSetsResponse_byteMatchSets = Lens.lens (\ListByteMatchSetsResponse' {byteMatchSets} -> byteMatchSets) (\s@ListByteMatchSetsResponse' {} a -> s {byteMatchSets = a} :: ListByteMatchSetsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listByteMatchSetsResponse_httpStatus :: Lens.Lens' ListByteMatchSetsResponse Prelude.Int
listByteMatchSetsResponse_httpStatus = Lens.lens (\ListByteMatchSetsResponse' {httpStatus} -> httpStatus) (\s@ListByteMatchSetsResponse' {} a -> s {httpStatus = a} :: ListByteMatchSetsResponse)

instance Prelude.NFData ListByteMatchSetsResponse
