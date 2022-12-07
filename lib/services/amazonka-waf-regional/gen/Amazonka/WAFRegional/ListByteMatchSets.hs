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
-- Module      : Amazonka.WAFRegional.ListByteMatchSets
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.WAFRegional.ListByteMatchSets
  ( -- * Creating a Request
    ListByteMatchSets (..),
    newListByteMatchSets,

    -- * Request Lenses
    listByteMatchSets_limit,
    listByteMatchSets_nextMarker,

    -- * Destructuring the Response
    ListByteMatchSetsResponse (..),
    newListByteMatchSetsResponse,

    -- * Response Lenses
    listByteMatchSetsResponse_byteMatchSets,
    listByteMatchSetsResponse_nextMarker,
    listByteMatchSetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | /See:/ 'newListByteMatchSets' smart constructor.
data ListByteMatchSets = ListByteMatchSets'
  { -- | Specifies the number of @ByteMatchSet@ objects that you want AWS WAF to
    -- return for this request. If you have more @ByteMatchSets@ objects than
    -- the number you specify for @Limit@, the response includes a @NextMarker@
    -- value that you can use to get another batch of @ByteMatchSet@ objects.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | If you specify a value for @Limit@ and you have more @ByteMatchSets@
    -- than the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
    -- response that allows you to list another group of @ByteMatchSets@. For
    -- the second and subsequent @ListByteMatchSets@ requests, specify the
    -- value of @NextMarker@ from the previous response to get information
    -- about another batch of @ByteMatchSets@.
    nextMarker :: Prelude.Maybe Prelude.Text
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
-- 'limit', 'listByteMatchSets_limit' - Specifies the number of @ByteMatchSet@ objects that you want AWS WAF to
-- return for this request. If you have more @ByteMatchSets@ objects than
-- the number you specify for @Limit@, the response includes a @NextMarker@
-- value that you can use to get another batch of @ByteMatchSet@ objects.
--
-- 'nextMarker', 'listByteMatchSets_nextMarker' - If you specify a value for @Limit@ and you have more @ByteMatchSets@
-- than the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
-- response that allows you to list another group of @ByteMatchSets@. For
-- the second and subsequent @ListByteMatchSets@ requests, specify the
-- value of @NextMarker@ from the previous response to get information
-- about another batch of @ByteMatchSets@.
newListByteMatchSets ::
  ListByteMatchSets
newListByteMatchSets =
  ListByteMatchSets'
    { limit = Prelude.Nothing,
      nextMarker = Prelude.Nothing
    }

-- | Specifies the number of @ByteMatchSet@ objects that you want AWS WAF to
-- return for this request. If you have more @ByteMatchSets@ objects than
-- the number you specify for @Limit@, the response includes a @NextMarker@
-- value that you can use to get another batch of @ByteMatchSet@ objects.
listByteMatchSets_limit :: Lens.Lens' ListByteMatchSets (Prelude.Maybe Prelude.Natural)
listByteMatchSets_limit = Lens.lens (\ListByteMatchSets' {limit} -> limit) (\s@ListByteMatchSets' {} a -> s {limit = a} :: ListByteMatchSets)

-- | If you specify a value for @Limit@ and you have more @ByteMatchSets@
-- than the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
-- response that allows you to list another group of @ByteMatchSets@. For
-- the second and subsequent @ListByteMatchSets@ requests, specify the
-- value of @NextMarker@ from the previous response to get information
-- about another batch of @ByteMatchSets@.
listByteMatchSets_nextMarker :: Lens.Lens' ListByteMatchSets (Prelude.Maybe Prelude.Text)
listByteMatchSets_nextMarker = Lens.lens (\ListByteMatchSets' {nextMarker} -> nextMarker) (\s@ListByteMatchSets' {} a -> s {nextMarker = a} :: ListByteMatchSets)

instance Core.AWSRequest ListByteMatchSets where
  type
    AWSResponse ListByteMatchSets =
      ListByteMatchSetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListByteMatchSetsResponse'
            Prelude.<$> (x Data..?> "ByteMatchSets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListByteMatchSets where
  hashWithSalt _salt ListByteMatchSets' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextMarker

instance Prelude.NFData ListByteMatchSets where
  rnf ListByteMatchSets' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextMarker

instance Data.ToHeaders ListByteMatchSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_Regional_20161128.ListByteMatchSets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListByteMatchSets where
  toJSON ListByteMatchSets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextMarker" Data..=) Prelude.<$> nextMarker
          ]
      )

instance Data.ToPath ListByteMatchSets where
  toPath = Prelude.const "/"

instance Data.ToQuery ListByteMatchSets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListByteMatchSetsResponse' smart constructor.
data ListByteMatchSetsResponse = ListByteMatchSetsResponse'
  { -- | An array of ByteMatchSetSummary objects.
    byteMatchSets :: Prelude.Maybe [ByteMatchSetSummary],
    -- | If you have more @ByteMatchSet@ objects than the number that you
    -- specified for @Limit@ in the request, the response includes a
    -- @NextMarker@ value. To list more @ByteMatchSet@ objects, submit another
    -- @ListByteMatchSets@ request, and specify the @NextMarker@ value from the
    -- response in the @NextMarker@ value in the next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
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
-- 'byteMatchSets', 'listByteMatchSetsResponse_byteMatchSets' - An array of ByteMatchSetSummary objects.
--
-- 'nextMarker', 'listByteMatchSetsResponse_nextMarker' - If you have more @ByteMatchSet@ objects than the number that you
-- specified for @Limit@ in the request, the response includes a
-- @NextMarker@ value. To list more @ByteMatchSet@ objects, submit another
-- @ListByteMatchSets@ request, and specify the @NextMarker@ value from the
-- response in the @NextMarker@ value in the next request.
--
-- 'httpStatus', 'listByteMatchSetsResponse_httpStatus' - The response's http status code.
newListByteMatchSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListByteMatchSetsResponse
newListByteMatchSetsResponse pHttpStatus_ =
  ListByteMatchSetsResponse'
    { byteMatchSets =
        Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of ByteMatchSetSummary objects.
listByteMatchSetsResponse_byteMatchSets :: Lens.Lens' ListByteMatchSetsResponse (Prelude.Maybe [ByteMatchSetSummary])
listByteMatchSetsResponse_byteMatchSets = Lens.lens (\ListByteMatchSetsResponse' {byteMatchSets} -> byteMatchSets) (\s@ListByteMatchSetsResponse' {} a -> s {byteMatchSets = a} :: ListByteMatchSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If you have more @ByteMatchSet@ objects than the number that you
-- specified for @Limit@ in the request, the response includes a
-- @NextMarker@ value. To list more @ByteMatchSet@ objects, submit another
-- @ListByteMatchSets@ request, and specify the @NextMarker@ value from the
-- response in the @NextMarker@ value in the next request.
listByteMatchSetsResponse_nextMarker :: Lens.Lens' ListByteMatchSetsResponse (Prelude.Maybe Prelude.Text)
listByteMatchSetsResponse_nextMarker = Lens.lens (\ListByteMatchSetsResponse' {nextMarker} -> nextMarker) (\s@ListByteMatchSetsResponse' {} a -> s {nextMarker = a} :: ListByteMatchSetsResponse)

-- | The response's http status code.
listByteMatchSetsResponse_httpStatus :: Lens.Lens' ListByteMatchSetsResponse Prelude.Int
listByteMatchSetsResponse_httpStatus = Lens.lens (\ListByteMatchSetsResponse' {httpStatus} -> httpStatus) (\s@ListByteMatchSetsResponse' {} a -> s {httpStatus = a} :: ListByteMatchSetsResponse)

instance Prelude.NFData ListByteMatchSetsResponse where
  rnf ListByteMatchSetsResponse' {..} =
    Prelude.rnf byteMatchSets
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
