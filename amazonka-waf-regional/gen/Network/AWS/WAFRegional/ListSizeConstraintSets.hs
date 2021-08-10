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
-- Module      : Network.AWS.WAFRegional.ListSizeConstraintSets
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
-- Returns an array of SizeConstraintSetSummary objects.
module Network.AWS.WAFRegional.ListSizeConstraintSets
  ( -- * Creating a Request
    ListSizeConstraintSets (..),
    newListSizeConstraintSets,

    -- * Request Lenses
    listSizeConstraintSets_nextMarker,
    listSizeConstraintSets_limit,

    -- * Destructuring the Response
    ListSizeConstraintSetsResponse (..),
    newListSizeConstraintSetsResponse,

    -- * Response Lenses
    listSizeConstraintSetsResponse_sizeConstraintSets,
    listSizeConstraintSetsResponse_nextMarker,
    listSizeConstraintSetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAFRegional.Types

-- | /See:/ 'newListSizeConstraintSets' smart constructor.
data ListSizeConstraintSets = ListSizeConstraintSets'
  { -- | If you specify a value for @Limit@ and you have more
    -- @SizeConstraintSets@ than the value of @Limit@, AWS WAF returns a
    -- @NextMarker@ value in the response that allows you to list another group
    -- of @SizeConstraintSets@. For the second and subsequent
    -- @ListSizeConstraintSets@ requests, specify the value of @NextMarker@
    -- from the previous response to get information about another batch of
    -- @SizeConstraintSets@.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | Specifies the number of @SizeConstraintSet@ objects that you want AWS
    -- WAF to return for this request. If you have more @SizeConstraintSets@
    -- objects than the number you specify for @Limit@, the response includes a
    -- @NextMarker@ value that you can use to get another batch of
    -- @SizeConstraintSet@ objects.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSizeConstraintSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listSizeConstraintSets_nextMarker' - If you specify a value for @Limit@ and you have more
-- @SizeConstraintSets@ than the value of @Limit@, AWS WAF returns a
-- @NextMarker@ value in the response that allows you to list another group
-- of @SizeConstraintSets@. For the second and subsequent
-- @ListSizeConstraintSets@ requests, specify the value of @NextMarker@
-- from the previous response to get information about another batch of
-- @SizeConstraintSets@.
--
-- 'limit', 'listSizeConstraintSets_limit' - Specifies the number of @SizeConstraintSet@ objects that you want AWS
-- WAF to return for this request. If you have more @SizeConstraintSets@
-- objects than the number you specify for @Limit@, the response includes a
-- @NextMarker@ value that you can use to get another batch of
-- @SizeConstraintSet@ objects.
newListSizeConstraintSets ::
  ListSizeConstraintSets
newListSizeConstraintSets =
  ListSizeConstraintSets'
    { nextMarker =
        Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | If you specify a value for @Limit@ and you have more
-- @SizeConstraintSets@ than the value of @Limit@, AWS WAF returns a
-- @NextMarker@ value in the response that allows you to list another group
-- of @SizeConstraintSets@. For the second and subsequent
-- @ListSizeConstraintSets@ requests, specify the value of @NextMarker@
-- from the previous response to get information about another batch of
-- @SizeConstraintSets@.
listSizeConstraintSets_nextMarker :: Lens.Lens' ListSizeConstraintSets (Prelude.Maybe Prelude.Text)
listSizeConstraintSets_nextMarker = Lens.lens (\ListSizeConstraintSets' {nextMarker} -> nextMarker) (\s@ListSizeConstraintSets' {} a -> s {nextMarker = a} :: ListSizeConstraintSets)

-- | Specifies the number of @SizeConstraintSet@ objects that you want AWS
-- WAF to return for this request. If you have more @SizeConstraintSets@
-- objects than the number you specify for @Limit@, the response includes a
-- @NextMarker@ value that you can use to get another batch of
-- @SizeConstraintSet@ objects.
listSizeConstraintSets_limit :: Lens.Lens' ListSizeConstraintSets (Prelude.Maybe Prelude.Natural)
listSizeConstraintSets_limit = Lens.lens (\ListSizeConstraintSets' {limit} -> limit) (\s@ListSizeConstraintSets' {} a -> s {limit = a} :: ListSizeConstraintSets)

instance Core.AWSRequest ListSizeConstraintSets where
  type
    AWSResponse ListSizeConstraintSets =
      ListSizeConstraintSetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSizeConstraintSetsResponse'
            Prelude.<$> ( x Core..?> "SizeConstraintSets"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSizeConstraintSets

instance Prelude.NFData ListSizeConstraintSets

instance Core.ToHeaders ListSizeConstraintSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_Regional_20161128.ListSizeConstraintSets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListSizeConstraintSets where
  toJSON ListSizeConstraintSets' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextMarker" Core..=) Prelude.<$> nextMarker,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath ListSizeConstraintSets where
  toPath = Prelude.const "/"

instance Core.ToQuery ListSizeConstraintSets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSizeConstraintSetsResponse' smart constructor.
data ListSizeConstraintSetsResponse = ListSizeConstraintSetsResponse'
  { -- | An array of SizeConstraintSetSummary objects.
    sizeConstraintSets :: Prelude.Maybe [SizeConstraintSetSummary],
    -- | If you have more @SizeConstraintSet@ objects than the number that you
    -- specified for @Limit@ in the request, the response includes a
    -- @NextMarker@ value. To list more @SizeConstraintSet@ objects, submit
    -- another @ListSizeConstraintSets@ request, and specify the @NextMarker@
    -- value from the response in the @NextMarker@ value in the next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSizeConstraintSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeConstraintSets', 'listSizeConstraintSetsResponse_sizeConstraintSets' - An array of SizeConstraintSetSummary objects.
--
-- 'nextMarker', 'listSizeConstraintSetsResponse_nextMarker' - If you have more @SizeConstraintSet@ objects than the number that you
-- specified for @Limit@ in the request, the response includes a
-- @NextMarker@ value. To list more @SizeConstraintSet@ objects, submit
-- another @ListSizeConstraintSets@ request, and specify the @NextMarker@
-- value from the response in the @NextMarker@ value in the next request.
--
-- 'httpStatus', 'listSizeConstraintSetsResponse_httpStatus' - The response's http status code.
newListSizeConstraintSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSizeConstraintSetsResponse
newListSizeConstraintSetsResponse pHttpStatus_ =
  ListSizeConstraintSetsResponse'
    { sizeConstraintSets =
        Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of SizeConstraintSetSummary objects.
listSizeConstraintSetsResponse_sizeConstraintSets :: Lens.Lens' ListSizeConstraintSetsResponse (Prelude.Maybe [SizeConstraintSetSummary])
listSizeConstraintSetsResponse_sizeConstraintSets = Lens.lens (\ListSizeConstraintSetsResponse' {sizeConstraintSets} -> sizeConstraintSets) (\s@ListSizeConstraintSetsResponse' {} a -> s {sizeConstraintSets = a} :: ListSizeConstraintSetsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | If you have more @SizeConstraintSet@ objects than the number that you
-- specified for @Limit@ in the request, the response includes a
-- @NextMarker@ value. To list more @SizeConstraintSet@ objects, submit
-- another @ListSizeConstraintSets@ request, and specify the @NextMarker@
-- value from the response in the @NextMarker@ value in the next request.
listSizeConstraintSetsResponse_nextMarker :: Lens.Lens' ListSizeConstraintSetsResponse (Prelude.Maybe Prelude.Text)
listSizeConstraintSetsResponse_nextMarker = Lens.lens (\ListSizeConstraintSetsResponse' {nextMarker} -> nextMarker) (\s@ListSizeConstraintSetsResponse' {} a -> s {nextMarker = a} :: ListSizeConstraintSetsResponse)

-- | The response's http status code.
listSizeConstraintSetsResponse_httpStatus :: Lens.Lens' ListSizeConstraintSetsResponse Prelude.Int
listSizeConstraintSetsResponse_httpStatus = Lens.lens (\ListSizeConstraintSetsResponse' {httpStatus} -> httpStatus) (\s@ListSizeConstraintSetsResponse' {} a -> s {httpStatus = a} :: ListSizeConstraintSetsResponse)

instance
  Prelude.NFData
    ListSizeConstraintSetsResponse
