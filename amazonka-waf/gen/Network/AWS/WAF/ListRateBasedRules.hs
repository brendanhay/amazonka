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
-- Module      : Network.AWS.WAF.ListRateBasedRules
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
-- Returns an array of RuleSummary objects.
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListRateBasedRules
  ( -- * Creating a Request
    ListRateBasedRules (..),
    newListRateBasedRules,

    -- * Request Lenses
    listRateBasedRules_nextMarker,
    listRateBasedRules_limit,

    -- * Destructuring the Response
    ListRateBasedRulesResponse (..),
    newListRateBasedRulesResponse,

    -- * Response Lenses
    listRateBasedRulesResponse_nextMarker,
    listRateBasedRulesResponse_rules,
    listRateBasedRulesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newListRateBasedRules' smart constructor.
data ListRateBasedRules = ListRateBasedRules'
  { -- | If you specify a value for @Limit@ and you have more @Rules@ than the
    -- value of @Limit@, AWS WAF returns a @NextMarker@ value in the response
    -- that allows you to list another group of @Rules@. For the second and
    -- subsequent @ListRateBasedRules@ requests, specify the value of
    -- @NextMarker@ from the previous response to get information about another
    -- batch of @Rules@.
    nextMarker :: Core.Maybe Core.Text,
    -- | Specifies the number of @Rules@ that you want AWS WAF to return for this
    -- request. If you have more @Rules@ than the number that you specify for
    -- @Limit@, the response includes a @NextMarker@ value that you can use to
    -- get another batch of @Rules@.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRateBasedRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listRateBasedRules_nextMarker' - If you specify a value for @Limit@ and you have more @Rules@ than the
-- value of @Limit@, AWS WAF returns a @NextMarker@ value in the response
-- that allows you to list another group of @Rules@. For the second and
-- subsequent @ListRateBasedRules@ requests, specify the value of
-- @NextMarker@ from the previous response to get information about another
-- batch of @Rules@.
--
-- 'limit', 'listRateBasedRules_limit' - Specifies the number of @Rules@ that you want AWS WAF to return for this
-- request. If you have more @Rules@ than the number that you specify for
-- @Limit@, the response includes a @NextMarker@ value that you can use to
-- get another batch of @Rules@.
newListRateBasedRules ::
  ListRateBasedRules
newListRateBasedRules =
  ListRateBasedRules'
    { nextMarker = Core.Nothing,
      limit = Core.Nothing
    }

-- | If you specify a value for @Limit@ and you have more @Rules@ than the
-- value of @Limit@, AWS WAF returns a @NextMarker@ value in the response
-- that allows you to list another group of @Rules@. For the second and
-- subsequent @ListRateBasedRules@ requests, specify the value of
-- @NextMarker@ from the previous response to get information about another
-- batch of @Rules@.
listRateBasedRules_nextMarker :: Lens.Lens' ListRateBasedRules (Core.Maybe Core.Text)
listRateBasedRules_nextMarker = Lens.lens (\ListRateBasedRules' {nextMarker} -> nextMarker) (\s@ListRateBasedRules' {} a -> s {nextMarker = a} :: ListRateBasedRules)

-- | Specifies the number of @Rules@ that you want AWS WAF to return for this
-- request. If you have more @Rules@ than the number that you specify for
-- @Limit@, the response includes a @NextMarker@ value that you can use to
-- get another batch of @Rules@.
listRateBasedRules_limit :: Lens.Lens' ListRateBasedRules (Core.Maybe Core.Natural)
listRateBasedRules_limit = Lens.lens (\ListRateBasedRules' {limit} -> limit) (\s@ListRateBasedRules' {} a -> s {limit = a} :: ListRateBasedRules)

instance Core.AWSPager ListRateBasedRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRateBasedRulesResponse_nextMarker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listRateBasedRulesResponse_rules Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listRateBasedRules_nextMarker
          Lens..~ rs
          Lens.^? listRateBasedRulesResponse_nextMarker
            Core.. Lens._Just

instance Core.AWSRequest ListRateBasedRules where
  type
    AWSResponse ListRateBasedRules =
      ListRateBasedRulesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRateBasedRulesResponse'
            Core.<$> (x Core..?> "NextMarker")
            Core.<*> (x Core..?> "Rules" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListRateBasedRules

instance Core.NFData ListRateBasedRules

instance Core.ToHeaders ListRateBasedRules where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.ListRateBasedRules" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListRateBasedRules where
  toJSON ListRateBasedRules' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextMarker" Core..=) Core.<$> nextMarker,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath ListRateBasedRules where
  toPath = Core.const "/"

instance Core.ToQuery ListRateBasedRules where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListRateBasedRulesResponse' smart constructor.
data ListRateBasedRulesResponse = ListRateBasedRulesResponse'
  { -- | If you have more @Rules@ than the number that you specified for @Limit@
    -- in the request, the response includes a @NextMarker@ value. To list more
    -- @Rules@, submit another @ListRateBasedRules@ request, and specify the
    -- @NextMarker@ value from the response in the @NextMarker@ value in the
    -- next request.
    nextMarker :: Core.Maybe Core.Text,
    -- | An array of RuleSummary objects.
    rules :: Core.Maybe [RuleSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRateBasedRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listRateBasedRulesResponse_nextMarker' - If you have more @Rules@ than the number that you specified for @Limit@
-- in the request, the response includes a @NextMarker@ value. To list more
-- @Rules@, submit another @ListRateBasedRules@ request, and specify the
-- @NextMarker@ value from the response in the @NextMarker@ value in the
-- next request.
--
-- 'rules', 'listRateBasedRulesResponse_rules' - An array of RuleSummary objects.
--
-- 'httpStatus', 'listRateBasedRulesResponse_httpStatus' - The response's http status code.
newListRateBasedRulesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListRateBasedRulesResponse
newListRateBasedRulesResponse pHttpStatus_ =
  ListRateBasedRulesResponse'
    { nextMarker =
        Core.Nothing,
      rules = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you have more @Rules@ than the number that you specified for @Limit@
-- in the request, the response includes a @NextMarker@ value. To list more
-- @Rules@, submit another @ListRateBasedRules@ request, and specify the
-- @NextMarker@ value from the response in the @NextMarker@ value in the
-- next request.
listRateBasedRulesResponse_nextMarker :: Lens.Lens' ListRateBasedRulesResponse (Core.Maybe Core.Text)
listRateBasedRulesResponse_nextMarker = Lens.lens (\ListRateBasedRulesResponse' {nextMarker} -> nextMarker) (\s@ListRateBasedRulesResponse' {} a -> s {nextMarker = a} :: ListRateBasedRulesResponse)

-- | An array of RuleSummary objects.
listRateBasedRulesResponse_rules :: Lens.Lens' ListRateBasedRulesResponse (Core.Maybe [RuleSummary])
listRateBasedRulesResponse_rules = Lens.lens (\ListRateBasedRulesResponse' {rules} -> rules) (\s@ListRateBasedRulesResponse' {} a -> s {rules = a} :: ListRateBasedRulesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listRateBasedRulesResponse_httpStatus :: Lens.Lens' ListRateBasedRulesResponse Core.Int
listRateBasedRulesResponse_httpStatus = Lens.lens (\ListRateBasedRulesResponse' {httpStatus} -> httpStatus) (\s@ListRateBasedRulesResponse' {} a -> s {httpStatus = a} :: ListRateBasedRulesResponse)

instance Core.NFData ListRateBasedRulesResponse
