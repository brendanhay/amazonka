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
-- Module      : Network.AWS.WAF.ListRules
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
module Network.AWS.WAF.ListRules
  ( -- * Creating a Request
    ListRules (..),
    newListRules,

    -- * Request Lenses
    listRules_nextMarker,
    listRules_limit,

    -- * Destructuring the Response
    ListRulesResponse (..),
    newListRulesResponse,

    -- * Response Lenses
    listRulesResponse_nextMarker,
    listRulesResponse_rules,
    listRulesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newListRules' smart constructor.
data ListRules = ListRules'
  { -- | If you specify a value for @Limit@ and you have more @Rules@ than the
    -- value of @Limit@, AWS WAF returns a @NextMarker@ value in the response
    -- that allows you to list another group of @Rules@. For the second and
    -- subsequent @ListRules@ requests, specify the value of @NextMarker@ from
    -- the previous response to get information about another batch of @Rules@.
    nextMarker :: Core.Maybe Core.Text,
    -- | Specifies the number of @Rules@ that you want AWS WAF to return for this
    -- request. If you have more @Rules@ than the number that you specify for
    -- @Limit@, the response includes a @NextMarker@ value that you can use to
    -- get another batch of @Rules@.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listRules_nextMarker' - If you specify a value for @Limit@ and you have more @Rules@ than the
-- value of @Limit@, AWS WAF returns a @NextMarker@ value in the response
-- that allows you to list another group of @Rules@. For the second and
-- subsequent @ListRules@ requests, specify the value of @NextMarker@ from
-- the previous response to get information about another batch of @Rules@.
--
-- 'limit', 'listRules_limit' - Specifies the number of @Rules@ that you want AWS WAF to return for this
-- request. If you have more @Rules@ than the number that you specify for
-- @Limit@, the response includes a @NextMarker@ value that you can use to
-- get another batch of @Rules@.
newListRules ::
  ListRules
newListRules =
  ListRules'
    { nextMarker = Core.Nothing,
      limit = Core.Nothing
    }

-- | If you specify a value for @Limit@ and you have more @Rules@ than the
-- value of @Limit@, AWS WAF returns a @NextMarker@ value in the response
-- that allows you to list another group of @Rules@. For the second and
-- subsequent @ListRules@ requests, specify the value of @NextMarker@ from
-- the previous response to get information about another batch of @Rules@.
listRules_nextMarker :: Lens.Lens' ListRules (Core.Maybe Core.Text)
listRules_nextMarker = Lens.lens (\ListRules' {nextMarker} -> nextMarker) (\s@ListRules' {} a -> s {nextMarker = a} :: ListRules)

-- | Specifies the number of @Rules@ that you want AWS WAF to return for this
-- request. If you have more @Rules@ than the number that you specify for
-- @Limit@, the response includes a @NextMarker@ value that you can use to
-- get another batch of @Rules@.
listRules_limit :: Lens.Lens' ListRules (Core.Maybe Core.Natural)
listRules_limit = Lens.lens (\ListRules' {limit} -> limit) (\s@ListRules' {} a -> s {limit = a} :: ListRules)

instance Core.AWSPager ListRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRulesResponse_nextMarker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listRulesResponse_rules Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listRules_nextMarker
          Lens..~ rs
          Lens.^? listRulesResponse_nextMarker Core.. Lens._Just

instance Core.AWSRequest ListRules where
  type AWSResponse ListRules = ListRulesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRulesResponse'
            Core.<$> (x Core..?> "NextMarker")
            Core.<*> (x Core..?> "Rules" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListRules

instance Core.NFData ListRules

instance Core.ToHeaders ListRules where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSWAF_20150824.ListRules" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListRules where
  toJSON ListRules' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextMarker" Core..=) Core.<$> nextMarker,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath ListRules where
  toPath = Core.const "/"

instance Core.ToQuery ListRules where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListRulesResponse' smart constructor.
data ListRulesResponse = ListRulesResponse'
  { -- | If you have more @Rules@ than the number that you specified for @Limit@
    -- in the request, the response includes a @NextMarker@ value. To list more
    -- @Rules@, submit another @ListRules@ request, and specify the
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
-- Create a value of 'ListRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listRulesResponse_nextMarker' - If you have more @Rules@ than the number that you specified for @Limit@
-- in the request, the response includes a @NextMarker@ value. To list more
-- @Rules@, submit another @ListRules@ request, and specify the
-- @NextMarker@ value from the response in the @NextMarker@ value in the
-- next request.
--
-- 'rules', 'listRulesResponse_rules' - An array of RuleSummary objects.
--
-- 'httpStatus', 'listRulesResponse_httpStatus' - The response's http status code.
newListRulesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListRulesResponse
newListRulesResponse pHttpStatus_ =
  ListRulesResponse'
    { nextMarker = Core.Nothing,
      rules = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you have more @Rules@ than the number that you specified for @Limit@
-- in the request, the response includes a @NextMarker@ value. To list more
-- @Rules@, submit another @ListRules@ request, and specify the
-- @NextMarker@ value from the response in the @NextMarker@ value in the
-- next request.
listRulesResponse_nextMarker :: Lens.Lens' ListRulesResponse (Core.Maybe Core.Text)
listRulesResponse_nextMarker = Lens.lens (\ListRulesResponse' {nextMarker} -> nextMarker) (\s@ListRulesResponse' {} a -> s {nextMarker = a} :: ListRulesResponse)

-- | An array of RuleSummary objects.
listRulesResponse_rules :: Lens.Lens' ListRulesResponse (Core.Maybe [RuleSummary])
listRulesResponse_rules = Lens.lens (\ListRulesResponse' {rules} -> rules) (\s@ListRulesResponse' {} a -> s {rules = a} :: ListRulesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listRulesResponse_httpStatus :: Lens.Lens' ListRulesResponse Core.Int
listRulesResponse_httpStatus = Lens.lens (\ListRulesResponse' {httpStatus} -> httpStatus) (\s@ListRulesResponse' {} a -> s {httpStatus = a} :: ListRulesResponse)

instance Core.NFData ListRulesResponse
