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
-- Module      : Network.AWS.WAF.ListSubscribedRuleGroups
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
-- Returns an array of RuleGroup objects that you are subscribed to.
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListSubscribedRuleGroups
  ( -- * Creating a Request
    ListSubscribedRuleGroups (..),
    newListSubscribedRuleGroups,

    -- * Request Lenses
    listSubscribedRuleGroups_nextMarker,
    listSubscribedRuleGroups_limit,

    -- * Destructuring the Response
    ListSubscribedRuleGroupsResponse (..),
    newListSubscribedRuleGroupsResponse,

    -- * Response Lenses
    listSubscribedRuleGroupsResponse_nextMarker,
    listSubscribedRuleGroupsResponse_ruleGroups,
    listSubscribedRuleGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newListSubscribedRuleGroups' smart constructor.
data ListSubscribedRuleGroups = ListSubscribedRuleGroups'
  { -- | If you specify a value for @Limit@ and you have more
    -- @ByteMatchSets@subscribed rule groups than the value of @Limit@, AWS WAF
    -- returns a @NextMarker@ value in the response that allows you to list
    -- another group of subscribed rule groups. For the second and subsequent
    -- @ListSubscribedRuleGroupsRequest@ requests, specify the value of
    -- @NextMarker@ from the previous response to get information about another
    -- batch of subscribed rule groups.
    nextMarker :: Core.Maybe Core.Text,
    -- | Specifies the number of subscribed rule groups that you want AWS WAF to
    -- return for this request. If you have more objects than the number you
    -- specify for @Limit@, the response includes a @NextMarker@ value that you
    -- can use to get another batch of objects.
    limit :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSubscribedRuleGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listSubscribedRuleGroups_nextMarker' - If you specify a value for @Limit@ and you have more
-- @ByteMatchSets@subscribed rule groups than the value of @Limit@, AWS WAF
-- returns a @NextMarker@ value in the response that allows you to list
-- another group of subscribed rule groups. For the second and subsequent
-- @ListSubscribedRuleGroupsRequest@ requests, specify the value of
-- @NextMarker@ from the previous response to get information about another
-- batch of subscribed rule groups.
--
-- 'limit', 'listSubscribedRuleGroups_limit' - Specifies the number of subscribed rule groups that you want AWS WAF to
-- return for this request. If you have more objects than the number you
-- specify for @Limit@, the response includes a @NextMarker@ value that you
-- can use to get another batch of objects.
newListSubscribedRuleGroups ::
  ListSubscribedRuleGroups
newListSubscribedRuleGroups =
  ListSubscribedRuleGroups'
    { nextMarker =
        Core.Nothing,
      limit = Core.Nothing
    }

-- | If you specify a value for @Limit@ and you have more
-- @ByteMatchSets@subscribed rule groups than the value of @Limit@, AWS WAF
-- returns a @NextMarker@ value in the response that allows you to list
-- another group of subscribed rule groups. For the second and subsequent
-- @ListSubscribedRuleGroupsRequest@ requests, specify the value of
-- @NextMarker@ from the previous response to get information about another
-- batch of subscribed rule groups.
listSubscribedRuleGroups_nextMarker :: Lens.Lens' ListSubscribedRuleGroups (Core.Maybe Core.Text)
listSubscribedRuleGroups_nextMarker = Lens.lens (\ListSubscribedRuleGroups' {nextMarker} -> nextMarker) (\s@ListSubscribedRuleGroups' {} a -> s {nextMarker = a} :: ListSubscribedRuleGroups)

-- | Specifies the number of subscribed rule groups that you want AWS WAF to
-- return for this request. If you have more objects than the number you
-- specify for @Limit@, the response includes a @NextMarker@ value that you
-- can use to get another batch of objects.
listSubscribedRuleGroups_limit :: Lens.Lens' ListSubscribedRuleGroups (Core.Maybe Core.Natural)
listSubscribedRuleGroups_limit = Lens.lens (\ListSubscribedRuleGroups' {limit} -> limit) (\s@ListSubscribedRuleGroups' {} a -> s {limit = a} :: ListSubscribedRuleGroups)

instance Core.AWSPager ListSubscribedRuleGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSubscribedRuleGroupsResponse_nextMarker
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listSubscribedRuleGroupsResponse_ruleGroups
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listSubscribedRuleGroups_nextMarker
          Lens..~ rs
          Lens.^? listSubscribedRuleGroupsResponse_nextMarker
            Core.. Lens._Just

instance Core.AWSRequest ListSubscribedRuleGroups where
  type
    AWSResponse ListSubscribedRuleGroups =
      ListSubscribedRuleGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSubscribedRuleGroupsResponse'
            Core.<$> (x Core..?> "NextMarker")
            Core.<*> (x Core..?> "RuleGroups" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListSubscribedRuleGroups

instance Core.NFData ListSubscribedRuleGroups

instance Core.ToHeaders ListSubscribedRuleGroups where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.ListSubscribedRuleGroups" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListSubscribedRuleGroups where
  toJSON ListSubscribedRuleGroups' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextMarker" Core..=) Core.<$> nextMarker,
            ("Limit" Core..=) Core.<$> limit
          ]
      )

instance Core.ToPath ListSubscribedRuleGroups where
  toPath = Core.const "/"

instance Core.ToQuery ListSubscribedRuleGroups where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListSubscribedRuleGroupsResponse' smart constructor.
data ListSubscribedRuleGroupsResponse = ListSubscribedRuleGroupsResponse'
  { -- | If you have more objects than the number that you specified for @Limit@
    -- in the request, the response includes a @NextMarker@ value. To list more
    -- objects, submit another @ListSubscribedRuleGroups@ request, and specify
    -- the @NextMarker@ value from the response in the @NextMarker@ value in
    -- the next request.
    nextMarker :: Core.Maybe Core.Text,
    -- | An array of RuleGroup objects.
    ruleGroups :: Core.Maybe [SubscribedRuleGroupSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSubscribedRuleGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listSubscribedRuleGroupsResponse_nextMarker' - If you have more objects than the number that you specified for @Limit@
-- in the request, the response includes a @NextMarker@ value. To list more
-- objects, submit another @ListSubscribedRuleGroups@ request, and specify
-- the @NextMarker@ value from the response in the @NextMarker@ value in
-- the next request.
--
-- 'ruleGroups', 'listSubscribedRuleGroupsResponse_ruleGroups' - An array of RuleGroup objects.
--
-- 'httpStatus', 'listSubscribedRuleGroupsResponse_httpStatus' - The response's http status code.
newListSubscribedRuleGroupsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListSubscribedRuleGroupsResponse
newListSubscribedRuleGroupsResponse pHttpStatus_ =
  ListSubscribedRuleGroupsResponse'
    { nextMarker =
        Core.Nothing,
      ruleGroups = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you have more objects than the number that you specified for @Limit@
-- in the request, the response includes a @NextMarker@ value. To list more
-- objects, submit another @ListSubscribedRuleGroups@ request, and specify
-- the @NextMarker@ value from the response in the @NextMarker@ value in
-- the next request.
listSubscribedRuleGroupsResponse_nextMarker :: Lens.Lens' ListSubscribedRuleGroupsResponse (Core.Maybe Core.Text)
listSubscribedRuleGroupsResponse_nextMarker = Lens.lens (\ListSubscribedRuleGroupsResponse' {nextMarker} -> nextMarker) (\s@ListSubscribedRuleGroupsResponse' {} a -> s {nextMarker = a} :: ListSubscribedRuleGroupsResponse)

-- | An array of RuleGroup objects.
listSubscribedRuleGroupsResponse_ruleGroups :: Lens.Lens' ListSubscribedRuleGroupsResponse (Core.Maybe [SubscribedRuleGroupSummary])
listSubscribedRuleGroupsResponse_ruleGroups = Lens.lens (\ListSubscribedRuleGroupsResponse' {ruleGroups} -> ruleGroups) (\s@ListSubscribedRuleGroupsResponse' {} a -> s {ruleGroups = a} :: ListSubscribedRuleGroupsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSubscribedRuleGroupsResponse_httpStatus :: Lens.Lens' ListSubscribedRuleGroupsResponse Core.Int
listSubscribedRuleGroupsResponse_httpStatus = Lens.lens (\ListSubscribedRuleGroupsResponse' {httpStatus} -> httpStatus) (\s@ListSubscribedRuleGroupsResponse' {} a -> s {httpStatus = a} :: ListSubscribedRuleGroupsResponse)

instance Core.NFData ListSubscribedRuleGroupsResponse
