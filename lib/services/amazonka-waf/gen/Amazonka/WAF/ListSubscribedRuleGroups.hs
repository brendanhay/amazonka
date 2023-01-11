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
-- Module      : Amazonka.WAF.ListSubscribedRuleGroups
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
-- Returns an array of RuleGroup objects that you are subscribed to.
--
-- This operation returns paginated results.
module Amazonka.WAF.ListSubscribedRuleGroups
  ( -- * Creating a Request
    ListSubscribedRuleGroups (..),
    newListSubscribedRuleGroups,

    -- * Request Lenses
    listSubscribedRuleGroups_limit,
    listSubscribedRuleGroups_nextMarker,

    -- * Destructuring the Response
    ListSubscribedRuleGroupsResponse (..),
    newListSubscribedRuleGroupsResponse,

    -- * Response Lenses
    listSubscribedRuleGroupsResponse_nextMarker,
    listSubscribedRuleGroupsResponse_ruleGroups,
    listSubscribedRuleGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAF.Types

-- | /See:/ 'newListSubscribedRuleGroups' smart constructor.
data ListSubscribedRuleGroups = ListSubscribedRuleGroups'
  { -- | Specifies the number of subscribed rule groups that you want AWS WAF to
    -- return for this request. If you have more objects than the number you
    -- specify for @Limit@, the response includes a @NextMarker@ value that you
    -- can use to get another batch of objects.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | If you specify a value for @Limit@ and you have more
    -- @ByteMatchSets@subscribed rule groups than the value of @Limit@, AWS WAF
    -- returns a @NextMarker@ value in the response that allows you to list
    -- another group of subscribed rule groups. For the second and subsequent
    -- @ListSubscribedRuleGroupsRequest@ requests, specify the value of
    -- @NextMarker@ from the previous response to get information about another
    -- batch of subscribed rule groups.
    nextMarker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSubscribedRuleGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listSubscribedRuleGroups_limit' - Specifies the number of subscribed rule groups that you want AWS WAF to
-- return for this request. If you have more objects than the number you
-- specify for @Limit@, the response includes a @NextMarker@ value that you
-- can use to get another batch of objects.
--
-- 'nextMarker', 'listSubscribedRuleGroups_nextMarker' - If you specify a value for @Limit@ and you have more
-- @ByteMatchSets@subscribed rule groups than the value of @Limit@, AWS WAF
-- returns a @NextMarker@ value in the response that allows you to list
-- another group of subscribed rule groups. For the second and subsequent
-- @ListSubscribedRuleGroupsRequest@ requests, specify the value of
-- @NextMarker@ from the previous response to get information about another
-- batch of subscribed rule groups.
newListSubscribedRuleGroups ::
  ListSubscribedRuleGroups
newListSubscribedRuleGroups =
  ListSubscribedRuleGroups'
    { limit = Prelude.Nothing,
      nextMarker = Prelude.Nothing
    }

-- | Specifies the number of subscribed rule groups that you want AWS WAF to
-- return for this request. If you have more objects than the number you
-- specify for @Limit@, the response includes a @NextMarker@ value that you
-- can use to get another batch of objects.
listSubscribedRuleGroups_limit :: Lens.Lens' ListSubscribedRuleGroups (Prelude.Maybe Prelude.Natural)
listSubscribedRuleGroups_limit = Lens.lens (\ListSubscribedRuleGroups' {limit} -> limit) (\s@ListSubscribedRuleGroups' {} a -> s {limit = a} :: ListSubscribedRuleGroups)

-- | If you specify a value for @Limit@ and you have more
-- @ByteMatchSets@subscribed rule groups than the value of @Limit@, AWS WAF
-- returns a @NextMarker@ value in the response that allows you to list
-- another group of subscribed rule groups. For the second and subsequent
-- @ListSubscribedRuleGroupsRequest@ requests, specify the value of
-- @NextMarker@ from the previous response to get information about another
-- batch of subscribed rule groups.
listSubscribedRuleGroups_nextMarker :: Lens.Lens' ListSubscribedRuleGroups (Prelude.Maybe Prelude.Text)
listSubscribedRuleGroups_nextMarker = Lens.lens (\ListSubscribedRuleGroups' {nextMarker} -> nextMarker) (\s@ListSubscribedRuleGroups' {} a -> s {nextMarker = a} :: ListSubscribedRuleGroups)

instance Core.AWSPager ListSubscribedRuleGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSubscribedRuleGroupsResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSubscribedRuleGroupsResponse_ruleGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listSubscribedRuleGroups_nextMarker
          Lens..~ rs
          Lens.^? listSubscribedRuleGroupsResponse_nextMarker
            Prelude.. Lens._Just

instance Core.AWSRequest ListSubscribedRuleGroups where
  type
    AWSResponse ListSubscribedRuleGroups =
      ListSubscribedRuleGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSubscribedRuleGroupsResponse'
            Prelude.<$> (x Data..?> "NextMarker")
            Prelude.<*> (x Data..?> "RuleGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSubscribedRuleGroups where
  hashWithSalt _salt ListSubscribedRuleGroups' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextMarker

instance Prelude.NFData ListSubscribedRuleGroups where
  rnf ListSubscribedRuleGroups' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextMarker

instance Data.ToHeaders ListSubscribedRuleGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20150824.ListSubscribedRuleGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSubscribedRuleGroups where
  toJSON ListSubscribedRuleGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextMarker" Data..=) Prelude.<$> nextMarker
          ]
      )

instance Data.ToPath ListSubscribedRuleGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSubscribedRuleGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSubscribedRuleGroupsResponse' smart constructor.
data ListSubscribedRuleGroupsResponse = ListSubscribedRuleGroupsResponse'
  { -- | If you have more objects than the number that you specified for @Limit@
    -- in the request, the response includes a @NextMarker@ value. To list more
    -- objects, submit another @ListSubscribedRuleGroups@ request, and specify
    -- the @NextMarker@ value from the response in the @NextMarker@ value in
    -- the next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | An array of RuleGroup objects.
    ruleGroups :: Prelude.Maybe [SubscribedRuleGroupSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListSubscribedRuleGroupsResponse
newListSubscribedRuleGroupsResponse pHttpStatus_ =
  ListSubscribedRuleGroupsResponse'
    { nextMarker =
        Prelude.Nothing,
      ruleGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you have more objects than the number that you specified for @Limit@
-- in the request, the response includes a @NextMarker@ value. To list more
-- objects, submit another @ListSubscribedRuleGroups@ request, and specify
-- the @NextMarker@ value from the response in the @NextMarker@ value in
-- the next request.
listSubscribedRuleGroupsResponse_nextMarker :: Lens.Lens' ListSubscribedRuleGroupsResponse (Prelude.Maybe Prelude.Text)
listSubscribedRuleGroupsResponse_nextMarker = Lens.lens (\ListSubscribedRuleGroupsResponse' {nextMarker} -> nextMarker) (\s@ListSubscribedRuleGroupsResponse' {} a -> s {nextMarker = a} :: ListSubscribedRuleGroupsResponse)

-- | An array of RuleGroup objects.
listSubscribedRuleGroupsResponse_ruleGroups :: Lens.Lens' ListSubscribedRuleGroupsResponse (Prelude.Maybe [SubscribedRuleGroupSummary])
listSubscribedRuleGroupsResponse_ruleGroups = Lens.lens (\ListSubscribedRuleGroupsResponse' {ruleGroups} -> ruleGroups) (\s@ListSubscribedRuleGroupsResponse' {} a -> s {ruleGroups = a} :: ListSubscribedRuleGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSubscribedRuleGroupsResponse_httpStatus :: Lens.Lens' ListSubscribedRuleGroupsResponse Prelude.Int
listSubscribedRuleGroupsResponse_httpStatus = Lens.lens (\ListSubscribedRuleGroupsResponse' {httpStatus} -> httpStatus) (\s@ListSubscribedRuleGroupsResponse' {} a -> s {httpStatus = a} :: ListSubscribedRuleGroupsResponse)

instance
  Prelude.NFData
    ListSubscribedRuleGroupsResponse
  where
  rnf ListSubscribedRuleGroupsResponse' {..} =
    Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf ruleGroups
      `Prelude.seq` Prelude.rnf httpStatus
