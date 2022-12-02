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
-- Module      : Amazonka.WAF.ListRuleGroups
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
-- Returns an array of RuleGroup objects.
--
-- This operation returns paginated results.
module Amazonka.WAF.ListRuleGroups
  ( -- * Creating a Request
    ListRuleGroups (..),
    newListRuleGroups,

    -- * Request Lenses
    listRuleGroups_limit,
    listRuleGroups_nextMarker,

    -- * Destructuring the Response
    ListRuleGroupsResponse (..),
    newListRuleGroupsResponse,

    -- * Response Lenses
    listRuleGroupsResponse_ruleGroups,
    listRuleGroupsResponse_nextMarker,
    listRuleGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAF.Types

-- | /See:/ 'newListRuleGroups' smart constructor.
data ListRuleGroups = ListRuleGroups'
  { -- | Specifies the number of @RuleGroups@ that you want AWS WAF to return for
    -- this request. If you have more @RuleGroups@ than the number that you
    -- specify for @Limit@, the response includes a @NextMarker@ value that you
    -- can use to get another batch of @RuleGroups@.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | If you specify a value for @Limit@ and you have more @RuleGroups@ than
    -- the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
    -- response that allows you to list another group of @RuleGroups@. For the
    -- second and subsequent @ListRuleGroups@ requests, specify the value of
    -- @NextMarker@ from the previous response to get information about another
    -- batch of @RuleGroups@.
    nextMarker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRuleGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listRuleGroups_limit' - Specifies the number of @RuleGroups@ that you want AWS WAF to return for
-- this request. If you have more @RuleGroups@ than the number that you
-- specify for @Limit@, the response includes a @NextMarker@ value that you
-- can use to get another batch of @RuleGroups@.
--
-- 'nextMarker', 'listRuleGroups_nextMarker' - If you specify a value for @Limit@ and you have more @RuleGroups@ than
-- the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
-- response that allows you to list another group of @RuleGroups@. For the
-- second and subsequent @ListRuleGroups@ requests, specify the value of
-- @NextMarker@ from the previous response to get information about another
-- batch of @RuleGroups@.
newListRuleGroups ::
  ListRuleGroups
newListRuleGroups =
  ListRuleGroups'
    { limit = Prelude.Nothing,
      nextMarker = Prelude.Nothing
    }

-- | Specifies the number of @RuleGroups@ that you want AWS WAF to return for
-- this request. If you have more @RuleGroups@ than the number that you
-- specify for @Limit@, the response includes a @NextMarker@ value that you
-- can use to get another batch of @RuleGroups@.
listRuleGroups_limit :: Lens.Lens' ListRuleGroups (Prelude.Maybe Prelude.Natural)
listRuleGroups_limit = Lens.lens (\ListRuleGroups' {limit} -> limit) (\s@ListRuleGroups' {} a -> s {limit = a} :: ListRuleGroups)

-- | If you specify a value for @Limit@ and you have more @RuleGroups@ than
-- the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
-- response that allows you to list another group of @RuleGroups@. For the
-- second and subsequent @ListRuleGroups@ requests, specify the value of
-- @NextMarker@ from the previous response to get information about another
-- batch of @RuleGroups@.
listRuleGroups_nextMarker :: Lens.Lens' ListRuleGroups (Prelude.Maybe Prelude.Text)
listRuleGroups_nextMarker = Lens.lens (\ListRuleGroups' {nextMarker} -> nextMarker) (\s@ListRuleGroups' {} a -> s {nextMarker = a} :: ListRuleGroups)

instance Core.AWSPager ListRuleGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRuleGroupsResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRuleGroupsResponse_ruleGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRuleGroups_nextMarker
          Lens..~ rs
          Lens.^? listRuleGroupsResponse_nextMarker
            Prelude.. Lens._Just

instance Core.AWSRequest ListRuleGroups where
  type
    AWSResponse ListRuleGroups =
      ListRuleGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRuleGroupsResponse'
            Prelude.<$> (x Data..?> "RuleGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRuleGroups where
  hashWithSalt _salt ListRuleGroups' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextMarker

instance Prelude.NFData ListRuleGroups where
  rnf ListRuleGroups' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextMarker

instance Data.ToHeaders ListRuleGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20150824.ListRuleGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRuleGroups where
  toJSON ListRuleGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextMarker" Data..=) Prelude.<$> nextMarker
          ]
      )

instance Data.ToPath ListRuleGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery ListRuleGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRuleGroupsResponse' smart constructor.
data ListRuleGroupsResponse = ListRuleGroupsResponse'
  { -- | An array of RuleGroup objects.
    ruleGroups :: Prelude.Maybe [RuleGroupSummary],
    -- | If you have more @RuleGroups@ than the number that you specified for
    -- @Limit@ in the request, the response includes a @NextMarker@ value. To
    -- list more @RuleGroups@, submit another @ListRuleGroups@ request, and
    -- specify the @NextMarker@ value from the response in the @NextMarker@
    -- value in the next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRuleGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleGroups', 'listRuleGroupsResponse_ruleGroups' - An array of RuleGroup objects.
--
-- 'nextMarker', 'listRuleGroupsResponse_nextMarker' - If you have more @RuleGroups@ than the number that you specified for
-- @Limit@ in the request, the response includes a @NextMarker@ value. To
-- list more @RuleGroups@, submit another @ListRuleGroups@ request, and
-- specify the @NextMarker@ value from the response in the @NextMarker@
-- value in the next request.
--
-- 'httpStatus', 'listRuleGroupsResponse_httpStatus' - The response's http status code.
newListRuleGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRuleGroupsResponse
newListRuleGroupsResponse pHttpStatus_ =
  ListRuleGroupsResponse'
    { ruleGroups =
        Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of RuleGroup objects.
listRuleGroupsResponse_ruleGroups :: Lens.Lens' ListRuleGroupsResponse (Prelude.Maybe [RuleGroupSummary])
listRuleGroupsResponse_ruleGroups = Lens.lens (\ListRuleGroupsResponse' {ruleGroups} -> ruleGroups) (\s@ListRuleGroupsResponse' {} a -> s {ruleGroups = a} :: ListRuleGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If you have more @RuleGroups@ than the number that you specified for
-- @Limit@ in the request, the response includes a @NextMarker@ value. To
-- list more @RuleGroups@, submit another @ListRuleGroups@ request, and
-- specify the @NextMarker@ value from the response in the @NextMarker@
-- value in the next request.
listRuleGroupsResponse_nextMarker :: Lens.Lens' ListRuleGroupsResponse (Prelude.Maybe Prelude.Text)
listRuleGroupsResponse_nextMarker = Lens.lens (\ListRuleGroupsResponse' {nextMarker} -> nextMarker) (\s@ListRuleGroupsResponse' {} a -> s {nextMarker = a} :: ListRuleGroupsResponse)

-- | The response's http status code.
listRuleGroupsResponse_httpStatus :: Lens.Lens' ListRuleGroupsResponse Prelude.Int
listRuleGroupsResponse_httpStatus = Lens.lens (\ListRuleGroupsResponse' {httpStatus} -> httpStatus) (\s@ListRuleGroupsResponse' {} a -> s {httpStatus = a} :: ListRuleGroupsResponse)

instance Prelude.NFData ListRuleGroupsResponse where
  rnf ListRuleGroupsResponse' {..} =
    Prelude.rnf ruleGroups
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
