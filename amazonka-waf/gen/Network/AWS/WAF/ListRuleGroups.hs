{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.WAF.ListRuleGroups
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
-- Returns an array of RuleGroup objects.
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListRuleGroups
  ( -- * Creating a Request
    ListRuleGroups (..),
    newListRuleGroups,

    -- * Request Lenses
    listRuleGroups_nextMarker,
    listRuleGroups_limit,

    -- * Destructuring the Response
    ListRuleGroupsResponse (..),
    newListRuleGroupsResponse,

    -- * Response Lenses
    listRuleGroupsResponse_nextMarker,
    listRuleGroupsResponse_ruleGroups,
    listRuleGroupsResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WAF.Types

-- | /See:/ 'newListRuleGroups' smart constructor.
data ListRuleGroups = ListRuleGroups'
  { -- | If you specify a value for @Limit@ and you have more @RuleGroups@ than
    -- the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
    -- response that allows you to list another group of @RuleGroups@. For the
    -- second and subsequent @ListRuleGroups@ requests, specify the value of
    -- @NextMarker@ from the previous response to get information about another
    -- batch of @RuleGroups@.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | Specifies the number of @RuleGroups@ that you want AWS WAF to return for
    -- this request. If you have more @RuleGroups@ than the number that you
    -- specify for @Limit@, the response includes a @NextMarker@ value that you
    -- can use to get another batch of @RuleGroups@.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListRuleGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listRuleGroups_nextMarker' - If you specify a value for @Limit@ and you have more @RuleGroups@ than
-- the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
-- response that allows you to list another group of @RuleGroups@. For the
-- second and subsequent @ListRuleGroups@ requests, specify the value of
-- @NextMarker@ from the previous response to get information about another
-- batch of @RuleGroups@.
--
-- 'limit', 'listRuleGroups_limit' - Specifies the number of @RuleGroups@ that you want AWS WAF to return for
-- this request. If you have more @RuleGroups@ than the number that you
-- specify for @Limit@, the response includes a @NextMarker@ value that you
-- can use to get another batch of @RuleGroups@.
newListRuleGroups ::
  ListRuleGroups
newListRuleGroups =
  ListRuleGroups'
    { nextMarker = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | If you specify a value for @Limit@ and you have more @RuleGroups@ than
-- the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
-- response that allows you to list another group of @RuleGroups@. For the
-- second and subsequent @ListRuleGroups@ requests, specify the value of
-- @NextMarker@ from the previous response to get information about another
-- batch of @RuleGroups@.
listRuleGroups_nextMarker :: Lens.Lens' ListRuleGroups (Prelude.Maybe Prelude.Text)
listRuleGroups_nextMarker = Lens.lens (\ListRuleGroups' {nextMarker} -> nextMarker) (\s@ListRuleGroups' {} a -> s {nextMarker = a} :: ListRuleGroups)

-- | Specifies the number of @RuleGroups@ that you want AWS WAF to return for
-- this request. If you have more @RuleGroups@ than the number that you
-- specify for @Limit@, the response includes a @NextMarker@ value that you
-- can use to get another batch of @RuleGroups@.
listRuleGroups_limit :: Lens.Lens' ListRuleGroups (Prelude.Maybe Prelude.Natural)
listRuleGroups_limit = Lens.lens (\ListRuleGroups' {limit} -> limit) (\s@ListRuleGroups' {} a -> s {limit = a} :: ListRuleGroups)

instance Pager.AWSPager ListRuleGroups where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listRuleGroupsResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listRuleGroupsResponse_ruleGroups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listRuleGroups_nextMarker
          Lens..~ rs
          Lens.^? listRuleGroupsResponse_nextMarker
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListRuleGroups where
  type Rs ListRuleGroups = ListRuleGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRuleGroupsResponse'
            Prelude.<$> (x Prelude..?> "NextMarker")
            Prelude.<*> ( x Prelude..?> "RuleGroups"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRuleGroups

instance Prelude.NFData ListRuleGroups

instance Prelude.ToHeaders ListRuleGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSWAF_20150824.ListRuleGroups" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListRuleGroups where
  toJSON ListRuleGroups' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextMarker" Prelude..=) Prelude.<$> nextMarker,
            ("Limit" Prelude..=) Prelude.<$> limit
          ]
      )

instance Prelude.ToPath ListRuleGroups where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListRuleGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRuleGroupsResponse' smart constructor.
data ListRuleGroupsResponse = ListRuleGroupsResponse'
  { -- | If you have more @RuleGroups@ than the number that you specified for
    -- @Limit@ in the request, the response includes a @NextMarker@ value. To
    -- list more @RuleGroups@, submit another @ListRuleGroups@ request, and
    -- specify the @NextMarker@ value from the response in the @NextMarker@
    -- value in the next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | An array of RuleGroup objects.
    ruleGroups :: Prelude.Maybe [RuleGroupSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListRuleGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listRuleGroupsResponse_nextMarker' - If you have more @RuleGroups@ than the number that you specified for
-- @Limit@ in the request, the response includes a @NextMarker@ value. To
-- list more @RuleGroups@, submit another @ListRuleGroups@ request, and
-- specify the @NextMarker@ value from the response in the @NextMarker@
-- value in the next request.
--
-- 'ruleGroups', 'listRuleGroupsResponse_ruleGroups' - An array of RuleGroup objects.
--
-- 'httpStatus', 'listRuleGroupsResponse_httpStatus' - The response's http status code.
newListRuleGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRuleGroupsResponse
newListRuleGroupsResponse pHttpStatus_ =
  ListRuleGroupsResponse'
    { nextMarker =
        Prelude.Nothing,
      ruleGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you have more @RuleGroups@ than the number that you specified for
-- @Limit@ in the request, the response includes a @NextMarker@ value. To
-- list more @RuleGroups@, submit another @ListRuleGroups@ request, and
-- specify the @NextMarker@ value from the response in the @NextMarker@
-- value in the next request.
listRuleGroupsResponse_nextMarker :: Lens.Lens' ListRuleGroupsResponse (Prelude.Maybe Prelude.Text)
listRuleGroupsResponse_nextMarker = Lens.lens (\ListRuleGroupsResponse' {nextMarker} -> nextMarker) (\s@ListRuleGroupsResponse' {} a -> s {nextMarker = a} :: ListRuleGroupsResponse)

-- | An array of RuleGroup objects.
listRuleGroupsResponse_ruleGroups :: Lens.Lens' ListRuleGroupsResponse (Prelude.Maybe [RuleGroupSummary])
listRuleGroupsResponse_ruleGroups = Lens.lens (\ListRuleGroupsResponse' {ruleGroups} -> ruleGroups) (\s@ListRuleGroupsResponse' {} a -> s {ruleGroups = a} :: ListRuleGroupsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listRuleGroupsResponse_httpStatus :: Lens.Lens' ListRuleGroupsResponse Prelude.Int
listRuleGroupsResponse_httpStatus = Lens.lens (\ListRuleGroupsResponse' {httpStatus} -> httpStatus) (\s@ListRuleGroupsResponse' {} a -> s {httpStatus = a} :: ListRuleGroupsResponse)

instance Prelude.NFData ListRuleGroupsResponse
