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
-- Module      : Amazonka.WAF.ListActivatedRulesInRuleGroup
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
-- Returns an array of ActivatedRule objects.
--
-- This operation returns paginated results.
module Amazonka.WAF.ListActivatedRulesInRuleGroup
  ( -- * Creating a Request
    ListActivatedRulesInRuleGroup (..),
    newListActivatedRulesInRuleGroup,

    -- * Request Lenses
    listActivatedRulesInRuleGroup_ruleGroupId,
    listActivatedRulesInRuleGroup_limit,
    listActivatedRulesInRuleGroup_nextMarker,

    -- * Destructuring the Response
    ListActivatedRulesInRuleGroupResponse (..),
    newListActivatedRulesInRuleGroupResponse,

    -- * Response Lenses
    listActivatedRulesInRuleGroupResponse_nextMarker,
    listActivatedRulesInRuleGroupResponse_activatedRules,
    listActivatedRulesInRuleGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAF.Types

-- | /See:/ 'newListActivatedRulesInRuleGroup' smart constructor.
data ListActivatedRulesInRuleGroup = ListActivatedRulesInRuleGroup'
  { -- | The @RuleGroupId@ of the RuleGroup for which you want to get a list of
    -- ActivatedRule objects.
    ruleGroupId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the number of @ActivatedRules@ that you want AWS WAF to return
    -- for this request. If you have more @ActivatedRules@ than the number that
    -- you specify for @Limit@, the response includes a @NextMarker@ value that
    -- you can use to get another batch of @ActivatedRules@.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | If you specify a value for @Limit@ and you have more @ActivatedRules@
    -- than the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
    -- response that allows you to list another group of @ActivatedRules@. For
    -- the second and subsequent @ListActivatedRulesInRuleGroup@ requests,
    -- specify the value of @NextMarker@ from the previous response to get
    -- information about another batch of @ActivatedRules@.
    nextMarker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListActivatedRulesInRuleGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleGroupId', 'listActivatedRulesInRuleGroup_ruleGroupId' - The @RuleGroupId@ of the RuleGroup for which you want to get a list of
-- ActivatedRule objects.
--
-- 'limit', 'listActivatedRulesInRuleGroup_limit' - Specifies the number of @ActivatedRules@ that you want AWS WAF to return
-- for this request. If you have more @ActivatedRules@ than the number that
-- you specify for @Limit@, the response includes a @NextMarker@ value that
-- you can use to get another batch of @ActivatedRules@.
--
-- 'nextMarker', 'listActivatedRulesInRuleGroup_nextMarker' - If you specify a value for @Limit@ and you have more @ActivatedRules@
-- than the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
-- response that allows you to list another group of @ActivatedRules@. For
-- the second and subsequent @ListActivatedRulesInRuleGroup@ requests,
-- specify the value of @NextMarker@ from the previous response to get
-- information about another batch of @ActivatedRules@.
newListActivatedRulesInRuleGroup ::
  ListActivatedRulesInRuleGroup
newListActivatedRulesInRuleGroup =
  ListActivatedRulesInRuleGroup'
    { ruleGroupId =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      nextMarker = Prelude.Nothing
    }

-- | The @RuleGroupId@ of the RuleGroup for which you want to get a list of
-- ActivatedRule objects.
listActivatedRulesInRuleGroup_ruleGroupId :: Lens.Lens' ListActivatedRulesInRuleGroup (Prelude.Maybe Prelude.Text)
listActivatedRulesInRuleGroup_ruleGroupId = Lens.lens (\ListActivatedRulesInRuleGroup' {ruleGroupId} -> ruleGroupId) (\s@ListActivatedRulesInRuleGroup' {} a -> s {ruleGroupId = a} :: ListActivatedRulesInRuleGroup)

-- | Specifies the number of @ActivatedRules@ that you want AWS WAF to return
-- for this request. If you have more @ActivatedRules@ than the number that
-- you specify for @Limit@, the response includes a @NextMarker@ value that
-- you can use to get another batch of @ActivatedRules@.
listActivatedRulesInRuleGroup_limit :: Lens.Lens' ListActivatedRulesInRuleGroup (Prelude.Maybe Prelude.Natural)
listActivatedRulesInRuleGroup_limit = Lens.lens (\ListActivatedRulesInRuleGroup' {limit} -> limit) (\s@ListActivatedRulesInRuleGroup' {} a -> s {limit = a} :: ListActivatedRulesInRuleGroup)

-- | If you specify a value for @Limit@ and you have more @ActivatedRules@
-- than the value of @Limit@, AWS WAF returns a @NextMarker@ value in the
-- response that allows you to list another group of @ActivatedRules@. For
-- the second and subsequent @ListActivatedRulesInRuleGroup@ requests,
-- specify the value of @NextMarker@ from the previous response to get
-- information about another batch of @ActivatedRules@.
listActivatedRulesInRuleGroup_nextMarker :: Lens.Lens' ListActivatedRulesInRuleGroup (Prelude.Maybe Prelude.Text)
listActivatedRulesInRuleGroup_nextMarker = Lens.lens (\ListActivatedRulesInRuleGroup' {nextMarker} -> nextMarker) (\s@ListActivatedRulesInRuleGroup' {} a -> s {nextMarker = a} :: ListActivatedRulesInRuleGroup)

instance Core.AWSPager ListActivatedRulesInRuleGroup where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listActivatedRulesInRuleGroupResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listActivatedRulesInRuleGroupResponse_activatedRules
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listActivatedRulesInRuleGroup_nextMarker
          Lens..~ rs
          Lens.^? listActivatedRulesInRuleGroupResponse_nextMarker
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListActivatedRulesInRuleGroup
  where
  type
    AWSResponse ListActivatedRulesInRuleGroup =
      ListActivatedRulesInRuleGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListActivatedRulesInRuleGroupResponse'
            Prelude.<$> (x Core..?> "NextMarker")
            Prelude.<*> (x Core..?> "ActivatedRules" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListActivatedRulesInRuleGroup
  where
  hashWithSalt _salt ListActivatedRulesInRuleGroup' {..} =
    _salt `Prelude.hashWithSalt` ruleGroupId
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextMarker

instance Prelude.NFData ListActivatedRulesInRuleGroup where
  rnf ListActivatedRulesInRuleGroup' {..} =
    Prelude.rnf ruleGroupId
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextMarker

instance Core.ToHeaders ListActivatedRulesInRuleGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20150824.ListActivatedRulesInRuleGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListActivatedRulesInRuleGroup where
  toJSON ListActivatedRulesInRuleGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RuleGroupId" Core..=) Prelude.<$> ruleGroupId,
            ("Limit" Core..=) Prelude.<$> limit,
            ("NextMarker" Core..=) Prelude.<$> nextMarker
          ]
      )

instance Core.ToPath ListActivatedRulesInRuleGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery ListActivatedRulesInRuleGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListActivatedRulesInRuleGroupResponse' smart constructor.
data ListActivatedRulesInRuleGroupResponse = ListActivatedRulesInRuleGroupResponse'
  { -- | If you have more @ActivatedRules@ than the number that you specified for
    -- @Limit@ in the request, the response includes a @NextMarker@ value. To
    -- list more @ActivatedRules@, submit another
    -- @ListActivatedRulesInRuleGroup@ request, and specify the @NextMarker@
    -- value from the response in the @NextMarker@ value in the next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | An array of @ActivatedRules@ objects.
    activatedRules :: Prelude.Maybe [ActivatedRule],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListActivatedRulesInRuleGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listActivatedRulesInRuleGroupResponse_nextMarker' - If you have more @ActivatedRules@ than the number that you specified for
-- @Limit@ in the request, the response includes a @NextMarker@ value. To
-- list more @ActivatedRules@, submit another
-- @ListActivatedRulesInRuleGroup@ request, and specify the @NextMarker@
-- value from the response in the @NextMarker@ value in the next request.
--
-- 'activatedRules', 'listActivatedRulesInRuleGroupResponse_activatedRules' - An array of @ActivatedRules@ objects.
--
-- 'httpStatus', 'listActivatedRulesInRuleGroupResponse_httpStatus' - The response's http status code.
newListActivatedRulesInRuleGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListActivatedRulesInRuleGroupResponse
newListActivatedRulesInRuleGroupResponse pHttpStatus_ =
  ListActivatedRulesInRuleGroupResponse'
    { nextMarker =
        Prelude.Nothing,
      activatedRules = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you have more @ActivatedRules@ than the number that you specified for
-- @Limit@ in the request, the response includes a @NextMarker@ value. To
-- list more @ActivatedRules@, submit another
-- @ListActivatedRulesInRuleGroup@ request, and specify the @NextMarker@
-- value from the response in the @NextMarker@ value in the next request.
listActivatedRulesInRuleGroupResponse_nextMarker :: Lens.Lens' ListActivatedRulesInRuleGroupResponse (Prelude.Maybe Prelude.Text)
listActivatedRulesInRuleGroupResponse_nextMarker = Lens.lens (\ListActivatedRulesInRuleGroupResponse' {nextMarker} -> nextMarker) (\s@ListActivatedRulesInRuleGroupResponse' {} a -> s {nextMarker = a} :: ListActivatedRulesInRuleGroupResponse)

-- | An array of @ActivatedRules@ objects.
listActivatedRulesInRuleGroupResponse_activatedRules :: Lens.Lens' ListActivatedRulesInRuleGroupResponse (Prelude.Maybe [ActivatedRule])
listActivatedRulesInRuleGroupResponse_activatedRules = Lens.lens (\ListActivatedRulesInRuleGroupResponse' {activatedRules} -> activatedRules) (\s@ListActivatedRulesInRuleGroupResponse' {} a -> s {activatedRules = a} :: ListActivatedRulesInRuleGroupResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listActivatedRulesInRuleGroupResponse_httpStatus :: Lens.Lens' ListActivatedRulesInRuleGroupResponse Prelude.Int
listActivatedRulesInRuleGroupResponse_httpStatus = Lens.lens (\ListActivatedRulesInRuleGroupResponse' {httpStatus} -> httpStatus) (\s@ListActivatedRulesInRuleGroupResponse' {} a -> s {httpStatus = a} :: ListActivatedRulesInRuleGroupResponse)

instance
  Prelude.NFData
    ListActivatedRulesInRuleGroupResponse
  where
  rnf ListActivatedRulesInRuleGroupResponse' {..} =
    Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf activatedRules
      `Prelude.seq` Prelude.rnf httpStatus
