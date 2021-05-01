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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
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
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | Specifies the number of @Rules@ that you want AWS WAF to return for this
    -- request. If you have more @Rules@ than the number that you specify for
    -- @Limit@, the response includes a @NextMarker@ value that you can use to
    -- get another batch of @Rules@.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { nextMarker = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | If you specify a value for @Limit@ and you have more @Rules@ than the
-- value of @Limit@, AWS WAF returns a @NextMarker@ value in the response
-- that allows you to list another group of @Rules@. For the second and
-- subsequent @ListRateBasedRules@ requests, specify the value of
-- @NextMarker@ from the previous response to get information about another
-- batch of @Rules@.
listRateBasedRules_nextMarker :: Lens.Lens' ListRateBasedRules (Prelude.Maybe Prelude.Text)
listRateBasedRules_nextMarker = Lens.lens (\ListRateBasedRules' {nextMarker} -> nextMarker) (\s@ListRateBasedRules' {} a -> s {nextMarker = a} :: ListRateBasedRules)

-- | Specifies the number of @Rules@ that you want AWS WAF to return for this
-- request. If you have more @Rules@ than the number that you specify for
-- @Limit@, the response includes a @NextMarker@ value that you can use to
-- get another batch of @Rules@.
listRateBasedRules_limit :: Lens.Lens' ListRateBasedRules (Prelude.Maybe Prelude.Natural)
listRateBasedRules_limit = Lens.lens (\ListRateBasedRules' {limit} -> limit) (\s@ListRateBasedRules' {} a -> s {limit = a} :: ListRateBasedRules)

instance Pager.AWSPager ListRateBasedRules where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listRateBasedRulesResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listRateBasedRulesResponse_rules
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listRateBasedRules_nextMarker
          Lens..~ rs
          Lens.^? listRateBasedRulesResponse_nextMarker
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListRateBasedRules where
  type
    Rs ListRateBasedRules =
      ListRateBasedRulesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRateBasedRulesResponse'
            Prelude.<$> (x Prelude..?> "NextMarker")
            Prelude.<*> (x Prelude..?> "Rules" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRateBasedRules

instance Prelude.NFData ListRateBasedRules

instance Prelude.ToHeaders ListRateBasedRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSWAF_20150824.ListRateBasedRules" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListRateBasedRules where
  toJSON ListRateBasedRules' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextMarker" Prelude..=) Prelude.<$> nextMarker,
            ("Limit" Prelude..=) Prelude.<$> limit
          ]
      )

instance Prelude.ToPath ListRateBasedRules where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListRateBasedRules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRateBasedRulesResponse' smart constructor.
data ListRateBasedRulesResponse = ListRateBasedRulesResponse'
  { -- | If you have more @Rules@ than the number that you specified for @Limit@
    -- in the request, the response includes a @NextMarker@ value. To list more
    -- @Rules@, submit another @ListRateBasedRules@ request, and specify the
    -- @NextMarker@ value from the response in the @NextMarker@ value in the
    -- next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | An array of RuleSummary objects.
    rules :: Prelude.Maybe [RuleSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListRateBasedRulesResponse
newListRateBasedRulesResponse pHttpStatus_ =
  ListRateBasedRulesResponse'
    { nextMarker =
        Prelude.Nothing,
      rules = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If you have more @Rules@ than the number that you specified for @Limit@
-- in the request, the response includes a @NextMarker@ value. To list more
-- @Rules@, submit another @ListRateBasedRules@ request, and specify the
-- @NextMarker@ value from the response in the @NextMarker@ value in the
-- next request.
listRateBasedRulesResponse_nextMarker :: Lens.Lens' ListRateBasedRulesResponse (Prelude.Maybe Prelude.Text)
listRateBasedRulesResponse_nextMarker = Lens.lens (\ListRateBasedRulesResponse' {nextMarker} -> nextMarker) (\s@ListRateBasedRulesResponse' {} a -> s {nextMarker = a} :: ListRateBasedRulesResponse)

-- | An array of RuleSummary objects.
listRateBasedRulesResponse_rules :: Lens.Lens' ListRateBasedRulesResponse (Prelude.Maybe [RuleSummary])
listRateBasedRulesResponse_rules = Lens.lens (\ListRateBasedRulesResponse' {rules} -> rules) (\s@ListRateBasedRulesResponse' {} a -> s {rules = a} :: ListRateBasedRulesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
listRateBasedRulesResponse_httpStatus :: Lens.Lens' ListRateBasedRulesResponse Prelude.Int
listRateBasedRulesResponse_httpStatus = Lens.lens (\ListRateBasedRulesResponse' {httpStatus} -> httpStatus) (\s@ListRateBasedRulesResponse' {} a -> s {httpStatus = a} :: ListRateBasedRulesResponse)

instance Prelude.NFData ListRateBasedRulesResponse
