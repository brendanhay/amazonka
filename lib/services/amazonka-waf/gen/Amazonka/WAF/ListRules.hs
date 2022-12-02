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
-- Module      : Amazonka.WAF.ListRules
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
-- Returns an array of RuleSummary objects.
--
-- This operation returns paginated results.
module Amazonka.WAF.ListRules
  ( -- * Creating a Request
    ListRules (..),
    newListRules,

    -- * Request Lenses
    listRules_limit,
    listRules_nextMarker,

    -- * Destructuring the Response
    ListRulesResponse (..),
    newListRulesResponse,

    -- * Response Lenses
    listRulesResponse_rules,
    listRulesResponse_nextMarker,
    listRulesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAF.Types

-- | /See:/ 'newListRules' smart constructor.
data ListRules = ListRules'
  { -- | Specifies the number of @Rules@ that you want AWS WAF to return for this
    -- request. If you have more @Rules@ than the number that you specify for
    -- @Limit@, the response includes a @NextMarker@ value that you can use to
    -- get another batch of @Rules@.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | If you specify a value for @Limit@ and you have more @Rules@ than the
    -- value of @Limit@, AWS WAF returns a @NextMarker@ value in the response
    -- that allows you to list another group of @Rules@. For the second and
    -- subsequent @ListRules@ requests, specify the value of @NextMarker@ from
    -- the previous response to get information about another batch of @Rules@.
    nextMarker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listRules_limit' - Specifies the number of @Rules@ that you want AWS WAF to return for this
-- request. If you have more @Rules@ than the number that you specify for
-- @Limit@, the response includes a @NextMarker@ value that you can use to
-- get another batch of @Rules@.
--
-- 'nextMarker', 'listRules_nextMarker' - If you specify a value for @Limit@ and you have more @Rules@ than the
-- value of @Limit@, AWS WAF returns a @NextMarker@ value in the response
-- that allows you to list another group of @Rules@. For the second and
-- subsequent @ListRules@ requests, specify the value of @NextMarker@ from
-- the previous response to get information about another batch of @Rules@.
newListRules ::
  ListRules
newListRules =
  ListRules'
    { limit = Prelude.Nothing,
      nextMarker = Prelude.Nothing
    }

-- | Specifies the number of @Rules@ that you want AWS WAF to return for this
-- request. If you have more @Rules@ than the number that you specify for
-- @Limit@, the response includes a @NextMarker@ value that you can use to
-- get another batch of @Rules@.
listRules_limit :: Lens.Lens' ListRules (Prelude.Maybe Prelude.Natural)
listRules_limit = Lens.lens (\ListRules' {limit} -> limit) (\s@ListRules' {} a -> s {limit = a} :: ListRules)

-- | If you specify a value for @Limit@ and you have more @Rules@ than the
-- value of @Limit@, AWS WAF returns a @NextMarker@ value in the response
-- that allows you to list another group of @Rules@. For the second and
-- subsequent @ListRules@ requests, specify the value of @NextMarker@ from
-- the previous response to get information about another batch of @Rules@.
listRules_nextMarker :: Lens.Lens' ListRules (Prelude.Maybe Prelude.Text)
listRules_nextMarker = Lens.lens (\ListRules' {nextMarker} -> nextMarker) (\s@ListRules' {} a -> s {nextMarker = a} :: ListRules)

instance Core.AWSPager ListRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRulesResponse_nextMarker Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRulesResponse_rules Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRules_nextMarker
          Lens..~ rs
          Lens.^? listRulesResponse_nextMarker Prelude.. Lens._Just

instance Core.AWSRequest ListRules where
  type AWSResponse ListRules = ListRulesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRulesResponse'
            Prelude.<$> (x Data..?> "Rules" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRules where
  hashWithSalt _salt ListRules' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextMarker

instance Prelude.NFData ListRules where
  rnf ListRules' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextMarker

instance Data.ToHeaders ListRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSWAF_20150824.ListRules" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRules where
  toJSON ListRules' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextMarker" Data..=) Prelude.<$> nextMarker
          ]
      )

instance Data.ToPath ListRules where
  toPath = Prelude.const "/"

instance Data.ToQuery ListRules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRulesResponse' smart constructor.
data ListRulesResponse = ListRulesResponse'
  { -- | An array of RuleSummary objects.
    rules :: Prelude.Maybe [RuleSummary],
    -- | If you have more @Rules@ than the number that you specified for @Limit@
    -- in the request, the response includes a @NextMarker@ value. To list more
    -- @Rules@, submit another @ListRules@ request, and specify the
    -- @NextMarker@ value from the response in the @NextMarker@ value in the
    -- next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rules', 'listRulesResponse_rules' - An array of RuleSummary objects.
--
-- 'nextMarker', 'listRulesResponse_nextMarker' - If you have more @Rules@ than the number that you specified for @Limit@
-- in the request, the response includes a @NextMarker@ value. To list more
-- @Rules@, submit another @ListRules@ request, and specify the
-- @NextMarker@ value from the response in the @NextMarker@ value in the
-- next request.
--
-- 'httpStatus', 'listRulesResponse_httpStatus' - The response's http status code.
newListRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRulesResponse
newListRulesResponse pHttpStatus_ =
  ListRulesResponse'
    { rules = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of RuleSummary objects.
listRulesResponse_rules :: Lens.Lens' ListRulesResponse (Prelude.Maybe [RuleSummary])
listRulesResponse_rules = Lens.lens (\ListRulesResponse' {rules} -> rules) (\s@ListRulesResponse' {} a -> s {rules = a} :: ListRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If you have more @Rules@ than the number that you specified for @Limit@
-- in the request, the response includes a @NextMarker@ value. To list more
-- @Rules@, submit another @ListRules@ request, and specify the
-- @NextMarker@ value from the response in the @NextMarker@ value in the
-- next request.
listRulesResponse_nextMarker :: Lens.Lens' ListRulesResponse (Prelude.Maybe Prelude.Text)
listRulesResponse_nextMarker = Lens.lens (\ListRulesResponse' {nextMarker} -> nextMarker) (\s@ListRulesResponse' {} a -> s {nextMarker = a} :: ListRulesResponse)

-- | The response's http status code.
listRulesResponse_httpStatus :: Lens.Lens' ListRulesResponse Prelude.Int
listRulesResponse_httpStatus = Lens.lens (\ListRulesResponse' {httpStatus} -> httpStatus) (\s@ListRulesResponse' {} a -> s {httpStatus = a} :: ListRulesResponse)

instance Prelude.NFData ListRulesResponse where
  rnf ListRulesResponse' {..} =
    Prelude.rnf rules
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
