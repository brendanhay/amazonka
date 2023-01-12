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
-- Module      : Amazonka.WAFV2.ListRuleGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an array of RuleGroupSummary objects for the rule groups that
-- you manage.
module Amazonka.WAFV2.ListRuleGroups
  ( -- * Creating a Request
    ListRuleGroups (..),
    newListRuleGroups,

    -- * Request Lenses
    listRuleGroups_limit,
    listRuleGroups_nextMarker,
    listRuleGroups_scope,

    -- * Destructuring the Response
    ListRuleGroupsResponse (..),
    newListRuleGroupsResponse,

    -- * Response Lenses
    listRuleGroupsResponse_nextMarker,
    listRuleGroupsResponse_ruleGroups,
    listRuleGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newListRuleGroups' smart constructor.
data ListRuleGroups = ListRuleGroups'
  { -- | The maximum number of objects that you want WAF to return for this
    -- request. If more objects are available, in the response, WAF provides a
    -- @NextMarker@ value that you can use in a subsequent call to get the next
    -- batch of objects.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | When you request a list of objects with a @Limit@ setting, if the number
    -- of objects that are still available for retrieval exceeds the limit, WAF
    -- returns a @NextMarker@ value in the response. To retrieve the next batch
    -- of objects, provide the marker from the prior call in your next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether this is for an Amazon CloudFront distribution or for a
    -- regional application. A regional application can be an Application Load
    -- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
    -- or an Amazon Cognito user pool.
    --
    -- To work with CloudFront, you must also specify the Region US East (N.
    -- Virginia) as follows:
    --
    -- -   CLI - Specify the Region when you use the CloudFront scope:
    --     @--scope=CLOUDFRONT --region=us-east-1@.
    --
    -- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
    scope :: Scope
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
-- 'limit', 'listRuleGroups_limit' - The maximum number of objects that you want WAF to return for this
-- request. If more objects are available, in the response, WAF provides a
-- @NextMarker@ value that you can use in a subsequent call to get the next
-- batch of objects.
--
-- 'nextMarker', 'listRuleGroups_nextMarker' - When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
--
-- 'scope', 'listRuleGroups_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- or an Amazon Cognito user pool.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
newListRuleGroups ::
  -- | 'scope'
  Scope ->
  ListRuleGroups
newListRuleGroups pScope_ =
  ListRuleGroups'
    { limit = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      scope = pScope_
    }

-- | The maximum number of objects that you want WAF to return for this
-- request. If more objects are available, in the response, WAF provides a
-- @NextMarker@ value that you can use in a subsequent call to get the next
-- batch of objects.
listRuleGroups_limit :: Lens.Lens' ListRuleGroups (Prelude.Maybe Prelude.Natural)
listRuleGroups_limit = Lens.lens (\ListRuleGroups' {limit} -> limit) (\s@ListRuleGroups' {} a -> s {limit = a} :: ListRuleGroups)

-- | When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
listRuleGroups_nextMarker :: Lens.Lens' ListRuleGroups (Prelude.Maybe Prelude.Text)
listRuleGroups_nextMarker = Lens.lens (\ListRuleGroups' {nextMarker} -> nextMarker) (\s@ListRuleGroups' {} a -> s {nextMarker = a} :: ListRuleGroups)

-- | Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- or an Amazon Cognito user pool.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
listRuleGroups_scope :: Lens.Lens' ListRuleGroups Scope
listRuleGroups_scope = Lens.lens (\ListRuleGroups' {scope} -> scope) (\s@ListRuleGroups' {} a -> s {scope = a} :: ListRuleGroups)

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
            Prelude.<$> (x Data..?> "NextMarker")
            Prelude.<*> (x Data..?> "RuleGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRuleGroups where
  hashWithSalt _salt ListRuleGroups' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextMarker
      `Prelude.hashWithSalt` scope

instance Prelude.NFData ListRuleGroups where
  rnf ListRuleGroups' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf scope

instance Data.ToHeaders ListRuleGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.ListRuleGroups" ::
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
            ("NextMarker" Data..=) Prelude.<$> nextMarker,
            Prelude.Just ("Scope" Data..= scope)
          ]
      )

instance Data.ToPath ListRuleGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery ListRuleGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRuleGroupsResponse' smart constructor.
data ListRuleGroupsResponse = ListRuleGroupsResponse'
  { -- | When you request a list of objects with a @Limit@ setting, if the number
    -- of objects that are still available for retrieval exceeds the limit, WAF
    -- returns a @NextMarker@ value in the response. To retrieve the next batch
    -- of objects, provide the marker from the prior call in your next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    ruleGroups :: Prelude.Maybe [RuleGroupSummary],
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
-- 'nextMarker', 'listRuleGroupsResponse_nextMarker' - When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
--
-- 'ruleGroups', 'listRuleGroupsResponse_ruleGroups' -
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

-- | When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
listRuleGroupsResponse_nextMarker :: Lens.Lens' ListRuleGroupsResponse (Prelude.Maybe Prelude.Text)
listRuleGroupsResponse_nextMarker = Lens.lens (\ListRuleGroupsResponse' {nextMarker} -> nextMarker) (\s@ListRuleGroupsResponse' {} a -> s {nextMarker = a} :: ListRuleGroupsResponse)

-- |
listRuleGroupsResponse_ruleGroups :: Lens.Lens' ListRuleGroupsResponse (Prelude.Maybe [RuleGroupSummary])
listRuleGroupsResponse_ruleGroups = Lens.lens (\ListRuleGroupsResponse' {ruleGroups} -> ruleGroups) (\s@ListRuleGroupsResponse' {} a -> s {ruleGroups = a} :: ListRuleGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRuleGroupsResponse_httpStatus :: Lens.Lens' ListRuleGroupsResponse Prelude.Int
listRuleGroupsResponse_httpStatus = Lens.lens (\ListRuleGroupsResponse' {httpStatus} -> httpStatus) (\s@ListRuleGroupsResponse' {} a -> s {httpStatus = a} :: ListRuleGroupsResponse)

instance Prelude.NFData ListRuleGroupsResponse where
  rnf ListRuleGroupsResponse' {..} =
    Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf ruleGroups
      `Prelude.seq` Prelude.rnf httpStatus
