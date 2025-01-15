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
-- Module      : Amazonka.WAFV2.ListAvailableManagedRuleGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an array of managed rule groups that are available for you to
-- use. This list includes all Amazon Web Services Managed Rules rule
-- groups and all of the Amazon Web Services Marketplace managed rule
-- groups that you\'re subscribed to.
module Amazonka.WAFV2.ListAvailableManagedRuleGroups
  ( -- * Creating a Request
    ListAvailableManagedRuleGroups (..),
    newListAvailableManagedRuleGroups,

    -- * Request Lenses
    listAvailableManagedRuleGroups_limit,
    listAvailableManagedRuleGroups_nextMarker,
    listAvailableManagedRuleGroups_scope,

    -- * Destructuring the Response
    ListAvailableManagedRuleGroupsResponse (..),
    newListAvailableManagedRuleGroupsResponse,

    -- * Response Lenses
    listAvailableManagedRuleGroupsResponse_managedRuleGroups,
    listAvailableManagedRuleGroupsResponse_nextMarker,
    listAvailableManagedRuleGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newListAvailableManagedRuleGroups' smart constructor.
data ListAvailableManagedRuleGroups = ListAvailableManagedRuleGroups'
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
-- Create a value of 'ListAvailableManagedRuleGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listAvailableManagedRuleGroups_limit' - The maximum number of objects that you want WAF to return for this
-- request. If more objects are available, in the response, WAF provides a
-- @NextMarker@ value that you can use in a subsequent call to get the next
-- batch of objects.
--
-- 'nextMarker', 'listAvailableManagedRuleGroups_nextMarker' - When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
--
-- 'scope', 'listAvailableManagedRuleGroups_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
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
newListAvailableManagedRuleGroups ::
  -- | 'scope'
  Scope ->
  ListAvailableManagedRuleGroups
newListAvailableManagedRuleGroups pScope_ =
  ListAvailableManagedRuleGroups'
    { limit =
        Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      scope = pScope_
    }

-- | The maximum number of objects that you want WAF to return for this
-- request. If more objects are available, in the response, WAF provides a
-- @NextMarker@ value that you can use in a subsequent call to get the next
-- batch of objects.
listAvailableManagedRuleGroups_limit :: Lens.Lens' ListAvailableManagedRuleGroups (Prelude.Maybe Prelude.Natural)
listAvailableManagedRuleGroups_limit = Lens.lens (\ListAvailableManagedRuleGroups' {limit} -> limit) (\s@ListAvailableManagedRuleGroups' {} a -> s {limit = a} :: ListAvailableManagedRuleGroups)

-- | When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
listAvailableManagedRuleGroups_nextMarker :: Lens.Lens' ListAvailableManagedRuleGroups (Prelude.Maybe Prelude.Text)
listAvailableManagedRuleGroups_nextMarker = Lens.lens (\ListAvailableManagedRuleGroups' {nextMarker} -> nextMarker) (\s@ListAvailableManagedRuleGroups' {} a -> s {nextMarker = a} :: ListAvailableManagedRuleGroups)

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
listAvailableManagedRuleGroups_scope :: Lens.Lens' ListAvailableManagedRuleGroups Scope
listAvailableManagedRuleGroups_scope = Lens.lens (\ListAvailableManagedRuleGroups' {scope} -> scope) (\s@ListAvailableManagedRuleGroups' {} a -> s {scope = a} :: ListAvailableManagedRuleGroups)

instance
  Core.AWSRequest
    ListAvailableManagedRuleGroups
  where
  type
    AWSResponse ListAvailableManagedRuleGroups =
      ListAvailableManagedRuleGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAvailableManagedRuleGroupsResponse'
            Prelude.<$> ( x
                            Data..?> "ManagedRuleGroups"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListAvailableManagedRuleGroups
  where
  hashWithSalt
    _salt
    ListAvailableManagedRuleGroups' {..} =
      _salt
        `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` nextMarker
        `Prelude.hashWithSalt` scope

instance
  Prelude.NFData
    ListAvailableManagedRuleGroups
  where
  rnf ListAvailableManagedRuleGroups' {..} =
    Prelude.rnf limit `Prelude.seq`
      Prelude.rnf nextMarker `Prelude.seq`
        Prelude.rnf scope

instance
  Data.ToHeaders
    ListAvailableManagedRuleGroups
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.ListAvailableManagedRuleGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAvailableManagedRuleGroups where
  toJSON ListAvailableManagedRuleGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextMarker" Data..=) Prelude.<$> nextMarker,
            Prelude.Just ("Scope" Data..= scope)
          ]
      )

instance Data.ToPath ListAvailableManagedRuleGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAvailableManagedRuleGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAvailableManagedRuleGroupsResponse' smart constructor.
data ListAvailableManagedRuleGroupsResponse = ListAvailableManagedRuleGroupsResponse'
  { managedRuleGroups :: Prelude.Maybe [ManagedRuleGroupSummary],
    -- | When you request a list of objects with a @Limit@ setting, if the number
    -- of objects that are still available for retrieval exceeds the limit, WAF
    -- returns a @NextMarker@ value in the response. To retrieve the next batch
    -- of objects, provide the marker from the prior call in your next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAvailableManagedRuleGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managedRuleGroups', 'listAvailableManagedRuleGroupsResponse_managedRuleGroups' -
--
-- 'nextMarker', 'listAvailableManagedRuleGroupsResponse_nextMarker' - When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
--
-- 'httpStatus', 'listAvailableManagedRuleGroupsResponse_httpStatus' - The response's http status code.
newListAvailableManagedRuleGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAvailableManagedRuleGroupsResponse
newListAvailableManagedRuleGroupsResponse
  pHttpStatus_ =
    ListAvailableManagedRuleGroupsResponse'
      { managedRuleGroups =
          Prelude.Nothing,
        nextMarker = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

listAvailableManagedRuleGroupsResponse_managedRuleGroups :: Lens.Lens' ListAvailableManagedRuleGroupsResponse (Prelude.Maybe [ManagedRuleGroupSummary])
listAvailableManagedRuleGroupsResponse_managedRuleGroups = Lens.lens (\ListAvailableManagedRuleGroupsResponse' {managedRuleGroups} -> managedRuleGroups) (\s@ListAvailableManagedRuleGroupsResponse' {} a -> s {managedRuleGroups = a} :: ListAvailableManagedRuleGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
listAvailableManagedRuleGroupsResponse_nextMarker :: Lens.Lens' ListAvailableManagedRuleGroupsResponse (Prelude.Maybe Prelude.Text)
listAvailableManagedRuleGroupsResponse_nextMarker = Lens.lens (\ListAvailableManagedRuleGroupsResponse' {nextMarker} -> nextMarker) (\s@ListAvailableManagedRuleGroupsResponse' {} a -> s {nextMarker = a} :: ListAvailableManagedRuleGroupsResponse)

-- | The response's http status code.
listAvailableManagedRuleGroupsResponse_httpStatus :: Lens.Lens' ListAvailableManagedRuleGroupsResponse Prelude.Int
listAvailableManagedRuleGroupsResponse_httpStatus = Lens.lens (\ListAvailableManagedRuleGroupsResponse' {httpStatus} -> httpStatus) (\s@ListAvailableManagedRuleGroupsResponse' {} a -> s {httpStatus = a} :: ListAvailableManagedRuleGroupsResponse)

instance
  Prelude.NFData
    ListAvailableManagedRuleGroupsResponse
  where
  rnf ListAvailableManagedRuleGroupsResponse' {..} =
    Prelude.rnf managedRuleGroups `Prelude.seq`
      Prelude.rnf nextMarker `Prelude.seq`
        Prelude.rnf httpStatus
