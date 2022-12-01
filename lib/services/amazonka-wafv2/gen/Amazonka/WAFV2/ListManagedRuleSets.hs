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
-- Module      : Amazonka.WAFV2.ListManagedRuleSets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the managed rule sets that you own.
--
-- This is intended for use only by vendors of managed rule sets. Vendors
-- are Amazon Web Services and Amazon Web Services Marketplace sellers.
--
-- Vendors, you can use the managed rule set APIs to provide controlled
-- rollout of your versioned managed rule group offerings for your
-- customers. The APIs are @ListManagedRuleSets@, @GetManagedRuleSet@,
-- @PutManagedRuleSetVersions@, and
-- @UpdateManagedRuleSetVersionExpiryDate@.
module Amazonka.WAFV2.ListManagedRuleSets
  ( -- * Creating a Request
    ListManagedRuleSets (..),
    newListManagedRuleSets,

    -- * Request Lenses
    listManagedRuleSets_limit,
    listManagedRuleSets_nextMarker,
    listManagedRuleSets_scope,

    -- * Destructuring the Response
    ListManagedRuleSetsResponse (..),
    newListManagedRuleSetsResponse,

    -- * Response Lenses
    listManagedRuleSetsResponse_nextMarker,
    listManagedRuleSetsResponse_managedRuleSets,
    listManagedRuleSetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newListManagedRuleSets' smart constructor.
data ListManagedRuleSets = ListManagedRuleSets'
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
-- Create a value of 'ListManagedRuleSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listManagedRuleSets_limit' - The maximum number of objects that you want WAF to return for this
-- request. If more objects are available, in the response, WAF provides a
-- @NextMarker@ value that you can use in a subsequent call to get the next
-- batch of objects.
--
-- 'nextMarker', 'listManagedRuleSets_nextMarker' - When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
--
-- 'scope', 'listManagedRuleSets_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
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
newListManagedRuleSets ::
  -- | 'scope'
  Scope ->
  ListManagedRuleSets
newListManagedRuleSets pScope_ =
  ListManagedRuleSets'
    { limit = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      scope = pScope_
    }

-- | The maximum number of objects that you want WAF to return for this
-- request. If more objects are available, in the response, WAF provides a
-- @NextMarker@ value that you can use in a subsequent call to get the next
-- batch of objects.
listManagedRuleSets_limit :: Lens.Lens' ListManagedRuleSets (Prelude.Maybe Prelude.Natural)
listManagedRuleSets_limit = Lens.lens (\ListManagedRuleSets' {limit} -> limit) (\s@ListManagedRuleSets' {} a -> s {limit = a} :: ListManagedRuleSets)

-- | When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
listManagedRuleSets_nextMarker :: Lens.Lens' ListManagedRuleSets (Prelude.Maybe Prelude.Text)
listManagedRuleSets_nextMarker = Lens.lens (\ListManagedRuleSets' {nextMarker} -> nextMarker) (\s@ListManagedRuleSets' {} a -> s {nextMarker = a} :: ListManagedRuleSets)

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
listManagedRuleSets_scope :: Lens.Lens' ListManagedRuleSets Scope
listManagedRuleSets_scope = Lens.lens (\ListManagedRuleSets' {scope} -> scope) (\s@ListManagedRuleSets' {} a -> s {scope = a} :: ListManagedRuleSets)

instance Core.AWSRequest ListManagedRuleSets where
  type
    AWSResponse ListManagedRuleSets =
      ListManagedRuleSetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListManagedRuleSetsResponse'
            Prelude.<$> (x Core..?> "NextMarker")
            Prelude.<*> ( x Core..?> "ManagedRuleSets"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListManagedRuleSets where
  hashWithSalt _salt ListManagedRuleSets' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextMarker
      `Prelude.hashWithSalt` scope

instance Prelude.NFData ListManagedRuleSets where
  rnf ListManagedRuleSets' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf scope

instance Core.ToHeaders ListManagedRuleSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20190729.ListManagedRuleSets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListManagedRuleSets where
  toJSON ListManagedRuleSets' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Limit" Core..=) Prelude.<$> limit,
            ("NextMarker" Core..=) Prelude.<$> nextMarker,
            Prelude.Just ("Scope" Core..= scope)
          ]
      )

instance Core.ToPath ListManagedRuleSets where
  toPath = Prelude.const "/"

instance Core.ToQuery ListManagedRuleSets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListManagedRuleSetsResponse' smart constructor.
data ListManagedRuleSetsResponse = ListManagedRuleSetsResponse'
  { -- | When you request a list of objects with a @Limit@ setting, if the number
    -- of objects that are still available for retrieval exceeds the limit, WAF
    -- returns a @NextMarker@ value in the response. To retrieve the next batch
    -- of objects, provide the marker from the prior call in your next request.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | Your managed rule sets.
    managedRuleSets :: Prelude.Maybe [ManagedRuleSetSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListManagedRuleSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listManagedRuleSetsResponse_nextMarker' - When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
--
-- 'managedRuleSets', 'listManagedRuleSetsResponse_managedRuleSets' - Your managed rule sets.
--
-- 'httpStatus', 'listManagedRuleSetsResponse_httpStatus' - The response's http status code.
newListManagedRuleSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListManagedRuleSetsResponse
newListManagedRuleSetsResponse pHttpStatus_ =
  ListManagedRuleSetsResponse'
    { nextMarker =
        Prelude.Nothing,
      managedRuleSets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
listManagedRuleSetsResponse_nextMarker :: Lens.Lens' ListManagedRuleSetsResponse (Prelude.Maybe Prelude.Text)
listManagedRuleSetsResponse_nextMarker = Lens.lens (\ListManagedRuleSetsResponse' {nextMarker} -> nextMarker) (\s@ListManagedRuleSetsResponse' {} a -> s {nextMarker = a} :: ListManagedRuleSetsResponse)

-- | Your managed rule sets.
listManagedRuleSetsResponse_managedRuleSets :: Lens.Lens' ListManagedRuleSetsResponse (Prelude.Maybe [ManagedRuleSetSummary])
listManagedRuleSetsResponse_managedRuleSets = Lens.lens (\ListManagedRuleSetsResponse' {managedRuleSets} -> managedRuleSets) (\s@ListManagedRuleSetsResponse' {} a -> s {managedRuleSets = a} :: ListManagedRuleSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listManagedRuleSetsResponse_httpStatus :: Lens.Lens' ListManagedRuleSetsResponse Prelude.Int
listManagedRuleSetsResponse_httpStatus = Lens.lens (\ListManagedRuleSetsResponse' {httpStatus} -> httpStatus) (\s@ListManagedRuleSetsResponse' {} a -> s {httpStatus = a} :: ListManagedRuleSetsResponse)

instance Prelude.NFData ListManagedRuleSetsResponse where
  rnf ListManagedRuleSetsResponse' {..} =
    Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf managedRuleSets
      `Prelude.seq` Prelude.rnf httpStatus
