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
-- Module      : Amazonka.WAFV2.ListIPSets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an array of IPSetSummary objects for the IP sets that you
-- manage.
module Amazonka.WAFV2.ListIPSets
  ( -- * Creating a Request
    ListIPSets (..),
    newListIPSets,

    -- * Request Lenses
    listIPSets_limit,
    listIPSets_nextMarker,
    listIPSets_scope,

    -- * Destructuring the Response
    ListIPSetsResponse (..),
    newListIPSetsResponse,

    -- * Response Lenses
    listIPSetsResponse_iPSets,
    listIPSetsResponse_nextMarker,
    listIPSetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newListIPSets' smart constructor.
data ListIPSets = ListIPSets'
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
-- Create a value of 'ListIPSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listIPSets_limit' - The maximum number of objects that you want WAF to return for this
-- request. If more objects are available, in the response, WAF provides a
-- @NextMarker@ value that you can use in a subsequent call to get the next
-- batch of objects.
--
-- 'nextMarker', 'listIPSets_nextMarker' - When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
--
-- 'scope', 'listIPSets_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
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
newListIPSets ::
  -- | 'scope'
  Scope ->
  ListIPSets
newListIPSets pScope_ =
  ListIPSets'
    { limit = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      scope = pScope_
    }

-- | The maximum number of objects that you want WAF to return for this
-- request. If more objects are available, in the response, WAF provides a
-- @NextMarker@ value that you can use in a subsequent call to get the next
-- batch of objects.
listIPSets_limit :: Lens.Lens' ListIPSets (Prelude.Maybe Prelude.Natural)
listIPSets_limit = Lens.lens (\ListIPSets' {limit} -> limit) (\s@ListIPSets' {} a -> s {limit = a} :: ListIPSets)

-- | When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
listIPSets_nextMarker :: Lens.Lens' ListIPSets (Prelude.Maybe Prelude.Text)
listIPSets_nextMarker = Lens.lens (\ListIPSets' {nextMarker} -> nextMarker) (\s@ListIPSets' {} a -> s {nextMarker = a} :: ListIPSets)

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
listIPSets_scope :: Lens.Lens' ListIPSets Scope
listIPSets_scope = Lens.lens (\ListIPSets' {scope} -> scope) (\s@ListIPSets' {} a -> s {scope = a} :: ListIPSets)

instance Core.AWSRequest ListIPSets where
  type AWSResponse ListIPSets = ListIPSetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListIPSetsResponse'
            Prelude.<$> (x Data..?> "IPSets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListIPSets where
  hashWithSalt _salt ListIPSets' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextMarker
      `Prelude.hashWithSalt` scope

instance Prelude.NFData ListIPSets where
  rnf ListIPSets' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf scope

instance Data.ToHeaders ListIPSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSWAF_20190729.ListIPSets" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListIPSets where
  toJSON ListIPSets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextMarker" Data..=) Prelude.<$> nextMarker,
            Prelude.Just ("Scope" Data..= scope)
          ]
      )

instance Data.ToPath ListIPSets where
  toPath = Prelude.const "/"

instance Data.ToQuery ListIPSets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListIPSetsResponse' smart constructor.
data ListIPSetsResponse = ListIPSetsResponse'
  { -- | Array of IPSets. This may not be the full list of IPSets that you have
    -- defined. See the @Limit@ specification for this request.
    iPSets :: Prelude.Maybe [IPSetSummary],
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
-- Create a value of 'ListIPSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'iPSets', 'listIPSetsResponse_iPSets' - Array of IPSets. This may not be the full list of IPSets that you have
-- defined. See the @Limit@ specification for this request.
--
-- 'nextMarker', 'listIPSetsResponse_nextMarker' - When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
--
-- 'httpStatus', 'listIPSetsResponse_httpStatus' - The response's http status code.
newListIPSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListIPSetsResponse
newListIPSetsResponse pHttpStatus_ =
  ListIPSetsResponse'
    { iPSets = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Array of IPSets. This may not be the full list of IPSets that you have
-- defined. See the @Limit@ specification for this request.
listIPSetsResponse_iPSets :: Lens.Lens' ListIPSetsResponse (Prelude.Maybe [IPSetSummary])
listIPSetsResponse_iPSets = Lens.lens (\ListIPSetsResponse' {iPSets} -> iPSets) (\s@ListIPSetsResponse' {} a -> s {iPSets = a} :: ListIPSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
listIPSetsResponse_nextMarker :: Lens.Lens' ListIPSetsResponse (Prelude.Maybe Prelude.Text)
listIPSetsResponse_nextMarker = Lens.lens (\ListIPSetsResponse' {nextMarker} -> nextMarker) (\s@ListIPSetsResponse' {} a -> s {nextMarker = a} :: ListIPSetsResponse)

-- | The response's http status code.
listIPSetsResponse_httpStatus :: Lens.Lens' ListIPSetsResponse Prelude.Int
listIPSetsResponse_httpStatus = Lens.lens (\ListIPSetsResponse' {httpStatus} -> httpStatus) (\s@ListIPSetsResponse' {} a -> s {httpStatus = a} :: ListIPSetsResponse)

instance Prelude.NFData ListIPSetsResponse where
  rnf ListIPSetsResponse' {..} =
    Prelude.rnf iPSets
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
