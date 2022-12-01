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
-- Module      : Amazonka.WAFV2.ListWebACLs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an array of WebACLSummary objects for the web ACLs that you
-- manage.
module Amazonka.WAFV2.ListWebACLs
  ( -- * Creating a Request
    ListWebACLs (..),
    newListWebACLs,

    -- * Request Lenses
    listWebACLs_limit,
    listWebACLs_nextMarker,
    listWebACLs_scope,

    -- * Destructuring the Response
    ListWebACLsResponse (..),
    newListWebACLsResponse,

    -- * Response Lenses
    listWebACLsResponse_webACLs,
    listWebACLsResponse_nextMarker,
    listWebACLsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newListWebACLs' smart constructor.
data ListWebACLs = ListWebACLs'
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
-- Create a value of 'ListWebACLs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listWebACLs_limit' - The maximum number of objects that you want WAF to return for this
-- request. If more objects are available, in the response, WAF provides a
-- @NextMarker@ value that you can use in a subsequent call to get the next
-- batch of objects.
--
-- 'nextMarker', 'listWebACLs_nextMarker' - When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
--
-- 'scope', 'listWebACLs_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
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
newListWebACLs ::
  -- | 'scope'
  Scope ->
  ListWebACLs
newListWebACLs pScope_ =
  ListWebACLs'
    { limit = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      scope = pScope_
    }

-- | The maximum number of objects that you want WAF to return for this
-- request. If more objects are available, in the response, WAF provides a
-- @NextMarker@ value that you can use in a subsequent call to get the next
-- batch of objects.
listWebACLs_limit :: Lens.Lens' ListWebACLs (Prelude.Maybe Prelude.Natural)
listWebACLs_limit = Lens.lens (\ListWebACLs' {limit} -> limit) (\s@ListWebACLs' {} a -> s {limit = a} :: ListWebACLs)

-- | When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
listWebACLs_nextMarker :: Lens.Lens' ListWebACLs (Prelude.Maybe Prelude.Text)
listWebACLs_nextMarker = Lens.lens (\ListWebACLs' {nextMarker} -> nextMarker) (\s@ListWebACLs' {} a -> s {nextMarker = a} :: ListWebACLs)

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
listWebACLs_scope :: Lens.Lens' ListWebACLs Scope
listWebACLs_scope = Lens.lens (\ListWebACLs' {scope} -> scope) (\s@ListWebACLs' {} a -> s {scope = a} :: ListWebACLs)

instance Core.AWSRequest ListWebACLs where
  type AWSResponse ListWebACLs = ListWebACLsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWebACLsResponse'
            Prelude.<$> (x Core..?> "WebACLs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListWebACLs where
  hashWithSalt _salt ListWebACLs' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextMarker
      `Prelude.hashWithSalt` scope

instance Prelude.NFData ListWebACLs where
  rnf ListWebACLs' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf scope

instance Core.ToHeaders ListWebACLs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20190729.ListWebACLs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListWebACLs where
  toJSON ListWebACLs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Limit" Core..=) Prelude.<$> limit,
            ("NextMarker" Core..=) Prelude.<$> nextMarker,
            Prelude.Just ("Scope" Core..= scope)
          ]
      )

instance Core.ToPath ListWebACLs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListWebACLs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListWebACLsResponse' smart constructor.
data ListWebACLsResponse = ListWebACLsResponse'
  { webACLs :: Prelude.Maybe [WebACLSummary],
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
-- Create a value of 'ListWebACLsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'webACLs', 'listWebACLsResponse_webACLs' -
--
-- 'nextMarker', 'listWebACLsResponse_nextMarker' - When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
--
-- 'httpStatus', 'listWebACLsResponse_httpStatus' - The response's http status code.
newListWebACLsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWebACLsResponse
newListWebACLsResponse pHttpStatus_ =
  ListWebACLsResponse'
    { webACLs = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- |
listWebACLsResponse_webACLs :: Lens.Lens' ListWebACLsResponse (Prelude.Maybe [WebACLSummary])
listWebACLsResponse_webACLs = Lens.lens (\ListWebACLsResponse' {webACLs} -> webACLs) (\s@ListWebACLsResponse' {} a -> s {webACLs = a} :: ListWebACLsResponse) Prelude.. Lens.mapping Lens.coerced

-- | When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
listWebACLsResponse_nextMarker :: Lens.Lens' ListWebACLsResponse (Prelude.Maybe Prelude.Text)
listWebACLsResponse_nextMarker = Lens.lens (\ListWebACLsResponse' {nextMarker} -> nextMarker) (\s@ListWebACLsResponse' {} a -> s {nextMarker = a} :: ListWebACLsResponse)

-- | The response's http status code.
listWebACLsResponse_httpStatus :: Lens.Lens' ListWebACLsResponse Prelude.Int
listWebACLsResponse_httpStatus = Lens.lens (\ListWebACLsResponse' {httpStatus} -> httpStatus) (\s@ListWebACLsResponse' {} a -> s {httpStatus = a} :: ListWebACLsResponse)

instance Prelude.NFData ListWebACLsResponse where
  rnf ListWebACLsResponse' {..} =
    Prelude.rnf webACLs
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
