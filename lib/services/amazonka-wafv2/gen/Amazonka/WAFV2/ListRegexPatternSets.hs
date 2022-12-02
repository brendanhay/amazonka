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
-- Module      : Amazonka.WAFV2.ListRegexPatternSets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an array of RegexPatternSetSummary objects for the regex
-- pattern sets that you manage.
module Amazonka.WAFV2.ListRegexPatternSets
  ( -- * Creating a Request
    ListRegexPatternSets (..),
    newListRegexPatternSets,

    -- * Request Lenses
    listRegexPatternSets_limit,
    listRegexPatternSets_nextMarker,
    listRegexPatternSets_scope,

    -- * Destructuring the Response
    ListRegexPatternSetsResponse (..),
    newListRegexPatternSetsResponse,

    -- * Response Lenses
    listRegexPatternSetsResponse_regexPatternSets,
    listRegexPatternSetsResponse_nextMarker,
    listRegexPatternSetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newListRegexPatternSets' smart constructor.
data ListRegexPatternSets = ListRegexPatternSets'
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
-- Create a value of 'ListRegexPatternSets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listRegexPatternSets_limit' - The maximum number of objects that you want WAF to return for this
-- request. If more objects are available, in the response, WAF provides a
-- @NextMarker@ value that you can use in a subsequent call to get the next
-- batch of objects.
--
-- 'nextMarker', 'listRegexPatternSets_nextMarker' - When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
--
-- 'scope', 'listRegexPatternSets_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
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
newListRegexPatternSets ::
  -- | 'scope'
  Scope ->
  ListRegexPatternSets
newListRegexPatternSets pScope_ =
  ListRegexPatternSets'
    { limit = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      scope = pScope_
    }

-- | The maximum number of objects that you want WAF to return for this
-- request. If more objects are available, in the response, WAF provides a
-- @NextMarker@ value that you can use in a subsequent call to get the next
-- batch of objects.
listRegexPatternSets_limit :: Lens.Lens' ListRegexPatternSets (Prelude.Maybe Prelude.Natural)
listRegexPatternSets_limit = Lens.lens (\ListRegexPatternSets' {limit} -> limit) (\s@ListRegexPatternSets' {} a -> s {limit = a} :: ListRegexPatternSets)

-- | When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
listRegexPatternSets_nextMarker :: Lens.Lens' ListRegexPatternSets (Prelude.Maybe Prelude.Text)
listRegexPatternSets_nextMarker = Lens.lens (\ListRegexPatternSets' {nextMarker} -> nextMarker) (\s@ListRegexPatternSets' {} a -> s {nextMarker = a} :: ListRegexPatternSets)

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
listRegexPatternSets_scope :: Lens.Lens' ListRegexPatternSets Scope
listRegexPatternSets_scope = Lens.lens (\ListRegexPatternSets' {scope} -> scope) (\s@ListRegexPatternSets' {} a -> s {scope = a} :: ListRegexPatternSets)

instance Core.AWSRequest ListRegexPatternSets where
  type
    AWSResponse ListRegexPatternSets =
      ListRegexPatternSetsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRegexPatternSetsResponse'
            Prelude.<$> ( x Data..?> "RegexPatternSets"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRegexPatternSets where
  hashWithSalt _salt ListRegexPatternSets' {..} =
    _salt `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextMarker
      `Prelude.hashWithSalt` scope

instance Prelude.NFData ListRegexPatternSets where
  rnf ListRegexPatternSets' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf scope

instance Data.ToHeaders ListRegexPatternSets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.ListRegexPatternSets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRegexPatternSets where
  toJSON ListRegexPatternSets' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextMarker" Data..=) Prelude.<$> nextMarker,
            Prelude.Just ("Scope" Data..= scope)
          ]
      )

instance Data.ToPath ListRegexPatternSets where
  toPath = Prelude.const "/"

instance Data.ToQuery ListRegexPatternSets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRegexPatternSetsResponse' smart constructor.
data ListRegexPatternSetsResponse = ListRegexPatternSetsResponse'
  { regexPatternSets :: Prelude.Maybe [RegexPatternSetSummary],
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
-- Create a value of 'ListRegexPatternSetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regexPatternSets', 'listRegexPatternSetsResponse_regexPatternSets' -
--
-- 'nextMarker', 'listRegexPatternSetsResponse_nextMarker' - When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
--
-- 'httpStatus', 'listRegexPatternSetsResponse_httpStatus' - The response's http status code.
newListRegexPatternSetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRegexPatternSetsResponse
newListRegexPatternSetsResponse pHttpStatus_ =
  ListRegexPatternSetsResponse'
    { regexPatternSets =
        Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- |
listRegexPatternSetsResponse_regexPatternSets :: Lens.Lens' ListRegexPatternSetsResponse (Prelude.Maybe [RegexPatternSetSummary])
listRegexPatternSetsResponse_regexPatternSets = Lens.lens (\ListRegexPatternSetsResponse' {regexPatternSets} -> regexPatternSets) (\s@ListRegexPatternSetsResponse' {} a -> s {regexPatternSets = a} :: ListRegexPatternSetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
listRegexPatternSetsResponse_nextMarker :: Lens.Lens' ListRegexPatternSetsResponse (Prelude.Maybe Prelude.Text)
listRegexPatternSetsResponse_nextMarker = Lens.lens (\ListRegexPatternSetsResponse' {nextMarker} -> nextMarker) (\s@ListRegexPatternSetsResponse' {} a -> s {nextMarker = a} :: ListRegexPatternSetsResponse)

-- | The response's http status code.
listRegexPatternSetsResponse_httpStatus :: Lens.Lens' ListRegexPatternSetsResponse Prelude.Int
listRegexPatternSetsResponse_httpStatus = Lens.lens (\ListRegexPatternSetsResponse' {httpStatus} -> httpStatus) (\s@ListRegexPatternSetsResponse' {} a -> s {httpStatus = a} :: ListRegexPatternSetsResponse)

instance Prelude.NFData ListRegexPatternSetsResponse where
  rnf ListRegexPatternSetsResponse' {..} =
    Prelude.rnf regexPatternSets
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
