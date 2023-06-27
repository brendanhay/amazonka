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
-- Module      : Amazonka.WAFV2.ListAPIKeys
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of the API keys that you\'ve defined for the specified
-- scope.
--
-- API keys are required for the integration of the CAPTCHA API in your
-- JavaScript client applications. The API lets you customize the placement
-- and characteristics of the CAPTCHA puzzle for your end users. For more
-- information about the CAPTCHA JavaScript integration, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-application-integration.html WAF client application integration>
-- in the /WAF Developer Guide/.
module Amazonka.WAFV2.ListAPIKeys
  ( -- * Creating a Request
    ListAPIKeys (..),
    newListAPIKeys,

    -- * Request Lenses
    listAPIKeys_limit,
    listAPIKeys_nextMarker,
    listAPIKeys_scope,

    -- * Destructuring the Response
    ListAPIKeysResponse (..),
    newListAPIKeysResponse,

    -- * Response Lenses
    listAPIKeysResponse_aPIKeySummaries,
    listAPIKeysResponse_applicationIntegrationURL,
    listAPIKeysResponse_nextMarker,
    listAPIKeysResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newListAPIKeys' smart constructor.
data ListAPIKeys = ListAPIKeys'
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
    -- an Amazon Cognito user pool, an App Runner service, or an Amazon Web
    -- Services Verified Access instance.
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
-- Create a value of 'ListAPIKeys' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listAPIKeys_limit' - The maximum number of objects that you want WAF to return for this
-- request. If more objects are available, in the response, WAF provides a
-- @NextMarker@ value that you can use in a subsequent call to get the next
-- batch of objects.
--
-- 'nextMarker', 'listAPIKeys_nextMarker' - When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
--
-- 'scope', 'listAPIKeys_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- an Amazon Cognito user pool, an App Runner service, or an Amazon Web
-- Services Verified Access instance.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
newListAPIKeys ::
  -- | 'scope'
  Scope ->
  ListAPIKeys
newListAPIKeys pScope_ =
  ListAPIKeys'
    { limit = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      scope = pScope_
    }

-- | The maximum number of objects that you want WAF to return for this
-- request. If more objects are available, in the response, WAF provides a
-- @NextMarker@ value that you can use in a subsequent call to get the next
-- batch of objects.
listAPIKeys_limit :: Lens.Lens' ListAPIKeys (Prelude.Maybe Prelude.Natural)
listAPIKeys_limit = Lens.lens (\ListAPIKeys' {limit} -> limit) (\s@ListAPIKeys' {} a -> s {limit = a} :: ListAPIKeys)

-- | When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
listAPIKeys_nextMarker :: Lens.Lens' ListAPIKeys (Prelude.Maybe Prelude.Text)
listAPIKeys_nextMarker = Lens.lens (\ListAPIKeys' {nextMarker} -> nextMarker) (\s@ListAPIKeys' {} a -> s {nextMarker = a} :: ListAPIKeys)

-- | Specifies whether this is for an Amazon CloudFront distribution or for a
-- regional application. A regional application can be an Application Load
-- Balancer (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API,
-- an Amazon Cognito user pool, an App Runner service, or an Amazon Web
-- Services Verified Access instance.
--
-- To work with CloudFront, you must also specify the Region US East (N.
-- Virginia) as follows:
--
-- -   CLI - Specify the Region when you use the CloudFront scope:
--     @--scope=CLOUDFRONT --region=us-east-1@.
--
-- -   API and SDKs - For all calls, use the Region endpoint us-east-1.
listAPIKeys_scope :: Lens.Lens' ListAPIKeys Scope
listAPIKeys_scope = Lens.lens (\ListAPIKeys' {scope} -> scope) (\s@ListAPIKeys' {} a -> s {scope = a} :: ListAPIKeys)

instance Core.AWSRequest ListAPIKeys where
  type AWSResponse ListAPIKeys = ListAPIKeysResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAPIKeysResponse'
            Prelude.<$> ( x
                            Data..?> "APIKeySummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "ApplicationIntegrationURL")
            Prelude.<*> (x Data..?> "NextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAPIKeys where
  hashWithSalt _salt ListAPIKeys' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextMarker
      `Prelude.hashWithSalt` scope

instance Prelude.NFData ListAPIKeys where
  rnf ListAPIKeys' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf scope

instance Data.ToHeaders ListAPIKeys where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.ListAPIKeys" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAPIKeys where
  toJSON ListAPIKeys' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextMarker" Data..=) Prelude.<$> nextMarker,
            Prelude.Just ("Scope" Data..= scope)
          ]
      )

instance Data.ToPath ListAPIKeys where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAPIKeys where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAPIKeysResponse' smart constructor.
data ListAPIKeysResponse = ListAPIKeysResponse'
  { -- | The array of key summaries. If you specified a @Limit@ in your request,
    -- this might not be the full list.
    aPIKeySummaries :: Prelude.Maybe [APIKeySummary],
    -- | The CAPTCHA application integration URL, for use in your JavaScript
    -- implementation.
    applicationIntegrationURL :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'ListAPIKeysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aPIKeySummaries', 'listAPIKeysResponse_aPIKeySummaries' - The array of key summaries. If you specified a @Limit@ in your request,
-- this might not be the full list.
--
-- 'applicationIntegrationURL', 'listAPIKeysResponse_applicationIntegrationURL' - The CAPTCHA application integration URL, for use in your JavaScript
-- implementation.
--
-- 'nextMarker', 'listAPIKeysResponse_nextMarker' - When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
--
-- 'httpStatus', 'listAPIKeysResponse_httpStatus' - The response's http status code.
newListAPIKeysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAPIKeysResponse
newListAPIKeysResponse pHttpStatus_ =
  ListAPIKeysResponse'
    { aPIKeySummaries =
        Prelude.Nothing,
      applicationIntegrationURL = Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The array of key summaries. If you specified a @Limit@ in your request,
-- this might not be the full list.
listAPIKeysResponse_aPIKeySummaries :: Lens.Lens' ListAPIKeysResponse (Prelude.Maybe [APIKeySummary])
listAPIKeysResponse_aPIKeySummaries = Lens.lens (\ListAPIKeysResponse' {aPIKeySummaries} -> aPIKeySummaries) (\s@ListAPIKeysResponse' {} a -> s {aPIKeySummaries = a} :: ListAPIKeysResponse) Prelude.. Lens.mapping Lens.coerced

-- | The CAPTCHA application integration URL, for use in your JavaScript
-- implementation.
listAPIKeysResponse_applicationIntegrationURL :: Lens.Lens' ListAPIKeysResponse (Prelude.Maybe Prelude.Text)
listAPIKeysResponse_applicationIntegrationURL = Lens.lens (\ListAPIKeysResponse' {applicationIntegrationURL} -> applicationIntegrationURL) (\s@ListAPIKeysResponse' {} a -> s {applicationIntegrationURL = a} :: ListAPIKeysResponse)

-- | When you request a list of objects with a @Limit@ setting, if the number
-- of objects that are still available for retrieval exceeds the limit, WAF
-- returns a @NextMarker@ value in the response. To retrieve the next batch
-- of objects, provide the marker from the prior call in your next request.
listAPIKeysResponse_nextMarker :: Lens.Lens' ListAPIKeysResponse (Prelude.Maybe Prelude.Text)
listAPIKeysResponse_nextMarker = Lens.lens (\ListAPIKeysResponse' {nextMarker} -> nextMarker) (\s@ListAPIKeysResponse' {} a -> s {nextMarker = a} :: ListAPIKeysResponse)

-- | The response's http status code.
listAPIKeysResponse_httpStatus :: Lens.Lens' ListAPIKeysResponse Prelude.Int
listAPIKeysResponse_httpStatus = Lens.lens (\ListAPIKeysResponse' {httpStatus} -> httpStatus) (\s@ListAPIKeysResponse' {} a -> s {httpStatus = a} :: ListAPIKeysResponse)

instance Prelude.NFData ListAPIKeysResponse where
  rnf ListAPIKeysResponse' {..} =
    Prelude.rnf aPIKeySummaries
      `Prelude.seq` Prelude.rnf applicationIntegrationURL
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
