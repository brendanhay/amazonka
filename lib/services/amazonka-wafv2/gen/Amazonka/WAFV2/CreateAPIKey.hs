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
-- Module      : Amazonka.WAFV2.CreateAPIKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an API key that contains a set of token domains.
--
-- API keys are required for the integration of the CAPTCHA API in your
-- JavaScript client applications. The API lets you customize the placement
-- and characteristics of the CAPTCHA puzzle for your end users. For more
-- information about the CAPTCHA JavaScript integration, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-application-integration.html WAF client application integration>
-- in the /WAF Developer Guide/.
--
-- You can use a single key for up to 5 domains. After you generate a key,
-- you can copy it for use in your JavaScript integration.
module Amazonka.WAFV2.CreateAPIKey
  ( -- * Creating a Request
    CreateAPIKey (..),
    newCreateAPIKey,

    -- * Request Lenses
    createAPIKey_scope,
    createAPIKey_tokenDomains,

    -- * Destructuring the Response
    CreateAPIKeyResponse (..),
    newCreateAPIKeyResponse,

    -- * Response Lenses
    createAPIKeyResponse_aPIKey,
    createAPIKeyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newCreateAPIKey' smart constructor.
data CreateAPIKey = CreateAPIKey'
  { -- | Specifies whether this is for an Amazon CloudFront distribution or for a
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
    scope :: Scope,
    -- | The client application domains that you want to use this API key for.
    --
    -- Example JSON: @\"TokenDomains\": [\"abc.com\", \"store.abc.com\"]@
    --
    -- Public suffixes aren\'t allowed. For example, you can\'t use @usa.gov@
    -- or @co.uk@ as token domains.
    tokenDomains :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAPIKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scope', 'createAPIKey_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
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
--
-- 'tokenDomains', 'createAPIKey_tokenDomains' - The client application domains that you want to use this API key for.
--
-- Example JSON: @\"TokenDomains\": [\"abc.com\", \"store.abc.com\"]@
--
-- Public suffixes aren\'t allowed. For example, you can\'t use @usa.gov@
-- or @co.uk@ as token domains.
newCreateAPIKey ::
  -- | 'scope'
  Scope ->
  -- | 'tokenDomains'
  Prelude.NonEmpty Prelude.Text ->
  CreateAPIKey
newCreateAPIKey pScope_ pTokenDomains_ =
  CreateAPIKey'
    { scope = pScope_,
      tokenDomains = Lens.coerced Lens.# pTokenDomains_
    }

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
createAPIKey_scope :: Lens.Lens' CreateAPIKey Scope
createAPIKey_scope = Lens.lens (\CreateAPIKey' {scope} -> scope) (\s@CreateAPIKey' {} a -> s {scope = a} :: CreateAPIKey)

-- | The client application domains that you want to use this API key for.
--
-- Example JSON: @\"TokenDomains\": [\"abc.com\", \"store.abc.com\"]@
--
-- Public suffixes aren\'t allowed. For example, you can\'t use @usa.gov@
-- or @co.uk@ as token domains.
createAPIKey_tokenDomains :: Lens.Lens' CreateAPIKey (Prelude.NonEmpty Prelude.Text)
createAPIKey_tokenDomains = Lens.lens (\CreateAPIKey' {tokenDomains} -> tokenDomains) (\s@CreateAPIKey' {} a -> s {tokenDomains = a} :: CreateAPIKey) Prelude.. Lens.coerced

instance Core.AWSRequest CreateAPIKey where
  type AWSResponse CreateAPIKey = CreateAPIKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAPIKeyResponse'
            Prelude.<$> (x Data..?> "APIKey")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAPIKey where
  hashWithSalt _salt CreateAPIKey' {..} =
    _salt
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` tokenDomains

instance Prelude.NFData CreateAPIKey where
  rnf CreateAPIKey' {..} =
    Prelude.rnf scope
      `Prelude.seq` Prelude.rnf tokenDomains

instance Data.ToHeaders CreateAPIKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.CreateAPIKey" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAPIKey where
  toJSON CreateAPIKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Scope" Data..= scope),
            Prelude.Just ("TokenDomains" Data..= tokenDomains)
          ]
      )

instance Data.ToPath CreateAPIKey where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateAPIKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAPIKeyResponse' smart constructor.
data CreateAPIKeyResponse = CreateAPIKeyResponse'
  { -- | The generated, encrypted API key. You can copy this for use in your
    -- JavaScript CAPTCHA integration.
    aPIKey :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAPIKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aPIKey', 'createAPIKeyResponse_aPIKey' - The generated, encrypted API key. You can copy this for use in your
-- JavaScript CAPTCHA integration.
--
-- 'httpStatus', 'createAPIKeyResponse_httpStatus' - The response's http status code.
newCreateAPIKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAPIKeyResponse
newCreateAPIKeyResponse pHttpStatus_ =
  CreateAPIKeyResponse'
    { aPIKey = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The generated, encrypted API key. You can copy this for use in your
-- JavaScript CAPTCHA integration.
createAPIKeyResponse_aPIKey :: Lens.Lens' CreateAPIKeyResponse (Prelude.Maybe Prelude.Text)
createAPIKeyResponse_aPIKey = Lens.lens (\CreateAPIKeyResponse' {aPIKey} -> aPIKey) (\s@CreateAPIKeyResponse' {} a -> s {aPIKey = a} :: CreateAPIKeyResponse)

-- | The response's http status code.
createAPIKeyResponse_httpStatus :: Lens.Lens' CreateAPIKeyResponse Prelude.Int
createAPIKeyResponse_httpStatus = Lens.lens (\CreateAPIKeyResponse' {httpStatus} -> httpStatus) (\s@CreateAPIKeyResponse' {} a -> s {httpStatus = a} :: CreateAPIKeyResponse)

instance Prelude.NFData CreateAPIKeyResponse where
  rnf CreateAPIKeyResponse' {..} =
    Prelude.rnf aPIKey
      `Prelude.seq` Prelude.rnf httpStatus
