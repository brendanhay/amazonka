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
-- Module      : Amazonka.WAFV2.GetDecryptedAPIKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns your API key in decrypted form. Use this to check the token
-- domains that you have defined for the key.
--
-- API keys are required for the integration of the CAPTCHA API in your
-- JavaScript client applications. The API lets you customize the placement
-- and characteristics of the CAPTCHA puzzle for your end users. For more
-- information about the CAPTCHA JavaScript integration, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-application-integration.html WAF client application integration>
-- in the /WAF Developer Guide/.
module Amazonka.WAFV2.GetDecryptedAPIKey
  ( -- * Creating a Request
    GetDecryptedAPIKey (..),
    newGetDecryptedAPIKey,

    -- * Request Lenses
    getDecryptedAPIKey_scope,
    getDecryptedAPIKey_aPIKey,

    -- * Destructuring the Response
    GetDecryptedAPIKeyResponse (..),
    newGetDecryptedAPIKeyResponse,

    -- * Response Lenses
    getDecryptedAPIKeyResponse_creationTimestamp,
    getDecryptedAPIKeyResponse_tokenDomains,
    getDecryptedAPIKeyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newGetDecryptedAPIKey' smart constructor.
data GetDecryptedAPIKey = GetDecryptedAPIKey'
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
    -- | The encrypted API key.
    aPIKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDecryptedAPIKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scope', 'getDecryptedAPIKey_scope' - Specifies whether this is for an Amazon CloudFront distribution or for a
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
-- 'aPIKey', 'getDecryptedAPIKey_aPIKey' - The encrypted API key.
newGetDecryptedAPIKey ::
  -- | 'scope'
  Scope ->
  -- | 'aPIKey'
  Prelude.Text ->
  GetDecryptedAPIKey
newGetDecryptedAPIKey pScope_ pAPIKey_ =
  GetDecryptedAPIKey'
    { scope = pScope_,
      aPIKey = pAPIKey_
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
getDecryptedAPIKey_scope :: Lens.Lens' GetDecryptedAPIKey Scope
getDecryptedAPIKey_scope = Lens.lens (\GetDecryptedAPIKey' {scope} -> scope) (\s@GetDecryptedAPIKey' {} a -> s {scope = a} :: GetDecryptedAPIKey)

-- | The encrypted API key.
getDecryptedAPIKey_aPIKey :: Lens.Lens' GetDecryptedAPIKey Prelude.Text
getDecryptedAPIKey_aPIKey = Lens.lens (\GetDecryptedAPIKey' {aPIKey} -> aPIKey) (\s@GetDecryptedAPIKey' {} a -> s {aPIKey = a} :: GetDecryptedAPIKey)

instance Core.AWSRequest GetDecryptedAPIKey where
  type
    AWSResponse GetDecryptedAPIKey =
      GetDecryptedAPIKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDecryptedAPIKeyResponse'
            Prelude.<$> (x Data..?> "CreationTimestamp")
            Prelude.<*> (x Data..?> "TokenDomains" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDecryptedAPIKey where
  hashWithSalt _salt GetDecryptedAPIKey' {..} =
    _salt
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` aPIKey

instance Prelude.NFData GetDecryptedAPIKey where
  rnf GetDecryptedAPIKey' {..} =
    Prelude.rnf scope `Prelude.seq` Prelude.rnf aPIKey

instance Data.ToHeaders GetDecryptedAPIKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.GetDecryptedAPIKey" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDecryptedAPIKey where
  toJSON GetDecryptedAPIKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Scope" Data..= scope),
            Prelude.Just ("APIKey" Data..= aPIKey)
          ]
      )

instance Data.ToPath GetDecryptedAPIKey where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDecryptedAPIKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDecryptedAPIKeyResponse' smart constructor.
data GetDecryptedAPIKeyResponse = GetDecryptedAPIKeyResponse'
  { -- | The date and time that the key was created.
    creationTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The token domains that are defined in this API key.
    tokenDomains :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDecryptedAPIKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimestamp', 'getDecryptedAPIKeyResponse_creationTimestamp' - The date and time that the key was created.
--
-- 'tokenDomains', 'getDecryptedAPIKeyResponse_tokenDomains' - The token domains that are defined in this API key.
--
-- 'httpStatus', 'getDecryptedAPIKeyResponse_httpStatus' - The response's http status code.
newGetDecryptedAPIKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDecryptedAPIKeyResponse
newGetDecryptedAPIKeyResponse pHttpStatus_ =
  GetDecryptedAPIKeyResponse'
    { creationTimestamp =
        Prelude.Nothing,
      tokenDomains = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time that the key was created.
getDecryptedAPIKeyResponse_creationTimestamp :: Lens.Lens' GetDecryptedAPIKeyResponse (Prelude.Maybe Prelude.UTCTime)
getDecryptedAPIKeyResponse_creationTimestamp = Lens.lens (\GetDecryptedAPIKeyResponse' {creationTimestamp} -> creationTimestamp) (\s@GetDecryptedAPIKeyResponse' {} a -> s {creationTimestamp = a} :: GetDecryptedAPIKeyResponse) Prelude.. Lens.mapping Data._Time

-- | The token domains that are defined in this API key.
getDecryptedAPIKeyResponse_tokenDomains :: Lens.Lens' GetDecryptedAPIKeyResponse (Prelude.Maybe [Prelude.Text])
getDecryptedAPIKeyResponse_tokenDomains = Lens.lens (\GetDecryptedAPIKeyResponse' {tokenDomains} -> tokenDomains) (\s@GetDecryptedAPIKeyResponse' {} a -> s {tokenDomains = a} :: GetDecryptedAPIKeyResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getDecryptedAPIKeyResponse_httpStatus :: Lens.Lens' GetDecryptedAPIKeyResponse Prelude.Int
getDecryptedAPIKeyResponse_httpStatus = Lens.lens (\GetDecryptedAPIKeyResponse' {httpStatus} -> httpStatus) (\s@GetDecryptedAPIKeyResponse' {} a -> s {httpStatus = a} :: GetDecryptedAPIKeyResponse)

instance Prelude.NFData GetDecryptedAPIKeyResponse where
  rnf GetDecryptedAPIKeyResponse' {..} =
    Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf tokenDomains
      `Prelude.seq` Prelude.rnf httpStatus
