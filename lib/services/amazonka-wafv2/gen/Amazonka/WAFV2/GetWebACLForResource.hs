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
-- Module      : Amazonka.WAFV2.GetWebACLForResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the WebACL for the specified resource.
module Amazonka.WAFV2.GetWebACLForResource
  ( -- * Creating a Request
    GetWebACLForResource (..),
    newGetWebACLForResource,

    -- * Request Lenses
    getWebACLForResource_resourceArn,

    -- * Destructuring the Response
    GetWebACLForResourceResponse (..),
    newGetWebACLForResourceResponse,

    -- * Response Lenses
    getWebACLForResourceResponse_webACL,
    getWebACLForResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newGetWebACLForResource' smart constructor.
data GetWebACLForResource = GetWebACLForResource'
  { -- | The Amazon Resource Name (ARN) of the resource whose web ACL you want to
    -- retrieve.
    --
    -- The ARN must be in one of the following formats:
    --
    -- -   For an Application Load Balancer:
    --     @arn:aws:elasticloadbalancing:region:account-id:loadbalancer\/app\/load-balancer-name\/load-balancer-id @
    --
    -- -   For an Amazon API Gateway REST API:
    --     @arn:aws:apigateway:region::\/restapis\/api-id\/stages\/stage-name @
    --
    -- -   For an AppSync GraphQL API:
    --     @arn:aws:appsync:region:account-id:apis\/GraphQLApiId @
    --
    -- -   For an Amazon Cognito user pool:
    --     @arn:aws:cognito-idp:region:account-id:userpool\/user-pool-id @
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWebACLForResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'getWebACLForResource_resourceArn' - The Amazon Resource Name (ARN) of the resource whose web ACL you want to
-- retrieve.
--
-- The ARN must be in one of the following formats:
--
-- -   For an Application Load Balancer:
--     @arn:aws:elasticloadbalancing:region:account-id:loadbalancer\/app\/load-balancer-name\/load-balancer-id @
--
-- -   For an Amazon API Gateway REST API:
--     @arn:aws:apigateway:region::\/restapis\/api-id\/stages\/stage-name @
--
-- -   For an AppSync GraphQL API:
--     @arn:aws:appsync:region:account-id:apis\/GraphQLApiId @
--
-- -   For an Amazon Cognito user pool:
--     @arn:aws:cognito-idp:region:account-id:userpool\/user-pool-id @
newGetWebACLForResource ::
  -- | 'resourceArn'
  Prelude.Text ->
  GetWebACLForResource
newGetWebACLForResource pResourceArn_ =
  GetWebACLForResource' {resourceArn = pResourceArn_}

-- | The Amazon Resource Name (ARN) of the resource whose web ACL you want to
-- retrieve.
--
-- The ARN must be in one of the following formats:
--
-- -   For an Application Load Balancer:
--     @arn:aws:elasticloadbalancing:region:account-id:loadbalancer\/app\/load-balancer-name\/load-balancer-id @
--
-- -   For an Amazon API Gateway REST API:
--     @arn:aws:apigateway:region::\/restapis\/api-id\/stages\/stage-name @
--
-- -   For an AppSync GraphQL API:
--     @arn:aws:appsync:region:account-id:apis\/GraphQLApiId @
--
-- -   For an Amazon Cognito user pool:
--     @arn:aws:cognito-idp:region:account-id:userpool\/user-pool-id @
getWebACLForResource_resourceArn :: Lens.Lens' GetWebACLForResource Prelude.Text
getWebACLForResource_resourceArn = Lens.lens (\GetWebACLForResource' {resourceArn} -> resourceArn) (\s@GetWebACLForResource' {} a -> s {resourceArn = a} :: GetWebACLForResource)

instance Core.AWSRequest GetWebACLForResource where
  type
    AWSResponse GetWebACLForResource =
      GetWebACLForResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWebACLForResourceResponse'
            Prelude.<$> (x Data..?> "WebACL")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWebACLForResource where
  hashWithSalt _salt GetWebACLForResource' {..} =
    _salt `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData GetWebACLForResource where
  rnf GetWebACLForResource' {..} =
    Prelude.rnf resourceArn

instance Data.ToHeaders GetWebACLForResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_20190729.GetWebACLForResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetWebACLForResource where
  toJSON GetWebACLForResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceArn" Data..= resourceArn)]
      )

instance Data.ToPath GetWebACLForResource where
  toPath = Prelude.const "/"

instance Data.ToQuery GetWebACLForResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWebACLForResourceResponse' smart constructor.
data GetWebACLForResourceResponse = GetWebACLForResourceResponse'
  { -- | The web ACL that is associated with the resource. If there is no
    -- associated resource, WAF returns a null web ACL.
    webACL :: Prelude.Maybe WebACL,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWebACLForResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'webACL', 'getWebACLForResourceResponse_webACL' - The web ACL that is associated with the resource. If there is no
-- associated resource, WAF returns a null web ACL.
--
-- 'httpStatus', 'getWebACLForResourceResponse_httpStatus' - The response's http status code.
newGetWebACLForResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWebACLForResourceResponse
newGetWebACLForResourceResponse pHttpStatus_ =
  GetWebACLForResourceResponse'
    { webACL =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The web ACL that is associated with the resource. If there is no
-- associated resource, WAF returns a null web ACL.
getWebACLForResourceResponse_webACL :: Lens.Lens' GetWebACLForResourceResponse (Prelude.Maybe WebACL)
getWebACLForResourceResponse_webACL = Lens.lens (\GetWebACLForResourceResponse' {webACL} -> webACL) (\s@GetWebACLForResourceResponse' {} a -> s {webACL = a} :: GetWebACLForResourceResponse)

-- | The response's http status code.
getWebACLForResourceResponse_httpStatus :: Lens.Lens' GetWebACLForResourceResponse Prelude.Int
getWebACLForResourceResponse_httpStatus = Lens.lens (\GetWebACLForResourceResponse' {httpStatus} -> httpStatus) (\s@GetWebACLForResourceResponse' {} a -> s {httpStatus = a} :: GetWebACLForResourceResponse)

instance Prelude.NFData GetWebACLForResourceResponse where
  rnf GetWebACLForResourceResponse' {..} =
    Prelude.rnf webACL
      `Prelude.seq` Prelude.rnf httpStatus
