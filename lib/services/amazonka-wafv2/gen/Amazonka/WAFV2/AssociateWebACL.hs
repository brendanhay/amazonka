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
-- Module      : Amazonka.WAFV2.AssociateWebACL
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a web ACL with a regional application resource, to protect
-- the resource. A regional application can be an Application Load Balancer
-- (ALB), an Amazon API Gateway REST API, an AppSync GraphQL API, or an
-- Amazon Cognito user pool.
--
-- For Amazon CloudFront, don\'t use this call. Instead, use your
-- CloudFront distribution configuration. To associate a web ACL, in the
-- CloudFront call @UpdateDistribution@, set the web ACL ID to the Amazon
-- Resource Name (ARN) of the web ACL. For information, see
-- <https://docs.aws.amazon.com/cloudfront/latest/APIReference/API_UpdateDistribution.html UpdateDistribution>.
--
-- When you make changes to web ACLs or web ACL components, like rules and
-- rule groups, WAF propagates the changes everywhere that the web ACL and
-- its components are stored and used. Your changes are applied within
-- seconds, but there might be a brief period of inconsistency when the
-- changes have arrived in some places and not in others. So, for example,
-- if you change a rule action setting, the action might be the old action
-- in one area and the new action in another area. Or if you add an IP
-- address to an IP set used in a blocking rule, the new address might
-- briefly be blocked in one area while still allowed in another. This
-- temporary inconsistency can occur when you first associate a web ACL
-- with an Amazon Web Services resource and when you change a web ACL that
-- is already associated with a resource. Generally, any inconsistencies of
-- this type last only a few seconds.
module Amazonka.WAFV2.AssociateWebACL
  ( -- * Creating a Request
    AssociateWebACL (..),
    newAssociateWebACL,

    -- * Request Lenses
    associateWebACL_webACLArn,
    associateWebACL_resourceArn,

    -- * Destructuring the Response
    AssociateWebACLResponse (..),
    newAssociateWebACLResponse,

    -- * Response Lenses
    associateWebACLResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFV2.Types

-- | /See:/ 'newAssociateWebACL' smart constructor.
data AssociateWebACL = AssociateWebACL'
  { -- | The Amazon Resource Name (ARN) of the web ACL that you want to associate
    -- with the resource.
    webACLArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resource to associate with the web
    -- ACL.
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
-- Create a value of 'AssociateWebACL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'webACLArn', 'associateWebACL_webACLArn' - The Amazon Resource Name (ARN) of the web ACL that you want to associate
-- with the resource.
--
-- 'resourceArn', 'associateWebACL_resourceArn' - The Amazon Resource Name (ARN) of the resource to associate with the web
-- ACL.
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
newAssociateWebACL ::
  -- | 'webACLArn'
  Prelude.Text ->
  -- | 'resourceArn'
  Prelude.Text ->
  AssociateWebACL
newAssociateWebACL pWebACLArn_ pResourceArn_ =
  AssociateWebACL'
    { webACLArn = pWebACLArn_,
      resourceArn = pResourceArn_
    }

-- | The Amazon Resource Name (ARN) of the web ACL that you want to associate
-- with the resource.
associateWebACL_webACLArn :: Lens.Lens' AssociateWebACL Prelude.Text
associateWebACL_webACLArn = Lens.lens (\AssociateWebACL' {webACLArn} -> webACLArn) (\s@AssociateWebACL' {} a -> s {webACLArn = a} :: AssociateWebACL)

-- | The Amazon Resource Name (ARN) of the resource to associate with the web
-- ACL.
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
associateWebACL_resourceArn :: Lens.Lens' AssociateWebACL Prelude.Text
associateWebACL_resourceArn = Lens.lens (\AssociateWebACL' {resourceArn} -> resourceArn) (\s@AssociateWebACL' {} a -> s {resourceArn = a} :: AssociateWebACL)

instance Core.AWSRequest AssociateWebACL where
  type
    AWSResponse AssociateWebACL =
      AssociateWebACLResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateWebACLResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateWebACL where
  hashWithSalt _salt AssociateWebACL' {..} =
    _salt `Prelude.hashWithSalt` webACLArn
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData AssociateWebACL where
  rnf AssociateWebACL' {..} =
    Prelude.rnf webACLArn
      `Prelude.seq` Prelude.rnf resourceArn

instance Core.ToHeaders AssociateWebACL where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSWAF_20190729.AssociateWebACL" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AssociateWebACL where
  toJSON AssociateWebACL' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("WebACLArn" Core..= webACLArn),
            Prelude.Just ("ResourceArn" Core..= resourceArn)
          ]
      )

instance Core.ToPath AssociateWebACL where
  toPath = Prelude.const "/"

instance Core.ToQuery AssociateWebACL where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateWebACLResponse' smart constructor.
data AssociateWebACLResponse = AssociateWebACLResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateWebACLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateWebACLResponse_httpStatus' - The response's http status code.
newAssociateWebACLResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateWebACLResponse
newAssociateWebACLResponse pHttpStatus_ =
  AssociateWebACLResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
associateWebACLResponse_httpStatus :: Lens.Lens' AssociateWebACLResponse Prelude.Int
associateWebACLResponse_httpStatus = Lens.lens (\AssociateWebACLResponse' {httpStatus} -> httpStatus) (\s@AssociateWebACLResponse' {} a -> s {httpStatus = a} :: AssociateWebACLResponse)

instance Prelude.NFData AssociateWebACLResponse where
  rnf AssociateWebACLResponse' {..} =
    Prelude.rnf httpStatus
