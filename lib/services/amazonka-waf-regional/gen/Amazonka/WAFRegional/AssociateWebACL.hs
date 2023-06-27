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
-- Module      : Amazonka.WAFRegional.AssociateWebACL
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is __AWS WAF Classic Regional__ documentation. For more
-- information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/classic-waf-chapter.html AWS WAF Classic>
-- in the developer guide.
--
-- __For the latest version of AWS WAF__, use the AWS WAFV2 API and see the
-- <https://docs.aws.amazon.com/waf/latest/developerguide/waf-chapter.html AWS WAF Developer Guide>.
-- With the latest version, AWS WAF has a single set of endpoints for
-- regional and global use.
--
-- Associates a web ACL with a resource, either an application load
-- balancer or Amazon API Gateway stage.
module Amazonka.WAFRegional.AssociateWebACL
  ( -- * Creating a Request
    AssociateWebACL (..),
    newAssociateWebACL,

    -- * Request Lenses
    associateWebACL_webACLId,
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | /See:/ 'newAssociateWebACL' smart constructor.
data AssociateWebACL = AssociateWebACL'
  { -- | A unique identifier (ID) for the web ACL.
    webACLId :: Prelude.Text,
    -- | The ARN (Amazon Resource Name) of the resource to be protected, either
    -- an application load balancer or Amazon API Gateway stage.
    --
    -- The ARN should be in one of the following formats:
    --
    -- -   For an Application Load Balancer:
    --     @arn:aws:elasticloadbalancing:@/@region@/@:@/@account-id@/@:loadbalancer\/app\/@/@load-balancer-name@/@\/@/@load-balancer-id@/@ @
    --
    -- -   For an Amazon API Gateway stage:
    --     @arn:aws:apigateway:@/@region@/@::\/restapis\/@/@api-id@/@\/stages\/@/@stage-name@/@ @
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
-- 'webACLId', 'associateWebACL_webACLId' - A unique identifier (ID) for the web ACL.
--
-- 'resourceArn', 'associateWebACL_resourceArn' - The ARN (Amazon Resource Name) of the resource to be protected, either
-- an application load balancer or Amazon API Gateway stage.
--
-- The ARN should be in one of the following formats:
--
-- -   For an Application Load Balancer:
--     @arn:aws:elasticloadbalancing:@/@region@/@:@/@account-id@/@:loadbalancer\/app\/@/@load-balancer-name@/@\/@/@load-balancer-id@/@ @
--
-- -   For an Amazon API Gateway stage:
--     @arn:aws:apigateway:@/@region@/@::\/restapis\/@/@api-id@/@\/stages\/@/@stage-name@/@ @
newAssociateWebACL ::
  -- | 'webACLId'
  Prelude.Text ->
  -- | 'resourceArn'
  Prelude.Text ->
  AssociateWebACL
newAssociateWebACL pWebACLId_ pResourceArn_ =
  AssociateWebACL'
    { webACLId = pWebACLId_,
      resourceArn = pResourceArn_
    }

-- | A unique identifier (ID) for the web ACL.
associateWebACL_webACLId :: Lens.Lens' AssociateWebACL Prelude.Text
associateWebACL_webACLId = Lens.lens (\AssociateWebACL' {webACLId} -> webACLId) (\s@AssociateWebACL' {} a -> s {webACLId = a} :: AssociateWebACL)

-- | The ARN (Amazon Resource Name) of the resource to be protected, either
-- an application load balancer or Amazon API Gateway stage.
--
-- The ARN should be in one of the following formats:
--
-- -   For an Application Load Balancer:
--     @arn:aws:elasticloadbalancing:@/@region@/@:@/@account-id@/@:loadbalancer\/app\/@/@load-balancer-name@/@\/@/@load-balancer-id@/@ @
--
-- -   For an Amazon API Gateway stage:
--     @arn:aws:apigateway:@/@region@/@::\/restapis\/@/@api-id@/@\/stages\/@/@stage-name@/@ @
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
    _salt
      `Prelude.hashWithSalt` webACLId
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData AssociateWebACL where
  rnf AssociateWebACL' {..} =
    Prelude.rnf webACLId
      `Prelude.seq` Prelude.rnf resourceArn

instance Data.ToHeaders AssociateWebACL where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_Regional_20161128.AssociateWebACL" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateWebACL where
  toJSON AssociateWebACL' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("WebACLId" Data..= webACLId),
            Prelude.Just ("ResourceArn" Data..= resourceArn)
          ]
      )

instance Data.ToPath AssociateWebACL where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateWebACL where
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
