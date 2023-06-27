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
-- Module      : Amazonka.WAFRegional.DisassociateWebACL
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
-- Removes a web ACL from the specified resource, either an application
-- load balancer or Amazon API Gateway stage.
module Amazonka.WAFRegional.DisassociateWebACL
  ( -- * Creating a Request
    DisassociateWebACL (..),
    newDisassociateWebACL,

    -- * Request Lenses
    disassociateWebACL_resourceArn,

    -- * Destructuring the Response
    DisassociateWebACLResponse (..),
    newDisassociateWebACLResponse,

    -- * Response Lenses
    disassociateWebACLResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WAFRegional.Types

-- | /See:/ 'newDisassociateWebACL' smart constructor.
data DisassociateWebACL = DisassociateWebACL'
  { -- | The ARN (Amazon Resource Name) of the resource from which the web ACL is
    -- being removed, either an application load balancer or Amazon API Gateway
    -- stage.
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
-- Create a value of 'DisassociateWebACL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'disassociateWebACL_resourceArn' - The ARN (Amazon Resource Name) of the resource from which the web ACL is
-- being removed, either an application load balancer or Amazon API Gateway
-- stage.
--
-- The ARN should be in one of the following formats:
--
-- -   For an Application Load Balancer:
--     @arn:aws:elasticloadbalancing:@/@region@/@:@/@account-id@/@:loadbalancer\/app\/@/@load-balancer-name@/@\/@/@load-balancer-id@/@ @
--
-- -   For an Amazon API Gateway stage:
--     @arn:aws:apigateway:@/@region@/@::\/restapis\/@/@api-id@/@\/stages\/@/@stage-name@/@ @
newDisassociateWebACL ::
  -- | 'resourceArn'
  Prelude.Text ->
  DisassociateWebACL
newDisassociateWebACL pResourceArn_ =
  DisassociateWebACL' {resourceArn = pResourceArn_}

-- | The ARN (Amazon Resource Name) of the resource from which the web ACL is
-- being removed, either an application load balancer or Amazon API Gateway
-- stage.
--
-- The ARN should be in one of the following formats:
--
-- -   For an Application Load Balancer:
--     @arn:aws:elasticloadbalancing:@/@region@/@:@/@account-id@/@:loadbalancer\/app\/@/@load-balancer-name@/@\/@/@load-balancer-id@/@ @
--
-- -   For an Amazon API Gateway stage:
--     @arn:aws:apigateway:@/@region@/@::\/restapis\/@/@api-id@/@\/stages\/@/@stage-name@/@ @
disassociateWebACL_resourceArn :: Lens.Lens' DisassociateWebACL Prelude.Text
disassociateWebACL_resourceArn = Lens.lens (\DisassociateWebACL' {resourceArn} -> resourceArn) (\s@DisassociateWebACL' {} a -> s {resourceArn = a} :: DisassociateWebACL)

instance Core.AWSRequest DisassociateWebACL where
  type
    AWSResponse DisassociateWebACL =
      DisassociateWebACLResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateWebACLResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateWebACL where
  hashWithSalt _salt DisassociateWebACL' {..} =
    _salt `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData DisassociateWebACL where
  rnf DisassociateWebACL' {..} = Prelude.rnf resourceArn

instance Data.ToHeaders DisassociateWebACL where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSWAF_Regional_20161128.DisassociateWebACL" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateWebACL where
  toJSON DisassociateWebACL' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceArn" Data..= resourceArn)]
      )

instance Data.ToPath DisassociateWebACL where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateWebACL where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateWebACLResponse' smart constructor.
data DisassociateWebACLResponse = DisassociateWebACLResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateWebACLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateWebACLResponse_httpStatus' - The response's http status code.
newDisassociateWebACLResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateWebACLResponse
newDisassociateWebACLResponse pHttpStatus_ =
  DisassociateWebACLResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateWebACLResponse_httpStatus :: Lens.Lens' DisassociateWebACLResponse Prelude.Int
disassociateWebACLResponse_httpStatus = Lens.lens (\DisassociateWebACLResponse' {httpStatus} -> httpStatus) (\s@DisassociateWebACLResponse' {} a -> s {httpStatus = a} :: DisassociateWebACLResponse)

instance Prelude.NFData DisassociateWebACLResponse where
  rnf DisassociateWebACLResponse' {..} =
    Prelude.rnf httpStatus
