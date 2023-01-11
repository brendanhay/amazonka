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
-- Module      : Amazonka.ELBV2.DescribeLoadBalancerAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the attributes for the specified Application Load Balancer,
-- Network Load Balancer, or Gateway Load Balancer.
--
-- For more information, see the following:
--
-- -   <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/application-load-balancers.html#load-balancer-attributes Load balancer attributes>
--     in the /Application Load Balancers Guide/
--
-- -   <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/network-load-balancers.html#load-balancer-attributes Load balancer attributes>
--     in the /Network Load Balancers Guide/
--
-- -   <https://docs.aws.amazon.com/elasticloadbalancing/latest/gateway/gateway-load-balancers.html#load-balancer-attributes Load balancer attributes>
--     in the /Gateway Load Balancers Guide/
module Amazonka.ELBV2.DescribeLoadBalancerAttributes
  ( -- * Creating a Request
    DescribeLoadBalancerAttributes (..),
    newDescribeLoadBalancerAttributes,

    -- * Request Lenses
    describeLoadBalancerAttributes_loadBalancerArn,

    -- * Destructuring the Response
    DescribeLoadBalancerAttributesResponse (..),
    newDescribeLoadBalancerAttributesResponse,

    -- * Response Lenses
    describeLoadBalancerAttributesResponse_attributes,
    describeLoadBalancerAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELBV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLoadBalancerAttributes' smart constructor.
data DescribeLoadBalancerAttributes = DescribeLoadBalancerAttributes'
  { -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLoadBalancerAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerArn', 'describeLoadBalancerAttributes_loadBalancerArn' - The Amazon Resource Name (ARN) of the load balancer.
newDescribeLoadBalancerAttributes ::
  -- | 'loadBalancerArn'
  Prelude.Text ->
  DescribeLoadBalancerAttributes
newDescribeLoadBalancerAttributes pLoadBalancerArn_ =
  DescribeLoadBalancerAttributes'
    { loadBalancerArn =
        pLoadBalancerArn_
    }

-- | The Amazon Resource Name (ARN) of the load balancer.
describeLoadBalancerAttributes_loadBalancerArn :: Lens.Lens' DescribeLoadBalancerAttributes Prelude.Text
describeLoadBalancerAttributes_loadBalancerArn = Lens.lens (\DescribeLoadBalancerAttributes' {loadBalancerArn} -> loadBalancerArn) (\s@DescribeLoadBalancerAttributes' {} a -> s {loadBalancerArn = a} :: DescribeLoadBalancerAttributes)

instance
  Core.AWSRequest
    DescribeLoadBalancerAttributes
  where
  type
    AWSResponse DescribeLoadBalancerAttributes =
      DescribeLoadBalancerAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeLoadBalancerAttributesResult"
      ( \s h x ->
          DescribeLoadBalancerAttributesResponse'
            Prelude.<$> ( x Data..@? "Attributes" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeLoadBalancerAttributes
  where
  hashWithSalt
    _salt
    DescribeLoadBalancerAttributes' {..} =
      _salt `Prelude.hashWithSalt` loadBalancerArn

instance
  Prelude.NFData
    DescribeLoadBalancerAttributes
  where
  rnf DescribeLoadBalancerAttributes' {..} =
    Prelude.rnf loadBalancerArn

instance
  Data.ToHeaders
    DescribeLoadBalancerAttributes
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeLoadBalancerAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLoadBalancerAttributes where
  toQuery DescribeLoadBalancerAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeLoadBalancerAttributes" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2015-12-01" :: Prelude.ByteString),
        "LoadBalancerArn" Data.=: loadBalancerArn
      ]

-- | /See:/ 'newDescribeLoadBalancerAttributesResponse' smart constructor.
data DescribeLoadBalancerAttributesResponse = DescribeLoadBalancerAttributesResponse'
  { -- | Information about the load balancer attributes.
    attributes :: Prelude.Maybe [LoadBalancerAttribute],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLoadBalancerAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'describeLoadBalancerAttributesResponse_attributes' - Information about the load balancer attributes.
--
-- 'httpStatus', 'describeLoadBalancerAttributesResponse_httpStatus' - The response's http status code.
newDescribeLoadBalancerAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLoadBalancerAttributesResponse
newDescribeLoadBalancerAttributesResponse
  pHttpStatus_ =
    DescribeLoadBalancerAttributesResponse'
      { attributes =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the load balancer attributes.
describeLoadBalancerAttributesResponse_attributes :: Lens.Lens' DescribeLoadBalancerAttributesResponse (Prelude.Maybe [LoadBalancerAttribute])
describeLoadBalancerAttributesResponse_attributes = Lens.lens (\DescribeLoadBalancerAttributesResponse' {attributes} -> attributes) (\s@DescribeLoadBalancerAttributesResponse' {} a -> s {attributes = a} :: DescribeLoadBalancerAttributesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeLoadBalancerAttributesResponse_httpStatus :: Lens.Lens' DescribeLoadBalancerAttributesResponse Prelude.Int
describeLoadBalancerAttributesResponse_httpStatus = Lens.lens (\DescribeLoadBalancerAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeLoadBalancerAttributesResponse' {} a -> s {httpStatus = a} :: DescribeLoadBalancerAttributesResponse)

instance
  Prelude.NFData
    DescribeLoadBalancerAttributesResponse
  where
  rnf DescribeLoadBalancerAttributesResponse' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf httpStatus
