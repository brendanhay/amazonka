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
-- Module      : Amazonka.ELB.DescribeLoadBalancerAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the attributes for the specified load balancer.
module Amazonka.ELB.DescribeLoadBalancerAttributes
  ( -- * Creating a Request
    DescribeLoadBalancerAttributes (..),
    newDescribeLoadBalancerAttributes,

    -- * Request Lenses
    describeLoadBalancerAttributes_loadBalancerName,

    -- * Destructuring the Response
    DescribeLoadBalancerAttributesResponse (..),
    newDescribeLoadBalancerAttributesResponse,

    -- * Response Lenses
    describeLoadBalancerAttributesResponse_loadBalancerAttributes,
    describeLoadBalancerAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ELB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for DescribeLoadBalancerAttributes.
--
-- /See:/ 'newDescribeLoadBalancerAttributes' smart constructor.
data DescribeLoadBalancerAttributes = DescribeLoadBalancerAttributes'
  { -- | The name of the load balancer.
    loadBalancerName :: Prelude.Text
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
-- 'loadBalancerName', 'describeLoadBalancerAttributes_loadBalancerName' - The name of the load balancer.
newDescribeLoadBalancerAttributes ::
  -- | 'loadBalancerName'
  Prelude.Text ->
  DescribeLoadBalancerAttributes
newDescribeLoadBalancerAttributes pLoadBalancerName_ =
  DescribeLoadBalancerAttributes'
    { loadBalancerName =
        pLoadBalancerName_
    }

-- | The name of the load balancer.
describeLoadBalancerAttributes_loadBalancerName :: Lens.Lens' DescribeLoadBalancerAttributes Prelude.Text
describeLoadBalancerAttributes_loadBalancerName = Lens.lens (\DescribeLoadBalancerAttributes' {loadBalancerName} -> loadBalancerName) (\s@DescribeLoadBalancerAttributes' {} a -> s {loadBalancerName = a} :: DescribeLoadBalancerAttributes)

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
            Prelude.<$> (x Core..@? "LoadBalancerAttributes")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeLoadBalancerAttributes
  where
  hashWithSalt
    _salt
    DescribeLoadBalancerAttributes' {..} =
      _salt `Prelude.hashWithSalt` loadBalancerName

instance
  Prelude.NFData
    DescribeLoadBalancerAttributes
  where
  rnf DescribeLoadBalancerAttributes' {..} =
    Prelude.rnf loadBalancerName

instance
  Core.ToHeaders
    DescribeLoadBalancerAttributes
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeLoadBalancerAttributes where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeLoadBalancerAttributes where
  toQuery DescribeLoadBalancerAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeLoadBalancerAttributes" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2012-06-01" :: Prelude.ByteString),
        "LoadBalancerName" Core.=: loadBalancerName
      ]

-- | Contains the output of DescribeLoadBalancerAttributes.
--
-- /See:/ 'newDescribeLoadBalancerAttributesResponse' smart constructor.
data DescribeLoadBalancerAttributesResponse = DescribeLoadBalancerAttributesResponse'
  { -- | Information about the load balancer attributes.
    loadBalancerAttributes :: Prelude.Maybe LoadBalancerAttributes,
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
-- 'loadBalancerAttributes', 'describeLoadBalancerAttributesResponse_loadBalancerAttributes' - Information about the load balancer attributes.
--
-- 'httpStatus', 'describeLoadBalancerAttributesResponse_httpStatus' - The response's http status code.
newDescribeLoadBalancerAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLoadBalancerAttributesResponse
newDescribeLoadBalancerAttributesResponse
  pHttpStatus_ =
    DescribeLoadBalancerAttributesResponse'
      { loadBalancerAttributes =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the load balancer attributes.
describeLoadBalancerAttributesResponse_loadBalancerAttributes :: Lens.Lens' DescribeLoadBalancerAttributesResponse (Prelude.Maybe LoadBalancerAttributes)
describeLoadBalancerAttributesResponse_loadBalancerAttributes = Lens.lens (\DescribeLoadBalancerAttributesResponse' {loadBalancerAttributes} -> loadBalancerAttributes) (\s@DescribeLoadBalancerAttributesResponse' {} a -> s {loadBalancerAttributes = a} :: DescribeLoadBalancerAttributesResponse)

-- | The response's http status code.
describeLoadBalancerAttributesResponse_httpStatus :: Lens.Lens' DescribeLoadBalancerAttributesResponse Prelude.Int
describeLoadBalancerAttributesResponse_httpStatus = Lens.lens (\DescribeLoadBalancerAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeLoadBalancerAttributesResponse' {} a -> s {httpStatus = a} :: DescribeLoadBalancerAttributesResponse)

instance
  Prelude.NFData
    DescribeLoadBalancerAttributesResponse
  where
  rnf DescribeLoadBalancerAttributesResponse' {..} =
    Prelude.rnf loadBalancerAttributes
      `Prelude.seq` Prelude.rnf httpStatus
