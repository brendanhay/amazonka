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
-- Module      : Network.AWS.ELB.DescribeLoadBalancerAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the attributes for the specified load balancer.
module Network.AWS.ELB.DescribeLoadBalancerAttributes
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

import qualified Network.AWS.Core as Core
import Network.AWS.ELB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DescribeLoadBalancerAttributes.
--
-- /See:/ 'newDescribeLoadBalancerAttributes' smart constructor.
data DescribeLoadBalancerAttributes = DescribeLoadBalancerAttributes'
  { -- | The name of the load balancer.
    loadBalancerName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeLoadBalancerAttributes
newDescribeLoadBalancerAttributes pLoadBalancerName_ =
  DescribeLoadBalancerAttributes'
    { loadBalancerName =
        pLoadBalancerName_
    }

-- | The name of the load balancer.
describeLoadBalancerAttributes_loadBalancerName :: Lens.Lens' DescribeLoadBalancerAttributes Core.Text
describeLoadBalancerAttributes_loadBalancerName = Lens.lens (\DescribeLoadBalancerAttributes' {loadBalancerName} -> loadBalancerName) (\s@DescribeLoadBalancerAttributes' {} a -> s {loadBalancerName = a} :: DescribeLoadBalancerAttributes)

instance
  Core.AWSRequest
    DescribeLoadBalancerAttributes
  where
  type
    AWSResponse DescribeLoadBalancerAttributes =
      DescribeLoadBalancerAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeLoadBalancerAttributesResult"
      ( \s h x ->
          DescribeLoadBalancerAttributesResponse'
            Core.<$> (x Core..@? "LoadBalancerAttributes")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeLoadBalancerAttributes

instance Core.NFData DescribeLoadBalancerAttributes

instance
  Core.ToHeaders
    DescribeLoadBalancerAttributes
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeLoadBalancerAttributes where
  toPath = Core.const "/"

instance Core.ToQuery DescribeLoadBalancerAttributes where
  toQuery DescribeLoadBalancerAttributes' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeLoadBalancerAttributes" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2012-06-01" :: Core.ByteString),
        "LoadBalancerName" Core.=: loadBalancerName
      ]

-- | Contains the output of DescribeLoadBalancerAttributes.
--
-- /See:/ 'newDescribeLoadBalancerAttributesResponse' smart constructor.
data DescribeLoadBalancerAttributesResponse = DescribeLoadBalancerAttributesResponse'
  { -- | Information about the load balancer attributes.
    loadBalancerAttributes :: Core.Maybe LoadBalancerAttributes,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeLoadBalancerAttributesResponse
newDescribeLoadBalancerAttributesResponse
  pHttpStatus_ =
    DescribeLoadBalancerAttributesResponse'
      { loadBalancerAttributes =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the load balancer attributes.
describeLoadBalancerAttributesResponse_loadBalancerAttributes :: Lens.Lens' DescribeLoadBalancerAttributesResponse (Core.Maybe LoadBalancerAttributes)
describeLoadBalancerAttributesResponse_loadBalancerAttributes = Lens.lens (\DescribeLoadBalancerAttributesResponse' {loadBalancerAttributes} -> loadBalancerAttributes) (\s@DescribeLoadBalancerAttributesResponse' {} a -> s {loadBalancerAttributes = a} :: DescribeLoadBalancerAttributesResponse)

-- | The response's http status code.
describeLoadBalancerAttributesResponse_httpStatus :: Lens.Lens' DescribeLoadBalancerAttributesResponse Core.Int
describeLoadBalancerAttributesResponse_httpStatus = Lens.lens (\DescribeLoadBalancerAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeLoadBalancerAttributesResponse' {} a -> s {httpStatus = a} :: DescribeLoadBalancerAttributesResponse)

instance
  Core.NFData
    DescribeLoadBalancerAttributesResponse
