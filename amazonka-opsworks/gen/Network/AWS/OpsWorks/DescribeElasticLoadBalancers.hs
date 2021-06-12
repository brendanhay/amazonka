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
-- Module      : Network.AWS.OpsWorks.DescribeElasticLoadBalancers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a stack\'s Elastic Load Balancing instances.
--
-- This call accepts only one resource-identifying parameter.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information about
-- user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
module Network.AWS.OpsWorks.DescribeElasticLoadBalancers
  ( -- * Creating a Request
    DescribeElasticLoadBalancers (..),
    newDescribeElasticLoadBalancers,

    -- * Request Lenses
    describeElasticLoadBalancers_stackId,
    describeElasticLoadBalancers_layerIds,

    -- * Destructuring the Response
    DescribeElasticLoadBalancersResponse (..),
    newDescribeElasticLoadBalancersResponse,

    -- * Response Lenses
    describeElasticLoadBalancersResponse_elasticLoadBalancers,
    describeElasticLoadBalancersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeElasticLoadBalancers' smart constructor.
data DescribeElasticLoadBalancers = DescribeElasticLoadBalancers'
  { -- | A stack ID. The action describes the stack\'s Elastic Load Balancing
    -- instances.
    stackId :: Core.Maybe Core.Text,
    -- | A list of layer IDs. The action describes the Elastic Load Balancing
    -- instances for the specified layers.
    layerIds :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeElasticLoadBalancers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'describeElasticLoadBalancers_stackId' - A stack ID. The action describes the stack\'s Elastic Load Balancing
-- instances.
--
-- 'layerIds', 'describeElasticLoadBalancers_layerIds' - A list of layer IDs. The action describes the Elastic Load Balancing
-- instances for the specified layers.
newDescribeElasticLoadBalancers ::
  DescribeElasticLoadBalancers
newDescribeElasticLoadBalancers =
  DescribeElasticLoadBalancers'
    { stackId =
        Core.Nothing,
      layerIds = Core.Nothing
    }

-- | A stack ID. The action describes the stack\'s Elastic Load Balancing
-- instances.
describeElasticLoadBalancers_stackId :: Lens.Lens' DescribeElasticLoadBalancers (Core.Maybe Core.Text)
describeElasticLoadBalancers_stackId = Lens.lens (\DescribeElasticLoadBalancers' {stackId} -> stackId) (\s@DescribeElasticLoadBalancers' {} a -> s {stackId = a} :: DescribeElasticLoadBalancers)

-- | A list of layer IDs. The action describes the Elastic Load Balancing
-- instances for the specified layers.
describeElasticLoadBalancers_layerIds :: Lens.Lens' DescribeElasticLoadBalancers (Core.Maybe [Core.Text])
describeElasticLoadBalancers_layerIds = Lens.lens (\DescribeElasticLoadBalancers' {layerIds} -> layerIds) (\s@DescribeElasticLoadBalancers' {} a -> s {layerIds = a} :: DescribeElasticLoadBalancers) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeElasticLoadBalancers where
  type
    AWSResponse DescribeElasticLoadBalancers =
      DescribeElasticLoadBalancersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeElasticLoadBalancersResponse'
            Core.<$> ( x Core..?> "ElasticLoadBalancers"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeElasticLoadBalancers

instance Core.NFData DescribeElasticLoadBalancers

instance Core.ToHeaders DescribeElasticLoadBalancers where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DescribeElasticLoadBalancers" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeElasticLoadBalancers where
  toJSON DescribeElasticLoadBalancers' {..} =
    Core.object
      ( Core.catMaybes
          [ ("StackId" Core..=) Core.<$> stackId,
            ("LayerIds" Core..=) Core.<$> layerIds
          ]
      )

instance Core.ToPath DescribeElasticLoadBalancers where
  toPath = Core.const "/"

instance Core.ToQuery DescribeElasticLoadBalancers where
  toQuery = Core.const Core.mempty

-- | Contains the response to a @DescribeElasticLoadBalancers@ request.
--
-- /See:/ 'newDescribeElasticLoadBalancersResponse' smart constructor.
data DescribeElasticLoadBalancersResponse = DescribeElasticLoadBalancersResponse'
  { -- | A list of @ElasticLoadBalancer@ objects that describe the specified
    -- Elastic Load Balancing instances.
    elasticLoadBalancers :: Core.Maybe [ElasticLoadBalancer],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeElasticLoadBalancersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'elasticLoadBalancers', 'describeElasticLoadBalancersResponse_elasticLoadBalancers' - A list of @ElasticLoadBalancer@ objects that describe the specified
-- Elastic Load Balancing instances.
--
-- 'httpStatus', 'describeElasticLoadBalancersResponse_httpStatus' - The response's http status code.
newDescribeElasticLoadBalancersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeElasticLoadBalancersResponse
newDescribeElasticLoadBalancersResponse pHttpStatus_ =
  DescribeElasticLoadBalancersResponse'
    { elasticLoadBalancers =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @ElasticLoadBalancer@ objects that describe the specified
-- Elastic Load Balancing instances.
describeElasticLoadBalancersResponse_elasticLoadBalancers :: Lens.Lens' DescribeElasticLoadBalancersResponse (Core.Maybe [ElasticLoadBalancer])
describeElasticLoadBalancersResponse_elasticLoadBalancers = Lens.lens (\DescribeElasticLoadBalancersResponse' {elasticLoadBalancers} -> elasticLoadBalancers) (\s@DescribeElasticLoadBalancersResponse' {} a -> s {elasticLoadBalancers = a} :: DescribeElasticLoadBalancersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeElasticLoadBalancersResponse_httpStatus :: Lens.Lens' DescribeElasticLoadBalancersResponse Core.Int
describeElasticLoadBalancersResponse_httpStatus = Lens.lens (\DescribeElasticLoadBalancersResponse' {httpStatus} -> httpStatus) (\s@DescribeElasticLoadBalancersResponse' {} a -> s {httpStatus = a} :: DescribeElasticLoadBalancersResponse)

instance
  Core.NFData
    DescribeElasticLoadBalancersResponse
