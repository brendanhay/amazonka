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
-- Module      : Amazonka.OpsWorks.DescribeElasticLoadBalancers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.OpsWorks.DescribeElasticLoadBalancers
  ( -- * Creating a Request
    DescribeElasticLoadBalancers (..),
    newDescribeElasticLoadBalancers,

    -- * Request Lenses
    describeElasticLoadBalancers_layerIds,
    describeElasticLoadBalancers_stackId,

    -- * Destructuring the Response
    DescribeElasticLoadBalancersResponse (..),
    newDescribeElasticLoadBalancersResponse,

    -- * Response Lenses
    describeElasticLoadBalancersResponse_elasticLoadBalancers,
    describeElasticLoadBalancersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeElasticLoadBalancers' smart constructor.
data DescribeElasticLoadBalancers = DescribeElasticLoadBalancers'
  { -- | A list of layer IDs. The action describes the Elastic Load Balancing
    -- instances for the specified layers.
    layerIds :: Prelude.Maybe [Prelude.Text],
    -- | A stack ID. The action describes the stack\'s Elastic Load Balancing
    -- instances.
    stackId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeElasticLoadBalancers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'layerIds', 'describeElasticLoadBalancers_layerIds' - A list of layer IDs. The action describes the Elastic Load Balancing
-- instances for the specified layers.
--
-- 'stackId', 'describeElasticLoadBalancers_stackId' - A stack ID. The action describes the stack\'s Elastic Load Balancing
-- instances.
newDescribeElasticLoadBalancers ::
  DescribeElasticLoadBalancers
newDescribeElasticLoadBalancers =
  DescribeElasticLoadBalancers'
    { layerIds =
        Prelude.Nothing,
      stackId = Prelude.Nothing
    }

-- | A list of layer IDs. The action describes the Elastic Load Balancing
-- instances for the specified layers.
describeElasticLoadBalancers_layerIds :: Lens.Lens' DescribeElasticLoadBalancers (Prelude.Maybe [Prelude.Text])
describeElasticLoadBalancers_layerIds = Lens.lens (\DescribeElasticLoadBalancers' {layerIds} -> layerIds) (\s@DescribeElasticLoadBalancers' {} a -> s {layerIds = a} :: DescribeElasticLoadBalancers) Prelude.. Lens.mapping Lens.coerced

-- | A stack ID. The action describes the stack\'s Elastic Load Balancing
-- instances.
describeElasticLoadBalancers_stackId :: Lens.Lens' DescribeElasticLoadBalancers (Prelude.Maybe Prelude.Text)
describeElasticLoadBalancers_stackId = Lens.lens (\DescribeElasticLoadBalancers' {stackId} -> stackId) (\s@DescribeElasticLoadBalancers' {} a -> s {stackId = a} :: DescribeElasticLoadBalancers)

instance Core.AWSRequest DescribeElasticLoadBalancers where
  type
    AWSResponse DescribeElasticLoadBalancers =
      DescribeElasticLoadBalancersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeElasticLoadBalancersResponse'
            Prelude.<$> ( x
                            Data..?> "ElasticLoadBalancers"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeElasticLoadBalancers
  where
  hashWithSalt _salt DescribeElasticLoadBalancers' {..} =
    _salt
      `Prelude.hashWithSalt` layerIds
      `Prelude.hashWithSalt` stackId

instance Prelude.NFData DescribeElasticLoadBalancers where
  rnf DescribeElasticLoadBalancers' {..} =
    Prelude.rnf layerIds
      `Prelude.seq` Prelude.rnf stackId

instance Data.ToHeaders DescribeElasticLoadBalancers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.DescribeElasticLoadBalancers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeElasticLoadBalancers where
  toJSON DescribeElasticLoadBalancers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LayerIds" Data..=) Prelude.<$> layerIds,
            ("StackId" Data..=) Prelude.<$> stackId
          ]
      )

instance Data.ToPath DescribeElasticLoadBalancers where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeElasticLoadBalancers where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @DescribeElasticLoadBalancers@ request.
--
-- /See:/ 'newDescribeElasticLoadBalancersResponse' smart constructor.
data DescribeElasticLoadBalancersResponse = DescribeElasticLoadBalancersResponse'
  { -- | A list of @ElasticLoadBalancer@ objects that describe the specified
    -- Elastic Load Balancing instances.
    elasticLoadBalancers :: Prelude.Maybe [ElasticLoadBalancer],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeElasticLoadBalancersResponse
newDescribeElasticLoadBalancersResponse pHttpStatus_ =
  DescribeElasticLoadBalancersResponse'
    { elasticLoadBalancers =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @ElasticLoadBalancer@ objects that describe the specified
-- Elastic Load Balancing instances.
describeElasticLoadBalancersResponse_elasticLoadBalancers :: Lens.Lens' DescribeElasticLoadBalancersResponse (Prelude.Maybe [ElasticLoadBalancer])
describeElasticLoadBalancersResponse_elasticLoadBalancers = Lens.lens (\DescribeElasticLoadBalancersResponse' {elasticLoadBalancers} -> elasticLoadBalancers) (\s@DescribeElasticLoadBalancersResponse' {} a -> s {elasticLoadBalancers = a} :: DescribeElasticLoadBalancersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeElasticLoadBalancersResponse_httpStatus :: Lens.Lens' DescribeElasticLoadBalancersResponse Prelude.Int
describeElasticLoadBalancersResponse_httpStatus = Lens.lens (\DescribeElasticLoadBalancersResponse' {httpStatus} -> httpStatus) (\s@DescribeElasticLoadBalancersResponse' {} a -> s {httpStatus = a} :: DescribeElasticLoadBalancersResponse)

instance
  Prelude.NFData
    DescribeElasticLoadBalancersResponse
  where
  rnf DescribeElasticLoadBalancersResponse' {..} =
    Prelude.rnf elasticLoadBalancers
      `Prelude.seq` Prelude.rnf httpStatus
