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
-- Module      : Amazonka.Route53RecoveryReadiness.GetResourceSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the details about a resource set, including a list of the
-- resources in the set.
module Amazonka.Route53RecoveryReadiness.GetResourceSet
  ( -- * Creating a Request
    GetResourceSet (..),
    newGetResourceSet,

    -- * Request Lenses
    getResourceSet_resourceSetName,

    -- * Destructuring the Response
    GetResourceSetResponse (..),
    newGetResourceSetResponse,

    -- * Response Lenses
    getResourceSetResponse_resourceSetArn,
    getResourceSetResponse_resourceSetName,
    getResourceSetResponse_resourceSetType,
    getResourceSetResponse_resources,
    getResourceSetResponse_tags,
    getResourceSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newGetResourceSet' smart constructor.
data GetResourceSet = GetResourceSet'
  { -- | Name of a resource set.
    resourceSetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceSetName', 'getResourceSet_resourceSetName' - Name of a resource set.
newGetResourceSet ::
  -- | 'resourceSetName'
  Prelude.Text ->
  GetResourceSet
newGetResourceSet pResourceSetName_ =
  GetResourceSet'
    { resourceSetName =
        pResourceSetName_
    }

-- | Name of a resource set.
getResourceSet_resourceSetName :: Lens.Lens' GetResourceSet Prelude.Text
getResourceSet_resourceSetName = Lens.lens (\GetResourceSet' {resourceSetName} -> resourceSetName) (\s@GetResourceSet' {} a -> s {resourceSetName = a} :: GetResourceSet)

instance Core.AWSRequest GetResourceSet where
  type
    AWSResponse GetResourceSet =
      GetResourceSetResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceSetResponse'
            Prelude.<$> (x Data..?> "resourceSetArn")
            Prelude.<*> (x Data..?> "resourceSetName")
            Prelude.<*> (x Data..?> "resourceSetType")
            Prelude.<*> (x Data..?> "resources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourceSet where
  hashWithSalt _salt GetResourceSet' {..} =
    _salt `Prelude.hashWithSalt` resourceSetName

instance Prelude.NFData GetResourceSet where
  rnf GetResourceSet' {..} = Prelude.rnf resourceSetName

instance Data.ToHeaders GetResourceSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetResourceSet where
  toPath GetResourceSet' {..} =
    Prelude.mconcat
      ["/resourcesets/", Data.toBS resourceSetName]

instance Data.ToQuery GetResourceSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourceSetResponse' smart constructor.
data GetResourceSetResponse = GetResourceSetResponse'
  { -- | The Amazon Resource Name (ARN) for the resource set.
    resourceSetArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource set.
    resourceSetName :: Prelude.Maybe Prelude.Text,
    -- | The resource type of the resources in the resource set. Enter one of the
    -- following values for resource type:
    --
    -- AWS::ApiGateway::Stage, AWS::ApiGatewayV2::Stage,
    -- AWS::AutoScaling::AutoScalingGroup, AWS::CloudWatch::Alarm,
    -- AWS::EC2::CustomerGateway, AWS::DynamoDB::Table, AWS::EC2::Volume,
    -- AWS::ElasticLoadBalancing::LoadBalancer,
    -- AWS::ElasticLoadBalancingV2::LoadBalancer, AWS::Lambda::Function,
    -- AWS::MSK::Cluster, AWS::RDS::DBCluster, AWS::Route53::HealthCheck,
    -- AWS::SQS::Queue, AWS::SNS::Topic, AWS::SNS::Subscription, AWS::EC2::VPC,
    -- AWS::EC2::VPNConnection, AWS::EC2::VPNGateway,
    -- AWS::Route53RecoveryReadiness::DNSTargetResource
    resourceSetType :: Prelude.Maybe Prelude.Text,
    -- | A list of resource objects.
    resources :: Prelude.Maybe [Resource],
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceSetArn', 'getResourceSetResponse_resourceSetArn' - The Amazon Resource Name (ARN) for the resource set.
--
-- 'resourceSetName', 'getResourceSetResponse_resourceSetName' - The name of the resource set.
--
-- 'resourceSetType', 'getResourceSetResponse_resourceSetType' - The resource type of the resources in the resource set. Enter one of the
-- following values for resource type:
--
-- AWS::ApiGateway::Stage, AWS::ApiGatewayV2::Stage,
-- AWS::AutoScaling::AutoScalingGroup, AWS::CloudWatch::Alarm,
-- AWS::EC2::CustomerGateway, AWS::DynamoDB::Table, AWS::EC2::Volume,
-- AWS::ElasticLoadBalancing::LoadBalancer,
-- AWS::ElasticLoadBalancingV2::LoadBalancer, AWS::Lambda::Function,
-- AWS::MSK::Cluster, AWS::RDS::DBCluster, AWS::Route53::HealthCheck,
-- AWS::SQS::Queue, AWS::SNS::Topic, AWS::SNS::Subscription, AWS::EC2::VPC,
-- AWS::EC2::VPNConnection, AWS::EC2::VPNGateway,
-- AWS::Route53RecoveryReadiness::DNSTargetResource
--
-- 'resources', 'getResourceSetResponse_resources' - A list of resource objects.
--
-- 'tags', 'getResourceSetResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'getResourceSetResponse_httpStatus' - The response's http status code.
newGetResourceSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourceSetResponse
newGetResourceSetResponse pHttpStatus_ =
  GetResourceSetResponse'
    { resourceSetArn =
        Prelude.Nothing,
      resourceSetName = Prelude.Nothing,
      resourceSetType = Prelude.Nothing,
      resources = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) for the resource set.
getResourceSetResponse_resourceSetArn :: Lens.Lens' GetResourceSetResponse (Prelude.Maybe Prelude.Text)
getResourceSetResponse_resourceSetArn = Lens.lens (\GetResourceSetResponse' {resourceSetArn} -> resourceSetArn) (\s@GetResourceSetResponse' {} a -> s {resourceSetArn = a} :: GetResourceSetResponse)

-- | The name of the resource set.
getResourceSetResponse_resourceSetName :: Lens.Lens' GetResourceSetResponse (Prelude.Maybe Prelude.Text)
getResourceSetResponse_resourceSetName = Lens.lens (\GetResourceSetResponse' {resourceSetName} -> resourceSetName) (\s@GetResourceSetResponse' {} a -> s {resourceSetName = a} :: GetResourceSetResponse)

-- | The resource type of the resources in the resource set. Enter one of the
-- following values for resource type:
--
-- AWS::ApiGateway::Stage, AWS::ApiGatewayV2::Stage,
-- AWS::AutoScaling::AutoScalingGroup, AWS::CloudWatch::Alarm,
-- AWS::EC2::CustomerGateway, AWS::DynamoDB::Table, AWS::EC2::Volume,
-- AWS::ElasticLoadBalancing::LoadBalancer,
-- AWS::ElasticLoadBalancingV2::LoadBalancer, AWS::Lambda::Function,
-- AWS::MSK::Cluster, AWS::RDS::DBCluster, AWS::Route53::HealthCheck,
-- AWS::SQS::Queue, AWS::SNS::Topic, AWS::SNS::Subscription, AWS::EC2::VPC,
-- AWS::EC2::VPNConnection, AWS::EC2::VPNGateway,
-- AWS::Route53RecoveryReadiness::DNSTargetResource
getResourceSetResponse_resourceSetType :: Lens.Lens' GetResourceSetResponse (Prelude.Maybe Prelude.Text)
getResourceSetResponse_resourceSetType = Lens.lens (\GetResourceSetResponse' {resourceSetType} -> resourceSetType) (\s@GetResourceSetResponse' {} a -> s {resourceSetType = a} :: GetResourceSetResponse)

-- | A list of resource objects.
getResourceSetResponse_resources :: Lens.Lens' GetResourceSetResponse (Prelude.Maybe [Resource])
getResourceSetResponse_resources = Lens.lens (\GetResourceSetResponse' {resources} -> resources) (\s@GetResourceSetResponse' {} a -> s {resources = a} :: GetResourceSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
getResourceSetResponse_tags :: Lens.Lens' GetResourceSetResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getResourceSetResponse_tags = Lens.lens (\GetResourceSetResponse' {tags} -> tags) (\s@GetResourceSetResponse' {} a -> s {tags = a} :: GetResourceSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getResourceSetResponse_httpStatus :: Lens.Lens' GetResourceSetResponse Prelude.Int
getResourceSetResponse_httpStatus = Lens.lens (\GetResourceSetResponse' {httpStatus} -> httpStatus) (\s@GetResourceSetResponse' {} a -> s {httpStatus = a} :: GetResourceSetResponse)

instance Prelude.NFData GetResourceSetResponse where
  rnf GetResourceSetResponse' {..} =
    Prelude.rnf resourceSetArn `Prelude.seq`
      Prelude.rnf resourceSetName `Prelude.seq`
        Prelude.rnf resourceSetType `Prelude.seq`
          Prelude.rnf resources `Prelude.seq`
            Prelude.rnf tags `Prelude.seq`
              Prelude.rnf httpStatus
