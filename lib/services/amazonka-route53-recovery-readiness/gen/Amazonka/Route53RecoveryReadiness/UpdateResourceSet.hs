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
-- Module      : Amazonka.Route53RecoveryReadiness.UpdateResourceSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a resource set.
module Amazonka.Route53RecoveryReadiness.UpdateResourceSet
  ( -- * Creating a Request
    UpdateResourceSet (..),
    newUpdateResourceSet,

    -- * Request Lenses
    updateResourceSet_resourceSetName,
    updateResourceSet_resourceSetType,
    updateResourceSet_resources,

    -- * Destructuring the Response
    UpdateResourceSetResponse (..),
    newUpdateResourceSetResponse,

    -- * Response Lenses
    updateResourceSetResponse_tags,
    updateResourceSetResponse_resourceSetType,
    updateResourceSetResponse_resourceSetName,
    updateResourceSetResponse_resources,
    updateResourceSetResponse_resourceSetArn,
    updateResourceSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | Name of a resource set.
--
-- /See:/ 'newUpdateResourceSet' smart constructor.
data UpdateResourceSet = UpdateResourceSet'
  { -- | Name of a resource set.
    resourceSetName :: Prelude.Text,
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
    resourceSetType :: Prelude.Text,
    -- | A list of resource objects.
    resources :: [Resource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourceSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceSetName', 'updateResourceSet_resourceSetName' - Name of a resource set.
--
-- 'resourceSetType', 'updateResourceSet_resourceSetType' - The resource type of the resources in the resource set. Enter one of the
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
-- 'resources', 'updateResourceSet_resources' - A list of resource objects.
newUpdateResourceSet ::
  -- | 'resourceSetName'
  Prelude.Text ->
  -- | 'resourceSetType'
  Prelude.Text ->
  UpdateResourceSet
newUpdateResourceSet
  pResourceSetName_
  pResourceSetType_ =
    UpdateResourceSet'
      { resourceSetName =
          pResourceSetName_,
        resourceSetType = pResourceSetType_,
        resources = Prelude.mempty
      }

-- | Name of a resource set.
updateResourceSet_resourceSetName :: Lens.Lens' UpdateResourceSet Prelude.Text
updateResourceSet_resourceSetName = Lens.lens (\UpdateResourceSet' {resourceSetName} -> resourceSetName) (\s@UpdateResourceSet' {} a -> s {resourceSetName = a} :: UpdateResourceSet)

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
updateResourceSet_resourceSetType :: Lens.Lens' UpdateResourceSet Prelude.Text
updateResourceSet_resourceSetType = Lens.lens (\UpdateResourceSet' {resourceSetType} -> resourceSetType) (\s@UpdateResourceSet' {} a -> s {resourceSetType = a} :: UpdateResourceSet)

-- | A list of resource objects.
updateResourceSet_resources :: Lens.Lens' UpdateResourceSet [Resource]
updateResourceSet_resources = Lens.lens (\UpdateResourceSet' {resources} -> resources) (\s@UpdateResourceSet' {} a -> s {resources = a} :: UpdateResourceSet) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateResourceSet where
  type
    AWSResponse UpdateResourceSet =
      UpdateResourceSetResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateResourceSetResponse'
            Prelude.<$> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "resourceSetType")
            Prelude.<*> (x Core..?> "resourceSetName")
            Prelude.<*> (x Core..?> "resources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "resourceSetArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateResourceSet where
  hashWithSalt _salt UpdateResourceSet' {..} =
    _salt `Prelude.hashWithSalt` resourceSetName
      `Prelude.hashWithSalt` resourceSetType
      `Prelude.hashWithSalt` resources

instance Prelude.NFData UpdateResourceSet where
  rnf UpdateResourceSet' {..} =
    Prelude.rnf resourceSetName
      `Prelude.seq` Prelude.rnf resourceSetType
      `Prelude.seq` Prelude.rnf resources

instance Core.ToHeaders UpdateResourceSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateResourceSet where
  toJSON UpdateResourceSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("resourceSetType" Core..= resourceSetType),
            Prelude.Just ("resources" Core..= resources)
          ]
      )

instance Core.ToPath UpdateResourceSet where
  toPath UpdateResourceSet' {..} =
    Prelude.mconcat
      ["/resourcesets/", Core.toBS resourceSetName]

instance Core.ToQuery UpdateResourceSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateResourceSetResponse' smart constructor.
data UpdateResourceSetResponse = UpdateResourceSetResponse'
  { tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
    -- | The name of the resource set.
    resourceSetName :: Prelude.Maybe Prelude.Text,
    -- | A list of resource objects.
    resources :: Prelude.Maybe [Resource],
    -- | The Amazon Resource Name (ARN) for the resource set.
    resourceSetArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourceSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'updateResourceSetResponse_tags' - Undocumented member.
--
-- 'resourceSetType', 'updateResourceSetResponse_resourceSetType' - The resource type of the resources in the resource set. Enter one of the
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
-- 'resourceSetName', 'updateResourceSetResponse_resourceSetName' - The name of the resource set.
--
-- 'resources', 'updateResourceSetResponse_resources' - A list of resource objects.
--
-- 'resourceSetArn', 'updateResourceSetResponse_resourceSetArn' - The Amazon Resource Name (ARN) for the resource set.
--
-- 'httpStatus', 'updateResourceSetResponse_httpStatus' - The response's http status code.
newUpdateResourceSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateResourceSetResponse
newUpdateResourceSetResponse pHttpStatus_ =
  UpdateResourceSetResponse'
    { tags = Prelude.Nothing,
      resourceSetType = Prelude.Nothing,
      resourceSetName = Prelude.Nothing,
      resources = Prelude.Nothing,
      resourceSetArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateResourceSetResponse_tags :: Lens.Lens' UpdateResourceSetResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateResourceSetResponse_tags = Lens.lens (\UpdateResourceSetResponse' {tags} -> tags) (\s@UpdateResourceSetResponse' {} a -> s {tags = a} :: UpdateResourceSetResponse) Prelude.. Lens.mapping Lens.coerced

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
updateResourceSetResponse_resourceSetType :: Lens.Lens' UpdateResourceSetResponse (Prelude.Maybe Prelude.Text)
updateResourceSetResponse_resourceSetType = Lens.lens (\UpdateResourceSetResponse' {resourceSetType} -> resourceSetType) (\s@UpdateResourceSetResponse' {} a -> s {resourceSetType = a} :: UpdateResourceSetResponse)

-- | The name of the resource set.
updateResourceSetResponse_resourceSetName :: Lens.Lens' UpdateResourceSetResponse (Prelude.Maybe Prelude.Text)
updateResourceSetResponse_resourceSetName = Lens.lens (\UpdateResourceSetResponse' {resourceSetName} -> resourceSetName) (\s@UpdateResourceSetResponse' {} a -> s {resourceSetName = a} :: UpdateResourceSetResponse)

-- | A list of resource objects.
updateResourceSetResponse_resources :: Lens.Lens' UpdateResourceSetResponse (Prelude.Maybe [Resource])
updateResourceSetResponse_resources = Lens.lens (\UpdateResourceSetResponse' {resources} -> resources) (\s@UpdateResourceSetResponse' {} a -> s {resources = a} :: UpdateResourceSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for the resource set.
updateResourceSetResponse_resourceSetArn :: Lens.Lens' UpdateResourceSetResponse (Prelude.Maybe Prelude.Text)
updateResourceSetResponse_resourceSetArn = Lens.lens (\UpdateResourceSetResponse' {resourceSetArn} -> resourceSetArn) (\s@UpdateResourceSetResponse' {} a -> s {resourceSetArn = a} :: UpdateResourceSetResponse)

-- | The response's http status code.
updateResourceSetResponse_httpStatus :: Lens.Lens' UpdateResourceSetResponse Prelude.Int
updateResourceSetResponse_httpStatus = Lens.lens (\UpdateResourceSetResponse' {httpStatus} -> httpStatus) (\s@UpdateResourceSetResponse' {} a -> s {httpStatus = a} :: UpdateResourceSetResponse)

instance Prelude.NFData UpdateResourceSetResponse where
  rnf UpdateResourceSetResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf resourceSetType
      `Prelude.seq` Prelude.rnf resourceSetName
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf resourceSetArn
      `Prelude.seq` Prelude.rnf httpStatus
