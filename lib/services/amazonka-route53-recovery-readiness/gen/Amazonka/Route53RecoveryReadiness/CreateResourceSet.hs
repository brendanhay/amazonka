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
-- Module      : Amazonka.Route53RecoveryReadiness.CreateResourceSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a resource set. A resource set is a set of resources of one type
-- that span multiple cells. You can associate a resource set with a
-- readiness check to monitor the resources for failover readiness.
module Amazonka.Route53RecoveryReadiness.CreateResourceSet
  ( -- * Creating a Request
    CreateResourceSet (..),
    newCreateResourceSet,

    -- * Request Lenses
    createResourceSet_tags,
    createResourceSet_resourceSetType,
    createResourceSet_resourceSetName,
    createResourceSet_resources,

    -- * Destructuring the Response
    CreateResourceSetResponse (..),
    newCreateResourceSetResponse,

    -- * Response Lenses
    createResourceSetResponse_resourceSetArn,
    createResourceSetResponse_resourceSetName,
    createResourceSetResponse_resourceSetType,
    createResourceSetResponse_resources,
    createResourceSetResponse_tags,
    createResourceSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newCreateResourceSet' smart constructor.
data CreateResourceSet = CreateResourceSet'
  { -- | A tag to associate with the parameters for a resource set.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
    -- | The name of the resource set to create.
    resourceSetName :: Prelude.Text,
    -- | A list of resource objects in the resource set.
    resources :: [Resource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateResourceSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createResourceSet_tags' - A tag to associate with the parameters for a resource set.
--
-- 'resourceSetType', 'createResourceSet_resourceSetType' - The resource type of the resources in the resource set. Enter one of the
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
-- 'resourceSetName', 'createResourceSet_resourceSetName' - The name of the resource set to create.
--
-- 'resources', 'createResourceSet_resources' - A list of resource objects in the resource set.
newCreateResourceSet ::
  -- | 'resourceSetType'
  Prelude.Text ->
  -- | 'resourceSetName'
  Prelude.Text ->
  CreateResourceSet
newCreateResourceSet
  pResourceSetType_
  pResourceSetName_ =
    CreateResourceSet'
      { tags = Prelude.Nothing,
        resourceSetType = pResourceSetType_,
        resourceSetName = pResourceSetName_,
        resources = Prelude.mempty
      }

-- | A tag to associate with the parameters for a resource set.
createResourceSet_tags :: Lens.Lens' CreateResourceSet (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createResourceSet_tags = Lens.lens (\CreateResourceSet' {tags} -> tags) (\s@CreateResourceSet' {} a -> s {tags = a} :: CreateResourceSet) Prelude.. Lens.mapping Lens.coerced

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
createResourceSet_resourceSetType :: Lens.Lens' CreateResourceSet Prelude.Text
createResourceSet_resourceSetType = Lens.lens (\CreateResourceSet' {resourceSetType} -> resourceSetType) (\s@CreateResourceSet' {} a -> s {resourceSetType = a} :: CreateResourceSet)

-- | The name of the resource set to create.
createResourceSet_resourceSetName :: Lens.Lens' CreateResourceSet Prelude.Text
createResourceSet_resourceSetName = Lens.lens (\CreateResourceSet' {resourceSetName} -> resourceSetName) (\s@CreateResourceSet' {} a -> s {resourceSetName = a} :: CreateResourceSet)

-- | A list of resource objects in the resource set.
createResourceSet_resources :: Lens.Lens' CreateResourceSet [Resource]
createResourceSet_resources = Lens.lens (\CreateResourceSet' {resources} -> resources) (\s@CreateResourceSet' {} a -> s {resources = a} :: CreateResourceSet) Prelude.. Lens.coerced

instance Core.AWSRequest CreateResourceSet where
  type
    AWSResponse CreateResourceSet =
      CreateResourceSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateResourceSetResponse'
            Prelude.<$> (x Data..?> "resourceSetArn")
            Prelude.<*> (x Data..?> "resourceSetName")
            Prelude.<*> (x Data..?> "resourceSetType")
            Prelude.<*> (x Data..?> "resources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateResourceSet where
  hashWithSalt _salt CreateResourceSet' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` resourceSetType
      `Prelude.hashWithSalt` resourceSetName
      `Prelude.hashWithSalt` resources

instance Prelude.NFData CreateResourceSet where
  rnf CreateResourceSet' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf resourceSetType
      `Prelude.seq` Prelude.rnf resourceSetName
      `Prelude.seq` Prelude.rnf resources

instance Data.ToHeaders CreateResourceSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateResourceSet where
  toJSON CreateResourceSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("resourceSetType" Data..= resourceSetType),
            Prelude.Just
              ("resourceSetName" Data..= resourceSetName),
            Prelude.Just ("resources" Data..= resources)
          ]
      )

instance Data.ToPath CreateResourceSet where
  toPath = Prelude.const "/resourcesets"

instance Data.ToQuery CreateResourceSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateResourceSetResponse' smart constructor.
data CreateResourceSetResponse = CreateResourceSetResponse'
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
-- Create a value of 'CreateResourceSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceSetArn', 'createResourceSetResponse_resourceSetArn' - The Amazon Resource Name (ARN) for the resource set.
--
-- 'resourceSetName', 'createResourceSetResponse_resourceSetName' - The name of the resource set.
--
-- 'resourceSetType', 'createResourceSetResponse_resourceSetType' - The resource type of the resources in the resource set. Enter one of the
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
-- 'resources', 'createResourceSetResponse_resources' - A list of resource objects.
--
-- 'tags', 'createResourceSetResponse_tags' - Undocumented member.
--
-- 'httpStatus', 'createResourceSetResponse_httpStatus' - The response's http status code.
newCreateResourceSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateResourceSetResponse
newCreateResourceSetResponse pHttpStatus_ =
  CreateResourceSetResponse'
    { resourceSetArn =
        Prelude.Nothing,
      resourceSetName = Prelude.Nothing,
      resourceSetType = Prelude.Nothing,
      resources = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) for the resource set.
createResourceSetResponse_resourceSetArn :: Lens.Lens' CreateResourceSetResponse (Prelude.Maybe Prelude.Text)
createResourceSetResponse_resourceSetArn = Lens.lens (\CreateResourceSetResponse' {resourceSetArn} -> resourceSetArn) (\s@CreateResourceSetResponse' {} a -> s {resourceSetArn = a} :: CreateResourceSetResponse)

-- | The name of the resource set.
createResourceSetResponse_resourceSetName :: Lens.Lens' CreateResourceSetResponse (Prelude.Maybe Prelude.Text)
createResourceSetResponse_resourceSetName = Lens.lens (\CreateResourceSetResponse' {resourceSetName} -> resourceSetName) (\s@CreateResourceSetResponse' {} a -> s {resourceSetName = a} :: CreateResourceSetResponse)

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
createResourceSetResponse_resourceSetType :: Lens.Lens' CreateResourceSetResponse (Prelude.Maybe Prelude.Text)
createResourceSetResponse_resourceSetType = Lens.lens (\CreateResourceSetResponse' {resourceSetType} -> resourceSetType) (\s@CreateResourceSetResponse' {} a -> s {resourceSetType = a} :: CreateResourceSetResponse)

-- | A list of resource objects.
createResourceSetResponse_resources :: Lens.Lens' CreateResourceSetResponse (Prelude.Maybe [Resource])
createResourceSetResponse_resources = Lens.lens (\CreateResourceSetResponse' {resources} -> resources) (\s@CreateResourceSetResponse' {} a -> s {resources = a} :: CreateResourceSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createResourceSetResponse_tags :: Lens.Lens' CreateResourceSetResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createResourceSetResponse_tags = Lens.lens (\CreateResourceSetResponse' {tags} -> tags) (\s@CreateResourceSetResponse' {} a -> s {tags = a} :: CreateResourceSetResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createResourceSetResponse_httpStatus :: Lens.Lens' CreateResourceSetResponse Prelude.Int
createResourceSetResponse_httpStatus = Lens.lens (\CreateResourceSetResponse' {httpStatus} -> httpStatus) (\s@CreateResourceSetResponse' {} a -> s {httpStatus = a} :: CreateResourceSetResponse)

instance Prelude.NFData CreateResourceSetResponse where
  rnf CreateResourceSetResponse' {..} =
    Prelude.rnf resourceSetArn
      `Prelude.seq` Prelude.rnf resourceSetName
      `Prelude.seq` Prelude.rnf resourceSetType
      `Prelude.seq` Prelude.rnf resources
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
