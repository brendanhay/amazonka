{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Route53RecoveryReadiness.Types.ResourceSetOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Types.ResourceSetOutput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53RecoveryReadiness.Types.Resource

-- | A collection of resources of the same type.
--
-- /See:/ 'newResourceSetOutput' smart constructor.
data ResourceSetOutput = ResourceSetOutput'
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
    resourceSetType :: Prelude.Text,
    -- | The name of the resource set.
    resourceSetName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the resource set.
    resourceSetArn :: Prelude.Text,
    -- | A list of resource objects.
    resources :: [Resource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceSetOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'resourceSetOutput_tags' - Undocumented member.
--
-- 'resourceSetType', 'resourceSetOutput_resourceSetType' - The resource type of the resources in the resource set. Enter one of the
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
-- 'resourceSetName', 'resourceSetOutput_resourceSetName' - The name of the resource set.
--
-- 'resourceSetArn', 'resourceSetOutput_resourceSetArn' - The Amazon Resource Name (ARN) for the resource set.
--
-- 'resources', 'resourceSetOutput_resources' - A list of resource objects.
newResourceSetOutput ::
  -- | 'resourceSetType'
  Prelude.Text ->
  -- | 'resourceSetName'
  Prelude.Text ->
  -- | 'resourceSetArn'
  Prelude.Text ->
  ResourceSetOutput
newResourceSetOutput
  pResourceSetType_
  pResourceSetName_
  pResourceSetArn_ =
    ResourceSetOutput'
      { tags = Prelude.Nothing,
        resourceSetType = pResourceSetType_,
        resourceSetName = pResourceSetName_,
        resourceSetArn = pResourceSetArn_,
        resources = Prelude.mempty
      }

-- | Undocumented member.
resourceSetOutput_tags :: Lens.Lens' ResourceSetOutput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
resourceSetOutput_tags = Lens.lens (\ResourceSetOutput' {tags} -> tags) (\s@ResourceSetOutput' {} a -> s {tags = a} :: ResourceSetOutput) Prelude.. Lens.mapping Lens.coerced

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
resourceSetOutput_resourceSetType :: Lens.Lens' ResourceSetOutput Prelude.Text
resourceSetOutput_resourceSetType = Lens.lens (\ResourceSetOutput' {resourceSetType} -> resourceSetType) (\s@ResourceSetOutput' {} a -> s {resourceSetType = a} :: ResourceSetOutput)

-- | The name of the resource set.
resourceSetOutput_resourceSetName :: Lens.Lens' ResourceSetOutput Prelude.Text
resourceSetOutput_resourceSetName = Lens.lens (\ResourceSetOutput' {resourceSetName} -> resourceSetName) (\s@ResourceSetOutput' {} a -> s {resourceSetName = a} :: ResourceSetOutput)

-- | The Amazon Resource Name (ARN) for the resource set.
resourceSetOutput_resourceSetArn :: Lens.Lens' ResourceSetOutput Prelude.Text
resourceSetOutput_resourceSetArn = Lens.lens (\ResourceSetOutput' {resourceSetArn} -> resourceSetArn) (\s@ResourceSetOutput' {} a -> s {resourceSetArn = a} :: ResourceSetOutput)

-- | A list of resource objects.
resourceSetOutput_resources :: Lens.Lens' ResourceSetOutput [Resource]
resourceSetOutput_resources = Lens.lens (\ResourceSetOutput' {resources} -> resources) (\s@ResourceSetOutput' {} a -> s {resources = a} :: ResourceSetOutput) Prelude.. Lens.coerced

instance Data.FromJSON ResourceSetOutput where
  parseJSON =
    Data.withObject
      "ResourceSetOutput"
      ( \x ->
          ResourceSetOutput'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "resourceSetType")
            Prelude.<*> (x Data..: "resourceSetName")
            Prelude.<*> (x Data..: "resourceSetArn")
            Prelude.<*> (x Data..:? "resources" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ResourceSetOutput where
  hashWithSalt _salt ResourceSetOutput' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` resourceSetType
      `Prelude.hashWithSalt` resourceSetName
      `Prelude.hashWithSalt` resourceSetArn
      `Prelude.hashWithSalt` resources

instance Prelude.NFData ResourceSetOutput where
  rnf ResourceSetOutput' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf resourceSetType
      `Prelude.seq` Prelude.rnf resourceSetName
      `Prelude.seq` Prelude.rnf resourceSetArn
      `Prelude.seq` Prelude.rnf resources
