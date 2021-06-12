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
-- Module      : Network.AWS.FMS.Types.PolicySummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.PolicySummary where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types.SecurityServiceType
import qualified Network.AWS.Lens as Lens

-- | Details of the AWS Firewall Manager policy.
--
-- /See:/ 'newPolicySummary' smart constructor.
data PolicySummary = PolicySummary'
  { -- | The name of the specified policy.
    policyName :: Core.Maybe Core.Text,
    -- | The service that the policy is using to protect the resources. This
    -- specifies the type of policy that is created, either an AWS WAF policy,
    -- a Shield Advanced policy, or a security group policy.
    securityServiceType :: Core.Maybe SecurityServiceType,
    -- | The type of resource protected by or in scope of the policy. This is in
    -- the format shown in the
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>.
    -- For AWS WAF and Shield Advanced, examples include
    -- @AWS::ElasticLoadBalancingV2::LoadBalancer@ and
    -- @AWS::CloudFront::Distribution@. For a security group common policy,
    -- valid values are @AWS::EC2::NetworkInterface@ and @AWS::EC2::Instance@.
    -- For a security group content audit policy, valid values are
    -- @AWS::EC2::SecurityGroup@, @AWS::EC2::NetworkInterface@, and
    -- @AWS::EC2::Instance@. For a security group usage audit policy, the value
    -- is @AWS::EC2::SecurityGroup@. For an AWS Network Firewall policy, the
    -- value is @AWS::EC2::VPC@.
    resourceType :: Core.Maybe Core.Text,
    -- | Indicates if the policy should be automatically applied to new
    -- resources.
    remediationEnabled :: Core.Maybe Core.Bool,
    -- | The ID of the specified policy.
    policyId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the specified policy.
    policyArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PolicySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'policySummary_policyName' - The name of the specified policy.
--
-- 'securityServiceType', 'policySummary_securityServiceType' - The service that the policy is using to protect the resources. This
-- specifies the type of policy that is created, either an AWS WAF policy,
-- a Shield Advanced policy, or a security group policy.
--
-- 'resourceType', 'policySummary_resourceType' - The type of resource protected by or in scope of the policy. This is in
-- the format shown in the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>.
-- For AWS WAF and Shield Advanced, examples include
-- @AWS::ElasticLoadBalancingV2::LoadBalancer@ and
-- @AWS::CloudFront::Distribution@. For a security group common policy,
-- valid values are @AWS::EC2::NetworkInterface@ and @AWS::EC2::Instance@.
-- For a security group content audit policy, valid values are
-- @AWS::EC2::SecurityGroup@, @AWS::EC2::NetworkInterface@, and
-- @AWS::EC2::Instance@. For a security group usage audit policy, the value
-- is @AWS::EC2::SecurityGroup@. For an AWS Network Firewall policy, the
-- value is @AWS::EC2::VPC@.
--
-- 'remediationEnabled', 'policySummary_remediationEnabled' - Indicates if the policy should be automatically applied to new
-- resources.
--
-- 'policyId', 'policySummary_policyId' - The ID of the specified policy.
--
-- 'policyArn', 'policySummary_policyArn' - The Amazon Resource Name (ARN) of the specified policy.
newPolicySummary ::
  PolicySummary
newPolicySummary =
  PolicySummary'
    { policyName = Core.Nothing,
      securityServiceType = Core.Nothing,
      resourceType = Core.Nothing,
      remediationEnabled = Core.Nothing,
      policyId = Core.Nothing,
      policyArn = Core.Nothing
    }

-- | The name of the specified policy.
policySummary_policyName :: Lens.Lens' PolicySummary (Core.Maybe Core.Text)
policySummary_policyName = Lens.lens (\PolicySummary' {policyName} -> policyName) (\s@PolicySummary' {} a -> s {policyName = a} :: PolicySummary)

-- | The service that the policy is using to protect the resources. This
-- specifies the type of policy that is created, either an AWS WAF policy,
-- a Shield Advanced policy, or a security group policy.
policySummary_securityServiceType :: Lens.Lens' PolicySummary (Core.Maybe SecurityServiceType)
policySummary_securityServiceType = Lens.lens (\PolicySummary' {securityServiceType} -> securityServiceType) (\s@PolicySummary' {} a -> s {securityServiceType = a} :: PolicySummary)

-- | The type of resource protected by or in scope of the policy. This is in
-- the format shown in the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html AWS Resource Types Reference>.
-- For AWS WAF and Shield Advanced, examples include
-- @AWS::ElasticLoadBalancingV2::LoadBalancer@ and
-- @AWS::CloudFront::Distribution@. For a security group common policy,
-- valid values are @AWS::EC2::NetworkInterface@ and @AWS::EC2::Instance@.
-- For a security group content audit policy, valid values are
-- @AWS::EC2::SecurityGroup@, @AWS::EC2::NetworkInterface@, and
-- @AWS::EC2::Instance@. For a security group usage audit policy, the value
-- is @AWS::EC2::SecurityGroup@. For an AWS Network Firewall policy, the
-- value is @AWS::EC2::VPC@.
policySummary_resourceType :: Lens.Lens' PolicySummary (Core.Maybe Core.Text)
policySummary_resourceType = Lens.lens (\PolicySummary' {resourceType} -> resourceType) (\s@PolicySummary' {} a -> s {resourceType = a} :: PolicySummary)

-- | Indicates if the policy should be automatically applied to new
-- resources.
policySummary_remediationEnabled :: Lens.Lens' PolicySummary (Core.Maybe Core.Bool)
policySummary_remediationEnabled = Lens.lens (\PolicySummary' {remediationEnabled} -> remediationEnabled) (\s@PolicySummary' {} a -> s {remediationEnabled = a} :: PolicySummary)

-- | The ID of the specified policy.
policySummary_policyId :: Lens.Lens' PolicySummary (Core.Maybe Core.Text)
policySummary_policyId = Lens.lens (\PolicySummary' {policyId} -> policyId) (\s@PolicySummary' {} a -> s {policyId = a} :: PolicySummary)

-- | The Amazon Resource Name (ARN) of the specified policy.
policySummary_policyArn :: Lens.Lens' PolicySummary (Core.Maybe Core.Text)
policySummary_policyArn = Lens.lens (\PolicySummary' {policyArn} -> policyArn) (\s@PolicySummary' {} a -> s {policyArn = a} :: PolicySummary)

instance Core.FromJSON PolicySummary where
  parseJSON =
    Core.withObject
      "PolicySummary"
      ( \x ->
          PolicySummary'
            Core.<$> (x Core..:? "PolicyName")
            Core.<*> (x Core..:? "SecurityServiceType")
            Core.<*> (x Core..:? "ResourceType")
            Core.<*> (x Core..:? "RemediationEnabled")
            Core.<*> (x Core..:? "PolicyId")
            Core.<*> (x Core..:? "PolicyArn")
      )

instance Core.Hashable PolicySummary

instance Core.NFData PolicySummary
