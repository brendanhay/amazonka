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
-- Module      : Amazonka.FMS.Types.PolicySummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.PolicySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.SecurityServiceType
import qualified Amazonka.Prelude as Prelude

-- | Details of the Firewall Manager policy.
--
-- /See:/ 'newPolicySummary' smart constructor.
data PolicySummary = PolicySummary'
  { -- | Indicates whether Firewall Manager should automatically remove
    -- protections from resources that leave the policy scope and clean up
    -- resources that Firewall Manager is managing for accounts when those
    -- accounts leave policy scope. For example, Firewall Manager will
    -- disassociate a Firewall Manager managed web ACL from a protected
    -- customer resource when the customer resource leaves policy scope.
    --
    -- By default, Firewall Manager doesn\'t remove protections or delete
    -- Firewall Manager managed resources.
    --
    -- This option is not available for Shield Advanced or WAF Classic
    -- policies.
    deleteUnusedFMManagedResources :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the specified policy.
    policyArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the specified policy.
    policyId :: Prelude.Maybe Prelude.Text,
    -- | The name of the specified policy.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | Indicates if the policy should be automatically applied to new
    -- resources.
    remediationEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The type of resource protected by or in scope of the policy. This is in
    -- the format shown in the
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services Resource Types Reference>.
    -- For WAF and Shield Advanced, examples include
    -- @AWS::ElasticLoadBalancingV2::LoadBalancer@ and
    -- @AWS::CloudFront::Distribution@. For a security group common policy,
    -- valid values are @AWS::EC2::NetworkInterface@ and @AWS::EC2::Instance@.
    -- For a security group content audit policy, valid values are
    -- @AWS::EC2::SecurityGroup@, @AWS::EC2::NetworkInterface@, and
    -- @AWS::EC2::Instance@. For a security group usage audit policy, the value
    -- is @AWS::EC2::SecurityGroup@. For an Network Firewall policy or DNS
    -- Firewall policy, the value is @AWS::EC2::VPC@.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The service that the policy is using to protect the resources. This
    -- specifies the type of policy that is created, either an WAF policy, a
    -- Shield Advanced policy, or a security group policy.
    securityServiceType :: Prelude.Maybe SecurityServiceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteUnusedFMManagedResources', 'policySummary_deleteUnusedFMManagedResources' - Indicates whether Firewall Manager should automatically remove
-- protections from resources that leave the policy scope and clean up
-- resources that Firewall Manager is managing for accounts when those
-- accounts leave policy scope. For example, Firewall Manager will
-- disassociate a Firewall Manager managed web ACL from a protected
-- customer resource when the customer resource leaves policy scope.
--
-- By default, Firewall Manager doesn\'t remove protections or delete
-- Firewall Manager managed resources.
--
-- This option is not available for Shield Advanced or WAF Classic
-- policies.
--
-- 'policyArn', 'policySummary_policyArn' - The Amazon Resource Name (ARN) of the specified policy.
--
-- 'policyId', 'policySummary_policyId' - The ID of the specified policy.
--
-- 'policyName', 'policySummary_policyName' - The name of the specified policy.
--
-- 'remediationEnabled', 'policySummary_remediationEnabled' - Indicates if the policy should be automatically applied to new
-- resources.
--
-- 'resourceType', 'policySummary_resourceType' - The type of resource protected by or in scope of the policy. This is in
-- the format shown in the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services Resource Types Reference>.
-- For WAF and Shield Advanced, examples include
-- @AWS::ElasticLoadBalancingV2::LoadBalancer@ and
-- @AWS::CloudFront::Distribution@. For a security group common policy,
-- valid values are @AWS::EC2::NetworkInterface@ and @AWS::EC2::Instance@.
-- For a security group content audit policy, valid values are
-- @AWS::EC2::SecurityGroup@, @AWS::EC2::NetworkInterface@, and
-- @AWS::EC2::Instance@. For a security group usage audit policy, the value
-- is @AWS::EC2::SecurityGroup@. For an Network Firewall policy or DNS
-- Firewall policy, the value is @AWS::EC2::VPC@.
--
-- 'securityServiceType', 'policySummary_securityServiceType' - The service that the policy is using to protect the resources. This
-- specifies the type of policy that is created, either an WAF policy, a
-- Shield Advanced policy, or a security group policy.
newPolicySummary ::
  PolicySummary
newPolicySummary =
  PolicySummary'
    { deleteUnusedFMManagedResources =
        Prelude.Nothing,
      policyArn = Prelude.Nothing,
      policyId = Prelude.Nothing,
      policyName = Prelude.Nothing,
      remediationEnabled = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      securityServiceType = Prelude.Nothing
    }

-- | Indicates whether Firewall Manager should automatically remove
-- protections from resources that leave the policy scope and clean up
-- resources that Firewall Manager is managing for accounts when those
-- accounts leave policy scope. For example, Firewall Manager will
-- disassociate a Firewall Manager managed web ACL from a protected
-- customer resource when the customer resource leaves policy scope.
--
-- By default, Firewall Manager doesn\'t remove protections or delete
-- Firewall Manager managed resources.
--
-- This option is not available for Shield Advanced or WAF Classic
-- policies.
policySummary_deleteUnusedFMManagedResources :: Lens.Lens' PolicySummary (Prelude.Maybe Prelude.Bool)
policySummary_deleteUnusedFMManagedResources = Lens.lens (\PolicySummary' {deleteUnusedFMManagedResources} -> deleteUnusedFMManagedResources) (\s@PolicySummary' {} a -> s {deleteUnusedFMManagedResources = a} :: PolicySummary)

-- | The Amazon Resource Name (ARN) of the specified policy.
policySummary_policyArn :: Lens.Lens' PolicySummary (Prelude.Maybe Prelude.Text)
policySummary_policyArn = Lens.lens (\PolicySummary' {policyArn} -> policyArn) (\s@PolicySummary' {} a -> s {policyArn = a} :: PolicySummary)

-- | The ID of the specified policy.
policySummary_policyId :: Lens.Lens' PolicySummary (Prelude.Maybe Prelude.Text)
policySummary_policyId = Lens.lens (\PolicySummary' {policyId} -> policyId) (\s@PolicySummary' {} a -> s {policyId = a} :: PolicySummary)

-- | The name of the specified policy.
policySummary_policyName :: Lens.Lens' PolicySummary (Prelude.Maybe Prelude.Text)
policySummary_policyName = Lens.lens (\PolicySummary' {policyName} -> policyName) (\s@PolicySummary' {} a -> s {policyName = a} :: PolicySummary)

-- | Indicates if the policy should be automatically applied to new
-- resources.
policySummary_remediationEnabled :: Lens.Lens' PolicySummary (Prelude.Maybe Prelude.Bool)
policySummary_remediationEnabled = Lens.lens (\PolicySummary' {remediationEnabled} -> remediationEnabled) (\s@PolicySummary' {} a -> s {remediationEnabled = a} :: PolicySummary)

-- | The type of resource protected by or in scope of the policy. This is in
-- the format shown in the
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-template-resource-type-ref.html Amazon Web Services Resource Types Reference>.
-- For WAF and Shield Advanced, examples include
-- @AWS::ElasticLoadBalancingV2::LoadBalancer@ and
-- @AWS::CloudFront::Distribution@. For a security group common policy,
-- valid values are @AWS::EC2::NetworkInterface@ and @AWS::EC2::Instance@.
-- For a security group content audit policy, valid values are
-- @AWS::EC2::SecurityGroup@, @AWS::EC2::NetworkInterface@, and
-- @AWS::EC2::Instance@. For a security group usage audit policy, the value
-- is @AWS::EC2::SecurityGroup@. For an Network Firewall policy or DNS
-- Firewall policy, the value is @AWS::EC2::VPC@.
policySummary_resourceType :: Lens.Lens' PolicySummary (Prelude.Maybe Prelude.Text)
policySummary_resourceType = Lens.lens (\PolicySummary' {resourceType} -> resourceType) (\s@PolicySummary' {} a -> s {resourceType = a} :: PolicySummary)

-- | The service that the policy is using to protect the resources. This
-- specifies the type of policy that is created, either an WAF policy, a
-- Shield Advanced policy, or a security group policy.
policySummary_securityServiceType :: Lens.Lens' PolicySummary (Prelude.Maybe SecurityServiceType)
policySummary_securityServiceType = Lens.lens (\PolicySummary' {securityServiceType} -> securityServiceType) (\s@PolicySummary' {} a -> s {securityServiceType = a} :: PolicySummary)

instance Data.FromJSON PolicySummary where
  parseJSON =
    Data.withObject
      "PolicySummary"
      ( \x ->
          PolicySummary'
            Prelude.<$> (x Data..:? "DeleteUnusedFMManagedResources")
            Prelude.<*> (x Data..:? "PolicyArn")
            Prelude.<*> (x Data..:? "PolicyId")
            Prelude.<*> (x Data..:? "PolicyName")
            Prelude.<*> (x Data..:? "RemediationEnabled")
            Prelude.<*> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "SecurityServiceType")
      )

instance Prelude.Hashable PolicySummary where
  hashWithSalt _salt PolicySummary' {..} =
    _salt
      `Prelude.hashWithSalt` deleteUnusedFMManagedResources
      `Prelude.hashWithSalt` policyArn
      `Prelude.hashWithSalt` policyId
      `Prelude.hashWithSalt` policyName
      `Prelude.hashWithSalt` remediationEnabled
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` securityServiceType

instance Prelude.NFData PolicySummary where
  rnf PolicySummary' {..} =
    Prelude.rnf deleteUnusedFMManagedResources
      `Prelude.seq` Prelude.rnf policyArn
      `Prelude.seq` Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf policyName
      `Prelude.seq` Prelude.rnf remediationEnabled
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf securityServiceType
