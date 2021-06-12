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
-- Module      : Network.AWS.FMS.Types.Policy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.Policy where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types.CustomerPolicyScopeIdType
import Network.AWS.FMS.Types.ResourceTag
import Network.AWS.FMS.Types.SecurityServicePolicyData
import qualified Network.AWS.Lens as Lens

-- | An AWS Firewall Manager policy.
--
-- /See:/ 'newPolicy' smart constructor.
data Policy = Policy'
  { -- | An array of @ResourceTag@ objects.
    resourceTags :: Core.Maybe [ResourceTag],
    -- | An array of @ResourceType@.
    resourceTypeList :: Core.Maybe [Core.Text],
    -- | A unique identifier for each update to the policy. When issuing a
    -- @PutPolicy@ request, the @PolicyUpdateToken@ in the request must match
    -- the @PolicyUpdateToken@ of the current policy version. To get the
    -- @PolicyUpdateToken@ of the current policy version, use a @GetPolicy@
    -- request.
    policyUpdateToken :: Core.Maybe Core.Text,
    -- | Specifies the AWS account IDs and AWS Organizations organizational units
    -- (OUs) to include in the policy. Specifying an OU is the equivalent of
    -- specifying all accounts in the OU and in any of its child OUs, including
    -- any child OUs and accounts that are added at a later time.
    --
    -- You can specify inclusions or exclusions, but not both. If you specify
    -- an @IncludeMap@, AWS Firewall Manager applies the policy to all accounts
    -- specified by the @IncludeMap@, and does not evaluate any @ExcludeMap@
    -- specifications. If you do not specify an @IncludeMap@, then Firewall
    -- Manager applies the policy to all accounts except for those specified by
    -- the @ExcludeMap@.
    --
    -- You can specify account IDs, OUs, or a combination:
    --
    -- -   Specify account IDs by setting the key to @ACCOUNT@. For example,
    --     the following is a valid map:
    --     @{“ACCOUNT” : [“accountID1”, “accountID2”]}@.
    --
    -- -   Specify OUs by setting the key to @ORG_UNIT@. For example, the
    --     following is a valid map: @{“ORG_UNIT” : [“ouid111”, “ouid112”]}@.
    --
    -- -   Specify accounts and OUs together in a single map, separated with a
    --     comma. For example, the following is a valid map:
    --     @{“ACCOUNT” : [“accountID1”, “accountID2”], “ORG_UNIT” : [“ouid111”, “ouid112”]}@.
    includeMap :: Core.Maybe (Core.HashMap CustomerPolicyScopeIdType [Core.Text]),
    -- | The ID of the AWS Firewall Manager policy.
    policyId :: Core.Maybe Core.Text,
    -- | Specifies the AWS account IDs and AWS Organizations organizational units
    -- (OUs) to exclude from the policy. Specifying an OU is the equivalent of
    -- specifying all accounts in the OU and in any of its child OUs, including
    -- any child OUs and accounts that are added at a later time.
    --
    -- You can specify inclusions or exclusions, but not both. If you specify
    -- an @IncludeMap@, AWS Firewall Manager applies the policy to all accounts
    -- specified by the @IncludeMap@, and does not evaluate any @ExcludeMap@
    -- specifications. If you do not specify an @IncludeMap@, then Firewall
    -- Manager applies the policy to all accounts except for those specified by
    -- the @ExcludeMap@.
    --
    -- You can specify account IDs, OUs, or a combination:
    --
    -- -   Specify account IDs by setting the key to @ACCOUNT@. For example,
    --     the following is a valid map:
    --     @{“ACCOUNT” : [“accountID1”, “accountID2”]}@.
    --
    -- -   Specify OUs by setting the key to @ORG_UNIT@. For example, the
    --     following is a valid map: @{“ORG_UNIT” : [“ouid111”, “ouid112”]}@.
    --
    -- -   Specify accounts and OUs together in a single map, separated with a
    --     comma. For example, the following is a valid map:
    --     @{“ACCOUNT” : [“accountID1”, “accountID2”], “ORG_UNIT” : [“ouid111”, “ouid112”]}@.
    excludeMap :: Core.Maybe (Core.HashMap CustomerPolicyScopeIdType [Core.Text]),
    -- | The name of the AWS Firewall Manager policy.
    policyName :: Core.Text,
    -- | Details about the security service that is being used to protect the
    -- resources.
    securityServicePolicyData :: SecurityServicePolicyData,
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
    resourceType :: Core.Text,
    -- | If set to @True@, resources with the tags that are specified in the
    -- @ResourceTag@ array are not in scope of the policy. If set to @False@,
    -- and the @ResourceTag@ array is not null, only resources with the
    -- specified tags are in scope of the policy.
    excludeResourceTags :: Core.Bool,
    -- | Indicates if the policy should be automatically applied to new
    -- resources.
    remediationEnabled :: Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Policy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceTags', 'policy_resourceTags' - An array of @ResourceTag@ objects.
--
-- 'resourceTypeList', 'policy_resourceTypeList' - An array of @ResourceType@.
--
-- 'policyUpdateToken', 'policy_policyUpdateToken' - A unique identifier for each update to the policy. When issuing a
-- @PutPolicy@ request, the @PolicyUpdateToken@ in the request must match
-- the @PolicyUpdateToken@ of the current policy version. To get the
-- @PolicyUpdateToken@ of the current policy version, use a @GetPolicy@
-- request.
--
-- 'includeMap', 'policy_includeMap' - Specifies the AWS account IDs and AWS Organizations organizational units
-- (OUs) to include in the policy. Specifying an OU is the equivalent of
-- specifying all accounts in the OU and in any of its child OUs, including
-- any child OUs and accounts that are added at a later time.
--
-- You can specify inclusions or exclusions, but not both. If you specify
-- an @IncludeMap@, AWS Firewall Manager applies the policy to all accounts
-- specified by the @IncludeMap@, and does not evaluate any @ExcludeMap@
-- specifications. If you do not specify an @IncludeMap@, then Firewall
-- Manager applies the policy to all accounts except for those specified by
-- the @ExcludeMap@.
--
-- You can specify account IDs, OUs, or a combination:
--
-- -   Specify account IDs by setting the key to @ACCOUNT@. For example,
--     the following is a valid map:
--     @{“ACCOUNT” : [“accountID1”, “accountID2”]}@.
--
-- -   Specify OUs by setting the key to @ORG_UNIT@. For example, the
--     following is a valid map: @{“ORG_UNIT” : [“ouid111”, “ouid112”]}@.
--
-- -   Specify accounts and OUs together in a single map, separated with a
--     comma. For example, the following is a valid map:
--     @{“ACCOUNT” : [“accountID1”, “accountID2”], “ORG_UNIT” : [“ouid111”, “ouid112”]}@.
--
-- 'policyId', 'policy_policyId' - The ID of the AWS Firewall Manager policy.
--
-- 'excludeMap', 'policy_excludeMap' - Specifies the AWS account IDs and AWS Organizations organizational units
-- (OUs) to exclude from the policy. Specifying an OU is the equivalent of
-- specifying all accounts in the OU and in any of its child OUs, including
-- any child OUs and accounts that are added at a later time.
--
-- You can specify inclusions or exclusions, but not both. If you specify
-- an @IncludeMap@, AWS Firewall Manager applies the policy to all accounts
-- specified by the @IncludeMap@, and does not evaluate any @ExcludeMap@
-- specifications. If you do not specify an @IncludeMap@, then Firewall
-- Manager applies the policy to all accounts except for those specified by
-- the @ExcludeMap@.
--
-- You can specify account IDs, OUs, or a combination:
--
-- -   Specify account IDs by setting the key to @ACCOUNT@. For example,
--     the following is a valid map:
--     @{“ACCOUNT” : [“accountID1”, “accountID2”]}@.
--
-- -   Specify OUs by setting the key to @ORG_UNIT@. For example, the
--     following is a valid map: @{“ORG_UNIT” : [“ouid111”, “ouid112”]}@.
--
-- -   Specify accounts and OUs together in a single map, separated with a
--     comma. For example, the following is a valid map:
--     @{“ACCOUNT” : [“accountID1”, “accountID2”], “ORG_UNIT” : [“ouid111”, “ouid112”]}@.
--
-- 'policyName', 'policy_policyName' - The name of the AWS Firewall Manager policy.
--
-- 'securityServicePolicyData', 'policy_securityServicePolicyData' - Details about the security service that is being used to protect the
-- resources.
--
-- 'resourceType', 'policy_resourceType' - The type of resource protected by or in scope of the policy. This is in
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
-- 'excludeResourceTags', 'policy_excludeResourceTags' - If set to @True@, resources with the tags that are specified in the
-- @ResourceTag@ array are not in scope of the policy. If set to @False@,
-- and the @ResourceTag@ array is not null, only resources with the
-- specified tags are in scope of the policy.
--
-- 'remediationEnabled', 'policy_remediationEnabled' - Indicates if the policy should be automatically applied to new
-- resources.
newPolicy ::
  -- | 'policyName'
  Core.Text ->
  -- | 'securityServicePolicyData'
  SecurityServicePolicyData ->
  -- | 'resourceType'
  Core.Text ->
  -- | 'excludeResourceTags'
  Core.Bool ->
  -- | 'remediationEnabled'
  Core.Bool ->
  Policy
newPolicy
  pPolicyName_
  pSecurityServicePolicyData_
  pResourceType_
  pExcludeResourceTags_
  pRemediationEnabled_ =
    Policy'
      { resourceTags = Core.Nothing,
        resourceTypeList = Core.Nothing,
        policyUpdateToken = Core.Nothing,
        includeMap = Core.Nothing,
        policyId = Core.Nothing,
        excludeMap = Core.Nothing,
        policyName = pPolicyName_,
        securityServicePolicyData =
          pSecurityServicePolicyData_,
        resourceType = pResourceType_,
        excludeResourceTags = pExcludeResourceTags_,
        remediationEnabled = pRemediationEnabled_
      }

-- | An array of @ResourceTag@ objects.
policy_resourceTags :: Lens.Lens' Policy (Core.Maybe [ResourceTag])
policy_resourceTags = Lens.lens (\Policy' {resourceTags} -> resourceTags) (\s@Policy' {} a -> s {resourceTags = a} :: Policy) Core.. Lens.mapping Lens._Coerce

-- | An array of @ResourceType@.
policy_resourceTypeList :: Lens.Lens' Policy (Core.Maybe [Core.Text])
policy_resourceTypeList = Lens.lens (\Policy' {resourceTypeList} -> resourceTypeList) (\s@Policy' {} a -> s {resourceTypeList = a} :: Policy) Core.. Lens.mapping Lens._Coerce

-- | A unique identifier for each update to the policy. When issuing a
-- @PutPolicy@ request, the @PolicyUpdateToken@ in the request must match
-- the @PolicyUpdateToken@ of the current policy version. To get the
-- @PolicyUpdateToken@ of the current policy version, use a @GetPolicy@
-- request.
policy_policyUpdateToken :: Lens.Lens' Policy (Core.Maybe Core.Text)
policy_policyUpdateToken = Lens.lens (\Policy' {policyUpdateToken} -> policyUpdateToken) (\s@Policy' {} a -> s {policyUpdateToken = a} :: Policy)

-- | Specifies the AWS account IDs and AWS Organizations organizational units
-- (OUs) to include in the policy. Specifying an OU is the equivalent of
-- specifying all accounts in the OU and in any of its child OUs, including
-- any child OUs and accounts that are added at a later time.
--
-- You can specify inclusions or exclusions, but not both. If you specify
-- an @IncludeMap@, AWS Firewall Manager applies the policy to all accounts
-- specified by the @IncludeMap@, and does not evaluate any @ExcludeMap@
-- specifications. If you do not specify an @IncludeMap@, then Firewall
-- Manager applies the policy to all accounts except for those specified by
-- the @ExcludeMap@.
--
-- You can specify account IDs, OUs, or a combination:
--
-- -   Specify account IDs by setting the key to @ACCOUNT@. For example,
--     the following is a valid map:
--     @{“ACCOUNT” : [“accountID1”, “accountID2”]}@.
--
-- -   Specify OUs by setting the key to @ORG_UNIT@. For example, the
--     following is a valid map: @{“ORG_UNIT” : [“ouid111”, “ouid112”]}@.
--
-- -   Specify accounts and OUs together in a single map, separated with a
--     comma. For example, the following is a valid map:
--     @{“ACCOUNT” : [“accountID1”, “accountID2”], “ORG_UNIT” : [“ouid111”, “ouid112”]}@.
policy_includeMap :: Lens.Lens' Policy (Core.Maybe (Core.HashMap CustomerPolicyScopeIdType [Core.Text]))
policy_includeMap = Lens.lens (\Policy' {includeMap} -> includeMap) (\s@Policy' {} a -> s {includeMap = a} :: Policy) Core.. Lens.mapping Lens._Coerce

-- | The ID of the AWS Firewall Manager policy.
policy_policyId :: Lens.Lens' Policy (Core.Maybe Core.Text)
policy_policyId = Lens.lens (\Policy' {policyId} -> policyId) (\s@Policy' {} a -> s {policyId = a} :: Policy)

-- | Specifies the AWS account IDs and AWS Organizations organizational units
-- (OUs) to exclude from the policy. Specifying an OU is the equivalent of
-- specifying all accounts in the OU and in any of its child OUs, including
-- any child OUs and accounts that are added at a later time.
--
-- You can specify inclusions or exclusions, but not both. If you specify
-- an @IncludeMap@, AWS Firewall Manager applies the policy to all accounts
-- specified by the @IncludeMap@, and does not evaluate any @ExcludeMap@
-- specifications. If you do not specify an @IncludeMap@, then Firewall
-- Manager applies the policy to all accounts except for those specified by
-- the @ExcludeMap@.
--
-- You can specify account IDs, OUs, or a combination:
--
-- -   Specify account IDs by setting the key to @ACCOUNT@. For example,
--     the following is a valid map:
--     @{“ACCOUNT” : [“accountID1”, “accountID2”]}@.
--
-- -   Specify OUs by setting the key to @ORG_UNIT@. For example, the
--     following is a valid map: @{“ORG_UNIT” : [“ouid111”, “ouid112”]}@.
--
-- -   Specify accounts and OUs together in a single map, separated with a
--     comma. For example, the following is a valid map:
--     @{“ACCOUNT” : [“accountID1”, “accountID2”], “ORG_UNIT” : [“ouid111”, “ouid112”]}@.
policy_excludeMap :: Lens.Lens' Policy (Core.Maybe (Core.HashMap CustomerPolicyScopeIdType [Core.Text]))
policy_excludeMap = Lens.lens (\Policy' {excludeMap} -> excludeMap) (\s@Policy' {} a -> s {excludeMap = a} :: Policy) Core.. Lens.mapping Lens._Coerce

-- | The name of the AWS Firewall Manager policy.
policy_policyName :: Lens.Lens' Policy Core.Text
policy_policyName = Lens.lens (\Policy' {policyName} -> policyName) (\s@Policy' {} a -> s {policyName = a} :: Policy)

-- | Details about the security service that is being used to protect the
-- resources.
policy_securityServicePolicyData :: Lens.Lens' Policy SecurityServicePolicyData
policy_securityServicePolicyData = Lens.lens (\Policy' {securityServicePolicyData} -> securityServicePolicyData) (\s@Policy' {} a -> s {securityServicePolicyData = a} :: Policy)

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
policy_resourceType :: Lens.Lens' Policy Core.Text
policy_resourceType = Lens.lens (\Policy' {resourceType} -> resourceType) (\s@Policy' {} a -> s {resourceType = a} :: Policy)

-- | If set to @True@, resources with the tags that are specified in the
-- @ResourceTag@ array are not in scope of the policy. If set to @False@,
-- and the @ResourceTag@ array is not null, only resources with the
-- specified tags are in scope of the policy.
policy_excludeResourceTags :: Lens.Lens' Policy Core.Bool
policy_excludeResourceTags = Lens.lens (\Policy' {excludeResourceTags} -> excludeResourceTags) (\s@Policy' {} a -> s {excludeResourceTags = a} :: Policy)

-- | Indicates if the policy should be automatically applied to new
-- resources.
policy_remediationEnabled :: Lens.Lens' Policy Core.Bool
policy_remediationEnabled = Lens.lens (\Policy' {remediationEnabled} -> remediationEnabled) (\s@Policy' {} a -> s {remediationEnabled = a} :: Policy)

instance Core.FromJSON Policy where
  parseJSON =
    Core.withObject
      "Policy"
      ( \x ->
          Policy'
            Core.<$> (x Core..:? "ResourceTags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "ResourceTypeList" Core..!= Core.mempty)
            Core.<*> (x Core..:? "PolicyUpdateToken")
            Core.<*> (x Core..:? "IncludeMap" Core..!= Core.mempty)
            Core.<*> (x Core..:? "PolicyId")
            Core.<*> (x Core..:? "ExcludeMap" Core..!= Core.mempty)
            Core.<*> (x Core..: "PolicyName")
            Core.<*> (x Core..: "SecurityServicePolicyData")
            Core.<*> (x Core..: "ResourceType")
            Core.<*> (x Core..: "ExcludeResourceTags")
            Core.<*> (x Core..: "RemediationEnabled")
      )

instance Core.Hashable Policy

instance Core.NFData Policy

instance Core.ToJSON Policy where
  toJSON Policy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ResourceTags" Core..=) Core.<$> resourceTags,
            ("ResourceTypeList" Core..=)
              Core.<$> resourceTypeList,
            ("PolicyUpdateToken" Core..=)
              Core.<$> policyUpdateToken,
            ("IncludeMap" Core..=) Core.<$> includeMap,
            ("PolicyId" Core..=) Core.<$> policyId,
            ("ExcludeMap" Core..=) Core.<$> excludeMap,
            Core.Just ("PolicyName" Core..= policyName),
            Core.Just
              ( "SecurityServicePolicyData"
                  Core..= securityServicePolicyData
              ),
            Core.Just ("ResourceType" Core..= resourceType),
            Core.Just
              ("ExcludeResourceTags" Core..= excludeResourceTags),
            Core.Just
              ("RemediationEnabled" Core..= remediationEnabled)
          ]
      )
