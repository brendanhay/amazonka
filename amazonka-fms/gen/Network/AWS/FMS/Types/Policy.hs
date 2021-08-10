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
import qualified Network.AWS.Prelude as Prelude

-- | An AWS Firewall Manager policy.
--
-- /See:/ 'newPolicy' smart constructor.
data Policy = Policy'
  { -- | An array of @ResourceTag@ objects.
    resourceTags :: Prelude.Maybe [ResourceTag],
    -- | An array of @ResourceType@.
    resourceTypeList :: Prelude.Maybe [Prelude.Text],
    -- | A unique identifier for each update to the policy. When issuing a
    -- @PutPolicy@ request, the @PolicyUpdateToken@ in the request must match
    -- the @PolicyUpdateToken@ of the current policy version. To get the
    -- @PolicyUpdateToken@ of the current policy version, use a @GetPolicy@
    -- request.
    policyUpdateToken :: Prelude.Maybe Prelude.Text,
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
    includeMap :: Prelude.Maybe (Prelude.HashMap CustomerPolicyScopeIdType [Prelude.Text]),
    -- | The ID of the AWS Firewall Manager policy.
    policyId :: Prelude.Maybe Prelude.Text,
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
    excludeMap :: Prelude.Maybe (Prelude.HashMap CustomerPolicyScopeIdType [Prelude.Text]),
    -- | The name of the AWS Firewall Manager policy.
    policyName :: Prelude.Text,
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
    resourceType :: Prelude.Text,
    -- | If set to @True@, resources with the tags that are specified in the
    -- @ResourceTag@ array are not in scope of the policy. If set to @False@,
    -- and the @ResourceTag@ array is not null, only resources with the
    -- specified tags are in scope of the policy.
    excludeResourceTags :: Prelude.Bool,
    -- | Indicates if the policy should be automatically applied to new
    -- resources.
    remediationEnabled :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'securityServicePolicyData'
  SecurityServicePolicyData ->
  -- | 'resourceType'
  Prelude.Text ->
  -- | 'excludeResourceTags'
  Prelude.Bool ->
  -- | 'remediationEnabled'
  Prelude.Bool ->
  Policy
newPolicy
  pPolicyName_
  pSecurityServicePolicyData_
  pResourceType_
  pExcludeResourceTags_
  pRemediationEnabled_ =
    Policy'
      { resourceTags = Prelude.Nothing,
        resourceTypeList = Prelude.Nothing,
        policyUpdateToken = Prelude.Nothing,
        includeMap = Prelude.Nothing,
        policyId = Prelude.Nothing,
        excludeMap = Prelude.Nothing,
        policyName = pPolicyName_,
        securityServicePolicyData =
          pSecurityServicePolicyData_,
        resourceType = pResourceType_,
        excludeResourceTags = pExcludeResourceTags_,
        remediationEnabled = pRemediationEnabled_
      }

-- | An array of @ResourceTag@ objects.
policy_resourceTags :: Lens.Lens' Policy (Prelude.Maybe [ResourceTag])
policy_resourceTags = Lens.lens (\Policy' {resourceTags} -> resourceTags) (\s@Policy' {} a -> s {resourceTags = a} :: Policy) Prelude.. Lens.mapping Lens._Coerce

-- | An array of @ResourceType@.
policy_resourceTypeList :: Lens.Lens' Policy (Prelude.Maybe [Prelude.Text])
policy_resourceTypeList = Lens.lens (\Policy' {resourceTypeList} -> resourceTypeList) (\s@Policy' {} a -> s {resourceTypeList = a} :: Policy) Prelude.. Lens.mapping Lens._Coerce

-- | A unique identifier for each update to the policy. When issuing a
-- @PutPolicy@ request, the @PolicyUpdateToken@ in the request must match
-- the @PolicyUpdateToken@ of the current policy version. To get the
-- @PolicyUpdateToken@ of the current policy version, use a @GetPolicy@
-- request.
policy_policyUpdateToken :: Lens.Lens' Policy (Prelude.Maybe Prelude.Text)
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
policy_includeMap :: Lens.Lens' Policy (Prelude.Maybe (Prelude.HashMap CustomerPolicyScopeIdType [Prelude.Text]))
policy_includeMap = Lens.lens (\Policy' {includeMap} -> includeMap) (\s@Policy' {} a -> s {includeMap = a} :: Policy) Prelude.. Lens.mapping Lens._Coerce

-- | The ID of the AWS Firewall Manager policy.
policy_policyId :: Lens.Lens' Policy (Prelude.Maybe Prelude.Text)
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
policy_excludeMap :: Lens.Lens' Policy (Prelude.Maybe (Prelude.HashMap CustomerPolicyScopeIdType [Prelude.Text]))
policy_excludeMap = Lens.lens (\Policy' {excludeMap} -> excludeMap) (\s@Policy' {} a -> s {excludeMap = a} :: Policy) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the AWS Firewall Manager policy.
policy_policyName :: Lens.Lens' Policy Prelude.Text
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
policy_resourceType :: Lens.Lens' Policy Prelude.Text
policy_resourceType = Lens.lens (\Policy' {resourceType} -> resourceType) (\s@Policy' {} a -> s {resourceType = a} :: Policy)

-- | If set to @True@, resources with the tags that are specified in the
-- @ResourceTag@ array are not in scope of the policy. If set to @False@,
-- and the @ResourceTag@ array is not null, only resources with the
-- specified tags are in scope of the policy.
policy_excludeResourceTags :: Lens.Lens' Policy Prelude.Bool
policy_excludeResourceTags = Lens.lens (\Policy' {excludeResourceTags} -> excludeResourceTags) (\s@Policy' {} a -> s {excludeResourceTags = a} :: Policy)

-- | Indicates if the policy should be automatically applied to new
-- resources.
policy_remediationEnabled :: Lens.Lens' Policy Prelude.Bool
policy_remediationEnabled = Lens.lens (\Policy' {remediationEnabled} -> remediationEnabled) (\s@Policy' {} a -> s {remediationEnabled = a} :: Policy)

instance Core.FromJSON Policy where
  parseJSON =
    Core.withObject
      "Policy"
      ( \x ->
          Policy'
            Prelude.<$> (x Core..:? "ResourceTags" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "ResourceTypeList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "PolicyUpdateToken")
            Prelude.<*> (x Core..:? "IncludeMap" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "PolicyId")
            Prelude.<*> (x Core..:? "ExcludeMap" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "PolicyName")
            Prelude.<*> (x Core..: "SecurityServicePolicyData")
            Prelude.<*> (x Core..: "ResourceType")
            Prelude.<*> (x Core..: "ExcludeResourceTags")
            Prelude.<*> (x Core..: "RemediationEnabled")
      )

instance Prelude.Hashable Policy

instance Prelude.NFData Policy

instance Core.ToJSON Policy where
  toJSON Policy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ResourceTags" Core..=) Prelude.<$> resourceTags,
            ("ResourceTypeList" Core..=)
              Prelude.<$> resourceTypeList,
            ("PolicyUpdateToken" Core..=)
              Prelude.<$> policyUpdateToken,
            ("IncludeMap" Core..=) Prelude.<$> includeMap,
            ("PolicyId" Core..=) Prelude.<$> policyId,
            ("ExcludeMap" Core..=) Prelude.<$> excludeMap,
            Prelude.Just ("PolicyName" Core..= policyName),
            Prelude.Just
              ( "SecurityServicePolicyData"
                  Core..= securityServicePolicyData
              ),
            Prelude.Just ("ResourceType" Core..= resourceType),
            Prelude.Just
              ("ExcludeResourceTags" Core..= excludeResourceTags),
            Prelude.Just
              ("RemediationEnabled" Core..= remediationEnabled)
          ]
      )
