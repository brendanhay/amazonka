{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.FMS.Types.PolicyComplianceDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.PolicyComplianceDetail where

import Network.AWS.FMS.Types.ComplianceViolator
import Network.AWS.FMS.Types.DependentServiceName
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the noncompliant resources in a member account for a specific
-- AWS Firewall Manager policy. A maximum of 100 entries are displayed. If
-- more than 100 resources are noncompliant, @EvaluationLimitExceeded@ is
-- set to @True@.
--
-- /See:/ 'newPolicyComplianceDetail' smart constructor.
data PolicyComplianceDetail = PolicyComplianceDetail'
  { -- | An array of resources that aren\'t protected by the AWS WAF or Shield
    -- Advanced policy or that aren\'t in compliance with the security group
    -- policy.
    violators :: Prelude.Maybe [ComplianceViolator],
    -- | The AWS account that created the AWS Firewall Manager policy.
    policyOwner :: Prelude.Maybe Prelude.Text,
    -- | The AWS account ID.
    memberAccount :: Prelude.Maybe Prelude.Text,
    -- | Indicates if over 100 resources are noncompliant with the AWS Firewall
    -- Manager policy.
    evaluationLimitExceeded :: Prelude.Maybe Prelude.Bool,
    -- | Details about problems with dependent services, such as AWS WAF or AWS
    -- Config, that are causing a resource to be noncompliant. The details
    -- include the name of the dependent service and the error message received
    -- that indicates the problem with the service.
    issueInfoMap :: Prelude.Maybe (Prelude.HashMap DependentServiceName Prelude.Text),
    -- | The ID of the AWS Firewall Manager policy.
    policyId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that indicates when the returned information should be
    -- considered out of date.
    expiredAt :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PolicyComplianceDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'violators', 'policyComplianceDetail_violators' - An array of resources that aren\'t protected by the AWS WAF or Shield
-- Advanced policy or that aren\'t in compliance with the security group
-- policy.
--
-- 'policyOwner', 'policyComplianceDetail_policyOwner' - The AWS account that created the AWS Firewall Manager policy.
--
-- 'memberAccount', 'policyComplianceDetail_memberAccount' - The AWS account ID.
--
-- 'evaluationLimitExceeded', 'policyComplianceDetail_evaluationLimitExceeded' - Indicates if over 100 resources are noncompliant with the AWS Firewall
-- Manager policy.
--
-- 'issueInfoMap', 'policyComplianceDetail_issueInfoMap' - Details about problems with dependent services, such as AWS WAF or AWS
-- Config, that are causing a resource to be noncompliant. The details
-- include the name of the dependent service and the error message received
-- that indicates the problem with the service.
--
-- 'policyId', 'policyComplianceDetail_policyId' - The ID of the AWS Firewall Manager policy.
--
-- 'expiredAt', 'policyComplianceDetail_expiredAt' - A timestamp that indicates when the returned information should be
-- considered out of date.
newPolicyComplianceDetail ::
  PolicyComplianceDetail
newPolicyComplianceDetail =
  PolicyComplianceDetail'
    { violators =
        Prelude.Nothing,
      policyOwner = Prelude.Nothing,
      memberAccount = Prelude.Nothing,
      evaluationLimitExceeded = Prelude.Nothing,
      issueInfoMap = Prelude.Nothing,
      policyId = Prelude.Nothing,
      expiredAt = Prelude.Nothing
    }

-- | An array of resources that aren\'t protected by the AWS WAF or Shield
-- Advanced policy or that aren\'t in compliance with the security group
-- policy.
policyComplianceDetail_violators :: Lens.Lens' PolicyComplianceDetail (Prelude.Maybe [ComplianceViolator])
policyComplianceDetail_violators = Lens.lens (\PolicyComplianceDetail' {violators} -> violators) (\s@PolicyComplianceDetail' {} a -> s {violators = a} :: PolicyComplianceDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | The AWS account that created the AWS Firewall Manager policy.
policyComplianceDetail_policyOwner :: Lens.Lens' PolicyComplianceDetail (Prelude.Maybe Prelude.Text)
policyComplianceDetail_policyOwner = Lens.lens (\PolicyComplianceDetail' {policyOwner} -> policyOwner) (\s@PolicyComplianceDetail' {} a -> s {policyOwner = a} :: PolicyComplianceDetail)

-- | The AWS account ID.
policyComplianceDetail_memberAccount :: Lens.Lens' PolicyComplianceDetail (Prelude.Maybe Prelude.Text)
policyComplianceDetail_memberAccount = Lens.lens (\PolicyComplianceDetail' {memberAccount} -> memberAccount) (\s@PolicyComplianceDetail' {} a -> s {memberAccount = a} :: PolicyComplianceDetail)

-- | Indicates if over 100 resources are noncompliant with the AWS Firewall
-- Manager policy.
policyComplianceDetail_evaluationLimitExceeded :: Lens.Lens' PolicyComplianceDetail (Prelude.Maybe Prelude.Bool)
policyComplianceDetail_evaluationLimitExceeded = Lens.lens (\PolicyComplianceDetail' {evaluationLimitExceeded} -> evaluationLimitExceeded) (\s@PolicyComplianceDetail' {} a -> s {evaluationLimitExceeded = a} :: PolicyComplianceDetail)

-- | Details about problems with dependent services, such as AWS WAF or AWS
-- Config, that are causing a resource to be noncompliant. The details
-- include the name of the dependent service and the error message received
-- that indicates the problem with the service.
policyComplianceDetail_issueInfoMap :: Lens.Lens' PolicyComplianceDetail (Prelude.Maybe (Prelude.HashMap DependentServiceName Prelude.Text))
policyComplianceDetail_issueInfoMap = Lens.lens (\PolicyComplianceDetail' {issueInfoMap} -> issueInfoMap) (\s@PolicyComplianceDetail' {} a -> s {issueInfoMap = a} :: PolicyComplianceDetail) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the AWS Firewall Manager policy.
policyComplianceDetail_policyId :: Lens.Lens' PolicyComplianceDetail (Prelude.Maybe Prelude.Text)
policyComplianceDetail_policyId = Lens.lens (\PolicyComplianceDetail' {policyId} -> policyId) (\s@PolicyComplianceDetail' {} a -> s {policyId = a} :: PolicyComplianceDetail)

-- | A timestamp that indicates when the returned information should be
-- considered out of date.
policyComplianceDetail_expiredAt :: Lens.Lens' PolicyComplianceDetail (Prelude.Maybe Prelude.UTCTime)
policyComplianceDetail_expiredAt = Lens.lens (\PolicyComplianceDetail' {expiredAt} -> expiredAt) (\s@PolicyComplianceDetail' {} a -> s {expiredAt = a} :: PolicyComplianceDetail) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON PolicyComplianceDetail where
  parseJSON =
    Prelude.withObject
      "PolicyComplianceDetail"
      ( \x ->
          PolicyComplianceDetail'
            Prelude.<$> ( x Prelude..:? "Violators"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "PolicyOwner")
            Prelude.<*> (x Prelude..:? "MemberAccount")
            Prelude.<*> (x Prelude..:? "EvaluationLimitExceeded")
            Prelude.<*> ( x Prelude..:? "IssueInfoMap"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "PolicyId")
            Prelude.<*> (x Prelude..:? "ExpiredAt")
      )

instance Prelude.Hashable PolicyComplianceDetail

instance Prelude.NFData PolicyComplianceDetail
