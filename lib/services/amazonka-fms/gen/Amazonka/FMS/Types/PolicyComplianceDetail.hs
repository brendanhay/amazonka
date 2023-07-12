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
-- Module      : Amazonka.FMS.Types.PolicyComplianceDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.PolicyComplianceDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types.ComplianceViolator
import Amazonka.FMS.Types.DependentServiceName
import qualified Amazonka.Prelude as Prelude

-- | Describes the noncompliant resources in a member account for a specific
-- Firewall Manager policy. A maximum of 100 entries are displayed. If more
-- than 100 resources are noncompliant, @EvaluationLimitExceeded@ is set to
-- @True@.
--
-- /See:/ 'newPolicyComplianceDetail' smart constructor.
data PolicyComplianceDetail = PolicyComplianceDetail'
  { -- | Indicates if over 100 resources are noncompliant with the Firewall
    -- Manager policy.
    evaluationLimitExceeded :: Prelude.Maybe Prelude.Bool,
    -- | A timestamp that indicates when the returned information should be
    -- considered out of date.
    expiredAt :: Prelude.Maybe Data.POSIX,
    -- | Details about problems with dependent services, such as WAF or Config,
    -- and the error message received that indicates the problem with the
    -- service.
    issueInfoMap :: Prelude.Maybe (Prelude.HashMap DependentServiceName Prelude.Text),
    -- | The Amazon Web Services account ID.
    memberAccount :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Firewall Manager policy.
    policyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account that created the Firewall Manager
    -- policy.
    policyOwner :: Prelude.Maybe Prelude.Text,
    -- | An array of resources that aren\'t protected by the WAF or Shield
    -- Advanced policy or that aren\'t in compliance with the security group
    -- policy.
    violators :: Prelude.Maybe [ComplianceViolator]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyComplianceDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationLimitExceeded', 'policyComplianceDetail_evaluationLimitExceeded' - Indicates if over 100 resources are noncompliant with the Firewall
-- Manager policy.
--
-- 'expiredAt', 'policyComplianceDetail_expiredAt' - A timestamp that indicates when the returned information should be
-- considered out of date.
--
-- 'issueInfoMap', 'policyComplianceDetail_issueInfoMap' - Details about problems with dependent services, such as WAF or Config,
-- and the error message received that indicates the problem with the
-- service.
--
-- 'memberAccount', 'policyComplianceDetail_memberAccount' - The Amazon Web Services account ID.
--
-- 'policyId', 'policyComplianceDetail_policyId' - The ID of the Firewall Manager policy.
--
-- 'policyOwner', 'policyComplianceDetail_policyOwner' - The Amazon Web Services account that created the Firewall Manager
-- policy.
--
-- 'violators', 'policyComplianceDetail_violators' - An array of resources that aren\'t protected by the WAF or Shield
-- Advanced policy or that aren\'t in compliance with the security group
-- policy.
newPolicyComplianceDetail ::
  PolicyComplianceDetail
newPolicyComplianceDetail =
  PolicyComplianceDetail'
    { evaluationLimitExceeded =
        Prelude.Nothing,
      expiredAt = Prelude.Nothing,
      issueInfoMap = Prelude.Nothing,
      memberAccount = Prelude.Nothing,
      policyId = Prelude.Nothing,
      policyOwner = Prelude.Nothing,
      violators = Prelude.Nothing
    }

-- | Indicates if over 100 resources are noncompliant with the Firewall
-- Manager policy.
policyComplianceDetail_evaluationLimitExceeded :: Lens.Lens' PolicyComplianceDetail (Prelude.Maybe Prelude.Bool)
policyComplianceDetail_evaluationLimitExceeded = Lens.lens (\PolicyComplianceDetail' {evaluationLimitExceeded} -> evaluationLimitExceeded) (\s@PolicyComplianceDetail' {} a -> s {evaluationLimitExceeded = a} :: PolicyComplianceDetail)

-- | A timestamp that indicates when the returned information should be
-- considered out of date.
policyComplianceDetail_expiredAt :: Lens.Lens' PolicyComplianceDetail (Prelude.Maybe Prelude.UTCTime)
policyComplianceDetail_expiredAt = Lens.lens (\PolicyComplianceDetail' {expiredAt} -> expiredAt) (\s@PolicyComplianceDetail' {} a -> s {expiredAt = a} :: PolicyComplianceDetail) Prelude.. Lens.mapping Data._Time

-- | Details about problems with dependent services, such as WAF or Config,
-- and the error message received that indicates the problem with the
-- service.
policyComplianceDetail_issueInfoMap :: Lens.Lens' PolicyComplianceDetail (Prelude.Maybe (Prelude.HashMap DependentServiceName Prelude.Text))
policyComplianceDetail_issueInfoMap = Lens.lens (\PolicyComplianceDetail' {issueInfoMap} -> issueInfoMap) (\s@PolicyComplianceDetail' {} a -> s {issueInfoMap = a} :: PolicyComplianceDetail) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account ID.
policyComplianceDetail_memberAccount :: Lens.Lens' PolicyComplianceDetail (Prelude.Maybe Prelude.Text)
policyComplianceDetail_memberAccount = Lens.lens (\PolicyComplianceDetail' {memberAccount} -> memberAccount) (\s@PolicyComplianceDetail' {} a -> s {memberAccount = a} :: PolicyComplianceDetail)

-- | The ID of the Firewall Manager policy.
policyComplianceDetail_policyId :: Lens.Lens' PolicyComplianceDetail (Prelude.Maybe Prelude.Text)
policyComplianceDetail_policyId = Lens.lens (\PolicyComplianceDetail' {policyId} -> policyId) (\s@PolicyComplianceDetail' {} a -> s {policyId = a} :: PolicyComplianceDetail)

-- | The Amazon Web Services account that created the Firewall Manager
-- policy.
policyComplianceDetail_policyOwner :: Lens.Lens' PolicyComplianceDetail (Prelude.Maybe Prelude.Text)
policyComplianceDetail_policyOwner = Lens.lens (\PolicyComplianceDetail' {policyOwner} -> policyOwner) (\s@PolicyComplianceDetail' {} a -> s {policyOwner = a} :: PolicyComplianceDetail)

-- | An array of resources that aren\'t protected by the WAF or Shield
-- Advanced policy or that aren\'t in compliance with the security group
-- policy.
policyComplianceDetail_violators :: Lens.Lens' PolicyComplianceDetail (Prelude.Maybe [ComplianceViolator])
policyComplianceDetail_violators = Lens.lens (\PolicyComplianceDetail' {violators} -> violators) (\s@PolicyComplianceDetail' {} a -> s {violators = a} :: PolicyComplianceDetail) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PolicyComplianceDetail where
  parseJSON =
    Data.withObject
      "PolicyComplianceDetail"
      ( \x ->
          PolicyComplianceDetail'
            Prelude.<$> (x Data..:? "EvaluationLimitExceeded")
            Prelude.<*> (x Data..:? "ExpiredAt")
            Prelude.<*> (x Data..:? "IssueInfoMap" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "MemberAccount")
            Prelude.<*> (x Data..:? "PolicyId")
            Prelude.<*> (x Data..:? "PolicyOwner")
            Prelude.<*> (x Data..:? "Violators" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable PolicyComplianceDetail where
  hashWithSalt _salt PolicyComplianceDetail' {..} =
    _salt
      `Prelude.hashWithSalt` evaluationLimitExceeded
      `Prelude.hashWithSalt` expiredAt
      `Prelude.hashWithSalt` issueInfoMap
      `Prelude.hashWithSalt` memberAccount
      `Prelude.hashWithSalt` policyId
      `Prelude.hashWithSalt` policyOwner
      `Prelude.hashWithSalt` violators

instance Prelude.NFData PolicyComplianceDetail where
  rnf PolicyComplianceDetail' {..} =
    Prelude.rnf evaluationLimitExceeded
      `Prelude.seq` Prelude.rnf expiredAt
      `Prelude.seq` Prelude.rnf issueInfoMap
      `Prelude.seq` Prelude.rnf memberAccount
      `Prelude.seq` Prelude.rnf policyId
      `Prelude.seq` Prelude.rnf policyOwner
      `Prelude.seq` Prelude.rnf violators
