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
-- Module      : Network.AWS.FMS.Types.EvaluationResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.EvaluationResult where

import qualified Network.AWS.Core as Core
import Network.AWS.FMS.Types.PolicyComplianceStatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the compliance status for the account. An account is
-- considered noncompliant if it includes resources that are not protected
-- by the specified policy or that don\'t comply with the policy.
--
-- /See:/ 'newEvaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { -- | Describes an AWS account\'s compliance with the AWS Firewall Manager
    -- policy.
    complianceStatus :: Prelude.Maybe PolicyComplianceStatusType,
    -- | Indicates that over 100 resources are noncompliant with the AWS Firewall
    -- Manager policy.
    evaluationLimitExceeded :: Prelude.Maybe Prelude.Bool,
    -- | The number of resources that are noncompliant with the specified policy.
    -- For AWS WAF and Shield Advanced policies, a resource is considered
    -- noncompliant if it is not associated with the policy. For security group
    -- policies, a resource is considered noncompliant if it doesn\'t comply
    -- with the rules of the policy and remediation is disabled or not
    -- possible.
    violatorCount :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'complianceStatus', 'evaluationResult_complianceStatus' - Describes an AWS account\'s compliance with the AWS Firewall Manager
-- policy.
--
-- 'evaluationLimitExceeded', 'evaluationResult_evaluationLimitExceeded' - Indicates that over 100 resources are noncompliant with the AWS Firewall
-- Manager policy.
--
-- 'violatorCount', 'evaluationResult_violatorCount' - The number of resources that are noncompliant with the specified policy.
-- For AWS WAF and Shield Advanced policies, a resource is considered
-- noncompliant if it is not associated with the policy. For security group
-- policies, a resource is considered noncompliant if it doesn\'t comply
-- with the rules of the policy and remediation is disabled or not
-- possible.
newEvaluationResult ::
  EvaluationResult
newEvaluationResult =
  EvaluationResult'
    { complianceStatus =
        Prelude.Nothing,
      evaluationLimitExceeded = Prelude.Nothing,
      violatorCount = Prelude.Nothing
    }

-- | Describes an AWS account\'s compliance with the AWS Firewall Manager
-- policy.
evaluationResult_complianceStatus :: Lens.Lens' EvaluationResult (Prelude.Maybe PolicyComplianceStatusType)
evaluationResult_complianceStatus = Lens.lens (\EvaluationResult' {complianceStatus} -> complianceStatus) (\s@EvaluationResult' {} a -> s {complianceStatus = a} :: EvaluationResult)

-- | Indicates that over 100 resources are noncompliant with the AWS Firewall
-- Manager policy.
evaluationResult_evaluationLimitExceeded :: Lens.Lens' EvaluationResult (Prelude.Maybe Prelude.Bool)
evaluationResult_evaluationLimitExceeded = Lens.lens (\EvaluationResult' {evaluationLimitExceeded} -> evaluationLimitExceeded) (\s@EvaluationResult' {} a -> s {evaluationLimitExceeded = a} :: EvaluationResult)

-- | The number of resources that are noncompliant with the specified policy.
-- For AWS WAF and Shield Advanced policies, a resource is considered
-- noncompliant if it is not associated with the policy. For security group
-- policies, a resource is considered noncompliant if it doesn\'t comply
-- with the rules of the policy and remediation is disabled or not
-- possible.
evaluationResult_violatorCount :: Lens.Lens' EvaluationResult (Prelude.Maybe Prelude.Natural)
evaluationResult_violatorCount = Lens.lens (\EvaluationResult' {violatorCount} -> violatorCount) (\s@EvaluationResult' {} a -> s {violatorCount = a} :: EvaluationResult)

instance Core.FromJSON EvaluationResult where
  parseJSON =
    Core.withObject
      "EvaluationResult"
      ( \x ->
          EvaluationResult'
            Prelude.<$> (x Core..:? "ComplianceStatus")
            Prelude.<*> (x Core..:? "EvaluationLimitExceeded")
            Prelude.<*> (x Core..:? "ViolatorCount")
      )

instance Prelude.Hashable EvaluationResult

instance Prelude.NFData EvaluationResult
