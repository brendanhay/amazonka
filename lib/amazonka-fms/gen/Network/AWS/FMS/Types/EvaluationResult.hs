{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.EvaluationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.EvaluationResult where

import Network.AWS.FMS.Types.PolicyComplianceStatusType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the compliance status for the account. An account is considered noncompliant if it includes resources that are not protected by the specified policy or that don't comply with the policy.
--
--
--
-- /See:/ 'evaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { _erViolatorCount ::
      !(Maybe Nat),
    _erComplianceStatus ::
      !(Maybe PolicyComplianceStatusType),
    _erEvaluationLimitExceeded :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EvaluationResult' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'erViolatorCount' - The number of resources that are noncompliant with the specified policy. For AWS WAF and Shield Advanced policies, a resource is considered noncompliant if it is not associated with the policy. For security group policies, a resource is considered noncompliant if it doesn't comply with the rules of the policy and remediation is disabled or not possible.
--
-- * 'erComplianceStatus' - Describes an AWS account's compliance with the AWS Firewall Manager policy.
--
-- * 'erEvaluationLimitExceeded' - Indicates that over 100 resources are noncompliant with the AWS Firewall Manager policy.
evaluationResult ::
  EvaluationResult
evaluationResult =
  EvaluationResult'
    { _erViolatorCount = Nothing,
      _erComplianceStatus = Nothing,
      _erEvaluationLimitExceeded = Nothing
    }

-- | The number of resources that are noncompliant with the specified policy. For AWS WAF and Shield Advanced policies, a resource is considered noncompliant if it is not associated with the policy. For security group policies, a resource is considered noncompliant if it doesn't comply with the rules of the policy and remediation is disabled or not possible.
erViolatorCount :: Lens' EvaluationResult (Maybe Natural)
erViolatorCount = lens _erViolatorCount (\s a -> s {_erViolatorCount = a}) . mapping _Nat

-- | Describes an AWS account's compliance with the AWS Firewall Manager policy.
erComplianceStatus :: Lens' EvaluationResult (Maybe PolicyComplianceStatusType)
erComplianceStatus = lens _erComplianceStatus (\s a -> s {_erComplianceStatus = a})

-- | Indicates that over 100 resources are noncompliant with the AWS Firewall Manager policy.
erEvaluationLimitExceeded :: Lens' EvaluationResult (Maybe Bool)
erEvaluationLimitExceeded = lens _erEvaluationLimitExceeded (\s a -> s {_erEvaluationLimitExceeded = a})

instance FromJSON EvaluationResult where
  parseJSON =
    withObject
      "EvaluationResult"
      ( \x ->
          EvaluationResult'
            <$> (x .:? "ViolatorCount")
            <*> (x .:? "ComplianceStatus")
            <*> (x .:? "EvaluationLimitExceeded")
      )

instance Hashable EvaluationResult

instance NFData EvaluationResult
