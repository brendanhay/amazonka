{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.PolicyComplianceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.PolicyComplianceStatus where

import Network.AWS.FMS.Types.DependentServiceName
import Network.AWS.FMS.Types.EvaluationResult
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Indicates whether the account is compliant with the specified policy. An account is considered noncompliant if it includes resources that are not protected by the policy, for AWS WAF and Shield Advanced policies, or that are noncompliant with the policy, for security group policies.
--
--
--
-- /See:/ 'policyComplianceStatus' smart constructor.
data PolicyComplianceStatus = PolicyComplianceStatus'
  { _pcsEvaluationResults ::
      !(Maybe [EvaluationResult]),
    _pcsLastUpdated :: !(Maybe POSIX),
    _pcsPolicyName :: !(Maybe Text),
    _pcsPolicyId :: !(Maybe Text),
    _pcsIssueInfoMap ::
      !(Maybe (Map DependentServiceName (Text))),
    _pcsPolicyOwner :: !(Maybe Text),
    _pcsMemberAccount :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PolicyComplianceStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcsEvaluationResults' - An array of @EvaluationResult@ objects.
--
-- * 'pcsLastUpdated' - Timestamp of the last update to the @EvaluationResult@ objects.
--
-- * 'pcsPolicyName' - The name of the AWS Firewall Manager policy.
--
-- * 'pcsPolicyId' - The ID of the AWS Firewall Manager policy.
--
-- * 'pcsIssueInfoMap' - Details about problems with dependent services, such as AWS WAF or AWS Config, that are causing a resource to be noncompliant. The details include the name of the dependent service and the error message received that indicates the problem with the service.
--
-- * 'pcsPolicyOwner' - The AWS account that created the AWS Firewall Manager policy.
--
-- * 'pcsMemberAccount' - The member account ID.
policyComplianceStatus ::
  PolicyComplianceStatus
policyComplianceStatus =
  PolicyComplianceStatus'
    { _pcsEvaluationResults = Nothing,
      _pcsLastUpdated = Nothing,
      _pcsPolicyName = Nothing,
      _pcsPolicyId = Nothing,
      _pcsIssueInfoMap = Nothing,
      _pcsPolicyOwner = Nothing,
      _pcsMemberAccount = Nothing
    }

-- | An array of @EvaluationResult@ objects.
pcsEvaluationResults :: Lens' PolicyComplianceStatus [EvaluationResult]
pcsEvaluationResults = lens _pcsEvaluationResults (\s a -> s {_pcsEvaluationResults = a}) . _Default . _Coerce

-- | Timestamp of the last update to the @EvaluationResult@ objects.
pcsLastUpdated :: Lens' PolicyComplianceStatus (Maybe UTCTime)
pcsLastUpdated = lens _pcsLastUpdated (\s a -> s {_pcsLastUpdated = a}) . mapping _Time

-- | The name of the AWS Firewall Manager policy.
pcsPolicyName :: Lens' PolicyComplianceStatus (Maybe Text)
pcsPolicyName = lens _pcsPolicyName (\s a -> s {_pcsPolicyName = a})

-- | The ID of the AWS Firewall Manager policy.
pcsPolicyId :: Lens' PolicyComplianceStatus (Maybe Text)
pcsPolicyId = lens _pcsPolicyId (\s a -> s {_pcsPolicyId = a})

-- | Details about problems with dependent services, such as AWS WAF or AWS Config, that are causing a resource to be noncompliant. The details include the name of the dependent service and the error message received that indicates the problem with the service.
pcsIssueInfoMap :: Lens' PolicyComplianceStatus (HashMap DependentServiceName (Text))
pcsIssueInfoMap = lens _pcsIssueInfoMap (\s a -> s {_pcsIssueInfoMap = a}) . _Default . _Map

-- | The AWS account that created the AWS Firewall Manager policy.
pcsPolicyOwner :: Lens' PolicyComplianceStatus (Maybe Text)
pcsPolicyOwner = lens _pcsPolicyOwner (\s a -> s {_pcsPolicyOwner = a})

-- | The member account ID.
pcsMemberAccount :: Lens' PolicyComplianceStatus (Maybe Text)
pcsMemberAccount = lens _pcsMemberAccount (\s a -> s {_pcsMemberAccount = a})

instance FromJSON PolicyComplianceStatus where
  parseJSON =
    withObject
      "PolicyComplianceStatus"
      ( \x ->
          PolicyComplianceStatus'
            <$> (x .:? "EvaluationResults" .!= mempty)
            <*> (x .:? "LastUpdated")
            <*> (x .:? "PolicyName")
            <*> (x .:? "PolicyId")
            <*> (x .:? "IssueInfoMap" .!= mempty)
            <*> (x .:? "PolicyOwner")
            <*> (x .:? "MemberAccount")
      )

instance Hashable PolicyComplianceStatus

instance NFData PolicyComplianceStatus
