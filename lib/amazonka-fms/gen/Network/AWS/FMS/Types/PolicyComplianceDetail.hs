{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.PolicyComplianceDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.PolicyComplianceDetail where

import Network.AWS.FMS.Types.ComplianceViolator
import Network.AWS.FMS.Types.DependentServiceName
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the noncompliant resources in a member account for a specific AWS Firewall Manager policy. A maximum of 100 entries are displayed. If more than 100 resources are noncompliant, @EvaluationLimitExceeded@ is set to @True@ .
--
--
--
-- /See:/ 'policyComplianceDetail' smart constructor.
data PolicyComplianceDetail = PolicyComplianceDetail'
  { _pcdExpiredAt ::
      !(Maybe POSIX),
    _pcdPolicyId :: !(Maybe Text),
    _pcdViolators ::
      !(Maybe [ComplianceViolator]),
    _pcdEvaluationLimitExceeded :: !(Maybe Bool),
    _pcdIssueInfoMap ::
      !(Maybe (Map DependentServiceName (Text))),
    _pcdPolicyOwner :: !(Maybe Text),
    _pcdMemberAccount :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PolicyComplianceDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pcdExpiredAt' - A timestamp that indicates when the returned information should be considered out of date.
--
-- * 'pcdPolicyId' - The ID of the AWS Firewall Manager policy.
--
-- * 'pcdViolators' - An array of resources that aren't protected by the AWS WAF or Shield Advanced policy or that aren't in compliance with the security group policy.
--
-- * 'pcdEvaluationLimitExceeded' - Indicates if over 100 resources are noncompliant with the AWS Firewall Manager policy.
--
-- * 'pcdIssueInfoMap' - Details about problems with dependent services, such as AWS WAF or AWS Config, that are causing a resource to be noncompliant. The details include the name of the dependent service and the error message received that indicates the problem with the service.
--
-- * 'pcdPolicyOwner' - The AWS account that created the AWS Firewall Manager policy.
--
-- * 'pcdMemberAccount' - The AWS account ID.
policyComplianceDetail ::
  PolicyComplianceDetail
policyComplianceDetail =
  PolicyComplianceDetail'
    { _pcdExpiredAt = Nothing,
      _pcdPolicyId = Nothing,
      _pcdViolators = Nothing,
      _pcdEvaluationLimitExceeded = Nothing,
      _pcdIssueInfoMap = Nothing,
      _pcdPolicyOwner = Nothing,
      _pcdMemberAccount = Nothing
    }

-- | A timestamp that indicates when the returned information should be considered out of date.
pcdExpiredAt :: Lens' PolicyComplianceDetail (Maybe UTCTime)
pcdExpiredAt = lens _pcdExpiredAt (\s a -> s {_pcdExpiredAt = a}) . mapping _Time

-- | The ID of the AWS Firewall Manager policy.
pcdPolicyId :: Lens' PolicyComplianceDetail (Maybe Text)
pcdPolicyId = lens _pcdPolicyId (\s a -> s {_pcdPolicyId = a})

-- | An array of resources that aren't protected by the AWS WAF or Shield Advanced policy or that aren't in compliance with the security group policy.
pcdViolators :: Lens' PolicyComplianceDetail [ComplianceViolator]
pcdViolators = lens _pcdViolators (\s a -> s {_pcdViolators = a}) . _Default . _Coerce

-- | Indicates if over 100 resources are noncompliant with the AWS Firewall Manager policy.
pcdEvaluationLimitExceeded :: Lens' PolicyComplianceDetail (Maybe Bool)
pcdEvaluationLimitExceeded = lens _pcdEvaluationLimitExceeded (\s a -> s {_pcdEvaluationLimitExceeded = a})

-- | Details about problems with dependent services, such as AWS WAF or AWS Config, that are causing a resource to be noncompliant. The details include the name of the dependent service and the error message received that indicates the problem with the service.
pcdIssueInfoMap :: Lens' PolicyComplianceDetail (HashMap DependentServiceName (Text))
pcdIssueInfoMap = lens _pcdIssueInfoMap (\s a -> s {_pcdIssueInfoMap = a}) . _Default . _Map

-- | The AWS account that created the AWS Firewall Manager policy.
pcdPolicyOwner :: Lens' PolicyComplianceDetail (Maybe Text)
pcdPolicyOwner = lens _pcdPolicyOwner (\s a -> s {_pcdPolicyOwner = a})

-- | The AWS account ID.
pcdMemberAccount :: Lens' PolicyComplianceDetail (Maybe Text)
pcdMemberAccount = lens _pcdMemberAccount (\s a -> s {_pcdMemberAccount = a})

instance FromJSON PolicyComplianceDetail where
  parseJSON =
    withObject
      "PolicyComplianceDetail"
      ( \x ->
          PolicyComplianceDetail'
            <$> (x .:? "ExpiredAt")
            <*> (x .:? "PolicyId")
            <*> (x .:? "Violators" .!= mempty)
            <*> (x .:? "EvaluationLimitExceeded")
            <*> (x .:? "IssueInfoMap" .!= mempty)
            <*> (x .:? "PolicyOwner")
            <*> (x .:? "MemberAccount")
      )

instance Hashable PolicyComplianceDetail

instance NFData PolicyComplianceDetail
