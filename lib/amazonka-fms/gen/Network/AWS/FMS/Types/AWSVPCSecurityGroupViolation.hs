{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.FMS.Types.AWSVPCSecurityGroupViolation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.FMS.Types.AWSVPCSecurityGroupViolation where

import Network.AWS.FMS.Types.PartialMatch
import Network.AWS.FMS.Types.SecurityGroupRemediationAction
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details of the rule violation in a security group when compared to the master security group of the AWS Firewall Manager policy.
--
--
--
-- /See:/ 'awsVPCSecurityGroupViolation' smart constructor.
data AWSVPCSecurityGroupViolation = AWSVPCSecurityGroupViolation'
  { _avsgvViolationTargetDescription ::
      !(Maybe Text),
    _avsgvPossibleSecurityGroupRemediationActions ::
      !( Maybe
           [SecurityGroupRemediationAction]
       ),
    _avsgvViolationTarget ::
      !(Maybe Text),
    _avsgvPartialMatches ::
      !(Maybe [PartialMatch])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AWSVPCSecurityGroupViolation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avsgvViolationTargetDescription' - A description of the security group that violates the policy.
--
-- * 'avsgvPossibleSecurityGroupRemediationActions' - Remediation options for the rule specified in the @ViolationTarget@ .
--
-- * 'avsgvViolationTarget' - The security group rule that is being evaluated.
--
-- * 'avsgvPartialMatches' - List of rules specified in the security group of the AWS Firewall Manager policy that partially match the @ViolationTarget@ rule.
awsVPCSecurityGroupViolation ::
  AWSVPCSecurityGroupViolation
awsVPCSecurityGroupViolation =
  AWSVPCSecurityGroupViolation'
    { _avsgvViolationTargetDescription =
        Nothing,
      _avsgvPossibleSecurityGroupRemediationActions = Nothing,
      _avsgvViolationTarget = Nothing,
      _avsgvPartialMatches = Nothing
    }

-- | A description of the security group that violates the policy.
avsgvViolationTargetDescription :: Lens' AWSVPCSecurityGroupViolation (Maybe Text)
avsgvViolationTargetDescription = lens _avsgvViolationTargetDescription (\s a -> s {_avsgvViolationTargetDescription = a})

-- | Remediation options for the rule specified in the @ViolationTarget@ .
avsgvPossibleSecurityGroupRemediationActions :: Lens' AWSVPCSecurityGroupViolation [SecurityGroupRemediationAction]
avsgvPossibleSecurityGroupRemediationActions = lens _avsgvPossibleSecurityGroupRemediationActions (\s a -> s {_avsgvPossibleSecurityGroupRemediationActions = a}) . _Default . _Coerce

-- | The security group rule that is being evaluated.
avsgvViolationTarget :: Lens' AWSVPCSecurityGroupViolation (Maybe Text)
avsgvViolationTarget = lens _avsgvViolationTarget (\s a -> s {_avsgvViolationTarget = a})

-- | List of rules specified in the security group of the AWS Firewall Manager policy that partially match the @ViolationTarget@ rule.
avsgvPartialMatches :: Lens' AWSVPCSecurityGroupViolation [PartialMatch]
avsgvPartialMatches = lens _avsgvPartialMatches (\s a -> s {_avsgvPartialMatches = a}) . _Default . _Coerce

instance FromJSON AWSVPCSecurityGroupViolation where
  parseJSON =
    withObject
      "AWSVPCSecurityGroupViolation"
      ( \x ->
          AWSVPCSecurityGroupViolation'
            <$> (x .:? "ViolationTargetDescription")
            <*> (x .:? "PossibleSecurityGroupRemediationActions" .!= mempty)
            <*> (x .:? "ViolationTarget")
            <*> (x .:? "PartialMatches" .!= mempty)
      )

instance Hashable AWSVPCSecurityGroupViolation

instance NFData AWSVPCSecurityGroupViolation
