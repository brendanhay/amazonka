{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.PolicySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.PolicySummary where

import Network.AWS.Lens
import Network.AWS.Organizations.Types.PolicyType
import Network.AWS.Prelude

-- | Contains information about a policy, but does not include the content. To see the content of a policy, see 'DescribePolicy' .
--
--
--
-- /See:/ 'policySummary' smart constructor.
data PolicySummary = PolicySummary'
  { _psARN :: !(Maybe Text),
    _psName :: !(Maybe Text),
    _psId :: !(Maybe Text),
    _psAWSManaged :: !(Maybe Bool),
    _psType :: !(Maybe PolicyType),
    _psDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PolicySummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'psARN' - The Amazon Resource Name (ARN) of the policy. For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- * 'psName' - The friendly name of the policy. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- * 'psId' - The unique identifier (ID) of the policy. The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
--
-- * 'psAWSManaged' - A boolean value that indicates whether the specified policy is an AWS managed policy. If true, then you can attach the policy to roots, OUs, or accounts, but you cannot edit it.
--
-- * 'psType' - The type of policy.
--
-- * 'psDescription' - The description of the policy.
policySummary ::
  PolicySummary
policySummary =
  PolicySummary'
    { _psARN = Nothing,
      _psName = Nothing,
      _psId = Nothing,
      _psAWSManaged = Nothing,
      _psType = Nothing,
      _psDescription = Nothing
    }

-- | The Amazon Resource Name (ARN) of the policy. For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
psARN :: Lens' PolicySummary (Maybe Text)
psARN = lens _psARN (\s a -> s {_psARN = a})

-- | The friendly name of the policy. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
psName :: Lens' PolicySummary (Maybe Text)
psName = lens _psName (\s a -> s {_psName = a})

-- | The unique identifier (ID) of the policy. The <http://wikipedia.org/wiki/regex regex pattern> for a policy ID string requires "p-" followed by from 8 to 128 lowercase or uppercase letters, digits, or the underscore character (_).
psId :: Lens' PolicySummary (Maybe Text)
psId = lens _psId (\s a -> s {_psId = a})

-- | A boolean value that indicates whether the specified policy is an AWS managed policy. If true, then you can attach the policy to roots, OUs, or accounts, but you cannot edit it.
psAWSManaged :: Lens' PolicySummary (Maybe Bool)
psAWSManaged = lens _psAWSManaged (\s a -> s {_psAWSManaged = a})

-- | The type of policy.
psType :: Lens' PolicySummary (Maybe PolicyType)
psType = lens _psType (\s a -> s {_psType = a})

-- | The description of the policy.
psDescription :: Lens' PolicySummary (Maybe Text)
psDescription = lens _psDescription (\s a -> s {_psDescription = a})

instance FromJSON PolicySummary where
  parseJSON =
    withObject
      "PolicySummary"
      ( \x ->
          PolicySummary'
            <$> (x .:? "Arn")
            <*> (x .:? "Name")
            <*> (x .:? "Id")
            <*> (x .:? "AwsManaged")
            <*> (x .:? "Type")
            <*> (x .:? "Description")
      )

instance Hashable PolicySummary

instance NFData PolicySummary
