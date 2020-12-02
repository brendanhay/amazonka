{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.PolicyTargetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.PolicyTargetSummary where

import Network.AWS.Lens
import Network.AWS.Organizations.Types.TargetType
import Network.AWS.Prelude

-- | Contains information about a root, OU, or account that a policy is attached to.
--
--
--
-- /See:/ 'policyTargetSummary' smart constructor.
data PolicyTargetSummary = PolicyTargetSummary'
  { _polTargetId ::
      !(Maybe Text),
    _polARN :: !(Maybe Text),
    _polName :: !(Maybe Text),
    _polType :: !(Maybe TargetType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PolicyTargetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'polTargetId' - The unique identifier (ID) of the policy target. The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.     * __Account__ - A string that consists of exactly 12 digits.     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
--
-- * 'polARN' - The Amazon Resource Name (ARN) of the policy target. For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- * 'polName' - The friendly name of the policy target. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- * 'polType' - The type of the policy target.
policyTargetSummary ::
  PolicyTargetSummary
policyTargetSummary =
  PolicyTargetSummary'
    { _polTargetId = Nothing,
      _polARN = Nothing,
      _polName = Nothing,
      _polType = Nothing
    }

-- | The unique identifier (ID) of the policy target. The <http://wikipedia.org/wiki/regex regex pattern> for a target ID string requires one of the following:     * __Root__ - A string that begins with "r-" followed by from 4 to 32 lowercase letters or digits.     * __Account__ - A string that consists of exactly 12 digits.     * __Organizational unit (OU)__ - A string that begins with "ou-" followed by from 4 to 32 lowercase letters or digits (the ID of the root that the OU is in). This string is followed by a second "-" dash and from 8 to 32 additional lowercase letters or digits.
polTargetId :: Lens' PolicyTargetSummary (Maybe Text)
polTargetId = lens _polTargetId (\s a -> s {_polTargetId = a})

-- | The Amazon Resource Name (ARN) of the policy target. For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
polARN :: Lens' PolicyTargetSummary (Maybe Text)
polARN = lens _polARN (\s a -> s {_polARN = a})

-- | The friendly name of the policy target. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
polName :: Lens' PolicyTargetSummary (Maybe Text)
polName = lens _polName (\s a -> s {_polName = a})

-- | The type of the policy target.
polType :: Lens' PolicyTargetSummary (Maybe TargetType)
polType = lens _polType (\s a -> s {_polType = a})

instance FromJSON PolicyTargetSummary where
  parseJSON =
    withObject
      "PolicyTargetSummary"
      ( \x ->
          PolicyTargetSummary'
            <$> (x .:? "TargetId")
            <*> (x .:? "Arn")
            <*> (x .:? "Name")
            <*> (x .:? "Type")
      )

instance Hashable PolicyTargetSummary

instance NFData PolicyTargetSummary
