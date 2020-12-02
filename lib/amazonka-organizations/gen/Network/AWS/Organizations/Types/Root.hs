{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.Root
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.Root where

import Network.AWS.Lens
import Network.AWS.Organizations.Types.PolicyTypeSummary
import Network.AWS.Prelude

-- | Contains details about a root. A root is a top-level parent node in the hierarchy of an organization that can contain organizational units (OUs) and accounts. The root contains every AWS account in the organization.
--
--
--
-- /See:/ 'root' smart constructor.
data Root = Root'
  { _rARN :: !(Maybe Text),
    _rName :: !(Maybe Text),
    _rId :: !(Maybe Text),
    _rPolicyTypes :: !(Maybe [PolicyTypeSummary])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Root' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rARN' - The Amazon Resource Name (ARN) of the root. For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
--
-- * 'rName' - The friendly name of the root. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
--
-- * 'rId' - The unique identifier (ID) for the root. The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lowercase letters or digits.
--
-- * 'rPolicyTypes' - The types of policies that are currently enabled for the root and therefore can be attached to the root or to its OUs or accounts.
root ::
  Root
root =
  Root'
    { _rARN = Nothing,
      _rName = Nothing,
      _rId = Nothing,
      _rPolicyTypes = Nothing
    }

-- | The Amazon Resource Name (ARN) of the root. For more information about ARNs in Organizations, see <https://docs.aws.amazon.com/organizations/latest/userguide/orgs_permissions.html#orgs-permissions-arns ARN Formats Supported by Organizations> in the /AWS Organizations User Guide/ .
rARN :: Lens' Root (Maybe Text)
rARN = lens _rARN (\s a -> s {_rARN = a})

-- | The friendly name of the root. The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of any of the characters in the ASCII character range.
rName :: Lens' Root (Maybe Text)
rName = lens _rName (\s a -> s {_rName = a})

-- | The unique identifier (ID) for the root. The <http://wikipedia.org/wiki/regex regex pattern> for a root ID string requires "r-" followed by from 4 to 32 lowercase letters or digits.
rId :: Lens' Root (Maybe Text)
rId = lens _rId (\s a -> s {_rId = a})

-- | The types of policies that are currently enabled for the root and therefore can be attached to the root or to its OUs or accounts.
rPolicyTypes :: Lens' Root [PolicyTypeSummary]
rPolicyTypes = lens _rPolicyTypes (\s a -> s {_rPolicyTypes = a}) . _Default . _Coerce

instance FromJSON Root where
  parseJSON =
    withObject
      "Root"
      ( \x ->
          Root'
            <$> (x .:? "Arn")
            <*> (x .:? "Name")
            <*> (x .:? "Id")
            <*> (x .:? "PolicyTypes" .!= mempty)
      )

instance Hashable Root

instance NFData Root
