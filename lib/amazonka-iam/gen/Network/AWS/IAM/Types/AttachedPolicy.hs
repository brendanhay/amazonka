{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.AttachedPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.AttachedPolicy where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an attached policy.
--
--
-- An attached policy is a managed policy that has been attached to a user, group, or role. This data type is used as a response element in the 'ListAttachedGroupPolicies' , 'ListAttachedRolePolicies' , 'ListAttachedUserPolicies' , and 'GetAccountAuthorizationDetails' operations.
--
-- For more information about managed policies, refer to <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
--
-- /See:/ 'attachedPolicy' smart constructor.
data AttachedPolicy = AttachedPolicy'
  { _apPolicyName ::
      !(Maybe Text),
    _apPolicyARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttachedPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apPolicyName' - The friendly name of the attached policy.
--
-- * 'apPolicyARN' - Undocumented member.
attachedPolicy ::
  AttachedPolicy
attachedPolicy =
  AttachedPolicy' {_apPolicyName = Nothing, _apPolicyARN = Nothing}

-- | The friendly name of the attached policy.
apPolicyName :: Lens' AttachedPolicy (Maybe Text)
apPolicyName = lens _apPolicyName (\s a -> s {_apPolicyName = a})

-- | Undocumented member.
apPolicyARN :: Lens' AttachedPolicy (Maybe Text)
apPolicyARN = lens _apPolicyARN (\s a -> s {_apPolicyARN = a})

instance FromXML AttachedPolicy where
  parseXML x =
    AttachedPolicy' <$> (x .@? "PolicyName") <*> (x .@? "PolicyArn")

instance Hashable AttachedPolicy

instance NFData AttachedPolicy
