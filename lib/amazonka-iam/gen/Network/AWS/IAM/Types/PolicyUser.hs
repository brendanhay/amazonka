{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyUser where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a user that a managed policy is attached to.
--
--
-- This data type is used as a response element in the 'ListEntitiesForPolicy' operation.
--
-- For more information about managed policies, refer to <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
--
-- /See:/ 'policyUser' smart constructor.
data PolicyUser = PolicyUser'
  { _puUserName :: !(Maybe Text),
    _puUserId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PolicyUser' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'puUserName' - The name (friendly name, not ARN) identifying the user.
--
-- * 'puUserId' - The stable and unique string identifying the user. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
policyUser ::
  PolicyUser
policyUser =
  PolicyUser' {_puUserName = Nothing, _puUserId = Nothing}

-- | The name (friendly name, not ARN) identifying the user.
puUserName :: Lens' PolicyUser (Maybe Text)
puUserName = lens _puUserName (\s a -> s {_puUserName = a})

-- | The stable and unique string identifying the user. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
puUserId :: Lens' PolicyUser (Maybe Text)
puUserId = lens _puUserId (\s a -> s {_puUserId = a})

instance FromXML PolicyUser where
  parseXML x =
    PolicyUser' <$> (x .@? "UserName") <*> (x .@? "UserId")

instance Hashable PolicyUser

instance NFData PolicyUser
