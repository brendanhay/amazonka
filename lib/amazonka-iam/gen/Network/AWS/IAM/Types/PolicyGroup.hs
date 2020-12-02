{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyGroup where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a group that a managed policy is attached to.
--
--
-- This data type is used as a response element in the 'ListEntitiesForPolicy' operation.
--
-- For more information about managed policies, refer to <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
--
-- /See:/ 'policyGroup' smart constructor.
data PolicyGroup = PolicyGroup'
  { _pgGroupId :: !(Maybe Text),
    _pgGroupName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PolicyGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgGroupId' - The stable and unique string identifying the group. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- * 'pgGroupName' - The name (friendly name, not ARN) identifying the group.
policyGroup ::
  PolicyGroup
policyGroup =
  PolicyGroup' {_pgGroupId = Nothing, _pgGroupName = Nothing}

-- | The stable and unique string identifying the group. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the /IAM User Guide/ .
pgGroupId :: Lens' PolicyGroup (Maybe Text)
pgGroupId = lens _pgGroupId (\s a -> s {_pgGroupId = a})

-- | The name (friendly name, not ARN) identifying the group.
pgGroupName :: Lens' PolicyGroup (Maybe Text)
pgGroupName = lens _pgGroupName (\s a -> s {_pgGroupName = a})

instance FromXML PolicyGroup where
  parseXML x =
    PolicyGroup' <$> (x .@? "GroupId") <*> (x .@? "GroupName")

instance Hashable PolicyGroup

instance NFData PolicyGroup
