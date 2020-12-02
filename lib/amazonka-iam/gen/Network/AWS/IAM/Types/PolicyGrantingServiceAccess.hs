{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PolicyGrantingServiceAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PolicyGrantingServiceAccess where

import Network.AWS.IAM.Types.PolicyOwnerEntityType
import Network.AWS.IAM.Types.PolicyType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about the permissions policies that are attached to the specified identity (user, group, or role).
--
--
-- This data type is an element of the 'ListPoliciesGrantingServiceAccessEntry' object.
--
--
-- /See:/ 'policyGrantingServiceAccess' smart constructor.
data PolicyGrantingServiceAccess = PolicyGrantingServiceAccess'
  { _pgsaEntityName ::
      !(Maybe Text),
    _pgsaEntityType ::
      !(Maybe PolicyOwnerEntityType),
    _pgsaPolicyARN :: !(Maybe Text),
    _pgsaPolicyName :: !Text,
    _pgsaPolicyType :: !PolicyType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PolicyGrantingServiceAccess' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pgsaEntityName' - The name of the entity (user or role) to which the inline policy is attached. This field is null for managed policies. For more information about these policy types, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- * 'pgsaEntityType' - The type of entity (user or role) that used the policy to access the service to which the inline policy is attached. This field is null for managed policies. For more information about these policy types, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
-- * 'pgsaPolicyARN' - Undocumented member.
--
-- * 'pgsaPolicyName' - The policy name.
--
-- * 'pgsaPolicyType' - The policy type. For more information about these policy types, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
policyGrantingServiceAccess ::
  -- | 'pgsaPolicyName'
  Text ->
  -- | 'pgsaPolicyType'
  PolicyType ->
  PolicyGrantingServiceAccess
policyGrantingServiceAccess pPolicyName_ pPolicyType_ =
  PolicyGrantingServiceAccess'
    { _pgsaEntityName = Nothing,
      _pgsaEntityType = Nothing,
      _pgsaPolicyARN = Nothing,
      _pgsaPolicyName = pPolicyName_,
      _pgsaPolicyType = pPolicyType_
    }

-- | The name of the entity (user or role) to which the inline policy is attached. This field is null for managed policies. For more information about these policy types, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
pgsaEntityName :: Lens' PolicyGrantingServiceAccess (Maybe Text)
pgsaEntityName = lens _pgsaEntityName (\s a -> s {_pgsaEntityName = a})

-- | The type of entity (user or role) that used the policy to access the service to which the inline policy is attached. This field is null for managed policies. For more information about these policy types, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
pgsaEntityType :: Lens' PolicyGrantingServiceAccess (Maybe PolicyOwnerEntityType)
pgsaEntityType = lens _pgsaEntityType (\s a -> s {_pgsaEntityType = a})

-- | Undocumented member.
pgsaPolicyARN :: Lens' PolicyGrantingServiceAccess (Maybe Text)
pgsaPolicyARN = lens _pgsaPolicyARN (\s a -> s {_pgsaPolicyARN = a})

-- | The policy name.
pgsaPolicyName :: Lens' PolicyGrantingServiceAccess Text
pgsaPolicyName = lens _pgsaPolicyName (\s a -> s {_pgsaPolicyName = a})

-- | The policy type. For more information about these policy types, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
pgsaPolicyType :: Lens' PolicyGrantingServiceAccess PolicyType
pgsaPolicyType = lens _pgsaPolicyType (\s a -> s {_pgsaPolicyType = a})

instance FromXML PolicyGrantingServiceAccess where
  parseXML x =
    PolicyGrantingServiceAccess'
      <$> (x .@? "EntityName")
      <*> (x .@? "EntityType")
      <*> (x .@? "PolicyArn")
      <*> (x .@ "PolicyName")
      <*> (x .@ "PolicyType")

instance Hashable PolicyGrantingServiceAccess

instance NFData PolicyGrantingServiceAccess
