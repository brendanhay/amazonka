{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.GroupDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.GroupDetail where

import Network.AWS.IAM.Types.AttachedPolicy
import Network.AWS.IAM.Types.PolicyDetail
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an IAM group, including all of the group's policies.
--
--
-- This data type is used as a response element in the 'GetAccountAuthorizationDetails' operation.
--
--
-- /See:/ 'groupDetail' smart constructor.
data GroupDetail = GroupDetail'
  { _gdARN :: !(Maybe Text),
    _gdPath :: !(Maybe Text),
    _gdCreateDate :: !(Maybe ISO8601),
    _gdGroupId :: !(Maybe Text),
    _gdGroupPolicyList :: !(Maybe [PolicyDetail]),
    _gdGroupName :: !(Maybe Text),
    _gdAttachedManagedPolicies :: !(Maybe [AttachedPolicy])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GroupDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdARN' - Undocumented member.
--
-- * 'gdPath' - The path to the group. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- * 'gdCreateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the group was created.
--
-- * 'gdGroupId' - The stable and unique string identifying the group. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- * 'gdGroupPolicyList' - A list of the inline policies embedded in the group.
--
-- * 'gdGroupName' - The friendly name that identifies the group.
--
-- * 'gdAttachedManagedPolicies' - A list of the managed policies attached to the group.
groupDetail ::
  GroupDetail
groupDetail =
  GroupDetail'
    { _gdARN = Nothing,
      _gdPath = Nothing,
      _gdCreateDate = Nothing,
      _gdGroupId = Nothing,
      _gdGroupPolicyList = Nothing,
      _gdGroupName = Nothing,
      _gdAttachedManagedPolicies = Nothing
    }

-- | Undocumented member.
gdARN :: Lens' GroupDetail (Maybe Text)
gdARN = lens _gdARN (\s a -> s {_gdARN = a})

-- | The path to the group. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
gdPath :: Lens' GroupDetail (Maybe Text)
gdPath = lens _gdPath (\s a -> s {_gdPath = a})

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the group was created.
gdCreateDate :: Lens' GroupDetail (Maybe UTCTime)
gdCreateDate = lens _gdCreateDate (\s a -> s {_gdCreateDate = a}) . mapping _Time

-- | The stable and unique string identifying the group. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
gdGroupId :: Lens' GroupDetail (Maybe Text)
gdGroupId = lens _gdGroupId (\s a -> s {_gdGroupId = a})

-- | A list of the inline policies embedded in the group.
gdGroupPolicyList :: Lens' GroupDetail [PolicyDetail]
gdGroupPolicyList = lens _gdGroupPolicyList (\s a -> s {_gdGroupPolicyList = a}) . _Default . _Coerce

-- | The friendly name that identifies the group.
gdGroupName :: Lens' GroupDetail (Maybe Text)
gdGroupName = lens _gdGroupName (\s a -> s {_gdGroupName = a})

-- | A list of the managed policies attached to the group.
gdAttachedManagedPolicies :: Lens' GroupDetail [AttachedPolicy]
gdAttachedManagedPolicies = lens _gdAttachedManagedPolicies (\s a -> s {_gdAttachedManagedPolicies = a}) . _Default . _Coerce

instance FromXML GroupDetail where
  parseXML x =
    GroupDetail'
      <$> (x .@? "Arn")
      <*> (x .@? "Path")
      <*> (x .@? "CreateDate")
      <*> (x .@? "GroupId")
      <*> ( x .@? "GroupPolicyList" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "GroupName")
      <*> ( x .@? "AttachedManagedPolicies" .!@ mempty
              >>= may (parseXMLList "member")
          )

instance Hashable GroupDetail

instance NFData GroupDetail
