{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ManagedPolicyDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.ManagedPolicyDetail where

import Network.AWS.IAM.Types.PolicyVersion
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a managed policy, including the policy's ARN, versions, and the number of principal entities (users, groups, and roles) that the policy is attached to.
--
--
-- This data type is used as a response element in the 'GetAccountAuthorizationDetails' operation.
--
-- For more information about managed policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-vs-inline.html Managed Policies and Inline Policies> in the /IAM User Guide/ .
--
--
-- /See:/ 'managedPolicyDetail' smart constructor.
data ManagedPolicyDetail = ManagedPolicyDetail'
  { _mpdPolicyName ::
      !(Maybe Text),
    _mpdARN :: !(Maybe Text),
    _mpdUpdateDate :: !(Maybe ISO8601),
    _mpdPolicyId :: !(Maybe Text),
    _mpdPath :: !(Maybe Text),
    _mpdPolicyVersionList :: !(Maybe [PolicyVersion]),
    _mpdCreateDate :: !(Maybe ISO8601),
    _mpdIsAttachable :: !(Maybe Bool),
    _mpdPermissionsBoundaryUsageCount :: !(Maybe Int),
    _mpdDefaultVersionId :: !(Maybe Text),
    _mpdAttachmentCount :: !(Maybe Int),
    _mpdDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ManagedPolicyDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mpdPolicyName' - The friendly name (not ARN) identifying the policy.
--
-- * 'mpdARN' - Undocumented member.
--
-- * 'mpdUpdateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was last updated. When a policy has only one version, this field contains the date and time when the policy was created. When a policy has more than one version, this field contains the date and time when the most recent policy version was created.
--
-- * 'mpdPolicyId' - The stable and unique string identifying the policy. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- * 'mpdPath' - The path to the policy. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- * 'mpdPolicyVersionList' - A list containing information about the versions of the policy.
--
-- * 'mpdCreateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was created.
--
-- * 'mpdIsAttachable' - Specifies whether the policy can be attached to an IAM user, group, or role.
--
-- * 'mpdPermissionsBoundaryUsageCount' - The number of entities (users and roles) for which the policy is used as the permissions boundary.  For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
--
-- * 'mpdDefaultVersionId' - The identifier for the version of the policy that is set as the default (operative) version. For more information about policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
--
-- * 'mpdAttachmentCount' - The number of principal entities (users, groups, and roles) that the policy is attached to.
--
-- * 'mpdDescription' - A friendly description of the policy.
managedPolicyDetail ::
  ManagedPolicyDetail
managedPolicyDetail =
  ManagedPolicyDetail'
    { _mpdPolicyName = Nothing,
      _mpdARN = Nothing,
      _mpdUpdateDate = Nothing,
      _mpdPolicyId = Nothing,
      _mpdPath = Nothing,
      _mpdPolicyVersionList = Nothing,
      _mpdCreateDate = Nothing,
      _mpdIsAttachable = Nothing,
      _mpdPermissionsBoundaryUsageCount = Nothing,
      _mpdDefaultVersionId = Nothing,
      _mpdAttachmentCount = Nothing,
      _mpdDescription = Nothing
    }

-- | The friendly name (not ARN) identifying the policy.
mpdPolicyName :: Lens' ManagedPolicyDetail (Maybe Text)
mpdPolicyName = lens _mpdPolicyName (\s a -> s {_mpdPolicyName = a})

-- | Undocumented member.
mpdARN :: Lens' ManagedPolicyDetail (Maybe Text)
mpdARN = lens _mpdARN (\s a -> s {_mpdARN = a})

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was last updated. When a policy has only one version, this field contains the date and time when the policy was created. When a policy has more than one version, this field contains the date and time when the most recent policy version was created.
mpdUpdateDate :: Lens' ManagedPolicyDetail (Maybe UTCTime)
mpdUpdateDate = lens _mpdUpdateDate (\s a -> s {_mpdUpdateDate = a}) . mapping _Time

-- | The stable and unique string identifying the policy. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
mpdPolicyId :: Lens' ManagedPolicyDetail (Maybe Text)
mpdPolicyId = lens _mpdPolicyId (\s a -> s {_mpdPolicyId = a})

-- | The path to the policy. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
mpdPath :: Lens' ManagedPolicyDetail (Maybe Text)
mpdPath = lens _mpdPath (\s a -> s {_mpdPath = a})

-- | A list containing information about the versions of the policy.
mpdPolicyVersionList :: Lens' ManagedPolicyDetail [PolicyVersion]
mpdPolicyVersionList = lens _mpdPolicyVersionList (\s a -> s {_mpdPolicyVersionList = a}) . _Default . _Coerce

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the policy was created.
mpdCreateDate :: Lens' ManagedPolicyDetail (Maybe UTCTime)
mpdCreateDate = lens _mpdCreateDate (\s a -> s {_mpdCreateDate = a}) . mapping _Time

-- | Specifies whether the policy can be attached to an IAM user, group, or role.
mpdIsAttachable :: Lens' ManagedPolicyDetail (Maybe Bool)
mpdIsAttachable = lens _mpdIsAttachable (\s a -> s {_mpdIsAttachable = a})

-- | The number of entities (users and roles) for which the policy is used as the permissions boundary.  For more information about permissions boundaries, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions Boundaries for IAM Identities > in the /IAM User Guide/ .
mpdPermissionsBoundaryUsageCount :: Lens' ManagedPolicyDetail (Maybe Int)
mpdPermissionsBoundaryUsageCount = lens _mpdPermissionsBoundaryUsageCount (\s a -> s {_mpdPermissionsBoundaryUsageCount = a})

-- | The identifier for the version of the policy that is set as the default (operative) version. For more information about policy versions, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/policies-managed-versions.html Versioning for Managed Policies> in the /IAM User Guide/ .
mpdDefaultVersionId :: Lens' ManagedPolicyDetail (Maybe Text)
mpdDefaultVersionId = lens _mpdDefaultVersionId (\s a -> s {_mpdDefaultVersionId = a})

-- | The number of principal entities (users, groups, and roles) that the policy is attached to.
mpdAttachmentCount :: Lens' ManagedPolicyDetail (Maybe Int)
mpdAttachmentCount = lens _mpdAttachmentCount (\s a -> s {_mpdAttachmentCount = a})

-- | A friendly description of the policy.
mpdDescription :: Lens' ManagedPolicyDetail (Maybe Text)
mpdDescription = lens _mpdDescription (\s a -> s {_mpdDescription = a})

instance FromXML ManagedPolicyDetail where
  parseXML x =
    ManagedPolicyDetail'
      <$> (x .@? "PolicyName")
      <*> (x .@? "Arn")
      <*> (x .@? "UpdateDate")
      <*> (x .@? "PolicyId")
      <*> (x .@? "Path")
      <*> ( x .@? "PolicyVersionList" .!@ mempty
              >>= may (parseXMLList "member")
          )
      <*> (x .@? "CreateDate")
      <*> (x .@? "IsAttachable")
      <*> (x .@? "PermissionsBoundaryUsageCount")
      <*> (x .@? "DefaultVersionId")
      <*> (x .@? "AttachmentCount")
      <*> (x .@? "Description")

instance Hashable ManagedPolicyDetail

instance NFData ManagedPolicyDetail
