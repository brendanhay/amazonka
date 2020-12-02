{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.InstanceProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.InstanceProfile where

import Network.AWS.IAM.Types.Role
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an instance profile.
--
--
-- This data type is used as a response element in the following operations:
--
--     * 'CreateInstanceProfile'
--
--     * 'GetInstanceProfile'
--
--     * 'ListInstanceProfiles'
--
--     * 'ListInstanceProfilesForRole'
--
--
--
--
-- /See:/ 'instanceProfile' smart constructor.
data InstanceProfile = InstanceProfile'
  { _ipPath :: !Text,
    _ipInstanceProfileName :: !Text,
    _ipInstanceProfileId :: !Text,
    _ipARN :: !Text,
    _ipCreateDate :: !ISO8601,
    _ipRoles :: ![Role]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstanceProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipPath' - The path to the instance profile. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- * 'ipInstanceProfileName' - The name identifying the instance profile.
--
-- * 'ipInstanceProfileId' - The stable and unique string identifying the instance profile. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- * 'ipARN' - The Amazon Resource Name (ARN) specifying the instance profile. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- * 'ipCreateDate' - The date when the instance profile was created.
--
-- * 'ipRoles' - The role associated with the instance profile.
instanceProfile ::
  -- | 'ipPath'
  Text ->
  -- | 'ipInstanceProfileName'
  Text ->
  -- | 'ipInstanceProfileId'
  Text ->
  -- | 'ipARN'
  Text ->
  -- | 'ipCreateDate'
  UTCTime ->
  InstanceProfile
instanceProfile
  pPath_
  pInstanceProfileName_
  pInstanceProfileId_
  pARN_
  pCreateDate_ =
    InstanceProfile'
      { _ipPath = pPath_,
        _ipInstanceProfileName = pInstanceProfileName_,
        _ipInstanceProfileId = pInstanceProfileId_,
        _ipARN = pARN_,
        _ipCreateDate = _Time # pCreateDate_,
        _ipRoles = mempty
      }

-- | The path to the instance profile. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
ipPath :: Lens' InstanceProfile Text
ipPath = lens _ipPath (\s a -> s {_ipPath = a})

-- | The name identifying the instance profile.
ipInstanceProfileName :: Lens' InstanceProfile Text
ipInstanceProfileName = lens _ipInstanceProfileName (\s a -> s {_ipInstanceProfileName = a})

-- | The stable and unique string identifying the instance profile. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
ipInstanceProfileId :: Lens' InstanceProfile Text
ipInstanceProfileId = lens _ipInstanceProfileId (\s a -> s {_ipInstanceProfileId = a})

-- | The Amazon Resource Name (ARN) specifying the instance profile. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
ipARN :: Lens' InstanceProfile Text
ipARN = lens _ipARN (\s a -> s {_ipARN = a})

-- | The date when the instance profile was created.
ipCreateDate :: Lens' InstanceProfile UTCTime
ipCreateDate = lens _ipCreateDate (\s a -> s {_ipCreateDate = a}) . _Time

-- | The role associated with the instance profile.
ipRoles :: Lens' InstanceProfile [Role]
ipRoles = lens _ipRoles (\s a -> s {_ipRoles = a}) . _Coerce

instance FromXML InstanceProfile where
  parseXML x =
    InstanceProfile'
      <$> (x .@ "Path")
      <*> (x .@ "InstanceProfileName")
      <*> (x .@ "InstanceProfileId")
      <*> (x .@ "Arn")
      <*> (x .@ "CreateDate")
      <*> (x .@? "Roles" .!@ mempty >>= parseXMLList "member")

instance Hashable InstanceProfile

instance NFData InstanceProfile
