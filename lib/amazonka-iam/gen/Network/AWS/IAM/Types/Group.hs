{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.Group
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.Group where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an IAM group entity.
--
--
-- This data type is used as a response element in the following operations:
--
--     * 'CreateGroup'
--
--     * 'GetGroup'
--
--     * 'ListGroups'
--
--
--
--
-- /See:/ 'group'' smart constructor.
data Group = Group'
  { _gPath :: !Text,
    _gGroupName :: !Text,
    _gGroupId :: !Text,
    _gARN :: !Text,
    _gCreateDate :: !ISO8601
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Group' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gPath' - The path to the group. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- * 'gGroupName' - The friendly name that identifies the group.
--
-- * 'gGroupId' - The stable and unique string identifying the group. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- * 'gARN' - The Amazon Resource Name (ARN) specifying the group. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- * 'gCreateDate' - The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the group was created.
group' ::
  -- | 'gPath'
  Text ->
  -- | 'gGroupName'
  Text ->
  -- | 'gGroupId'
  Text ->
  -- | 'gARN'
  Text ->
  -- | 'gCreateDate'
  UTCTime ->
  Group
group' pPath_ pGroupName_ pGroupId_ pARN_ pCreateDate_ =
  Group'
    { _gPath = pPath_,
      _gGroupName = pGroupName_,
      _gGroupId = pGroupId_,
      _gARN = pARN_,
      _gCreateDate = _Time # pCreateDate_
    }

-- | The path to the group. For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
gPath :: Lens' Group Text
gPath = lens _gPath (\s a -> s {_gPath = a})

-- | The friendly name that identifies the group.
gGroupName :: Lens' Group Text
gGroupName = lens _gGroupName (\s a -> s {_gGroupName = a})

-- | The stable and unique string identifying the group. For more information about IDs, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
gGroupId :: Lens' Group Text
gGroupId = lens _gGroupId (\s a -> s {_gGroupId = a})

-- | The Amazon Resource Name (ARN) specifying the group. For more information about ARNs and how to use them in policies, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
gARN :: Lens' Group Text
gARN = lens _gARN (\s a -> s {_gARN = a})

-- | The date and time, in <http://www.iso.org/iso/iso8601 ISO 8601 date-time format> , when the group was created.
gCreateDate :: Lens' Group UTCTime
gCreateDate = lens _gCreateDate (\s a -> s {_gCreateDate = a}) . _Time

instance FromXML Group where
  parseXML x =
    Group'
      <$> (x .@ "Path")
      <*> (x .@ "GroupName")
      <*> (x .@ "GroupId")
      <*> (x .@ "Arn")
      <*> (x .@ "CreateDate")

instance Hashable Group

instance NFData Group
