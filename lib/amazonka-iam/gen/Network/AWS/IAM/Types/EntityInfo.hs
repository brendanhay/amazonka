{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.EntityInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.EntityInfo where

import Network.AWS.IAM.Types.PolicyOwnerEntityType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains details about the specified entity (user or role).
--
--
-- This data type is an element of the 'EntityDetails' object.
--
--
-- /See:/ 'entityInfo' smart constructor.
data EntityInfo = EntityInfo'
  { _eiPath :: !(Maybe Text),
    _eiARN :: !Text,
    _eiName :: !Text,
    _eiType :: !PolicyOwnerEntityType,
    _eiId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EntityInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiPath' - The path to the entity (user or role). For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
--
-- * 'eiARN' - Undocumented member.
--
-- * 'eiName' - The name of the entity (user or role).
--
-- * 'eiType' - The type of entity (user or role).
--
-- * 'eiId' - The identifier of the entity (user or role).
entityInfo ::
  -- | 'eiARN'
  Text ->
  -- | 'eiName'
  Text ->
  -- | 'eiType'
  PolicyOwnerEntityType ->
  -- | 'eiId'
  Text ->
  EntityInfo
entityInfo pARN_ pName_ pType_ pId_ =
  EntityInfo'
    { _eiPath = Nothing,
      _eiARN = pARN_,
      _eiName = pName_,
      _eiType = pType_,
      _eiId = pId_
    }

-- | The path to the entity (user or role). For more information about paths, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ .
eiPath :: Lens' EntityInfo (Maybe Text)
eiPath = lens _eiPath (\s a -> s {_eiPath = a})

-- | Undocumented member.
eiARN :: Lens' EntityInfo Text
eiARN = lens _eiARN (\s a -> s {_eiARN = a})

-- | The name of the entity (user or role).
eiName :: Lens' EntityInfo Text
eiName = lens _eiName (\s a -> s {_eiName = a})

-- | The type of entity (user or role).
eiType :: Lens' EntityInfo PolicyOwnerEntityType
eiType = lens _eiType (\s a -> s {_eiType = a})

-- | The identifier of the entity (user or role).
eiId :: Lens' EntityInfo Text
eiId = lens _eiId (\s a -> s {_eiId = a})

instance FromXML EntityInfo where
  parseXML x =
    EntityInfo'
      <$> (x .@? "Path")
      <*> (x .@ "Arn")
      <*> (x .@ "Name")
      <*> (x .@ "Type")
      <*> (x .@ "Id")

instance Hashable EntityInfo

instance NFData EntityInfo
