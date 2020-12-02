{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.PermissionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.PermissionInfo where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkDocs.Types.RolePermissionType
import Network.AWS.WorkDocs.Types.RoleType

-- | Describes the permissions.
--
--
--
-- /See:/ 'permissionInfo' smart constructor.
data PermissionInfo = PermissionInfo'
  { _piRole :: !(Maybe RoleType),
    _piType :: !(Maybe RolePermissionType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PermissionInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piRole' - The role of the user.
--
-- * 'piType' - The type of permissions.
permissionInfo ::
  PermissionInfo
permissionInfo =
  PermissionInfo' {_piRole = Nothing, _piType = Nothing}

-- | The role of the user.
piRole :: Lens' PermissionInfo (Maybe RoleType)
piRole = lens _piRole (\s a -> s {_piRole = a})

-- | The type of permissions.
piType :: Lens' PermissionInfo (Maybe RolePermissionType)
piType = lens _piType (\s a -> s {_piType = a})

instance FromJSON PermissionInfo where
  parseJSON =
    withObject
      "PermissionInfo"
      (\x -> PermissionInfo' <$> (x .:? "Role") <*> (x .:? "Type"))

instance Hashable PermissionInfo

instance NFData PermissionInfo
