{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CreateVolumePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CreateVolumePermission where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PermissionGroup
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the user or group to be added or removed from the list of create volume permissions for a volume.
--
--
--
-- /See:/ 'createVolumePermission' smart constructor.
data CreateVolumePermission = CreateVolumePermission'
  { _cvpGroup ::
      !(Maybe PermissionGroup),
    _cvpUserId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateVolumePermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvpGroup' - The group to be added or removed. The possible value is @all@ .
--
-- * 'cvpUserId' - The AWS account ID to be added or removed.
createVolumePermission ::
  CreateVolumePermission
createVolumePermission =
  CreateVolumePermission'
    { _cvpGroup = Nothing,
      _cvpUserId = Nothing
    }

-- | The group to be added or removed. The possible value is @all@ .
cvpGroup :: Lens' CreateVolumePermission (Maybe PermissionGroup)
cvpGroup = lens _cvpGroup (\s a -> s {_cvpGroup = a})

-- | The AWS account ID to be added or removed.
cvpUserId :: Lens' CreateVolumePermission (Maybe Text)
cvpUserId = lens _cvpUserId (\s a -> s {_cvpUserId = a})

instance FromXML CreateVolumePermission where
  parseXML x =
    CreateVolumePermission' <$> (x .@? "group") <*> (x .@? "userId")

instance Hashable CreateVolumePermission

instance NFData CreateVolumePermission

instance ToQuery CreateVolumePermission where
  toQuery CreateVolumePermission' {..} =
    mconcat ["Group" =: _cvpGroup, "UserId" =: _cvpUserId]
