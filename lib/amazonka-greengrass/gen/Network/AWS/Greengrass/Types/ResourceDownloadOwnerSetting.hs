{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.ResourceDownloadOwnerSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.ResourceDownloadOwnerSetting where

import Network.AWS.Greengrass.Types.Permission
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The owner setting for downloaded machine learning resources.
--
-- /See:/ 'resourceDownloadOwnerSetting' smart constructor.
data ResourceDownloadOwnerSetting = ResourceDownloadOwnerSetting'
  { _rdosGroupOwner ::
      !Text,
    _rdosGroupPermission ::
      !Permission
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceDownloadOwnerSetting' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdosGroupOwner' - The group owner of the resource. This is the name of an existing Linux OS group on the system or a GID. The group's permissions are added to the Lambda process.
--
-- * 'rdosGroupPermission' - The permissions that the group owner has to the resource. Valid values are ''rw'' (read/write) or ''ro'' (read-only).
resourceDownloadOwnerSetting ::
  -- | 'rdosGroupOwner'
  Text ->
  -- | 'rdosGroupPermission'
  Permission ->
  ResourceDownloadOwnerSetting
resourceDownloadOwnerSetting pGroupOwner_ pGroupPermission_ =
  ResourceDownloadOwnerSetting'
    { _rdosGroupOwner = pGroupOwner_,
      _rdosGroupPermission = pGroupPermission_
    }

-- | The group owner of the resource. This is the name of an existing Linux OS group on the system or a GID. The group's permissions are added to the Lambda process.
rdosGroupOwner :: Lens' ResourceDownloadOwnerSetting Text
rdosGroupOwner = lens _rdosGroupOwner (\s a -> s {_rdosGroupOwner = a})

-- | The permissions that the group owner has to the resource. Valid values are ''rw'' (read/write) or ''ro'' (read-only).
rdosGroupPermission :: Lens' ResourceDownloadOwnerSetting Permission
rdosGroupPermission = lens _rdosGroupPermission (\s a -> s {_rdosGroupPermission = a})

instance FromJSON ResourceDownloadOwnerSetting where
  parseJSON =
    withObject
      "ResourceDownloadOwnerSetting"
      ( \x ->
          ResourceDownloadOwnerSetting'
            <$> (x .: "GroupOwner") <*> (x .: "GroupPermission")
      )

instance Hashable ResourceDownloadOwnerSetting

instance NFData ResourceDownloadOwnerSetting

instance ToJSON ResourceDownloadOwnerSetting where
  toJSON ResourceDownloadOwnerSetting' {..} =
    object
      ( catMaybes
          [ Just ("GroupOwner" .= _rdosGroupOwner),
            Just ("GroupPermission" .= _rdosGroupPermission)
          ]
      )
