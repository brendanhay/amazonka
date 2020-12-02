{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.SelfservicePermissions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.SelfservicePermissions where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WorkSpaces.Types.ReconnectEnum

-- | Describes the self-service permissions for a directory. For more information, see <https://docs.aws.amazon.com/workspaces/latest/adminguide/enable-user-self-service-workspace-management.html Enable Self-Service WorkSpace Management Capabilities for Your Users> .
--
--
--
-- /See:/ 'selfservicePermissions' smart constructor.
data SelfservicePermissions = SelfservicePermissions'
  { _spRestartWorkspace ::
      !(Maybe ReconnectEnum),
    _spChangeComputeType ::
      !(Maybe ReconnectEnum),
    _spSwitchRunningMode ::
      !(Maybe ReconnectEnum),
    _spRebuildWorkspace :: !(Maybe ReconnectEnum),
    _spIncreaseVolumeSize ::
      !(Maybe ReconnectEnum)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SelfservicePermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spRestartWorkspace' - Specifies whether users can restart their WorkSpace.
--
-- * 'spChangeComputeType' - Specifies whether users can change the compute type (bundle) for their WorkSpace.
--
-- * 'spSwitchRunningMode' - Specifies whether users can switch the running mode of their WorkSpace.
--
-- * 'spRebuildWorkspace' - Specifies whether users can rebuild the operating system of a WorkSpace to its original state.
--
-- * 'spIncreaseVolumeSize' - Specifies whether users can increase the volume size of the drives on their WorkSpace.
selfservicePermissions ::
  SelfservicePermissions
selfservicePermissions =
  SelfservicePermissions'
    { _spRestartWorkspace = Nothing,
      _spChangeComputeType = Nothing,
      _spSwitchRunningMode = Nothing,
      _spRebuildWorkspace = Nothing,
      _spIncreaseVolumeSize = Nothing
    }

-- | Specifies whether users can restart their WorkSpace.
spRestartWorkspace :: Lens' SelfservicePermissions (Maybe ReconnectEnum)
spRestartWorkspace = lens _spRestartWorkspace (\s a -> s {_spRestartWorkspace = a})

-- | Specifies whether users can change the compute type (bundle) for their WorkSpace.
spChangeComputeType :: Lens' SelfservicePermissions (Maybe ReconnectEnum)
spChangeComputeType = lens _spChangeComputeType (\s a -> s {_spChangeComputeType = a})

-- | Specifies whether users can switch the running mode of their WorkSpace.
spSwitchRunningMode :: Lens' SelfservicePermissions (Maybe ReconnectEnum)
spSwitchRunningMode = lens _spSwitchRunningMode (\s a -> s {_spSwitchRunningMode = a})

-- | Specifies whether users can rebuild the operating system of a WorkSpace to its original state.
spRebuildWorkspace :: Lens' SelfservicePermissions (Maybe ReconnectEnum)
spRebuildWorkspace = lens _spRebuildWorkspace (\s a -> s {_spRebuildWorkspace = a})

-- | Specifies whether users can increase the volume size of the drives on their WorkSpace.
spIncreaseVolumeSize :: Lens' SelfservicePermissions (Maybe ReconnectEnum)
spIncreaseVolumeSize = lens _spIncreaseVolumeSize (\s a -> s {_spIncreaseVolumeSize = a})

instance FromJSON SelfservicePermissions where
  parseJSON =
    withObject
      "SelfservicePermissions"
      ( \x ->
          SelfservicePermissions'
            <$> (x .:? "RestartWorkspace")
            <*> (x .:? "ChangeComputeType")
            <*> (x .:? "SwitchRunningMode")
            <*> (x .:? "RebuildWorkspace")
            <*> (x .:? "IncreaseVolumeSize")
      )

instance Hashable SelfservicePermissions

instance NFData SelfservicePermissions

instance ToJSON SelfservicePermissions where
  toJSON SelfservicePermissions' {..} =
    object
      ( catMaybes
          [ ("RestartWorkspace" .=) <$> _spRestartWorkspace,
            ("ChangeComputeType" .=) <$> _spChangeComputeType,
            ("SwitchRunningMode" .=) <$> _spSwitchRunningMode,
            ("RebuildWorkspace" .=) <$> _spRebuildWorkspace,
            ("IncreaseVolumeSize" .=) <$> _spIncreaseVolumeSize
          ]
      )
