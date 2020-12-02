{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowIdentityForTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowIdentityForTarget where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The maintenance window to which the specified target belongs.
--
--
--
-- /See:/ 'maintenanceWindowIdentityForTarget' smart constructor.
data MaintenanceWindowIdentityForTarget = MaintenanceWindowIdentityForTarget'
  { _mwiftName ::
      !(Maybe Text),
    _mwiftWindowId ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MaintenanceWindowIdentityForTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwiftName' - The name of the maintenance window.
--
-- * 'mwiftWindowId' - The ID of the maintenance window.
maintenanceWindowIdentityForTarget ::
  MaintenanceWindowIdentityForTarget
maintenanceWindowIdentityForTarget =
  MaintenanceWindowIdentityForTarget'
    { _mwiftName = Nothing,
      _mwiftWindowId = Nothing
    }

-- | The name of the maintenance window.
mwiftName :: Lens' MaintenanceWindowIdentityForTarget (Maybe Text)
mwiftName = lens _mwiftName (\s a -> s {_mwiftName = a})

-- | The ID of the maintenance window.
mwiftWindowId :: Lens' MaintenanceWindowIdentityForTarget (Maybe Text)
mwiftWindowId = lens _mwiftWindowId (\s a -> s {_mwiftWindowId = a})

instance FromJSON MaintenanceWindowIdentityForTarget where
  parseJSON =
    withObject
      "MaintenanceWindowIdentityForTarget"
      ( \x ->
          MaintenanceWindowIdentityForTarget'
            <$> (x .:? "Name") <*> (x .:? "WindowId")
      )

instance Hashable MaintenanceWindowIdentityForTarget

instance NFData MaintenanceWindowIdentityForTarget
