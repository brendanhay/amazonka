{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowTarget where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.MaintenanceWindowResourceType
import Network.AWS.SSM.Types.Target

-- | The target registered with the maintenance window.
--
--
--
-- /See:/ 'maintenanceWindowTarget' smart constructor.
data MaintenanceWindowTarget = MaintenanceWindowTarget'
  { _mResourceType ::
      !(Maybe MaintenanceWindowResourceType),
    _mOwnerInformation ::
      !(Maybe (Sensitive Text)),
    _mWindowTargetId :: !(Maybe Text),
    _mName :: !(Maybe Text),
    _mTargets :: !(Maybe [Target]),
    _mDescription :: !(Maybe (Sensitive Text)),
    _mWindowId :: !(Maybe Text)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'MaintenanceWindowTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mResourceType' - The type of target that is being registered with the maintenance window.
--
-- * 'mOwnerInformation' - A user-provided value that will be included in any CloudWatch events that are raised while running tasks for these targets in this maintenance window.
--
-- * 'mWindowTargetId' - The ID of the target.
--
-- * 'mName' - The name for the maintenance window target.
--
-- * 'mTargets' - The targets, either instances or tags. Specify instances using the following format: @Key=instanceids,Values=<instanceid1>,<instanceid2>@  Tags are specified using the following format: @Key=<tag name>,Values=<tag value>@ .
--
-- * 'mDescription' - A description for the target.
--
-- * 'mWindowId' - The ID of the maintenance window to register the target with.
maintenanceWindowTarget ::
  MaintenanceWindowTarget
maintenanceWindowTarget =
  MaintenanceWindowTarget'
    { _mResourceType = Nothing,
      _mOwnerInformation = Nothing,
      _mWindowTargetId = Nothing,
      _mName = Nothing,
      _mTargets = Nothing,
      _mDescription = Nothing,
      _mWindowId = Nothing
    }

-- | The type of target that is being registered with the maintenance window.
mResourceType :: Lens' MaintenanceWindowTarget (Maybe MaintenanceWindowResourceType)
mResourceType = lens _mResourceType (\s a -> s {_mResourceType = a})

-- | A user-provided value that will be included in any CloudWatch events that are raised while running tasks for these targets in this maintenance window.
mOwnerInformation :: Lens' MaintenanceWindowTarget (Maybe Text)
mOwnerInformation = lens _mOwnerInformation (\s a -> s {_mOwnerInformation = a}) . mapping _Sensitive

-- | The ID of the target.
mWindowTargetId :: Lens' MaintenanceWindowTarget (Maybe Text)
mWindowTargetId = lens _mWindowTargetId (\s a -> s {_mWindowTargetId = a})

-- | The name for the maintenance window target.
mName :: Lens' MaintenanceWindowTarget (Maybe Text)
mName = lens _mName (\s a -> s {_mName = a})

-- | The targets, either instances or tags. Specify instances using the following format: @Key=instanceids,Values=<instanceid1>,<instanceid2>@  Tags are specified using the following format: @Key=<tag name>,Values=<tag value>@ .
mTargets :: Lens' MaintenanceWindowTarget [Target]
mTargets = lens _mTargets (\s a -> s {_mTargets = a}) . _Default . _Coerce

-- | A description for the target.
mDescription :: Lens' MaintenanceWindowTarget (Maybe Text)
mDescription = lens _mDescription (\s a -> s {_mDescription = a}) . mapping _Sensitive

-- | The ID of the maintenance window to register the target with.
mWindowId :: Lens' MaintenanceWindowTarget (Maybe Text)
mWindowId = lens _mWindowId (\s a -> s {_mWindowId = a})

instance FromJSON MaintenanceWindowTarget where
  parseJSON =
    withObject
      "MaintenanceWindowTarget"
      ( \x ->
          MaintenanceWindowTarget'
            <$> (x .:? "ResourceType")
            <*> (x .:? "OwnerInformation")
            <*> (x .:? "WindowTargetId")
            <*> (x .:? "Name")
            <*> (x .:? "Targets" .!= mempty)
            <*> (x .:? "Description")
            <*> (x .:? "WindowId")
      )

instance Hashable MaintenanceWindowTarget

instance NFData MaintenanceWindowTarget
