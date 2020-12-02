{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateMaintenanceWindowTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the target of an existing maintenance window. You can change the following:
--
--
--     * Name
--
--     * Description
--
--     * Owner
--
--     * IDs for an ID target
--
--     * Tags for a Tag target
--
--     * From any supported tag type to another. The three supported tag types are ID target, Tag target, and resource group. For more information, see 'Target' .
module Network.AWS.SSM.UpdateMaintenanceWindowTarget
  ( -- * Creating a Request
    updateMaintenanceWindowTarget,
    UpdateMaintenanceWindowTarget,

    -- * Request Lenses
    uReplace,
    uOwnerInformation,
    uName,
    uTargets,
    uDescription,
    uWindowId,
    uWindowTargetId,

    -- * Destructuring the Response
    updateMaintenanceWindowTargetResponse,
    UpdateMaintenanceWindowTargetResponse,

    -- * Response Lenses
    ursOwnerInformation,
    ursWindowTargetId,
    ursName,
    ursTargets,
    ursDescription,
    ursWindowId,
    ursResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types

-- | /See:/ 'updateMaintenanceWindowTarget' smart constructor.
data UpdateMaintenanceWindowTarget = UpdateMaintenanceWindowTarget'
  { _uReplace ::
      !(Maybe Bool),
    _uOwnerInformation ::
      !(Maybe (Sensitive Text)),
    _uName :: !(Maybe Text),
    _uTargets :: !(Maybe [Target]),
    _uDescription ::
      !(Maybe (Sensitive Text)),
    _uWindowId :: !Text,
    _uWindowTargetId :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateMaintenanceWindowTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uReplace' - If True, then all fields that are required by the RegisterTargetWithMaintenanceWindow action are also required for this API request. Optional fields that are not specified are set to null.
--
-- * 'uOwnerInformation' - User-provided value that will be included in any CloudWatch events raised while running tasks for these targets in this maintenance window.
--
-- * 'uName' - A name for the update.
--
-- * 'uTargets' - The targets to add or replace.
--
-- * 'uDescription' - An optional description for the update.
--
-- * 'uWindowId' - The maintenance window ID with which to modify the target.
--
-- * 'uWindowTargetId' - The target ID to modify.
updateMaintenanceWindowTarget ::
  -- | 'uWindowId'
  Text ->
  -- | 'uWindowTargetId'
  Text ->
  UpdateMaintenanceWindowTarget
updateMaintenanceWindowTarget pWindowId_ pWindowTargetId_ =
  UpdateMaintenanceWindowTarget'
    { _uReplace = Nothing,
      _uOwnerInformation = Nothing,
      _uName = Nothing,
      _uTargets = Nothing,
      _uDescription = Nothing,
      _uWindowId = pWindowId_,
      _uWindowTargetId = pWindowTargetId_
    }

-- | If True, then all fields that are required by the RegisterTargetWithMaintenanceWindow action are also required for this API request. Optional fields that are not specified are set to null.
uReplace :: Lens' UpdateMaintenanceWindowTarget (Maybe Bool)
uReplace = lens _uReplace (\s a -> s {_uReplace = a})

-- | User-provided value that will be included in any CloudWatch events raised while running tasks for these targets in this maintenance window.
uOwnerInformation :: Lens' UpdateMaintenanceWindowTarget (Maybe Text)
uOwnerInformation = lens _uOwnerInformation (\s a -> s {_uOwnerInformation = a}) . mapping _Sensitive

-- | A name for the update.
uName :: Lens' UpdateMaintenanceWindowTarget (Maybe Text)
uName = lens _uName (\s a -> s {_uName = a})

-- | The targets to add or replace.
uTargets :: Lens' UpdateMaintenanceWindowTarget [Target]
uTargets = lens _uTargets (\s a -> s {_uTargets = a}) . _Default . _Coerce

-- | An optional description for the update.
uDescription :: Lens' UpdateMaintenanceWindowTarget (Maybe Text)
uDescription = lens _uDescription (\s a -> s {_uDescription = a}) . mapping _Sensitive

-- | The maintenance window ID with which to modify the target.
uWindowId :: Lens' UpdateMaintenanceWindowTarget Text
uWindowId = lens _uWindowId (\s a -> s {_uWindowId = a})

-- | The target ID to modify.
uWindowTargetId :: Lens' UpdateMaintenanceWindowTarget Text
uWindowTargetId = lens _uWindowTargetId (\s a -> s {_uWindowTargetId = a})

instance AWSRequest UpdateMaintenanceWindowTarget where
  type
    Rs UpdateMaintenanceWindowTarget =
      UpdateMaintenanceWindowTargetResponse
  request = postJSON ssm
  response =
    receiveJSON
      ( \s h x ->
          UpdateMaintenanceWindowTargetResponse'
            <$> (x .?> "OwnerInformation")
            <*> (x .?> "WindowTargetId")
            <*> (x .?> "Name")
            <*> (x .?> "Targets" .!@ mempty)
            <*> (x .?> "Description")
            <*> (x .?> "WindowId")
            <*> (pure (fromEnum s))
      )

instance Hashable UpdateMaintenanceWindowTarget

instance NFData UpdateMaintenanceWindowTarget

instance ToHeaders UpdateMaintenanceWindowTarget where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonSSM.UpdateMaintenanceWindowTarget" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateMaintenanceWindowTarget where
  toJSON UpdateMaintenanceWindowTarget' {..} =
    object
      ( catMaybes
          [ ("Replace" .=) <$> _uReplace,
            ("OwnerInformation" .=) <$> _uOwnerInformation,
            ("Name" .=) <$> _uName,
            ("Targets" .=) <$> _uTargets,
            ("Description" .=) <$> _uDescription,
            Just ("WindowId" .= _uWindowId),
            Just ("WindowTargetId" .= _uWindowTargetId)
          ]
      )

instance ToPath UpdateMaintenanceWindowTarget where
  toPath = const "/"

instance ToQuery UpdateMaintenanceWindowTarget where
  toQuery = const mempty

-- | /See:/ 'updateMaintenanceWindowTargetResponse' smart constructor.
data UpdateMaintenanceWindowTargetResponse = UpdateMaintenanceWindowTargetResponse'
  { _ursOwnerInformation ::
      !( Maybe
           ( Sensitive
               Text
           )
       ),
    _ursWindowTargetId ::
      !(Maybe Text),
    _ursName ::
      !(Maybe Text),
    _ursTargets ::
      !( Maybe
           [Target]
       ),
    _ursDescription ::
      !( Maybe
           ( Sensitive
               Text
           )
       ),
    _ursWindowId ::
      !(Maybe Text),
    _ursResponseStatus ::
      !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateMaintenanceWindowTargetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ursOwnerInformation' - The updated owner.
--
-- * 'ursWindowTargetId' - The target ID specified in the update request.
--
-- * 'ursName' - The updated name.
--
-- * 'ursTargets' - The updated targets.
--
-- * 'ursDescription' - The updated description.
--
-- * 'ursWindowId' - The maintenance window ID specified in the update request.
--
-- * 'ursResponseStatus' - -- | The response status code.
updateMaintenanceWindowTargetResponse ::
  -- | 'ursResponseStatus'
  Int ->
  UpdateMaintenanceWindowTargetResponse
updateMaintenanceWindowTargetResponse pResponseStatus_ =
  UpdateMaintenanceWindowTargetResponse'
    { _ursOwnerInformation =
        Nothing,
      _ursWindowTargetId = Nothing,
      _ursName = Nothing,
      _ursTargets = Nothing,
      _ursDescription = Nothing,
      _ursWindowId = Nothing,
      _ursResponseStatus = pResponseStatus_
    }

-- | The updated owner.
ursOwnerInformation :: Lens' UpdateMaintenanceWindowTargetResponse (Maybe Text)
ursOwnerInformation = lens _ursOwnerInformation (\s a -> s {_ursOwnerInformation = a}) . mapping _Sensitive

-- | The target ID specified in the update request.
ursWindowTargetId :: Lens' UpdateMaintenanceWindowTargetResponse (Maybe Text)
ursWindowTargetId = lens _ursWindowTargetId (\s a -> s {_ursWindowTargetId = a})

-- | The updated name.
ursName :: Lens' UpdateMaintenanceWindowTargetResponse (Maybe Text)
ursName = lens _ursName (\s a -> s {_ursName = a})

-- | The updated targets.
ursTargets :: Lens' UpdateMaintenanceWindowTargetResponse [Target]
ursTargets = lens _ursTargets (\s a -> s {_ursTargets = a}) . _Default . _Coerce

-- | The updated description.
ursDescription :: Lens' UpdateMaintenanceWindowTargetResponse (Maybe Text)
ursDescription = lens _ursDescription (\s a -> s {_ursDescription = a}) . mapping _Sensitive

-- | The maintenance window ID specified in the update request.
ursWindowId :: Lens' UpdateMaintenanceWindowTargetResponse (Maybe Text)
ursWindowId = lens _ursWindowId (\s a -> s {_ursWindowId = a})

-- | -- | The response status code.
ursResponseStatus :: Lens' UpdateMaintenanceWindowTargetResponse Int
ursResponseStatus = lens _ursResponseStatus (\s a -> s {_ursResponseStatus = a})

instance NFData UpdateMaintenanceWindowTargetResponse
