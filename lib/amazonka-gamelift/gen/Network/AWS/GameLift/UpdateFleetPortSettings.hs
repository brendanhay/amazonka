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
-- Module      : Network.AWS.GameLift.UpdateFleetPortSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates port settings for a fleet. To update settings, specify the fleet ID to be updated and list the permissions you want to update. List the permissions you want to add in @InboundPermissionAuthorizations@ , and permissions you want to remove in @InboundPermissionRevocations@ . Permissions to be removed must match existing fleet permissions. If successful, the fleet ID for the updated fleet is returned.
--
--
-- __Learn more__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets>
--
-- __Related operations__
--
--     * 'CreateFleet'
--
--     * 'ListFleets'
--
--     * 'DeleteFleet'
--
--     * 'DescribeFleetAttributes'
--
--     * Update fleets:
--
--     * 'UpdateFleetAttributes'
--
--     * 'UpdateFleetCapacity'
--
--     * 'UpdateFleetPortSettings'
--
--     * 'UpdateRuntimeConfiguration'
--
--
--
--     * 'StartFleetActions' or 'StopFleetActions'
module Network.AWS.GameLift.UpdateFleetPortSettings
  ( -- * Creating a Request
    updateFleetPortSettings,
    UpdateFleetPortSettings,

    -- * Request Lenses
    ufpsInboundPermissionRevocations,
    ufpsInboundPermissionAuthorizations,
    ufpsFleetId,

    -- * Destructuring the Response
    updateFleetPortSettingsResponse,
    UpdateFleetPortSettingsResponse,

    -- * Response Lenses
    ufpsrsFleetId,
    ufpsrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request operation.
--
--
--
-- /See:/ 'updateFleetPortSettings' smart constructor.
data UpdateFleetPortSettings = UpdateFleetPortSettings'
  { _ufpsInboundPermissionRevocations ::
      !(Maybe [IPPermission]),
    _ufpsInboundPermissionAuthorizations ::
      !(Maybe [IPPermission]),
    _ufpsFleetId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateFleetPortSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufpsInboundPermissionRevocations' - A collection of port settings to be removed from the fleet resource.
--
-- * 'ufpsInboundPermissionAuthorizations' - A collection of port settings to be added to the fleet resource.
--
-- * 'ufpsFleetId' - A unique identifier for a fleet to update port settings for. You can use either the fleet ID or ARN value.
updateFleetPortSettings ::
  -- | 'ufpsFleetId'
  Text ->
  UpdateFleetPortSettings
updateFleetPortSettings pFleetId_ =
  UpdateFleetPortSettings'
    { _ufpsInboundPermissionRevocations =
        Nothing,
      _ufpsInboundPermissionAuthorizations = Nothing,
      _ufpsFleetId = pFleetId_
    }

-- | A collection of port settings to be removed from the fleet resource.
ufpsInboundPermissionRevocations :: Lens' UpdateFleetPortSettings [IPPermission]
ufpsInboundPermissionRevocations = lens _ufpsInboundPermissionRevocations (\s a -> s {_ufpsInboundPermissionRevocations = a}) . _Default . _Coerce

-- | A collection of port settings to be added to the fleet resource.
ufpsInboundPermissionAuthorizations :: Lens' UpdateFleetPortSettings [IPPermission]
ufpsInboundPermissionAuthorizations = lens _ufpsInboundPermissionAuthorizations (\s a -> s {_ufpsInboundPermissionAuthorizations = a}) . _Default . _Coerce

-- | A unique identifier for a fleet to update port settings for. You can use either the fleet ID or ARN value.
ufpsFleetId :: Lens' UpdateFleetPortSettings Text
ufpsFleetId = lens _ufpsFleetId (\s a -> s {_ufpsFleetId = a})

instance AWSRequest UpdateFleetPortSettings where
  type Rs UpdateFleetPortSettings = UpdateFleetPortSettingsResponse
  request = postJSON gameLift
  response =
    receiveJSON
      ( \s h x ->
          UpdateFleetPortSettingsResponse'
            <$> (x .?> "FleetId") <*> (pure (fromEnum s))
      )

instance Hashable UpdateFleetPortSettings

instance NFData UpdateFleetPortSettings

instance ToHeaders UpdateFleetPortSettings where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("GameLift.UpdateFleetPortSettings" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateFleetPortSettings where
  toJSON UpdateFleetPortSettings' {..} =
    object
      ( catMaybes
          [ ("InboundPermissionRevocations" .=)
              <$> _ufpsInboundPermissionRevocations,
            ("InboundPermissionAuthorizations" .=)
              <$> _ufpsInboundPermissionAuthorizations,
            Just ("FleetId" .= _ufpsFleetId)
          ]
      )

instance ToPath UpdateFleetPortSettings where
  toPath = const "/"

instance ToQuery UpdateFleetPortSettings where
  toQuery = const mempty

-- | Represents the returned data in response to a request operation.
--
--
--
-- /See:/ 'updateFleetPortSettingsResponse' smart constructor.
data UpdateFleetPortSettingsResponse = UpdateFleetPortSettingsResponse'
  { _ufpsrsFleetId ::
      !(Maybe Text),
    _ufpsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateFleetPortSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ufpsrsFleetId' - A unique identifier for a fleet that was updated.
--
-- * 'ufpsrsResponseStatus' - -- | The response status code.
updateFleetPortSettingsResponse ::
  -- | 'ufpsrsResponseStatus'
  Int ->
  UpdateFleetPortSettingsResponse
updateFleetPortSettingsResponse pResponseStatus_ =
  UpdateFleetPortSettingsResponse'
    { _ufpsrsFleetId = Nothing,
      _ufpsrsResponseStatus = pResponseStatus_
    }

-- | A unique identifier for a fleet that was updated.
ufpsrsFleetId :: Lens' UpdateFleetPortSettingsResponse (Maybe Text)
ufpsrsFleetId = lens _ufpsrsFleetId (\s a -> s {_ufpsrsFleetId = a})

-- | -- | The response status code.
ufpsrsResponseStatus :: Lens' UpdateFleetPortSettingsResponse Int
ufpsrsResponseStatus = lens _ufpsrsResponseStatus (\s a -> s {_ufpsrsResponseStatus = a})

instance NFData UpdateFleetPortSettingsResponse
