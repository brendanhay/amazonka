{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing room profile by room profile ARN.
module Network.AWS.AlexaBusiness.UpdateProfile
  ( -- * Creating a Request
    UpdateProfile (..),
    newUpdateProfile,

    -- * Request Lenses
    updateProfile_profileName,
    updateProfile_isDefault,
    updateProfile_address,
    updateProfile_locale,
    updateProfile_temperatureUnit,
    updateProfile_setupModeDisabled,
    updateProfile_pSTNEnabled,
    updateProfile_maxVolumeLimit,
    updateProfile_meetingRoomConfiguration,
    updateProfile_wakeWord,
    updateProfile_profileArn,
    updateProfile_timezone,
    updateProfile_distanceUnit,

    -- * Destructuring the Response
    UpdateProfileResponse (..),
    newUpdateProfileResponse,

    -- * Response Lenses
    updateProfileResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateProfile' smart constructor.
data UpdateProfile = UpdateProfile'
  { -- | The updated name for the room profile.
    profileName :: Core.Maybe Core.Text,
    -- | Sets the profile as default if selected. If this is missing, no update
    -- is done to the default status.
    isDefault :: Core.Maybe Core.Bool,
    -- | The updated address for the room profile.
    address :: Core.Maybe Core.Text,
    -- | The updated locale for the room profile. (This is currently only
    -- available to a limited preview audience.)
    locale :: Core.Maybe Core.Text,
    -- | The updated temperature unit for the room profile.
    temperatureUnit :: Core.Maybe TemperatureUnit,
    -- | Whether the setup mode of the profile is enabled.
    setupModeDisabled :: Core.Maybe Core.Bool,
    -- | Whether the PSTN setting of the room profile is enabled.
    pSTNEnabled :: Core.Maybe Core.Bool,
    -- | The updated maximum volume limit for the room profile.
    maxVolumeLimit :: Core.Maybe Core.Int,
    -- | The updated meeting room settings of a room profile.
    meetingRoomConfiguration :: Core.Maybe UpdateMeetingRoomConfiguration,
    -- | The updated wake word for the room profile.
    wakeWord :: Core.Maybe WakeWord,
    -- | The ARN of the room profile to update. Required.
    profileArn :: Core.Maybe Core.Text,
    -- | The updated timezone for the room profile.
    timezone :: Core.Maybe Core.Text,
    -- | The updated distance unit for the room profile.
    distanceUnit :: Core.Maybe DistanceUnit
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileName', 'updateProfile_profileName' - The updated name for the room profile.
--
-- 'isDefault', 'updateProfile_isDefault' - Sets the profile as default if selected. If this is missing, no update
-- is done to the default status.
--
-- 'address', 'updateProfile_address' - The updated address for the room profile.
--
-- 'locale', 'updateProfile_locale' - The updated locale for the room profile. (This is currently only
-- available to a limited preview audience.)
--
-- 'temperatureUnit', 'updateProfile_temperatureUnit' - The updated temperature unit for the room profile.
--
-- 'setupModeDisabled', 'updateProfile_setupModeDisabled' - Whether the setup mode of the profile is enabled.
--
-- 'pSTNEnabled', 'updateProfile_pSTNEnabled' - Whether the PSTN setting of the room profile is enabled.
--
-- 'maxVolumeLimit', 'updateProfile_maxVolumeLimit' - The updated maximum volume limit for the room profile.
--
-- 'meetingRoomConfiguration', 'updateProfile_meetingRoomConfiguration' - The updated meeting room settings of a room profile.
--
-- 'wakeWord', 'updateProfile_wakeWord' - The updated wake word for the room profile.
--
-- 'profileArn', 'updateProfile_profileArn' - The ARN of the room profile to update. Required.
--
-- 'timezone', 'updateProfile_timezone' - The updated timezone for the room profile.
--
-- 'distanceUnit', 'updateProfile_distanceUnit' - The updated distance unit for the room profile.
newUpdateProfile ::
  UpdateProfile
newUpdateProfile =
  UpdateProfile'
    { profileName = Core.Nothing,
      isDefault = Core.Nothing,
      address = Core.Nothing,
      locale = Core.Nothing,
      temperatureUnit = Core.Nothing,
      setupModeDisabled = Core.Nothing,
      pSTNEnabled = Core.Nothing,
      maxVolumeLimit = Core.Nothing,
      meetingRoomConfiguration = Core.Nothing,
      wakeWord = Core.Nothing,
      profileArn = Core.Nothing,
      timezone = Core.Nothing,
      distanceUnit = Core.Nothing
    }

-- | The updated name for the room profile.
updateProfile_profileName :: Lens.Lens' UpdateProfile (Core.Maybe Core.Text)
updateProfile_profileName = Lens.lens (\UpdateProfile' {profileName} -> profileName) (\s@UpdateProfile' {} a -> s {profileName = a} :: UpdateProfile)

-- | Sets the profile as default if selected. If this is missing, no update
-- is done to the default status.
updateProfile_isDefault :: Lens.Lens' UpdateProfile (Core.Maybe Core.Bool)
updateProfile_isDefault = Lens.lens (\UpdateProfile' {isDefault} -> isDefault) (\s@UpdateProfile' {} a -> s {isDefault = a} :: UpdateProfile)

-- | The updated address for the room profile.
updateProfile_address :: Lens.Lens' UpdateProfile (Core.Maybe Core.Text)
updateProfile_address = Lens.lens (\UpdateProfile' {address} -> address) (\s@UpdateProfile' {} a -> s {address = a} :: UpdateProfile)

-- | The updated locale for the room profile. (This is currently only
-- available to a limited preview audience.)
updateProfile_locale :: Lens.Lens' UpdateProfile (Core.Maybe Core.Text)
updateProfile_locale = Lens.lens (\UpdateProfile' {locale} -> locale) (\s@UpdateProfile' {} a -> s {locale = a} :: UpdateProfile)

-- | The updated temperature unit for the room profile.
updateProfile_temperatureUnit :: Lens.Lens' UpdateProfile (Core.Maybe TemperatureUnit)
updateProfile_temperatureUnit = Lens.lens (\UpdateProfile' {temperatureUnit} -> temperatureUnit) (\s@UpdateProfile' {} a -> s {temperatureUnit = a} :: UpdateProfile)

-- | Whether the setup mode of the profile is enabled.
updateProfile_setupModeDisabled :: Lens.Lens' UpdateProfile (Core.Maybe Core.Bool)
updateProfile_setupModeDisabled = Lens.lens (\UpdateProfile' {setupModeDisabled} -> setupModeDisabled) (\s@UpdateProfile' {} a -> s {setupModeDisabled = a} :: UpdateProfile)

-- | Whether the PSTN setting of the room profile is enabled.
updateProfile_pSTNEnabled :: Lens.Lens' UpdateProfile (Core.Maybe Core.Bool)
updateProfile_pSTNEnabled = Lens.lens (\UpdateProfile' {pSTNEnabled} -> pSTNEnabled) (\s@UpdateProfile' {} a -> s {pSTNEnabled = a} :: UpdateProfile)

-- | The updated maximum volume limit for the room profile.
updateProfile_maxVolumeLimit :: Lens.Lens' UpdateProfile (Core.Maybe Core.Int)
updateProfile_maxVolumeLimit = Lens.lens (\UpdateProfile' {maxVolumeLimit} -> maxVolumeLimit) (\s@UpdateProfile' {} a -> s {maxVolumeLimit = a} :: UpdateProfile)

-- | The updated meeting room settings of a room profile.
updateProfile_meetingRoomConfiguration :: Lens.Lens' UpdateProfile (Core.Maybe UpdateMeetingRoomConfiguration)
updateProfile_meetingRoomConfiguration = Lens.lens (\UpdateProfile' {meetingRoomConfiguration} -> meetingRoomConfiguration) (\s@UpdateProfile' {} a -> s {meetingRoomConfiguration = a} :: UpdateProfile)

-- | The updated wake word for the room profile.
updateProfile_wakeWord :: Lens.Lens' UpdateProfile (Core.Maybe WakeWord)
updateProfile_wakeWord = Lens.lens (\UpdateProfile' {wakeWord} -> wakeWord) (\s@UpdateProfile' {} a -> s {wakeWord = a} :: UpdateProfile)

-- | The ARN of the room profile to update. Required.
updateProfile_profileArn :: Lens.Lens' UpdateProfile (Core.Maybe Core.Text)
updateProfile_profileArn = Lens.lens (\UpdateProfile' {profileArn} -> profileArn) (\s@UpdateProfile' {} a -> s {profileArn = a} :: UpdateProfile)

-- | The updated timezone for the room profile.
updateProfile_timezone :: Lens.Lens' UpdateProfile (Core.Maybe Core.Text)
updateProfile_timezone = Lens.lens (\UpdateProfile' {timezone} -> timezone) (\s@UpdateProfile' {} a -> s {timezone = a} :: UpdateProfile)

-- | The updated distance unit for the room profile.
updateProfile_distanceUnit :: Lens.Lens' UpdateProfile (Core.Maybe DistanceUnit)
updateProfile_distanceUnit = Lens.lens (\UpdateProfile' {distanceUnit} -> distanceUnit) (\s@UpdateProfile' {} a -> s {distanceUnit = a} :: UpdateProfile)

instance Core.AWSRequest UpdateProfile where
  type
    AWSResponse UpdateProfile =
      UpdateProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateProfileResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateProfile

instance Core.NFData UpdateProfile

instance Core.ToHeaders UpdateProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.UpdateProfile" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateProfile where
  toJSON UpdateProfile' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ProfileName" Core..=) Core.<$> profileName,
            ("IsDefault" Core..=) Core.<$> isDefault,
            ("Address" Core..=) Core.<$> address,
            ("Locale" Core..=) Core.<$> locale,
            ("TemperatureUnit" Core..=) Core.<$> temperatureUnit,
            ("SetupModeDisabled" Core..=)
              Core.<$> setupModeDisabled,
            ("PSTNEnabled" Core..=) Core.<$> pSTNEnabled,
            ("MaxVolumeLimit" Core..=) Core.<$> maxVolumeLimit,
            ("MeetingRoomConfiguration" Core..=)
              Core.<$> meetingRoomConfiguration,
            ("WakeWord" Core..=) Core.<$> wakeWord,
            ("ProfileArn" Core..=) Core.<$> profileArn,
            ("Timezone" Core..=) Core.<$> timezone,
            ("DistanceUnit" Core..=) Core.<$> distanceUnit
          ]
      )

instance Core.ToPath UpdateProfile where
  toPath = Core.const "/"

instance Core.ToQuery UpdateProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateProfileResponse' smart constructor.
data UpdateProfileResponse = UpdateProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateProfileResponse_httpStatus' - The response's http status code.
newUpdateProfileResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateProfileResponse
newUpdateProfileResponse pHttpStatus_ =
  UpdateProfileResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateProfileResponse_httpStatus :: Lens.Lens' UpdateProfileResponse Core.Int
updateProfileResponse_httpStatus = Lens.lens (\UpdateProfileResponse' {httpStatus} -> httpStatus) (\s@UpdateProfileResponse' {} a -> s {httpStatus = a} :: UpdateProfileResponse)

instance Core.NFData UpdateProfileResponse
