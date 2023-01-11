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
-- Module      : Amazonka.AlexaBusiness.UpdateProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing room profile by room profile ARN.
module Amazonka.AlexaBusiness.UpdateProfile
  ( -- * Creating a Request
    UpdateProfile (..),
    newUpdateProfile,

    -- * Request Lenses
    updateProfile_address,
    updateProfile_dataRetentionOptIn,
    updateProfile_distanceUnit,
    updateProfile_isDefault,
    updateProfile_locale,
    updateProfile_maxVolumeLimit,
    updateProfile_meetingRoomConfiguration,
    updateProfile_pSTNEnabled,
    updateProfile_profileArn,
    updateProfile_profileName,
    updateProfile_setupModeDisabled,
    updateProfile_temperatureUnit,
    updateProfile_timezone,
    updateProfile_wakeWord,

    -- * Destructuring the Response
    UpdateProfileResponse (..),
    newUpdateProfileResponse,

    -- * Response Lenses
    updateProfileResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateProfile' smart constructor.
data UpdateProfile = UpdateProfile'
  { -- | The updated address for the room profile.
    address :: Prelude.Maybe Prelude.Text,
    -- | Whether data retention of the profile is enabled.
    dataRetentionOptIn :: Prelude.Maybe Prelude.Bool,
    -- | The updated distance unit for the room profile.
    distanceUnit :: Prelude.Maybe DistanceUnit,
    -- | Sets the profile as default if selected. If this is missing, no update
    -- is done to the default status.
    isDefault :: Prelude.Maybe Prelude.Bool,
    -- | The updated locale for the room profile. (This is currently only
    -- available to a limited preview audience.)
    locale :: Prelude.Maybe Prelude.Text,
    -- | The updated maximum volume limit for the room profile.
    maxVolumeLimit :: Prelude.Maybe Prelude.Int,
    -- | The updated meeting room settings of a room profile.
    meetingRoomConfiguration :: Prelude.Maybe UpdateMeetingRoomConfiguration,
    -- | Whether the PSTN setting of the room profile is enabled.
    pSTNEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the room profile to update. Required.
    profileArn :: Prelude.Maybe Prelude.Text,
    -- | The updated name for the room profile.
    profileName :: Prelude.Maybe Prelude.Text,
    -- | Whether the setup mode of the profile is enabled.
    setupModeDisabled :: Prelude.Maybe Prelude.Bool,
    -- | The updated temperature unit for the room profile.
    temperatureUnit :: Prelude.Maybe TemperatureUnit,
    -- | The updated timezone for the room profile.
    timezone :: Prelude.Maybe Prelude.Text,
    -- | The updated wake word for the room profile.
    wakeWord :: Prelude.Maybe WakeWord
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'updateProfile_address' - The updated address for the room profile.
--
-- 'dataRetentionOptIn', 'updateProfile_dataRetentionOptIn' - Whether data retention of the profile is enabled.
--
-- 'distanceUnit', 'updateProfile_distanceUnit' - The updated distance unit for the room profile.
--
-- 'isDefault', 'updateProfile_isDefault' - Sets the profile as default if selected. If this is missing, no update
-- is done to the default status.
--
-- 'locale', 'updateProfile_locale' - The updated locale for the room profile. (This is currently only
-- available to a limited preview audience.)
--
-- 'maxVolumeLimit', 'updateProfile_maxVolumeLimit' - The updated maximum volume limit for the room profile.
--
-- 'meetingRoomConfiguration', 'updateProfile_meetingRoomConfiguration' - The updated meeting room settings of a room profile.
--
-- 'pSTNEnabled', 'updateProfile_pSTNEnabled' - Whether the PSTN setting of the room profile is enabled.
--
-- 'profileArn', 'updateProfile_profileArn' - The ARN of the room profile to update. Required.
--
-- 'profileName', 'updateProfile_profileName' - The updated name for the room profile.
--
-- 'setupModeDisabled', 'updateProfile_setupModeDisabled' - Whether the setup mode of the profile is enabled.
--
-- 'temperatureUnit', 'updateProfile_temperatureUnit' - The updated temperature unit for the room profile.
--
-- 'timezone', 'updateProfile_timezone' - The updated timezone for the room profile.
--
-- 'wakeWord', 'updateProfile_wakeWord' - The updated wake word for the room profile.
newUpdateProfile ::
  UpdateProfile
newUpdateProfile =
  UpdateProfile'
    { address = Prelude.Nothing,
      dataRetentionOptIn = Prelude.Nothing,
      distanceUnit = Prelude.Nothing,
      isDefault = Prelude.Nothing,
      locale = Prelude.Nothing,
      maxVolumeLimit = Prelude.Nothing,
      meetingRoomConfiguration = Prelude.Nothing,
      pSTNEnabled = Prelude.Nothing,
      profileArn = Prelude.Nothing,
      profileName = Prelude.Nothing,
      setupModeDisabled = Prelude.Nothing,
      temperatureUnit = Prelude.Nothing,
      timezone = Prelude.Nothing,
      wakeWord = Prelude.Nothing
    }

-- | The updated address for the room profile.
updateProfile_address :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_address = Lens.lens (\UpdateProfile' {address} -> address) (\s@UpdateProfile' {} a -> s {address = a} :: UpdateProfile)

-- | Whether data retention of the profile is enabled.
updateProfile_dataRetentionOptIn :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Bool)
updateProfile_dataRetentionOptIn = Lens.lens (\UpdateProfile' {dataRetentionOptIn} -> dataRetentionOptIn) (\s@UpdateProfile' {} a -> s {dataRetentionOptIn = a} :: UpdateProfile)

-- | The updated distance unit for the room profile.
updateProfile_distanceUnit :: Lens.Lens' UpdateProfile (Prelude.Maybe DistanceUnit)
updateProfile_distanceUnit = Lens.lens (\UpdateProfile' {distanceUnit} -> distanceUnit) (\s@UpdateProfile' {} a -> s {distanceUnit = a} :: UpdateProfile)

-- | Sets the profile as default if selected. If this is missing, no update
-- is done to the default status.
updateProfile_isDefault :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Bool)
updateProfile_isDefault = Lens.lens (\UpdateProfile' {isDefault} -> isDefault) (\s@UpdateProfile' {} a -> s {isDefault = a} :: UpdateProfile)

-- | The updated locale for the room profile. (This is currently only
-- available to a limited preview audience.)
updateProfile_locale :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_locale = Lens.lens (\UpdateProfile' {locale} -> locale) (\s@UpdateProfile' {} a -> s {locale = a} :: UpdateProfile)

-- | The updated maximum volume limit for the room profile.
updateProfile_maxVolumeLimit :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Int)
updateProfile_maxVolumeLimit = Lens.lens (\UpdateProfile' {maxVolumeLimit} -> maxVolumeLimit) (\s@UpdateProfile' {} a -> s {maxVolumeLimit = a} :: UpdateProfile)

-- | The updated meeting room settings of a room profile.
updateProfile_meetingRoomConfiguration :: Lens.Lens' UpdateProfile (Prelude.Maybe UpdateMeetingRoomConfiguration)
updateProfile_meetingRoomConfiguration = Lens.lens (\UpdateProfile' {meetingRoomConfiguration} -> meetingRoomConfiguration) (\s@UpdateProfile' {} a -> s {meetingRoomConfiguration = a} :: UpdateProfile)

-- | Whether the PSTN setting of the room profile is enabled.
updateProfile_pSTNEnabled :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Bool)
updateProfile_pSTNEnabled = Lens.lens (\UpdateProfile' {pSTNEnabled} -> pSTNEnabled) (\s@UpdateProfile' {} a -> s {pSTNEnabled = a} :: UpdateProfile)

-- | The ARN of the room profile to update. Required.
updateProfile_profileArn :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_profileArn = Lens.lens (\UpdateProfile' {profileArn} -> profileArn) (\s@UpdateProfile' {} a -> s {profileArn = a} :: UpdateProfile)

-- | The updated name for the room profile.
updateProfile_profileName :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_profileName = Lens.lens (\UpdateProfile' {profileName} -> profileName) (\s@UpdateProfile' {} a -> s {profileName = a} :: UpdateProfile)

-- | Whether the setup mode of the profile is enabled.
updateProfile_setupModeDisabled :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Bool)
updateProfile_setupModeDisabled = Lens.lens (\UpdateProfile' {setupModeDisabled} -> setupModeDisabled) (\s@UpdateProfile' {} a -> s {setupModeDisabled = a} :: UpdateProfile)

-- | The updated temperature unit for the room profile.
updateProfile_temperatureUnit :: Lens.Lens' UpdateProfile (Prelude.Maybe TemperatureUnit)
updateProfile_temperatureUnit = Lens.lens (\UpdateProfile' {temperatureUnit} -> temperatureUnit) (\s@UpdateProfile' {} a -> s {temperatureUnit = a} :: UpdateProfile)

-- | The updated timezone for the room profile.
updateProfile_timezone :: Lens.Lens' UpdateProfile (Prelude.Maybe Prelude.Text)
updateProfile_timezone = Lens.lens (\UpdateProfile' {timezone} -> timezone) (\s@UpdateProfile' {} a -> s {timezone = a} :: UpdateProfile)

-- | The updated wake word for the room profile.
updateProfile_wakeWord :: Lens.Lens' UpdateProfile (Prelude.Maybe WakeWord)
updateProfile_wakeWord = Lens.lens (\UpdateProfile' {wakeWord} -> wakeWord) (\s@UpdateProfile' {} a -> s {wakeWord = a} :: UpdateProfile)

instance Core.AWSRequest UpdateProfile where
  type
    AWSResponse UpdateProfile =
      UpdateProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateProfileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateProfile where
  hashWithSalt _salt UpdateProfile' {..} =
    _salt `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` dataRetentionOptIn
      `Prelude.hashWithSalt` distanceUnit
      `Prelude.hashWithSalt` isDefault
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` maxVolumeLimit
      `Prelude.hashWithSalt` meetingRoomConfiguration
      `Prelude.hashWithSalt` pSTNEnabled
      `Prelude.hashWithSalt` profileArn
      `Prelude.hashWithSalt` profileName
      `Prelude.hashWithSalt` setupModeDisabled
      `Prelude.hashWithSalt` temperatureUnit
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` wakeWord

instance Prelude.NFData UpdateProfile where
  rnf UpdateProfile' {..} =
    Prelude.rnf address
      `Prelude.seq` Prelude.rnf dataRetentionOptIn
      `Prelude.seq` Prelude.rnf distanceUnit
      `Prelude.seq` Prelude.rnf isDefault
      `Prelude.seq` Prelude.rnf locale
      `Prelude.seq` Prelude.rnf maxVolumeLimit
      `Prelude.seq` Prelude.rnf meetingRoomConfiguration
      `Prelude.seq` Prelude.rnf pSTNEnabled
      `Prelude.seq` Prelude.rnf profileArn
      `Prelude.seq` Prelude.rnf profileName
      `Prelude.seq` Prelude.rnf setupModeDisabled
      `Prelude.seq` Prelude.rnf temperatureUnit
      `Prelude.seq` Prelude.rnf timezone
      `Prelude.seq` Prelude.rnf wakeWord

instance Data.ToHeaders UpdateProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.UpdateProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateProfile where
  toJSON UpdateProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Address" Data..=) Prelude.<$> address,
            ("DataRetentionOptIn" Data..=)
              Prelude.<$> dataRetentionOptIn,
            ("DistanceUnit" Data..=) Prelude.<$> distanceUnit,
            ("IsDefault" Data..=) Prelude.<$> isDefault,
            ("Locale" Data..=) Prelude.<$> locale,
            ("MaxVolumeLimit" Data..=)
              Prelude.<$> maxVolumeLimit,
            ("MeetingRoomConfiguration" Data..=)
              Prelude.<$> meetingRoomConfiguration,
            ("PSTNEnabled" Data..=) Prelude.<$> pSTNEnabled,
            ("ProfileArn" Data..=) Prelude.<$> profileArn,
            ("ProfileName" Data..=) Prelude.<$> profileName,
            ("SetupModeDisabled" Data..=)
              Prelude.<$> setupModeDisabled,
            ("TemperatureUnit" Data..=)
              Prelude.<$> temperatureUnit,
            ("Timezone" Data..=) Prelude.<$> timezone,
            ("WakeWord" Data..=) Prelude.<$> wakeWord
          ]
      )

instance Data.ToPath UpdateProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProfileResponse' smart constructor.
data UpdateProfileResponse = UpdateProfileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateProfileResponse
newUpdateProfileResponse pHttpStatus_ =
  UpdateProfileResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateProfileResponse_httpStatus :: Lens.Lens' UpdateProfileResponse Prelude.Int
updateProfileResponse_httpStatus = Lens.lens (\UpdateProfileResponse' {httpStatus} -> httpStatus) (\s@UpdateProfileResponse' {} a -> s {httpStatus = a} :: UpdateProfileResponse)

instance Prelude.NFData UpdateProfileResponse where
  rnf UpdateProfileResponse' {..} =
    Prelude.rnf httpStatus
