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
-- Module      : Amazonka.AlexaBusiness.CreateProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new room profile with the specified details.
module Amazonka.AlexaBusiness.CreateProfile
  ( -- * Creating a Request
    CreateProfile (..),
    newCreateProfile,

    -- * Request Lenses
    createProfile_clientRequestToken,
    createProfile_dataRetentionOptIn,
    createProfile_locale,
    createProfile_maxVolumeLimit,
    createProfile_meetingRoomConfiguration,
    createProfile_pSTNEnabled,
    createProfile_setupModeDisabled,
    createProfile_tags,
    createProfile_profileName,
    createProfile_timezone,
    createProfile_address,
    createProfile_distanceUnit,
    createProfile_temperatureUnit,
    createProfile_wakeWord,

    -- * Destructuring the Response
    CreateProfileResponse (..),
    newCreateProfileResponse,

    -- * Response Lenses
    createProfileResponse_profileArn,
    createProfileResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateProfile' smart constructor.
data CreateProfile = CreateProfile'
  { -- | The user-specified token that is used during the creation of a profile.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Whether data retention of the profile is enabled.
    dataRetentionOptIn :: Prelude.Maybe Prelude.Bool,
    -- | The locale of the room profile. (This is currently only available to a
    -- limited preview audience.)
    locale :: Prelude.Maybe Prelude.Text,
    -- | The maximum volume limit for a room profile.
    maxVolumeLimit :: Prelude.Maybe Prelude.Int,
    -- | The meeting room settings of a room profile.
    meetingRoomConfiguration :: Prelude.Maybe CreateMeetingRoomConfiguration,
    -- | Whether PSTN calling is enabled.
    pSTNEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether room profile setup is enabled.
    setupModeDisabled :: Prelude.Maybe Prelude.Bool,
    -- | The tags for the profile.
    tags :: Prelude.Maybe [Tag],
    -- | The name of a room profile.
    profileName :: Prelude.Text,
    -- | The time zone used by a room profile.
    timezone :: Prelude.Text,
    -- | The valid address for the room.
    address :: Prelude.Text,
    -- | The distance unit to be used by devices in the profile.
    distanceUnit :: DistanceUnit,
    -- | The temperature unit to be used by devices in the profile.
    temperatureUnit :: TemperatureUnit,
    -- | A wake word for Alexa, Echo, Amazon, or a computer.
    wakeWord :: WakeWord
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createProfile_clientRequestToken' - The user-specified token that is used during the creation of a profile.
--
-- 'dataRetentionOptIn', 'createProfile_dataRetentionOptIn' - Whether data retention of the profile is enabled.
--
-- 'locale', 'createProfile_locale' - The locale of the room profile. (This is currently only available to a
-- limited preview audience.)
--
-- 'maxVolumeLimit', 'createProfile_maxVolumeLimit' - The maximum volume limit for a room profile.
--
-- 'meetingRoomConfiguration', 'createProfile_meetingRoomConfiguration' - The meeting room settings of a room profile.
--
-- 'pSTNEnabled', 'createProfile_pSTNEnabled' - Whether PSTN calling is enabled.
--
-- 'setupModeDisabled', 'createProfile_setupModeDisabled' - Whether room profile setup is enabled.
--
-- 'tags', 'createProfile_tags' - The tags for the profile.
--
-- 'profileName', 'createProfile_profileName' - The name of a room profile.
--
-- 'timezone', 'createProfile_timezone' - The time zone used by a room profile.
--
-- 'address', 'createProfile_address' - The valid address for the room.
--
-- 'distanceUnit', 'createProfile_distanceUnit' - The distance unit to be used by devices in the profile.
--
-- 'temperatureUnit', 'createProfile_temperatureUnit' - The temperature unit to be used by devices in the profile.
--
-- 'wakeWord', 'createProfile_wakeWord' - A wake word for Alexa, Echo, Amazon, or a computer.
newCreateProfile ::
  -- | 'profileName'
  Prelude.Text ->
  -- | 'timezone'
  Prelude.Text ->
  -- | 'address'
  Prelude.Text ->
  -- | 'distanceUnit'
  DistanceUnit ->
  -- | 'temperatureUnit'
  TemperatureUnit ->
  -- | 'wakeWord'
  WakeWord ->
  CreateProfile
newCreateProfile
  pProfileName_
  pTimezone_
  pAddress_
  pDistanceUnit_
  pTemperatureUnit_
  pWakeWord_ =
    CreateProfile'
      { clientRequestToken =
          Prelude.Nothing,
        dataRetentionOptIn = Prelude.Nothing,
        locale = Prelude.Nothing,
        maxVolumeLimit = Prelude.Nothing,
        meetingRoomConfiguration = Prelude.Nothing,
        pSTNEnabled = Prelude.Nothing,
        setupModeDisabled = Prelude.Nothing,
        tags = Prelude.Nothing,
        profileName = pProfileName_,
        timezone = pTimezone_,
        address = pAddress_,
        distanceUnit = pDistanceUnit_,
        temperatureUnit = pTemperatureUnit_,
        wakeWord = pWakeWord_
      }

-- | The user-specified token that is used during the creation of a profile.
createProfile_clientRequestToken :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_clientRequestToken = Lens.lens (\CreateProfile' {clientRequestToken} -> clientRequestToken) (\s@CreateProfile' {} a -> s {clientRequestToken = a} :: CreateProfile)

-- | Whether data retention of the profile is enabled.
createProfile_dataRetentionOptIn :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Bool)
createProfile_dataRetentionOptIn = Lens.lens (\CreateProfile' {dataRetentionOptIn} -> dataRetentionOptIn) (\s@CreateProfile' {} a -> s {dataRetentionOptIn = a} :: CreateProfile)

-- | The locale of the room profile. (This is currently only available to a
-- limited preview audience.)
createProfile_locale :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_locale = Lens.lens (\CreateProfile' {locale} -> locale) (\s@CreateProfile' {} a -> s {locale = a} :: CreateProfile)

-- | The maximum volume limit for a room profile.
createProfile_maxVolumeLimit :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Int)
createProfile_maxVolumeLimit = Lens.lens (\CreateProfile' {maxVolumeLimit} -> maxVolumeLimit) (\s@CreateProfile' {} a -> s {maxVolumeLimit = a} :: CreateProfile)

-- | The meeting room settings of a room profile.
createProfile_meetingRoomConfiguration :: Lens.Lens' CreateProfile (Prelude.Maybe CreateMeetingRoomConfiguration)
createProfile_meetingRoomConfiguration = Lens.lens (\CreateProfile' {meetingRoomConfiguration} -> meetingRoomConfiguration) (\s@CreateProfile' {} a -> s {meetingRoomConfiguration = a} :: CreateProfile)

-- | Whether PSTN calling is enabled.
createProfile_pSTNEnabled :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Bool)
createProfile_pSTNEnabled = Lens.lens (\CreateProfile' {pSTNEnabled} -> pSTNEnabled) (\s@CreateProfile' {} a -> s {pSTNEnabled = a} :: CreateProfile)

-- | Whether room profile setup is enabled.
createProfile_setupModeDisabled :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Bool)
createProfile_setupModeDisabled = Lens.lens (\CreateProfile' {setupModeDisabled} -> setupModeDisabled) (\s@CreateProfile' {} a -> s {setupModeDisabled = a} :: CreateProfile)

-- | The tags for the profile.
createProfile_tags :: Lens.Lens' CreateProfile (Prelude.Maybe [Tag])
createProfile_tags = Lens.lens (\CreateProfile' {tags} -> tags) (\s@CreateProfile' {} a -> s {tags = a} :: CreateProfile) Prelude.. Lens.mapping Lens.coerced

-- | The name of a room profile.
createProfile_profileName :: Lens.Lens' CreateProfile Prelude.Text
createProfile_profileName = Lens.lens (\CreateProfile' {profileName} -> profileName) (\s@CreateProfile' {} a -> s {profileName = a} :: CreateProfile)

-- | The time zone used by a room profile.
createProfile_timezone :: Lens.Lens' CreateProfile Prelude.Text
createProfile_timezone = Lens.lens (\CreateProfile' {timezone} -> timezone) (\s@CreateProfile' {} a -> s {timezone = a} :: CreateProfile)

-- | The valid address for the room.
createProfile_address :: Lens.Lens' CreateProfile Prelude.Text
createProfile_address = Lens.lens (\CreateProfile' {address} -> address) (\s@CreateProfile' {} a -> s {address = a} :: CreateProfile)

-- | The distance unit to be used by devices in the profile.
createProfile_distanceUnit :: Lens.Lens' CreateProfile DistanceUnit
createProfile_distanceUnit = Lens.lens (\CreateProfile' {distanceUnit} -> distanceUnit) (\s@CreateProfile' {} a -> s {distanceUnit = a} :: CreateProfile)

-- | The temperature unit to be used by devices in the profile.
createProfile_temperatureUnit :: Lens.Lens' CreateProfile TemperatureUnit
createProfile_temperatureUnit = Lens.lens (\CreateProfile' {temperatureUnit} -> temperatureUnit) (\s@CreateProfile' {} a -> s {temperatureUnit = a} :: CreateProfile)

-- | A wake word for Alexa, Echo, Amazon, or a computer.
createProfile_wakeWord :: Lens.Lens' CreateProfile WakeWord
createProfile_wakeWord = Lens.lens (\CreateProfile' {wakeWord} -> wakeWord) (\s@CreateProfile' {} a -> s {wakeWord = a} :: CreateProfile)

instance Core.AWSRequest CreateProfile where
  type
    AWSResponse CreateProfile =
      CreateProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProfileResponse'
            Prelude.<$> (x Data..?> "ProfileArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProfile where
  hashWithSalt _salt CreateProfile' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` dataRetentionOptIn
      `Prelude.hashWithSalt` locale
      `Prelude.hashWithSalt` maxVolumeLimit
      `Prelude.hashWithSalt` meetingRoomConfiguration
      `Prelude.hashWithSalt` pSTNEnabled
      `Prelude.hashWithSalt` setupModeDisabled
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` profileName
      `Prelude.hashWithSalt` timezone
      `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` distanceUnit
      `Prelude.hashWithSalt` temperatureUnit
      `Prelude.hashWithSalt` wakeWord

instance Prelude.NFData CreateProfile where
  rnf CreateProfile' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf dataRetentionOptIn
      `Prelude.seq` Prelude.rnf locale
      `Prelude.seq` Prelude.rnf maxVolumeLimit
      `Prelude.seq` Prelude.rnf meetingRoomConfiguration
      `Prelude.seq` Prelude.rnf pSTNEnabled
      `Prelude.seq` Prelude.rnf setupModeDisabled
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf profileName
      `Prelude.seq` Prelude.rnf timezone
      `Prelude.seq` Prelude.rnf address
      `Prelude.seq` Prelude.rnf distanceUnit
      `Prelude.seq` Prelude.rnf temperatureUnit
      `Prelude.seq` Prelude.rnf wakeWord

instance Data.ToHeaders CreateProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.CreateProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateProfile where
  toJSON CreateProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("DataRetentionOptIn" Data..=)
              Prelude.<$> dataRetentionOptIn,
            ("Locale" Data..=) Prelude.<$> locale,
            ("MaxVolumeLimit" Data..=)
              Prelude.<$> maxVolumeLimit,
            ("MeetingRoomConfiguration" Data..=)
              Prelude.<$> meetingRoomConfiguration,
            ("PSTNEnabled" Data..=) Prelude.<$> pSTNEnabled,
            ("SetupModeDisabled" Data..=)
              Prelude.<$> setupModeDisabled,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("ProfileName" Data..= profileName),
            Prelude.Just ("Timezone" Data..= timezone),
            Prelude.Just ("Address" Data..= address),
            Prelude.Just ("DistanceUnit" Data..= distanceUnit),
            Prelude.Just
              ("TemperatureUnit" Data..= temperatureUnit),
            Prelude.Just ("WakeWord" Data..= wakeWord)
          ]
      )

instance Data.ToPath CreateProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProfileResponse' smart constructor.
data CreateProfileResponse = CreateProfileResponse'
  { -- | The ARN of the newly created room profile in the response.
    profileArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileArn', 'createProfileResponse_profileArn' - The ARN of the newly created room profile in the response.
--
-- 'httpStatus', 'createProfileResponse_httpStatus' - The response's http status code.
newCreateProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProfileResponse
newCreateProfileResponse pHttpStatus_ =
  CreateProfileResponse'
    { profileArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the newly created room profile in the response.
createProfileResponse_profileArn :: Lens.Lens' CreateProfileResponse (Prelude.Maybe Prelude.Text)
createProfileResponse_profileArn = Lens.lens (\CreateProfileResponse' {profileArn} -> profileArn) (\s@CreateProfileResponse' {} a -> s {profileArn = a} :: CreateProfileResponse)

-- | The response's http status code.
createProfileResponse_httpStatus :: Lens.Lens' CreateProfileResponse Prelude.Int
createProfileResponse_httpStatus = Lens.lens (\CreateProfileResponse' {httpStatus} -> httpStatus) (\s@CreateProfileResponse' {} a -> s {httpStatus = a} :: CreateProfileResponse)

instance Prelude.NFData CreateProfileResponse where
  rnf CreateProfileResponse' {..} =
    Prelude.rnf profileArn
      `Prelude.seq` Prelude.rnf httpStatus
