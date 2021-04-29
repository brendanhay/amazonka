{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.AlexaBusiness.CreateProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new room profile with the specified details.
module Network.AWS.AlexaBusiness.CreateProfile
  ( -- * Creating a Request
    CreateProfile (..),
    newCreateProfile,

    -- * Request Lenses
    createProfile_locale,
    createProfile_setupModeDisabled,
    createProfile_pSTNEnabled,
    createProfile_tags,
    createProfile_maxVolumeLimit,
    createProfile_meetingRoomConfiguration,
    createProfile_clientRequestToken,
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

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateProfile' smart constructor.
data CreateProfile = CreateProfile'
  { -- | The locale of the room profile. (This is currently only available to a
    -- limited preview audience.)
    locale :: Prelude.Maybe Prelude.Text,
    -- | Whether room profile setup is enabled.
    setupModeDisabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether PSTN calling is enabled.
    pSTNEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The tags for the profile.
    tags :: Prelude.Maybe [Tag],
    -- | The maximum volume limit for a room profile.
    maxVolumeLimit :: Prelude.Maybe Prelude.Int,
    -- | The meeting room settings of a room profile.
    meetingRoomConfiguration :: Prelude.Maybe CreateMeetingRoomConfiguration,
    -- | The user-specified token that is used during the creation of a profile.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locale', 'createProfile_locale' - The locale of the room profile. (This is currently only available to a
-- limited preview audience.)
--
-- 'setupModeDisabled', 'createProfile_setupModeDisabled' - Whether room profile setup is enabled.
--
-- 'pSTNEnabled', 'createProfile_pSTNEnabled' - Whether PSTN calling is enabled.
--
-- 'tags', 'createProfile_tags' - The tags for the profile.
--
-- 'maxVolumeLimit', 'createProfile_maxVolumeLimit' - The maximum volume limit for a room profile.
--
-- 'meetingRoomConfiguration', 'createProfile_meetingRoomConfiguration' - The meeting room settings of a room profile.
--
-- 'clientRequestToken', 'createProfile_clientRequestToken' - The user-specified token that is used during the creation of a profile.
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
      { locale = Prelude.Nothing,
        setupModeDisabled = Prelude.Nothing,
        pSTNEnabled = Prelude.Nothing,
        tags = Prelude.Nothing,
        maxVolumeLimit = Prelude.Nothing,
        meetingRoomConfiguration = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        profileName = pProfileName_,
        timezone = pTimezone_,
        address = pAddress_,
        distanceUnit = pDistanceUnit_,
        temperatureUnit = pTemperatureUnit_,
        wakeWord = pWakeWord_
      }

-- | The locale of the room profile. (This is currently only available to a
-- limited preview audience.)
createProfile_locale :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_locale = Lens.lens (\CreateProfile' {locale} -> locale) (\s@CreateProfile' {} a -> s {locale = a} :: CreateProfile)

-- | Whether room profile setup is enabled.
createProfile_setupModeDisabled :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Bool)
createProfile_setupModeDisabled = Lens.lens (\CreateProfile' {setupModeDisabled} -> setupModeDisabled) (\s@CreateProfile' {} a -> s {setupModeDisabled = a} :: CreateProfile)

-- | Whether PSTN calling is enabled.
createProfile_pSTNEnabled :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Bool)
createProfile_pSTNEnabled = Lens.lens (\CreateProfile' {pSTNEnabled} -> pSTNEnabled) (\s@CreateProfile' {} a -> s {pSTNEnabled = a} :: CreateProfile)

-- | The tags for the profile.
createProfile_tags :: Lens.Lens' CreateProfile (Prelude.Maybe [Tag])
createProfile_tags = Lens.lens (\CreateProfile' {tags} -> tags) (\s@CreateProfile' {} a -> s {tags = a} :: CreateProfile) Prelude.. Lens.mapping Prelude._Coerce

-- | The maximum volume limit for a room profile.
createProfile_maxVolumeLimit :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Int)
createProfile_maxVolumeLimit = Lens.lens (\CreateProfile' {maxVolumeLimit} -> maxVolumeLimit) (\s@CreateProfile' {} a -> s {maxVolumeLimit = a} :: CreateProfile)

-- | The meeting room settings of a room profile.
createProfile_meetingRoomConfiguration :: Lens.Lens' CreateProfile (Prelude.Maybe CreateMeetingRoomConfiguration)
createProfile_meetingRoomConfiguration = Lens.lens (\CreateProfile' {meetingRoomConfiguration} -> meetingRoomConfiguration) (\s@CreateProfile' {} a -> s {meetingRoomConfiguration = a} :: CreateProfile)

-- | The user-specified token that is used during the creation of a profile.
createProfile_clientRequestToken :: Lens.Lens' CreateProfile (Prelude.Maybe Prelude.Text)
createProfile_clientRequestToken = Lens.lens (\CreateProfile' {clientRequestToken} -> clientRequestToken) (\s@CreateProfile' {} a -> s {clientRequestToken = a} :: CreateProfile)

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

instance Prelude.AWSRequest CreateProfile where
  type Rs CreateProfile = CreateProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProfileResponse'
            Prelude.<$> (x Prelude..?> "ProfileArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProfile

instance Prelude.NFData CreateProfile

instance Prelude.ToHeaders CreateProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.CreateProfile" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateProfile where
  toJSON CreateProfile' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Locale" Prelude..=) Prelude.<$> locale,
            ("SetupModeDisabled" Prelude..=)
              Prelude.<$> setupModeDisabled,
            ("PSTNEnabled" Prelude..=) Prelude.<$> pSTNEnabled,
            ("Tags" Prelude..=) Prelude.<$> tags,
            ("MaxVolumeLimit" Prelude..=)
              Prelude.<$> maxVolumeLimit,
            ("MeetingRoomConfiguration" Prelude..=)
              Prelude.<$> meetingRoomConfiguration,
            ("ClientRequestToken" Prelude..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("ProfileName" Prelude..= profileName),
            Prelude.Just ("Timezone" Prelude..= timezone),
            Prelude.Just ("Address" Prelude..= address),
            Prelude.Just
              ("DistanceUnit" Prelude..= distanceUnit),
            Prelude.Just
              ("TemperatureUnit" Prelude..= temperatureUnit),
            Prelude.Just ("WakeWord" Prelude..= wakeWord)
          ]
      )

instance Prelude.ToPath CreateProfile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProfileResponse' smart constructor.
data CreateProfileResponse = CreateProfileResponse'
  { -- | The ARN of the newly created room profile in the response.
    profileArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData CreateProfileResponse
