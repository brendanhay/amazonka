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
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateProfile' smart constructor.
data CreateProfile = CreateProfile'
  { -- | The locale of the room profile. (This is currently only available to a
    -- limited preview audience.)
    locale :: Core.Maybe Core.Text,
    -- | Whether room profile setup is enabled.
    setupModeDisabled :: Core.Maybe Core.Bool,
    -- | Whether PSTN calling is enabled.
    pSTNEnabled :: Core.Maybe Core.Bool,
    -- | The tags for the profile.
    tags :: Core.Maybe [Tag],
    -- | The maximum volume limit for a room profile.
    maxVolumeLimit :: Core.Maybe Core.Int,
    -- | The meeting room settings of a room profile.
    meetingRoomConfiguration :: Core.Maybe CreateMeetingRoomConfiguration,
    -- | The user-specified token that is used during the creation of a profile.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The name of a room profile.
    profileName :: Core.Text,
    -- | The time zone used by a room profile.
    timezone :: Core.Text,
    -- | The valid address for the room.
    address :: Core.Text,
    -- | The distance unit to be used by devices in the profile.
    distanceUnit :: DistanceUnit,
    -- | The temperature unit to be used by devices in the profile.
    temperatureUnit :: TemperatureUnit,
    -- | A wake word for Alexa, Echo, Amazon, or a computer.
    wakeWord :: WakeWord
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'timezone'
  Core.Text ->
  -- | 'address'
  Core.Text ->
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
      { locale = Core.Nothing,
        setupModeDisabled = Core.Nothing,
        pSTNEnabled = Core.Nothing,
        tags = Core.Nothing,
        maxVolumeLimit = Core.Nothing,
        meetingRoomConfiguration = Core.Nothing,
        clientRequestToken = Core.Nothing,
        profileName = pProfileName_,
        timezone = pTimezone_,
        address = pAddress_,
        distanceUnit = pDistanceUnit_,
        temperatureUnit = pTemperatureUnit_,
        wakeWord = pWakeWord_
      }

-- | The locale of the room profile. (This is currently only available to a
-- limited preview audience.)
createProfile_locale :: Lens.Lens' CreateProfile (Core.Maybe Core.Text)
createProfile_locale = Lens.lens (\CreateProfile' {locale} -> locale) (\s@CreateProfile' {} a -> s {locale = a} :: CreateProfile)

-- | Whether room profile setup is enabled.
createProfile_setupModeDisabled :: Lens.Lens' CreateProfile (Core.Maybe Core.Bool)
createProfile_setupModeDisabled = Lens.lens (\CreateProfile' {setupModeDisabled} -> setupModeDisabled) (\s@CreateProfile' {} a -> s {setupModeDisabled = a} :: CreateProfile)

-- | Whether PSTN calling is enabled.
createProfile_pSTNEnabled :: Lens.Lens' CreateProfile (Core.Maybe Core.Bool)
createProfile_pSTNEnabled = Lens.lens (\CreateProfile' {pSTNEnabled} -> pSTNEnabled) (\s@CreateProfile' {} a -> s {pSTNEnabled = a} :: CreateProfile)

-- | The tags for the profile.
createProfile_tags :: Lens.Lens' CreateProfile (Core.Maybe [Tag])
createProfile_tags = Lens.lens (\CreateProfile' {tags} -> tags) (\s@CreateProfile' {} a -> s {tags = a} :: CreateProfile) Core.. Lens.mapping Lens._Coerce

-- | The maximum volume limit for a room profile.
createProfile_maxVolumeLimit :: Lens.Lens' CreateProfile (Core.Maybe Core.Int)
createProfile_maxVolumeLimit = Lens.lens (\CreateProfile' {maxVolumeLimit} -> maxVolumeLimit) (\s@CreateProfile' {} a -> s {maxVolumeLimit = a} :: CreateProfile)

-- | The meeting room settings of a room profile.
createProfile_meetingRoomConfiguration :: Lens.Lens' CreateProfile (Core.Maybe CreateMeetingRoomConfiguration)
createProfile_meetingRoomConfiguration = Lens.lens (\CreateProfile' {meetingRoomConfiguration} -> meetingRoomConfiguration) (\s@CreateProfile' {} a -> s {meetingRoomConfiguration = a} :: CreateProfile)

-- | The user-specified token that is used during the creation of a profile.
createProfile_clientRequestToken :: Lens.Lens' CreateProfile (Core.Maybe Core.Text)
createProfile_clientRequestToken = Lens.lens (\CreateProfile' {clientRequestToken} -> clientRequestToken) (\s@CreateProfile' {} a -> s {clientRequestToken = a} :: CreateProfile)

-- | The name of a room profile.
createProfile_profileName :: Lens.Lens' CreateProfile Core.Text
createProfile_profileName = Lens.lens (\CreateProfile' {profileName} -> profileName) (\s@CreateProfile' {} a -> s {profileName = a} :: CreateProfile)

-- | The time zone used by a room profile.
createProfile_timezone :: Lens.Lens' CreateProfile Core.Text
createProfile_timezone = Lens.lens (\CreateProfile' {timezone} -> timezone) (\s@CreateProfile' {} a -> s {timezone = a} :: CreateProfile)

-- | The valid address for the room.
createProfile_address :: Lens.Lens' CreateProfile Core.Text
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProfileResponse'
            Core.<$> (x Core..?> "ProfileArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateProfile

instance Core.NFData CreateProfile

instance Core.ToHeaders CreateProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.CreateProfile" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateProfile where
  toJSON CreateProfile' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Locale" Core..=) Core.<$> locale,
            ("SetupModeDisabled" Core..=)
              Core.<$> setupModeDisabled,
            ("PSTNEnabled" Core..=) Core.<$> pSTNEnabled,
            ("Tags" Core..=) Core.<$> tags,
            ("MaxVolumeLimit" Core..=) Core.<$> maxVolumeLimit,
            ("MeetingRoomConfiguration" Core..=)
              Core.<$> meetingRoomConfiguration,
            ("ClientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            Core.Just ("ProfileName" Core..= profileName),
            Core.Just ("Timezone" Core..= timezone),
            Core.Just ("Address" Core..= address),
            Core.Just ("DistanceUnit" Core..= distanceUnit),
            Core.Just
              ("TemperatureUnit" Core..= temperatureUnit),
            Core.Just ("WakeWord" Core..= wakeWord)
          ]
      )

instance Core.ToPath CreateProfile where
  toPath = Core.const "/"

instance Core.ToQuery CreateProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateProfileResponse' smart constructor.
data CreateProfileResponse = CreateProfileResponse'
  { -- | The ARN of the newly created room profile in the response.
    profileArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateProfileResponse
newCreateProfileResponse pHttpStatus_ =
  CreateProfileResponse'
    { profileArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the newly created room profile in the response.
createProfileResponse_profileArn :: Lens.Lens' CreateProfileResponse (Core.Maybe Core.Text)
createProfileResponse_profileArn = Lens.lens (\CreateProfileResponse' {profileArn} -> profileArn) (\s@CreateProfileResponse' {} a -> s {profileArn = a} :: CreateProfileResponse)

-- | The response's http status code.
createProfileResponse_httpStatus :: Lens.Lens' CreateProfileResponse Core.Int
createProfileResponse_httpStatus = Lens.lens (\CreateProfileResponse' {httpStatus} -> httpStatus) (\s@CreateProfileResponse' {} a -> s {httpStatus = a} :: CreateProfileResponse)

instance Core.NFData CreateProfileResponse
