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
-- Module      : Amazonka.ChimeSdkVoice.UpdateVoiceProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified voice profile’s voice print and refreshes its
-- expiration timestamp.
--
-- As a condition of using this feature, you acknowledge that the
-- collection, use, storage, and retention of your caller’s biometric
-- identifiers and biometric information (“biometric data”) in the form of
-- a digital voiceprint requires the caller’s informed consent via a
-- written release. Such consent is required under various state laws,
-- including biometrics laws in Illinois, Texas, Washington and other state
-- privacy laws.
--
-- You must provide a written release to each caller through a process that
-- clearly reflects each caller’s informed consent before using Amazon
-- Chime SDK Voice Insights service, as required under the terms of your
-- agreement with AWS governing your use of the service.
module Amazonka.ChimeSdkVoice.UpdateVoiceProfile
  ( -- * Creating a Request
    UpdateVoiceProfile (..),
    newUpdateVoiceProfile,

    -- * Request Lenses
    updateVoiceProfile_voiceProfileId,
    updateVoiceProfile_speakerSearchTaskId,

    -- * Destructuring the Response
    UpdateVoiceProfileResponse (..),
    newUpdateVoiceProfileResponse,

    -- * Response Lenses
    updateVoiceProfileResponse_voiceProfile,
    updateVoiceProfileResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateVoiceProfile' smart constructor.
data UpdateVoiceProfile = UpdateVoiceProfile'
  { -- | The profile ID.
    voiceProfileId :: Prelude.Text,
    -- | The ID of the speaker search task.
    speakerSearchTaskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVoiceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceProfileId', 'updateVoiceProfile_voiceProfileId' - The profile ID.
--
-- 'speakerSearchTaskId', 'updateVoiceProfile_speakerSearchTaskId' - The ID of the speaker search task.
newUpdateVoiceProfile ::
  -- | 'voiceProfileId'
  Prelude.Text ->
  -- | 'speakerSearchTaskId'
  Prelude.Text ->
  UpdateVoiceProfile
newUpdateVoiceProfile
  pVoiceProfileId_
  pSpeakerSearchTaskId_ =
    UpdateVoiceProfile'
      { voiceProfileId =
          pVoiceProfileId_,
        speakerSearchTaskId = pSpeakerSearchTaskId_
      }

-- | The profile ID.
updateVoiceProfile_voiceProfileId :: Lens.Lens' UpdateVoiceProfile Prelude.Text
updateVoiceProfile_voiceProfileId = Lens.lens (\UpdateVoiceProfile' {voiceProfileId} -> voiceProfileId) (\s@UpdateVoiceProfile' {} a -> s {voiceProfileId = a} :: UpdateVoiceProfile)

-- | The ID of the speaker search task.
updateVoiceProfile_speakerSearchTaskId :: Lens.Lens' UpdateVoiceProfile Prelude.Text
updateVoiceProfile_speakerSearchTaskId = Lens.lens (\UpdateVoiceProfile' {speakerSearchTaskId} -> speakerSearchTaskId) (\s@UpdateVoiceProfile' {} a -> s {speakerSearchTaskId = a} :: UpdateVoiceProfile)

instance Core.AWSRequest UpdateVoiceProfile where
  type
    AWSResponse UpdateVoiceProfile =
      UpdateVoiceProfileResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVoiceProfileResponse'
            Prelude.<$> (x Data..?> "VoiceProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateVoiceProfile where
  hashWithSalt _salt UpdateVoiceProfile' {..} =
    _salt
      `Prelude.hashWithSalt` voiceProfileId
      `Prelude.hashWithSalt` speakerSearchTaskId

instance Prelude.NFData UpdateVoiceProfile where
  rnf UpdateVoiceProfile' {..} =
    Prelude.rnf voiceProfileId
      `Prelude.seq` Prelude.rnf speakerSearchTaskId

instance Data.ToHeaders UpdateVoiceProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateVoiceProfile where
  toJSON UpdateVoiceProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SpeakerSearchTaskId" Data..= speakerSearchTaskId)
          ]
      )

instance Data.ToPath UpdateVoiceProfile where
  toPath UpdateVoiceProfile' {..} =
    Prelude.mconcat
      ["/voice-profiles/", Data.toBS voiceProfileId]

instance Data.ToQuery UpdateVoiceProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateVoiceProfileResponse' smart constructor.
data UpdateVoiceProfileResponse = UpdateVoiceProfileResponse'
  { -- | The updated voice profile settings.
    voiceProfile :: Prelude.Maybe VoiceProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVoiceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceProfile', 'updateVoiceProfileResponse_voiceProfile' - The updated voice profile settings.
--
-- 'httpStatus', 'updateVoiceProfileResponse_httpStatus' - The response's http status code.
newUpdateVoiceProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateVoiceProfileResponse
newUpdateVoiceProfileResponse pHttpStatus_ =
  UpdateVoiceProfileResponse'
    { voiceProfile =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated voice profile settings.
updateVoiceProfileResponse_voiceProfile :: Lens.Lens' UpdateVoiceProfileResponse (Prelude.Maybe VoiceProfile)
updateVoiceProfileResponse_voiceProfile = Lens.lens (\UpdateVoiceProfileResponse' {voiceProfile} -> voiceProfile) (\s@UpdateVoiceProfileResponse' {} a -> s {voiceProfile = a} :: UpdateVoiceProfileResponse)

-- | The response's http status code.
updateVoiceProfileResponse_httpStatus :: Lens.Lens' UpdateVoiceProfileResponse Prelude.Int
updateVoiceProfileResponse_httpStatus = Lens.lens (\UpdateVoiceProfileResponse' {httpStatus} -> httpStatus) (\s@UpdateVoiceProfileResponse' {} a -> s {httpStatus = a} :: UpdateVoiceProfileResponse)

instance Prelude.NFData UpdateVoiceProfileResponse where
  rnf UpdateVoiceProfileResponse' {..} =
    Prelude.rnf voiceProfile
      `Prelude.seq` Prelude.rnf httpStatus
