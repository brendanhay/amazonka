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
-- Module      : Amazonka.ChimeSdkVoice.CreateVoiceProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a voice profile, which consists of an enrolled user and their
-- latest voice print.
--
-- Before creating any voice profiles, you must provide all notices and
-- obtain all consents from the speaker as required under applicable
-- privacy and biometrics laws, and as required under the
-- <https://aws.amazon.com/service-terms/ AWS service terms> for the Amazon
-- Chime SDK.
--
-- For more information about voice profiles and voice analytics, see
-- <https://docs.aws.amazon.com/chime-sdk/latest/dg/pstn-voice-analytics.html Using Amazon Chime SDK Voice Analytics>
-- in the /Amazon Chime SDK Developer Guide/.
module Amazonka.ChimeSdkVoice.CreateVoiceProfile
  ( -- * Creating a Request
    CreateVoiceProfile (..),
    newCreateVoiceProfile,

    -- * Request Lenses
    createVoiceProfile_speakerSearchTaskId,

    -- * Destructuring the Response
    CreateVoiceProfileResponse (..),
    newCreateVoiceProfileResponse,

    -- * Response Lenses
    createVoiceProfileResponse_voiceProfile,
    createVoiceProfileResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVoiceProfile' smart constructor.
data CreateVoiceProfile = CreateVoiceProfile'
  { -- | The ID of the speaker search task.
    speakerSearchTaskId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVoiceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'speakerSearchTaskId', 'createVoiceProfile_speakerSearchTaskId' - The ID of the speaker search task.
newCreateVoiceProfile ::
  -- | 'speakerSearchTaskId'
  Prelude.Text ->
  CreateVoiceProfile
newCreateVoiceProfile pSpeakerSearchTaskId_ =
  CreateVoiceProfile'
    { speakerSearchTaskId =
        pSpeakerSearchTaskId_
    }

-- | The ID of the speaker search task.
createVoiceProfile_speakerSearchTaskId :: Lens.Lens' CreateVoiceProfile Prelude.Text
createVoiceProfile_speakerSearchTaskId = Lens.lens (\CreateVoiceProfile' {speakerSearchTaskId} -> speakerSearchTaskId) (\s@CreateVoiceProfile' {} a -> s {speakerSearchTaskId = a} :: CreateVoiceProfile)

instance Core.AWSRequest CreateVoiceProfile where
  type
    AWSResponse CreateVoiceProfile =
      CreateVoiceProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVoiceProfileResponse'
            Prelude.<$> (x Data..?> "VoiceProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVoiceProfile where
  hashWithSalt _salt CreateVoiceProfile' {..} =
    _salt `Prelude.hashWithSalt` speakerSearchTaskId

instance Prelude.NFData CreateVoiceProfile where
  rnf CreateVoiceProfile' {..} =
    Prelude.rnf speakerSearchTaskId

instance Data.ToHeaders CreateVoiceProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateVoiceProfile where
  toJSON CreateVoiceProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SpeakerSearchTaskId" Data..= speakerSearchTaskId)
          ]
      )

instance Data.ToPath CreateVoiceProfile where
  toPath = Prelude.const "/voice-profiles"

instance Data.ToQuery CreateVoiceProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVoiceProfileResponse' smart constructor.
data CreateVoiceProfileResponse = CreateVoiceProfileResponse'
  { -- | The requested voice profile.
    voiceProfile :: Prelude.Maybe VoiceProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVoiceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceProfile', 'createVoiceProfileResponse_voiceProfile' - The requested voice profile.
--
-- 'httpStatus', 'createVoiceProfileResponse_httpStatus' - The response's http status code.
newCreateVoiceProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVoiceProfileResponse
newCreateVoiceProfileResponse pHttpStatus_ =
  CreateVoiceProfileResponse'
    { voiceProfile =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested voice profile.
createVoiceProfileResponse_voiceProfile :: Lens.Lens' CreateVoiceProfileResponse (Prelude.Maybe VoiceProfile)
createVoiceProfileResponse_voiceProfile = Lens.lens (\CreateVoiceProfileResponse' {voiceProfile} -> voiceProfile) (\s@CreateVoiceProfileResponse' {} a -> s {voiceProfile = a} :: CreateVoiceProfileResponse)

-- | The response's http status code.
createVoiceProfileResponse_httpStatus :: Lens.Lens' CreateVoiceProfileResponse Prelude.Int
createVoiceProfileResponse_httpStatus = Lens.lens (\CreateVoiceProfileResponse' {httpStatus} -> httpStatus) (\s@CreateVoiceProfileResponse' {} a -> s {httpStatus = a} :: CreateVoiceProfileResponse)

instance Prelude.NFData CreateVoiceProfileResponse where
  rnf CreateVoiceProfileResponse' {..} =
    Prelude.rnf voiceProfile
      `Prelude.seq` Prelude.rnf httpStatus
