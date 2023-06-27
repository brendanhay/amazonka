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
-- Module      : Amazonka.ChimeSdkVoice.GetVoiceProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of the specified voice profile.
module Amazonka.ChimeSdkVoice.GetVoiceProfile
  ( -- * Creating a Request
    GetVoiceProfile (..),
    newGetVoiceProfile,

    -- * Request Lenses
    getVoiceProfile_voiceProfileId,

    -- * Destructuring the Response
    GetVoiceProfileResponse (..),
    newGetVoiceProfileResponse,

    -- * Response Lenses
    getVoiceProfileResponse_voiceProfile,
    getVoiceProfileResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetVoiceProfile' smart constructor.
data GetVoiceProfile = GetVoiceProfile'
  { -- | The voice profile ID.
    voiceProfileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVoiceProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceProfileId', 'getVoiceProfile_voiceProfileId' - The voice profile ID.
newGetVoiceProfile ::
  -- | 'voiceProfileId'
  Prelude.Text ->
  GetVoiceProfile
newGetVoiceProfile pVoiceProfileId_ =
  GetVoiceProfile' {voiceProfileId = pVoiceProfileId_}

-- | The voice profile ID.
getVoiceProfile_voiceProfileId :: Lens.Lens' GetVoiceProfile Prelude.Text
getVoiceProfile_voiceProfileId = Lens.lens (\GetVoiceProfile' {voiceProfileId} -> voiceProfileId) (\s@GetVoiceProfile' {} a -> s {voiceProfileId = a} :: GetVoiceProfile)

instance Core.AWSRequest GetVoiceProfile where
  type
    AWSResponse GetVoiceProfile =
      GetVoiceProfileResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVoiceProfileResponse'
            Prelude.<$> (x Data..?> "VoiceProfile")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetVoiceProfile where
  hashWithSalt _salt GetVoiceProfile' {..} =
    _salt `Prelude.hashWithSalt` voiceProfileId

instance Prelude.NFData GetVoiceProfile where
  rnf GetVoiceProfile' {..} = Prelude.rnf voiceProfileId

instance Data.ToHeaders GetVoiceProfile where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetVoiceProfile where
  toPath GetVoiceProfile' {..} =
    Prelude.mconcat
      ["/voice-profiles/", Data.toBS voiceProfileId]

instance Data.ToQuery GetVoiceProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetVoiceProfileResponse' smart constructor.
data GetVoiceProfileResponse = GetVoiceProfileResponse'
  { -- | The voice profile details.
    voiceProfile :: Prelude.Maybe VoiceProfile,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVoiceProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceProfile', 'getVoiceProfileResponse_voiceProfile' - The voice profile details.
--
-- 'httpStatus', 'getVoiceProfileResponse_httpStatus' - The response's http status code.
newGetVoiceProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVoiceProfileResponse
newGetVoiceProfileResponse pHttpStatus_ =
  GetVoiceProfileResponse'
    { voiceProfile =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The voice profile details.
getVoiceProfileResponse_voiceProfile :: Lens.Lens' GetVoiceProfileResponse (Prelude.Maybe VoiceProfile)
getVoiceProfileResponse_voiceProfile = Lens.lens (\GetVoiceProfileResponse' {voiceProfile} -> voiceProfile) (\s@GetVoiceProfileResponse' {} a -> s {voiceProfile = a} :: GetVoiceProfileResponse)

-- | The response's http status code.
getVoiceProfileResponse_httpStatus :: Lens.Lens' GetVoiceProfileResponse Prelude.Int
getVoiceProfileResponse_httpStatus = Lens.lens (\GetVoiceProfileResponse' {httpStatus} -> httpStatus) (\s@GetVoiceProfileResponse' {} a -> s {httpStatus = a} :: GetVoiceProfileResponse)

instance Prelude.NFData GetVoiceProfileResponse where
  rnf GetVoiceProfileResponse' {..} =
    Prelude.rnf voiceProfile
      `Prelude.seq` Prelude.rnf httpStatus
