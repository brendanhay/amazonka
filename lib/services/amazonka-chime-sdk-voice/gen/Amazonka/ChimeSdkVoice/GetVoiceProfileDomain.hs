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
-- Module      : Amazonka.ChimeSdkVoice.GetVoiceProfileDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of the specified voice profile domain.
module Amazonka.ChimeSdkVoice.GetVoiceProfileDomain
  ( -- * Creating a Request
    GetVoiceProfileDomain (..),
    newGetVoiceProfileDomain,

    -- * Request Lenses
    getVoiceProfileDomain_voiceProfileDomainId,

    -- * Destructuring the Response
    GetVoiceProfileDomainResponse (..),
    newGetVoiceProfileDomainResponse,

    -- * Response Lenses
    getVoiceProfileDomainResponse_voiceProfileDomain,
    getVoiceProfileDomainResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetVoiceProfileDomain' smart constructor.
data GetVoiceProfileDomain = GetVoiceProfileDomain'
  { -- | The voice profile domain ID.
    voiceProfileDomainId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVoiceProfileDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceProfileDomainId', 'getVoiceProfileDomain_voiceProfileDomainId' - The voice profile domain ID.
newGetVoiceProfileDomain ::
  -- | 'voiceProfileDomainId'
  Prelude.Text ->
  GetVoiceProfileDomain
newGetVoiceProfileDomain pVoiceProfileDomainId_ =
  GetVoiceProfileDomain'
    { voiceProfileDomainId =
        pVoiceProfileDomainId_
    }

-- | The voice profile domain ID.
getVoiceProfileDomain_voiceProfileDomainId :: Lens.Lens' GetVoiceProfileDomain Prelude.Text
getVoiceProfileDomain_voiceProfileDomainId = Lens.lens (\GetVoiceProfileDomain' {voiceProfileDomainId} -> voiceProfileDomainId) (\s@GetVoiceProfileDomain' {} a -> s {voiceProfileDomainId = a} :: GetVoiceProfileDomain)

instance Core.AWSRequest GetVoiceProfileDomain where
  type
    AWSResponse GetVoiceProfileDomain =
      GetVoiceProfileDomainResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetVoiceProfileDomainResponse'
            Prelude.<$> (x Data..?> "VoiceProfileDomain")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetVoiceProfileDomain where
  hashWithSalt _salt GetVoiceProfileDomain' {..} =
    _salt `Prelude.hashWithSalt` voiceProfileDomainId

instance Prelude.NFData GetVoiceProfileDomain where
  rnf GetVoiceProfileDomain' {..} =
    Prelude.rnf voiceProfileDomainId

instance Data.ToHeaders GetVoiceProfileDomain where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetVoiceProfileDomain where
  toPath GetVoiceProfileDomain' {..} =
    Prelude.mconcat
      [ "/voice-profile-domains/",
        Data.toBS voiceProfileDomainId
      ]

instance Data.ToQuery GetVoiceProfileDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetVoiceProfileDomainResponse' smart constructor.
data GetVoiceProfileDomainResponse = GetVoiceProfileDomainResponse'
  { -- | The details of the voice profile domain.
    voiceProfileDomain :: Prelude.Maybe VoiceProfileDomain,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetVoiceProfileDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceProfileDomain', 'getVoiceProfileDomainResponse_voiceProfileDomain' - The details of the voice profile domain.
--
-- 'httpStatus', 'getVoiceProfileDomainResponse_httpStatus' - The response's http status code.
newGetVoiceProfileDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetVoiceProfileDomainResponse
newGetVoiceProfileDomainResponse pHttpStatus_ =
  GetVoiceProfileDomainResponse'
    { voiceProfileDomain =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The details of the voice profile domain.
getVoiceProfileDomainResponse_voiceProfileDomain :: Lens.Lens' GetVoiceProfileDomainResponse (Prelude.Maybe VoiceProfileDomain)
getVoiceProfileDomainResponse_voiceProfileDomain = Lens.lens (\GetVoiceProfileDomainResponse' {voiceProfileDomain} -> voiceProfileDomain) (\s@GetVoiceProfileDomainResponse' {} a -> s {voiceProfileDomain = a} :: GetVoiceProfileDomainResponse)

-- | The response's http status code.
getVoiceProfileDomainResponse_httpStatus :: Lens.Lens' GetVoiceProfileDomainResponse Prelude.Int
getVoiceProfileDomainResponse_httpStatus = Lens.lens (\GetVoiceProfileDomainResponse' {httpStatus} -> httpStatus) (\s@GetVoiceProfileDomainResponse' {} a -> s {httpStatus = a} :: GetVoiceProfileDomainResponse)

instance Prelude.NFData GetVoiceProfileDomainResponse where
  rnf GetVoiceProfileDomainResponse' {..} =
    Prelude.rnf voiceProfileDomain
      `Prelude.seq` Prelude.rnf httpStatus
