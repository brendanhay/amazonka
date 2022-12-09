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
-- Module      : Amazonka.ChimeSdkVoice.GetGlobalSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.GetGlobalSettings
  ( -- * Creating a Request
    GetGlobalSettings (..),
    newGetGlobalSettings,

    -- * Destructuring the Response
    GetGlobalSettingsResponse (..),
    newGetGlobalSettingsResponse,

    -- * Response Lenses
    getGlobalSettingsResponse_voiceConnector,
    getGlobalSettingsResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetGlobalSettings' smart constructor.
data GetGlobalSettings = GetGlobalSettings'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGlobalSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetGlobalSettings ::
  GetGlobalSettings
newGetGlobalSettings = GetGlobalSettings'

instance Core.AWSRequest GetGlobalSettings where
  type
    AWSResponse GetGlobalSettings =
      GetGlobalSettingsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGlobalSettingsResponse'
            Prelude.<$> (x Data..?> "VoiceConnector")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetGlobalSettings where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetGlobalSettings where
  rnf _ = ()

instance Data.ToHeaders GetGlobalSettings where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetGlobalSettings where
  toPath = Prelude.const "/settings"

instance Data.ToQuery GetGlobalSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetGlobalSettingsResponse' smart constructor.
data GetGlobalSettingsResponse = GetGlobalSettingsResponse'
  { voiceConnector :: Prelude.Maybe VoiceConnectorSettings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGlobalSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnector', 'getGlobalSettingsResponse_voiceConnector' - Undocumented member.
--
-- 'httpStatus', 'getGlobalSettingsResponse_httpStatus' - The response's http status code.
newGetGlobalSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetGlobalSettingsResponse
newGetGlobalSettingsResponse pHttpStatus_ =
  GetGlobalSettingsResponse'
    { voiceConnector =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getGlobalSettingsResponse_voiceConnector :: Lens.Lens' GetGlobalSettingsResponse (Prelude.Maybe VoiceConnectorSettings)
getGlobalSettingsResponse_voiceConnector = Lens.lens (\GetGlobalSettingsResponse' {voiceConnector} -> voiceConnector) (\s@GetGlobalSettingsResponse' {} a -> s {voiceConnector = a} :: GetGlobalSettingsResponse)

-- | The response's http status code.
getGlobalSettingsResponse_httpStatus :: Lens.Lens' GetGlobalSettingsResponse Prelude.Int
getGlobalSettingsResponse_httpStatus = Lens.lens (\GetGlobalSettingsResponse' {httpStatus} -> httpStatus) (\s@GetGlobalSettingsResponse' {} a -> s {httpStatus = a} :: GetGlobalSettingsResponse)

instance Prelude.NFData GetGlobalSettingsResponse where
  rnf GetGlobalSettingsResponse' {..} =
    Prelude.rnf voiceConnector
      `Prelude.seq` Prelude.rnf httpStatus
