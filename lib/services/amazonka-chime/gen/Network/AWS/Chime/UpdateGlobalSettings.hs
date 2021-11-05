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
-- Module      : Amazonka.Chime.UpdateGlobalSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates global settings for the administrator\'s AWS account, such as
-- Amazon Chime Business Calling and Amazon Chime Voice Connector settings.
module Amazonka.Chime.UpdateGlobalSettings
  ( -- * Creating a Request
    UpdateGlobalSettings (..),
    newUpdateGlobalSettings,

    -- * Request Lenses
    updateGlobalSettings_businessCalling,
    updateGlobalSettings_voiceConnector,

    -- * Destructuring the Response
    UpdateGlobalSettingsResponse (..),
    newUpdateGlobalSettingsResponse,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateGlobalSettings' smart constructor.
data UpdateGlobalSettings = UpdateGlobalSettings'
  { -- | The Amazon Chime Business Calling settings.
    businessCalling :: Prelude.Maybe BusinessCallingSettings,
    -- | The Amazon Chime Voice Connector settings.
    voiceConnector :: Prelude.Maybe VoiceConnectorSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGlobalSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'businessCalling', 'updateGlobalSettings_businessCalling' - The Amazon Chime Business Calling settings.
--
-- 'voiceConnector', 'updateGlobalSettings_voiceConnector' - The Amazon Chime Voice Connector settings.
newUpdateGlobalSettings ::
  UpdateGlobalSettings
newUpdateGlobalSettings =
  UpdateGlobalSettings'
    { businessCalling =
        Prelude.Nothing,
      voiceConnector = Prelude.Nothing
    }

-- | The Amazon Chime Business Calling settings.
updateGlobalSettings_businessCalling :: Lens.Lens' UpdateGlobalSettings (Prelude.Maybe BusinessCallingSettings)
updateGlobalSettings_businessCalling = Lens.lens (\UpdateGlobalSettings' {businessCalling} -> businessCalling) (\s@UpdateGlobalSettings' {} a -> s {businessCalling = a} :: UpdateGlobalSettings)

-- | The Amazon Chime Voice Connector settings.
updateGlobalSettings_voiceConnector :: Lens.Lens' UpdateGlobalSettings (Prelude.Maybe VoiceConnectorSettings)
updateGlobalSettings_voiceConnector = Lens.lens (\UpdateGlobalSettings' {voiceConnector} -> voiceConnector) (\s@UpdateGlobalSettings' {} a -> s {voiceConnector = a} :: UpdateGlobalSettings)

instance Core.AWSRequest UpdateGlobalSettings where
  type
    AWSResponse UpdateGlobalSettings =
      UpdateGlobalSettingsResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveNull UpdateGlobalSettingsResponse'

instance Prelude.Hashable UpdateGlobalSettings

instance Prelude.NFData UpdateGlobalSettings

instance Core.ToHeaders UpdateGlobalSettings where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateGlobalSettings where
  toJSON UpdateGlobalSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("BusinessCalling" Core..=)
              Prelude.<$> businessCalling,
            ("VoiceConnector" Core..=)
              Prelude.<$> voiceConnector
          ]
      )

instance Core.ToPath UpdateGlobalSettings where
  toPath = Prelude.const "/settings"

instance Core.ToQuery UpdateGlobalSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGlobalSettingsResponse' smart constructor.
data UpdateGlobalSettingsResponse = UpdateGlobalSettingsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGlobalSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateGlobalSettingsResponse ::
  UpdateGlobalSettingsResponse
newUpdateGlobalSettingsResponse =
  UpdateGlobalSettingsResponse'

instance Prelude.NFData UpdateGlobalSettingsResponse
