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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    updateGlobalSettings_voiceConnector,
    updateGlobalSettings_businessCalling,

    -- * Destructuring the Response
    UpdateGlobalSettingsResponse (..),
    newUpdateGlobalSettingsResponse,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateGlobalSettings' smart constructor.
data UpdateGlobalSettings = UpdateGlobalSettings'
  { -- | The Amazon Chime Voice Connector settings.
    voiceConnector :: Prelude.Maybe VoiceConnectorSettings,
    -- | The Amazon Chime Business Calling settings.
    businessCalling :: Prelude.Maybe BusinessCallingSettings
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
-- 'voiceConnector', 'updateGlobalSettings_voiceConnector' - The Amazon Chime Voice Connector settings.
--
-- 'businessCalling', 'updateGlobalSettings_businessCalling' - The Amazon Chime Business Calling settings.
newUpdateGlobalSettings ::
  UpdateGlobalSettings
newUpdateGlobalSettings =
  UpdateGlobalSettings'
    { voiceConnector =
        Prelude.Nothing,
      businessCalling = Prelude.Nothing
    }

-- | The Amazon Chime Voice Connector settings.
updateGlobalSettings_voiceConnector :: Lens.Lens' UpdateGlobalSettings (Prelude.Maybe VoiceConnectorSettings)
updateGlobalSettings_voiceConnector = Lens.lens (\UpdateGlobalSettings' {voiceConnector} -> voiceConnector) (\s@UpdateGlobalSettings' {} a -> s {voiceConnector = a} :: UpdateGlobalSettings)

-- | The Amazon Chime Business Calling settings.
updateGlobalSettings_businessCalling :: Lens.Lens' UpdateGlobalSettings (Prelude.Maybe BusinessCallingSettings)
updateGlobalSettings_businessCalling = Lens.lens (\UpdateGlobalSettings' {businessCalling} -> businessCalling) (\s@UpdateGlobalSettings' {} a -> s {businessCalling = a} :: UpdateGlobalSettings)

instance Core.AWSRequest UpdateGlobalSettings where
  type
    AWSResponse UpdateGlobalSettings =
      UpdateGlobalSettingsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateGlobalSettingsResponse'

instance Prelude.Hashable UpdateGlobalSettings where
  hashWithSalt _salt UpdateGlobalSettings' {..} =
    _salt `Prelude.hashWithSalt` voiceConnector
      `Prelude.hashWithSalt` businessCalling

instance Prelude.NFData UpdateGlobalSettings where
  rnf UpdateGlobalSettings' {..} =
    Prelude.rnf voiceConnector
      `Prelude.seq` Prelude.rnf businessCalling

instance Core.ToHeaders UpdateGlobalSettings where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateGlobalSettings where
  toJSON UpdateGlobalSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VoiceConnector" Core..=)
              Prelude.<$> voiceConnector,
            ("BusinessCalling" Core..=)
              Prelude.<$> businessCalling
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

instance Prelude.NFData UpdateGlobalSettingsResponse where
  rnf _ = ()
