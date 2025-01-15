{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ChimeSDKIdentity.Types.EndpointAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKIdentity.Types.EndpointAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The attributes of an @Endpoint@.
--
-- /See:/ 'newEndpointAttributes' smart constructor.
data EndpointAttributes = EndpointAttributes'
  { -- | The VOIP device token for the APNS and APNS_SANDBOX endpoint types.
    voipDeviceToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The device token for the GCM, APNS, and APNS_SANDBOX endpoint types.
    deviceToken :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voipDeviceToken', 'endpointAttributes_voipDeviceToken' - The VOIP device token for the APNS and APNS_SANDBOX endpoint types.
--
-- 'deviceToken', 'endpointAttributes_deviceToken' - The device token for the GCM, APNS, and APNS_SANDBOX endpoint types.
newEndpointAttributes ::
  -- | 'deviceToken'
  Prelude.Text ->
  EndpointAttributes
newEndpointAttributes pDeviceToken_ =
  EndpointAttributes'
    { voipDeviceToken =
        Prelude.Nothing,
      deviceToken = Data._Sensitive Lens.# pDeviceToken_
    }

-- | The VOIP device token for the APNS and APNS_SANDBOX endpoint types.
endpointAttributes_voipDeviceToken :: Lens.Lens' EndpointAttributes (Prelude.Maybe Prelude.Text)
endpointAttributes_voipDeviceToken = Lens.lens (\EndpointAttributes' {voipDeviceToken} -> voipDeviceToken) (\s@EndpointAttributes' {} a -> s {voipDeviceToken = a} :: EndpointAttributes) Prelude.. Lens.mapping Data._Sensitive

-- | The device token for the GCM, APNS, and APNS_SANDBOX endpoint types.
endpointAttributes_deviceToken :: Lens.Lens' EndpointAttributes Prelude.Text
endpointAttributes_deviceToken = Lens.lens (\EndpointAttributes' {deviceToken} -> deviceToken) (\s@EndpointAttributes' {} a -> s {deviceToken = a} :: EndpointAttributes) Prelude.. Data._Sensitive

instance Data.FromJSON EndpointAttributes where
  parseJSON =
    Data.withObject
      "EndpointAttributes"
      ( \x ->
          EndpointAttributes'
            Prelude.<$> (x Data..:? "VoipDeviceToken")
            Prelude.<*> (x Data..: "DeviceToken")
      )

instance Prelude.Hashable EndpointAttributes where
  hashWithSalt _salt EndpointAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` voipDeviceToken
      `Prelude.hashWithSalt` deviceToken

instance Prelude.NFData EndpointAttributes where
  rnf EndpointAttributes' {..} =
    Prelude.rnf voipDeviceToken `Prelude.seq`
      Prelude.rnf deviceToken

instance Data.ToJSON EndpointAttributes where
  toJSON EndpointAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("VoipDeviceToken" Data..=)
              Prelude.<$> voipDeviceToken,
            Prelude.Just ("DeviceToken" Data..= deviceToken)
          ]
      )
