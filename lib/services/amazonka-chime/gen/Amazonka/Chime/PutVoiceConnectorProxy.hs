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
-- Module      : Amazonka.Chime.PutVoiceConnectorProxy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Puts the specified proxy configuration to the specified Amazon Chime
-- Voice Connector.
module Amazonka.Chime.PutVoiceConnectorProxy
  ( -- * Creating a Request
    PutVoiceConnectorProxy (..),
    newPutVoiceConnectorProxy,

    -- * Request Lenses
    putVoiceConnectorProxy_disabled,
    putVoiceConnectorProxy_fallBackPhoneNumber,
    putVoiceConnectorProxy_defaultSessionExpiryMinutes,
    putVoiceConnectorProxy_phoneNumberPoolCountries,
    putVoiceConnectorProxy_voiceConnectorId,

    -- * Destructuring the Response
    PutVoiceConnectorProxyResponse (..),
    newPutVoiceConnectorProxyResponse,

    -- * Response Lenses
    putVoiceConnectorProxyResponse_proxy,
    putVoiceConnectorProxyResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutVoiceConnectorProxy' smart constructor.
data PutVoiceConnectorProxy = PutVoiceConnectorProxy'
  { -- | When true, stops proxy sessions from being created on the specified
    -- Amazon Chime Voice Connector.
    disabled :: Prelude.Maybe Prelude.Bool,
    -- | The phone number to route calls to after a proxy session expires.
    fallBackPhoneNumber :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The default number of minutes allowed for proxy sessions.
    defaultSessionExpiryMinutes :: Prelude.Int,
    -- | The countries for proxy phone numbers to be selected from.
    phoneNumberPoolCountries :: Prelude.NonEmpty Prelude.Text,
    -- | The Amazon Chime voice connector ID.
    voiceConnectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutVoiceConnectorProxy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disabled', 'putVoiceConnectorProxy_disabled' - When true, stops proxy sessions from being created on the specified
-- Amazon Chime Voice Connector.
--
-- 'fallBackPhoneNumber', 'putVoiceConnectorProxy_fallBackPhoneNumber' - The phone number to route calls to after a proxy session expires.
--
-- 'defaultSessionExpiryMinutes', 'putVoiceConnectorProxy_defaultSessionExpiryMinutes' - The default number of minutes allowed for proxy sessions.
--
-- 'phoneNumberPoolCountries', 'putVoiceConnectorProxy_phoneNumberPoolCountries' - The countries for proxy phone numbers to be selected from.
--
-- 'voiceConnectorId', 'putVoiceConnectorProxy_voiceConnectorId' - The Amazon Chime voice connector ID.
newPutVoiceConnectorProxy ::
  -- | 'defaultSessionExpiryMinutes'
  Prelude.Int ->
  -- | 'phoneNumberPoolCountries'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'voiceConnectorId'
  Prelude.Text ->
  PutVoiceConnectorProxy
newPutVoiceConnectorProxy
  pDefaultSessionExpiryMinutes_
  pPhoneNumberPoolCountries_
  pVoiceConnectorId_ =
    PutVoiceConnectorProxy'
      { disabled = Prelude.Nothing,
        fallBackPhoneNumber = Prelude.Nothing,
        defaultSessionExpiryMinutes =
          pDefaultSessionExpiryMinutes_,
        phoneNumberPoolCountries =
          Lens.coerced Lens.# pPhoneNumberPoolCountries_,
        voiceConnectorId = pVoiceConnectorId_
      }

-- | When true, stops proxy sessions from being created on the specified
-- Amazon Chime Voice Connector.
putVoiceConnectorProxy_disabled :: Lens.Lens' PutVoiceConnectorProxy (Prelude.Maybe Prelude.Bool)
putVoiceConnectorProxy_disabled = Lens.lens (\PutVoiceConnectorProxy' {disabled} -> disabled) (\s@PutVoiceConnectorProxy' {} a -> s {disabled = a} :: PutVoiceConnectorProxy)

-- | The phone number to route calls to after a proxy session expires.
putVoiceConnectorProxy_fallBackPhoneNumber :: Lens.Lens' PutVoiceConnectorProxy (Prelude.Maybe Prelude.Text)
putVoiceConnectorProxy_fallBackPhoneNumber = Lens.lens (\PutVoiceConnectorProxy' {fallBackPhoneNumber} -> fallBackPhoneNumber) (\s@PutVoiceConnectorProxy' {} a -> s {fallBackPhoneNumber = a} :: PutVoiceConnectorProxy) Prelude.. Lens.mapping Core._Sensitive

-- | The default number of minutes allowed for proxy sessions.
putVoiceConnectorProxy_defaultSessionExpiryMinutes :: Lens.Lens' PutVoiceConnectorProxy Prelude.Int
putVoiceConnectorProxy_defaultSessionExpiryMinutes = Lens.lens (\PutVoiceConnectorProxy' {defaultSessionExpiryMinutes} -> defaultSessionExpiryMinutes) (\s@PutVoiceConnectorProxy' {} a -> s {defaultSessionExpiryMinutes = a} :: PutVoiceConnectorProxy)

-- | The countries for proxy phone numbers to be selected from.
putVoiceConnectorProxy_phoneNumberPoolCountries :: Lens.Lens' PutVoiceConnectorProxy (Prelude.NonEmpty Prelude.Text)
putVoiceConnectorProxy_phoneNumberPoolCountries = Lens.lens (\PutVoiceConnectorProxy' {phoneNumberPoolCountries} -> phoneNumberPoolCountries) (\s@PutVoiceConnectorProxy' {} a -> s {phoneNumberPoolCountries = a} :: PutVoiceConnectorProxy) Prelude.. Lens.coerced

-- | The Amazon Chime voice connector ID.
putVoiceConnectorProxy_voiceConnectorId :: Lens.Lens' PutVoiceConnectorProxy Prelude.Text
putVoiceConnectorProxy_voiceConnectorId = Lens.lens (\PutVoiceConnectorProxy' {voiceConnectorId} -> voiceConnectorId) (\s@PutVoiceConnectorProxy' {} a -> s {voiceConnectorId = a} :: PutVoiceConnectorProxy)

instance Core.AWSRequest PutVoiceConnectorProxy where
  type
    AWSResponse PutVoiceConnectorProxy =
      PutVoiceConnectorProxyResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutVoiceConnectorProxyResponse'
            Prelude.<$> (x Core..?> "Proxy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutVoiceConnectorProxy

instance Prelude.NFData PutVoiceConnectorProxy

instance Core.ToHeaders PutVoiceConnectorProxy where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON PutVoiceConnectorProxy where
  toJSON PutVoiceConnectorProxy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Disabled" Core..=) Prelude.<$> disabled,
            ("FallBackPhoneNumber" Core..=)
              Prelude.<$> fallBackPhoneNumber,
            Prelude.Just
              ( "DefaultSessionExpiryMinutes"
                  Core..= defaultSessionExpiryMinutes
              ),
            Prelude.Just
              ( "PhoneNumberPoolCountries"
                  Core..= phoneNumberPoolCountries
              )
          ]
      )

instance Core.ToPath PutVoiceConnectorProxy where
  toPath PutVoiceConnectorProxy' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Core.toBS voiceConnectorId,
        "/programmable-numbers/proxy"
      ]

instance Core.ToQuery PutVoiceConnectorProxy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutVoiceConnectorProxyResponse' smart constructor.
data PutVoiceConnectorProxyResponse = PutVoiceConnectorProxyResponse'
  { -- | The proxy configuration details.
    proxy :: Prelude.Maybe Proxy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutVoiceConnectorProxyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'proxy', 'putVoiceConnectorProxyResponse_proxy' - The proxy configuration details.
--
-- 'httpStatus', 'putVoiceConnectorProxyResponse_httpStatus' - The response's http status code.
newPutVoiceConnectorProxyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutVoiceConnectorProxyResponse
newPutVoiceConnectorProxyResponse pHttpStatus_ =
  PutVoiceConnectorProxyResponse'
    { proxy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The proxy configuration details.
putVoiceConnectorProxyResponse_proxy :: Lens.Lens' PutVoiceConnectorProxyResponse (Prelude.Maybe Proxy)
putVoiceConnectorProxyResponse_proxy = Lens.lens (\PutVoiceConnectorProxyResponse' {proxy} -> proxy) (\s@PutVoiceConnectorProxyResponse' {} a -> s {proxy = a} :: PutVoiceConnectorProxyResponse)

-- | The response's http status code.
putVoiceConnectorProxyResponse_httpStatus :: Lens.Lens' PutVoiceConnectorProxyResponse Prelude.Int
putVoiceConnectorProxyResponse_httpStatus = Lens.lens (\PutVoiceConnectorProxyResponse' {httpStatus} -> httpStatus) (\s@PutVoiceConnectorProxyResponse' {} a -> s {httpStatus = a} :: PutVoiceConnectorProxyResponse)

instance
  Prelude.NFData
    PutVoiceConnectorProxyResponse
