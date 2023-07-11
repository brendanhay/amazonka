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
-- Module      : Amazonka.ChimeSdkVoice.PutVoiceConnectorProxy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.PutVoiceConnectorProxy
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

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutVoiceConnectorProxy' smart constructor.
data PutVoiceConnectorProxy = PutVoiceConnectorProxy'
  { disabled :: Prelude.Maybe Prelude.Bool,
    fallBackPhoneNumber :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    defaultSessionExpiryMinutes :: Prelude.Int,
    phoneNumberPoolCountries :: Prelude.NonEmpty Prelude.Text,
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
-- 'disabled', 'putVoiceConnectorProxy_disabled' - Undocumented member.
--
-- 'fallBackPhoneNumber', 'putVoiceConnectorProxy_fallBackPhoneNumber' - Undocumented member.
--
-- 'defaultSessionExpiryMinutes', 'putVoiceConnectorProxy_defaultSessionExpiryMinutes' - Undocumented member.
--
-- 'phoneNumberPoolCountries', 'putVoiceConnectorProxy_phoneNumberPoolCountries' - Undocumented member.
--
-- 'voiceConnectorId', 'putVoiceConnectorProxy_voiceConnectorId' - Undocumented member.
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

-- | Undocumented member.
putVoiceConnectorProxy_disabled :: Lens.Lens' PutVoiceConnectorProxy (Prelude.Maybe Prelude.Bool)
putVoiceConnectorProxy_disabled = Lens.lens (\PutVoiceConnectorProxy' {disabled} -> disabled) (\s@PutVoiceConnectorProxy' {} a -> s {disabled = a} :: PutVoiceConnectorProxy)

-- | Undocumented member.
putVoiceConnectorProxy_fallBackPhoneNumber :: Lens.Lens' PutVoiceConnectorProxy (Prelude.Maybe Prelude.Text)
putVoiceConnectorProxy_fallBackPhoneNumber = Lens.lens (\PutVoiceConnectorProxy' {fallBackPhoneNumber} -> fallBackPhoneNumber) (\s@PutVoiceConnectorProxy' {} a -> s {fallBackPhoneNumber = a} :: PutVoiceConnectorProxy) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
putVoiceConnectorProxy_defaultSessionExpiryMinutes :: Lens.Lens' PutVoiceConnectorProxy Prelude.Int
putVoiceConnectorProxy_defaultSessionExpiryMinutes = Lens.lens (\PutVoiceConnectorProxy' {defaultSessionExpiryMinutes} -> defaultSessionExpiryMinutes) (\s@PutVoiceConnectorProxy' {} a -> s {defaultSessionExpiryMinutes = a} :: PutVoiceConnectorProxy)

-- | Undocumented member.
putVoiceConnectorProxy_phoneNumberPoolCountries :: Lens.Lens' PutVoiceConnectorProxy (Prelude.NonEmpty Prelude.Text)
putVoiceConnectorProxy_phoneNumberPoolCountries = Lens.lens (\PutVoiceConnectorProxy' {phoneNumberPoolCountries} -> phoneNumberPoolCountries) (\s@PutVoiceConnectorProxy' {} a -> s {phoneNumberPoolCountries = a} :: PutVoiceConnectorProxy) Prelude.. Lens.coerced

-- | Undocumented member.
putVoiceConnectorProxy_voiceConnectorId :: Lens.Lens' PutVoiceConnectorProxy Prelude.Text
putVoiceConnectorProxy_voiceConnectorId = Lens.lens (\PutVoiceConnectorProxy' {voiceConnectorId} -> voiceConnectorId) (\s@PutVoiceConnectorProxy' {} a -> s {voiceConnectorId = a} :: PutVoiceConnectorProxy)

instance Core.AWSRequest PutVoiceConnectorProxy where
  type
    AWSResponse PutVoiceConnectorProxy =
      PutVoiceConnectorProxyResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutVoiceConnectorProxyResponse'
            Prelude.<$> (x Data..?> "Proxy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutVoiceConnectorProxy where
  hashWithSalt _salt PutVoiceConnectorProxy' {..} =
    _salt
      `Prelude.hashWithSalt` disabled
      `Prelude.hashWithSalt` fallBackPhoneNumber
      `Prelude.hashWithSalt` defaultSessionExpiryMinutes
      `Prelude.hashWithSalt` phoneNumberPoolCountries
      `Prelude.hashWithSalt` voiceConnectorId

instance Prelude.NFData PutVoiceConnectorProxy where
  rnf PutVoiceConnectorProxy' {..} =
    Prelude.rnf disabled
      `Prelude.seq` Prelude.rnf fallBackPhoneNumber
      `Prelude.seq` Prelude.rnf defaultSessionExpiryMinutes
      `Prelude.seq` Prelude.rnf phoneNumberPoolCountries
      `Prelude.seq` Prelude.rnf voiceConnectorId

instance Data.ToHeaders PutVoiceConnectorProxy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON PutVoiceConnectorProxy where
  toJSON PutVoiceConnectorProxy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Disabled" Data..=) Prelude.<$> disabled,
            ("FallBackPhoneNumber" Data..=)
              Prelude.<$> fallBackPhoneNumber,
            Prelude.Just
              ( "DefaultSessionExpiryMinutes"
                  Data..= defaultSessionExpiryMinutes
              ),
            Prelude.Just
              ( "PhoneNumberPoolCountries"
                  Data..= phoneNumberPoolCountries
              )
          ]
      )

instance Data.ToPath PutVoiceConnectorProxy where
  toPath PutVoiceConnectorProxy' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/programmable-numbers/proxy"
      ]

instance Data.ToQuery PutVoiceConnectorProxy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutVoiceConnectorProxyResponse' smart constructor.
data PutVoiceConnectorProxyResponse = PutVoiceConnectorProxyResponse'
  { proxy :: Prelude.Maybe Proxy,
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
-- 'proxy', 'putVoiceConnectorProxyResponse_proxy' - Undocumented member.
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

-- | Undocumented member.
putVoiceConnectorProxyResponse_proxy :: Lens.Lens' PutVoiceConnectorProxyResponse (Prelude.Maybe Proxy)
putVoiceConnectorProxyResponse_proxy = Lens.lens (\PutVoiceConnectorProxyResponse' {proxy} -> proxy) (\s@PutVoiceConnectorProxyResponse' {} a -> s {proxy = a} :: PutVoiceConnectorProxyResponse)

-- | The response's http status code.
putVoiceConnectorProxyResponse_httpStatus :: Lens.Lens' PutVoiceConnectorProxyResponse Prelude.Int
putVoiceConnectorProxyResponse_httpStatus = Lens.lens (\PutVoiceConnectorProxyResponse' {httpStatus} -> httpStatus) (\s@PutVoiceConnectorProxyResponse' {} a -> s {httpStatus = a} :: PutVoiceConnectorProxyResponse)

instance
  Prelude.NFData
    PutVoiceConnectorProxyResponse
  where
  rnf PutVoiceConnectorProxyResponse' {..} =
    Prelude.rnf proxy
      `Prelude.seq` Prelude.rnf httpStatus
