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
-- Module      : Amazonka.ChimeSdkVoice.CreateProxySession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.CreateProxySession
  ( -- * Creating a Request
    CreateProxySession (..),
    newCreateProxySession,

    -- * Request Lenses
    createProxySession_expiryMinutes,
    createProxySession_geoMatchLevel,
    createProxySession_geoMatchParams,
    createProxySession_name,
    createProxySession_numberSelectionBehavior,
    createProxySession_participantPhoneNumbers,
    createProxySession_capabilities,
    createProxySession_voiceConnectorId,

    -- * Destructuring the Response
    CreateProxySessionResponse (..),
    newCreateProxySessionResponse,

    -- * Response Lenses
    createProxySessionResponse_proxySession,
    createProxySessionResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateProxySession' smart constructor.
data CreateProxySession = CreateProxySession'
  { expiryMinutes :: Prelude.Maybe Prelude.Natural,
    geoMatchLevel :: Prelude.Maybe GeoMatchLevel,
    geoMatchParams :: Prelude.Maybe GeoMatchParams,
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    numberSelectionBehavior :: Prelude.Maybe NumberSelectionBehavior,
    participantPhoneNumbers :: Prelude.NonEmpty (Data.Sensitive Prelude.Text),
    capabilities :: [Capability],
    voiceConnectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProxySession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiryMinutes', 'createProxySession_expiryMinutes' - Undocumented member.
--
-- 'geoMatchLevel', 'createProxySession_geoMatchLevel' - Undocumented member.
--
-- 'geoMatchParams', 'createProxySession_geoMatchParams' - Undocumented member.
--
-- 'name', 'createProxySession_name' - Undocumented member.
--
-- 'numberSelectionBehavior', 'createProxySession_numberSelectionBehavior' - Undocumented member.
--
-- 'participantPhoneNumbers', 'createProxySession_participantPhoneNumbers' - Undocumented member.
--
-- 'capabilities', 'createProxySession_capabilities' - Undocumented member.
--
-- 'voiceConnectorId', 'createProxySession_voiceConnectorId' - Undocumented member.
newCreateProxySession ::
  -- | 'participantPhoneNumbers'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'voiceConnectorId'
  Prelude.Text ->
  CreateProxySession
newCreateProxySession
  pParticipantPhoneNumbers_
  pVoiceConnectorId_ =
    CreateProxySession'
      { expiryMinutes =
          Prelude.Nothing,
        geoMatchLevel = Prelude.Nothing,
        geoMatchParams = Prelude.Nothing,
        name = Prelude.Nothing,
        numberSelectionBehavior = Prelude.Nothing,
        participantPhoneNumbers =
          Lens.coerced Lens.# pParticipantPhoneNumbers_,
        capabilities = Prelude.mempty,
        voiceConnectorId = pVoiceConnectorId_
      }

-- | Undocumented member.
createProxySession_expiryMinutes :: Lens.Lens' CreateProxySession (Prelude.Maybe Prelude.Natural)
createProxySession_expiryMinutes = Lens.lens (\CreateProxySession' {expiryMinutes} -> expiryMinutes) (\s@CreateProxySession' {} a -> s {expiryMinutes = a} :: CreateProxySession)

-- | Undocumented member.
createProxySession_geoMatchLevel :: Lens.Lens' CreateProxySession (Prelude.Maybe GeoMatchLevel)
createProxySession_geoMatchLevel = Lens.lens (\CreateProxySession' {geoMatchLevel} -> geoMatchLevel) (\s@CreateProxySession' {} a -> s {geoMatchLevel = a} :: CreateProxySession)

-- | Undocumented member.
createProxySession_geoMatchParams :: Lens.Lens' CreateProxySession (Prelude.Maybe GeoMatchParams)
createProxySession_geoMatchParams = Lens.lens (\CreateProxySession' {geoMatchParams} -> geoMatchParams) (\s@CreateProxySession' {} a -> s {geoMatchParams = a} :: CreateProxySession)

-- | Undocumented member.
createProxySession_name :: Lens.Lens' CreateProxySession (Prelude.Maybe Prelude.Text)
createProxySession_name = Lens.lens (\CreateProxySession' {name} -> name) (\s@CreateProxySession' {} a -> s {name = a} :: CreateProxySession) Prelude.. Lens.mapping Data._Sensitive

-- | Undocumented member.
createProxySession_numberSelectionBehavior :: Lens.Lens' CreateProxySession (Prelude.Maybe NumberSelectionBehavior)
createProxySession_numberSelectionBehavior = Lens.lens (\CreateProxySession' {numberSelectionBehavior} -> numberSelectionBehavior) (\s@CreateProxySession' {} a -> s {numberSelectionBehavior = a} :: CreateProxySession)

-- | Undocumented member.
createProxySession_participantPhoneNumbers :: Lens.Lens' CreateProxySession (Prelude.NonEmpty Prelude.Text)
createProxySession_participantPhoneNumbers = Lens.lens (\CreateProxySession' {participantPhoneNumbers} -> participantPhoneNumbers) (\s@CreateProxySession' {} a -> s {participantPhoneNumbers = a} :: CreateProxySession) Prelude.. Lens.coerced

-- | Undocumented member.
createProxySession_capabilities :: Lens.Lens' CreateProxySession [Capability]
createProxySession_capabilities = Lens.lens (\CreateProxySession' {capabilities} -> capabilities) (\s@CreateProxySession' {} a -> s {capabilities = a} :: CreateProxySession) Prelude.. Lens.coerced

-- | Undocumented member.
createProxySession_voiceConnectorId :: Lens.Lens' CreateProxySession Prelude.Text
createProxySession_voiceConnectorId = Lens.lens (\CreateProxySession' {voiceConnectorId} -> voiceConnectorId) (\s@CreateProxySession' {} a -> s {voiceConnectorId = a} :: CreateProxySession)

instance Core.AWSRequest CreateProxySession where
  type
    AWSResponse CreateProxySession =
      CreateProxySessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProxySessionResponse'
            Prelude.<$> (x Data..?> "ProxySession")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProxySession where
  hashWithSalt _salt CreateProxySession' {..} =
    _salt `Prelude.hashWithSalt` expiryMinutes
      `Prelude.hashWithSalt` geoMatchLevel
      `Prelude.hashWithSalt` geoMatchParams
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` numberSelectionBehavior
      `Prelude.hashWithSalt` participantPhoneNumbers
      `Prelude.hashWithSalt` capabilities
      `Prelude.hashWithSalt` voiceConnectorId

instance Prelude.NFData CreateProxySession where
  rnf CreateProxySession' {..} =
    Prelude.rnf expiryMinutes
      `Prelude.seq` Prelude.rnf geoMatchLevel
      `Prelude.seq` Prelude.rnf geoMatchParams
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf numberSelectionBehavior
      `Prelude.seq` Prelude.rnf participantPhoneNumbers
      `Prelude.seq` Prelude.rnf capabilities
      `Prelude.seq` Prelude.rnf voiceConnectorId

instance Data.ToHeaders CreateProxySession where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateProxySession where
  toJSON CreateProxySession' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExpiryMinutes" Data..=) Prelude.<$> expiryMinutes,
            ("GeoMatchLevel" Data..=) Prelude.<$> geoMatchLevel,
            ("GeoMatchParams" Data..=)
              Prelude.<$> geoMatchParams,
            ("Name" Data..=) Prelude.<$> name,
            ("NumberSelectionBehavior" Data..=)
              Prelude.<$> numberSelectionBehavior,
            Prelude.Just
              ( "ParticipantPhoneNumbers"
                  Data..= participantPhoneNumbers
              ),
            Prelude.Just ("Capabilities" Data..= capabilities)
          ]
      )

instance Data.ToPath CreateProxySession where
  toPath CreateProxySession' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/proxy-sessions"
      ]

instance Data.ToQuery CreateProxySession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProxySessionResponse' smart constructor.
data CreateProxySessionResponse = CreateProxySessionResponse'
  { proxySession :: Prelude.Maybe ProxySession,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProxySessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'proxySession', 'createProxySessionResponse_proxySession' - Undocumented member.
--
-- 'httpStatus', 'createProxySessionResponse_httpStatus' - The response's http status code.
newCreateProxySessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProxySessionResponse
newCreateProxySessionResponse pHttpStatus_ =
  CreateProxySessionResponse'
    { proxySession =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createProxySessionResponse_proxySession :: Lens.Lens' CreateProxySessionResponse (Prelude.Maybe ProxySession)
createProxySessionResponse_proxySession = Lens.lens (\CreateProxySessionResponse' {proxySession} -> proxySession) (\s@CreateProxySessionResponse' {} a -> s {proxySession = a} :: CreateProxySessionResponse)

-- | The response's http status code.
createProxySessionResponse_httpStatus :: Lens.Lens' CreateProxySessionResponse Prelude.Int
createProxySessionResponse_httpStatus = Lens.lens (\CreateProxySessionResponse' {httpStatus} -> httpStatus) (\s@CreateProxySessionResponse' {} a -> s {httpStatus = a} :: CreateProxySessionResponse)

instance Prelude.NFData CreateProxySessionResponse where
  rnf CreateProxySessionResponse' {..} =
    Prelude.rnf proxySession
      `Prelude.seq` Prelude.rnf httpStatus
