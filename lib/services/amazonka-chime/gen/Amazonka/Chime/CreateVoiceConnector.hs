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
-- Module      : Amazonka.Chime.CreateVoiceConnector
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Chime Voice Connector under the administrator\'s AWS
-- account. You can choose to create an Amazon Chime Voice Connector in a
-- specific AWS Region.
--
-- Enabling CreateVoiceConnectorRequest$RequireEncryption configures your
-- Amazon Chime Voice Connector to use TLS transport for SIP signaling and
-- Secure RTP (SRTP) for media. Inbound calls use TLS transport, and
-- unencrypted outbound calls are blocked.
module Amazonka.Chime.CreateVoiceConnector
  ( -- * Creating a Request
    CreateVoiceConnector (..),
    newCreateVoiceConnector,

    -- * Request Lenses
    createVoiceConnector_awsRegion,
    createVoiceConnector_name,
    createVoiceConnector_requireEncryption,

    -- * Destructuring the Response
    CreateVoiceConnectorResponse (..),
    newCreateVoiceConnectorResponse,

    -- * Response Lenses
    createVoiceConnectorResponse_voiceConnector,
    createVoiceConnectorResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVoiceConnector' smart constructor.
data CreateVoiceConnector = CreateVoiceConnector'
  { -- | The AWS Region in which the Amazon Chime Voice Connector is created.
    -- Default value: @us-east-1@ .
    awsRegion :: Prelude.Maybe VoiceConnectorAwsRegion,
    -- | The name of the Amazon Chime Voice Connector.
    name :: Prelude.Text,
    -- | When enabled, requires encryption for the Amazon Chime Voice Connector.
    requireEncryption :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVoiceConnector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsRegion', 'createVoiceConnector_awsRegion' - The AWS Region in which the Amazon Chime Voice Connector is created.
-- Default value: @us-east-1@ .
--
-- 'name', 'createVoiceConnector_name' - The name of the Amazon Chime Voice Connector.
--
-- 'requireEncryption', 'createVoiceConnector_requireEncryption' - When enabled, requires encryption for the Amazon Chime Voice Connector.
newCreateVoiceConnector ::
  -- | 'name'
  Prelude.Text ->
  -- | 'requireEncryption'
  Prelude.Bool ->
  CreateVoiceConnector
newCreateVoiceConnector pName_ pRequireEncryption_ =
  CreateVoiceConnector'
    { awsRegion = Prelude.Nothing,
      name = pName_,
      requireEncryption = pRequireEncryption_
    }

-- | The AWS Region in which the Amazon Chime Voice Connector is created.
-- Default value: @us-east-1@ .
createVoiceConnector_awsRegion :: Lens.Lens' CreateVoiceConnector (Prelude.Maybe VoiceConnectorAwsRegion)
createVoiceConnector_awsRegion = Lens.lens (\CreateVoiceConnector' {awsRegion} -> awsRegion) (\s@CreateVoiceConnector' {} a -> s {awsRegion = a} :: CreateVoiceConnector)

-- | The name of the Amazon Chime Voice Connector.
createVoiceConnector_name :: Lens.Lens' CreateVoiceConnector Prelude.Text
createVoiceConnector_name = Lens.lens (\CreateVoiceConnector' {name} -> name) (\s@CreateVoiceConnector' {} a -> s {name = a} :: CreateVoiceConnector)

-- | When enabled, requires encryption for the Amazon Chime Voice Connector.
createVoiceConnector_requireEncryption :: Lens.Lens' CreateVoiceConnector Prelude.Bool
createVoiceConnector_requireEncryption = Lens.lens (\CreateVoiceConnector' {requireEncryption} -> requireEncryption) (\s@CreateVoiceConnector' {} a -> s {requireEncryption = a} :: CreateVoiceConnector)

instance Core.AWSRequest CreateVoiceConnector where
  type
    AWSResponse CreateVoiceConnector =
      CreateVoiceConnectorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVoiceConnectorResponse'
            Prelude.<$> (x Data..?> "VoiceConnector")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVoiceConnector where
  hashWithSalt _salt CreateVoiceConnector' {..} =
    _salt `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` requireEncryption

instance Prelude.NFData CreateVoiceConnector where
  rnf CreateVoiceConnector' {..} =
    Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf requireEncryption

instance Data.ToHeaders CreateVoiceConnector where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateVoiceConnector where
  toJSON CreateVoiceConnector' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AwsRegion" Data..=) Prelude.<$> awsRegion,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("RequireEncryption" Data..= requireEncryption)
          ]
      )

instance Data.ToPath CreateVoiceConnector where
  toPath = Prelude.const "/voice-connectors"

instance Data.ToQuery CreateVoiceConnector where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVoiceConnectorResponse' smart constructor.
data CreateVoiceConnectorResponse = CreateVoiceConnectorResponse'
  { -- | The Amazon Chime Voice Connector details.
    voiceConnector :: Prelude.Maybe VoiceConnector,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVoiceConnectorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceConnector', 'createVoiceConnectorResponse_voiceConnector' - The Amazon Chime Voice Connector details.
--
-- 'httpStatus', 'createVoiceConnectorResponse_httpStatus' - The response's http status code.
newCreateVoiceConnectorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVoiceConnectorResponse
newCreateVoiceConnectorResponse pHttpStatus_ =
  CreateVoiceConnectorResponse'
    { voiceConnector =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Chime Voice Connector details.
createVoiceConnectorResponse_voiceConnector :: Lens.Lens' CreateVoiceConnectorResponse (Prelude.Maybe VoiceConnector)
createVoiceConnectorResponse_voiceConnector = Lens.lens (\CreateVoiceConnectorResponse' {voiceConnector} -> voiceConnector) (\s@CreateVoiceConnectorResponse' {} a -> s {voiceConnector = a} :: CreateVoiceConnectorResponse)

-- | The response's http status code.
createVoiceConnectorResponse_httpStatus :: Lens.Lens' CreateVoiceConnectorResponse Prelude.Int
createVoiceConnectorResponse_httpStatus = Lens.lens (\CreateVoiceConnectorResponse' {httpStatus} -> httpStatus) (\s@CreateVoiceConnectorResponse' {} a -> s {httpStatus = a} :: CreateVoiceConnectorResponse)

instance Prelude.NFData CreateVoiceConnectorResponse where
  rnf CreateVoiceConnectorResponse' {..} =
    Prelude.rnf voiceConnector
      `Prelude.seq` Prelude.rnf httpStatus
