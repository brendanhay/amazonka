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
-- Module      : Amazonka.ChimeSdkVoice.CreateVoiceProfileDomain
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a voice profile domain, a collection of voice profiles, their
-- voice prints, and encrypted enrollment audio.
--
-- Before creating any voice profiles, you must provide all notices and
-- obtain all consents from the speaker as required under applicable
-- privacy and biometrics laws, and as required under the
-- <https://aws.amazon.com/service-terms/ AWS service terms> for the Amazon
-- Chime SDK.
--
-- For more information about voice profile domains, see
-- <https://docs.aws.amazon.com/chime-sdk/latest/dg/pstn-voice-analytics.html Using Amazon Chime SDK Voice Analytics>
-- in the /Amazon Chime SDK Developer Guide/.
module Amazonka.ChimeSdkVoice.CreateVoiceProfileDomain
  ( -- * Creating a Request
    CreateVoiceProfileDomain (..),
    newCreateVoiceProfileDomain,

    -- * Request Lenses
    createVoiceProfileDomain_clientRequestToken,
    createVoiceProfileDomain_description,
    createVoiceProfileDomain_tags,
    createVoiceProfileDomain_name,
    createVoiceProfileDomain_serverSideEncryptionConfiguration,

    -- * Destructuring the Response
    CreateVoiceProfileDomainResponse (..),
    newCreateVoiceProfileDomainResponse,

    -- * Response Lenses
    createVoiceProfileDomainResponse_voiceProfileDomain,
    createVoiceProfileDomainResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVoiceProfileDomain' smart constructor.
data CreateVoiceProfileDomain = CreateVoiceProfileDomain'
  { -- | The unique identifier for the client request. Use a different token for
    -- different domain creation requests.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | A description of the voice profile domain.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags assigned to the domain.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The name of the voice profile domain.
    name :: Prelude.Text,
    -- | The server-side encryption configuration for the request.
    serverSideEncryptionConfiguration :: ServerSideEncryptionConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVoiceProfileDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'createVoiceProfileDomain_clientRequestToken' - The unique identifier for the client request. Use a different token for
-- different domain creation requests.
--
-- 'description', 'createVoiceProfileDomain_description' - A description of the voice profile domain.
--
-- 'tags', 'createVoiceProfileDomain_tags' - The tags assigned to the domain.
--
-- 'name', 'createVoiceProfileDomain_name' - The name of the voice profile domain.
--
-- 'serverSideEncryptionConfiguration', 'createVoiceProfileDomain_serverSideEncryptionConfiguration' - The server-side encryption configuration for the request.
newCreateVoiceProfileDomain ::
  -- | 'name'
  Prelude.Text ->
  -- | 'serverSideEncryptionConfiguration'
  ServerSideEncryptionConfiguration ->
  CreateVoiceProfileDomain
newCreateVoiceProfileDomain
  pName_
  pServerSideEncryptionConfiguration_ =
    CreateVoiceProfileDomain'
      { clientRequestToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        serverSideEncryptionConfiguration =
          pServerSideEncryptionConfiguration_
      }

-- | The unique identifier for the client request. Use a different token for
-- different domain creation requests.
createVoiceProfileDomain_clientRequestToken :: Lens.Lens' CreateVoiceProfileDomain (Prelude.Maybe Prelude.Text)
createVoiceProfileDomain_clientRequestToken = Lens.lens (\CreateVoiceProfileDomain' {clientRequestToken} -> clientRequestToken) (\s@CreateVoiceProfileDomain' {} a -> s {clientRequestToken = a} :: CreateVoiceProfileDomain)

-- | A description of the voice profile domain.
createVoiceProfileDomain_description :: Lens.Lens' CreateVoiceProfileDomain (Prelude.Maybe Prelude.Text)
createVoiceProfileDomain_description = Lens.lens (\CreateVoiceProfileDomain' {description} -> description) (\s@CreateVoiceProfileDomain' {} a -> s {description = a} :: CreateVoiceProfileDomain)

-- | The tags assigned to the domain.
createVoiceProfileDomain_tags :: Lens.Lens' CreateVoiceProfileDomain (Prelude.Maybe (Prelude.NonEmpty Tag))
createVoiceProfileDomain_tags = Lens.lens (\CreateVoiceProfileDomain' {tags} -> tags) (\s@CreateVoiceProfileDomain' {} a -> s {tags = a} :: CreateVoiceProfileDomain) Prelude.. Lens.mapping Lens.coerced

-- | The name of the voice profile domain.
createVoiceProfileDomain_name :: Lens.Lens' CreateVoiceProfileDomain Prelude.Text
createVoiceProfileDomain_name = Lens.lens (\CreateVoiceProfileDomain' {name} -> name) (\s@CreateVoiceProfileDomain' {} a -> s {name = a} :: CreateVoiceProfileDomain)

-- | The server-side encryption configuration for the request.
createVoiceProfileDomain_serverSideEncryptionConfiguration :: Lens.Lens' CreateVoiceProfileDomain ServerSideEncryptionConfiguration
createVoiceProfileDomain_serverSideEncryptionConfiguration = Lens.lens (\CreateVoiceProfileDomain' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@CreateVoiceProfileDomain' {} a -> s {serverSideEncryptionConfiguration = a} :: CreateVoiceProfileDomain)

instance Core.AWSRequest CreateVoiceProfileDomain where
  type
    AWSResponse CreateVoiceProfileDomain =
      CreateVoiceProfileDomainResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVoiceProfileDomainResponse'
            Prelude.<$> (x Data..?> "VoiceProfileDomain")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVoiceProfileDomain where
  hashWithSalt _salt CreateVoiceProfileDomain' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` serverSideEncryptionConfiguration

instance Prelude.NFData CreateVoiceProfileDomain where
  rnf CreateVoiceProfileDomain' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf serverSideEncryptionConfiguration

instance Data.ToHeaders CreateVoiceProfileDomain where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateVoiceProfileDomain where
  toJSON CreateVoiceProfileDomain' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ( "ServerSideEncryptionConfiguration"
                  Data..= serverSideEncryptionConfiguration
              )
          ]
      )

instance Data.ToPath CreateVoiceProfileDomain where
  toPath = Prelude.const "/voice-profile-domains"

instance Data.ToQuery CreateVoiceProfileDomain where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVoiceProfileDomainResponse' smart constructor.
data CreateVoiceProfileDomainResponse = CreateVoiceProfileDomainResponse'
  { -- | The requested voice profile domain.
    voiceProfileDomain :: Prelude.Maybe VoiceProfileDomain,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVoiceProfileDomainResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'voiceProfileDomain', 'createVoiceProfileDomainResponse_voiceProfileDomain' - The requested voice profile domain.
--
-- 'httpStatus', 'createVoiceProfileDomainResponse_httpStatus' - The response's http status code.
newCreateVoiceProfileDomainResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVoiceProfileDomainResponse
newCreateVoiceProfileDomainResponse pHttpStatus_ =
  CreateVoiceProfileDomainResponse'
    { voiceProfileDomain =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested voice profile domain.
createVoiceProfileDomainResponse_voiceProfileDomain :: Lens.Lens' CreateVoiceProfileDomainResponse (Prelude.Maybe VoiceProfileDomain)
createVoiceProfileDomainResponse_voiceProfileDomain = Lens.lens (\CreateVoiceProfileDomainResponse' {voiceProfileDomain} -> voiceProfileDomain) (\s@CreateVoiceProfileDomainResponse' {} a -> s {voiceProfileDomain = a} :: CreateVoiceProfileDomainResponse)

-- | The response's http status code.
createVoiceProfileDomainResponse_httpStatus :: Lens.Lens' CreateVoiceProfileDomainResponse Prelude.Int
createVoiceProfileDomainResponse_httpStatus = Lens.lens (\CreateVoiceProfileDomainResponse' {httpStatus} -> httpStatus) (\s@CreateVoiceProfileDomainResponse' {} a -> s {httpStatus = a} :: CreateVoiceProfileDomainResponse)

instance
  Prelude.NFData
    CreateVoiceProfileDomainResponse
  where
  rnf CreateVoiceProfileDomainResponse' {..} =
    Prelude.rnf voiceProfileDomain
      `Prelude.seq` Prelude.rnf httpStatus
