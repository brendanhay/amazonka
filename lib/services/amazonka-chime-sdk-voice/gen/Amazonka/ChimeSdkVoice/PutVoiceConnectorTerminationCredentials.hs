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
-- Module      : Amazonka.ChimeSdkVoice.PutVoiceConnectorTerminationCredentials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.PutVoiceConnectorTerminationCredentials
  ( -- * Creating a Request
    PutVoiceConnectorTerminationCredentials (..),
    newPutVoiceConnectorTerminationCredentials,

    -- * Request Lenses
    putVoiceConnectorTerminationCredentials_credentials,
    putVoiceConnectorTerminationCredentials_voiceConnectorId,

    -- * Destructuring the Response
    PutVoiceConnectorTerminationCredentialsResponse (..),
    newPutVoiceConnectorTerminationCredentialsResponse,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutVoiceConnectorTerminationCredentials' smart constructor.
data PutVoiceConnectorTerminationCredentials = PutVoiceConnectorTerminationCredentials'
  { credentials :: Prelude.Maybe [Credential],
    voiceConnectorId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutVoiceConnectorTerminationCredentials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credentials', 'putVoiceConnectorTerminationCredentials_credentials' - Undocumented member.
--
-- 'voiceConnectorId', 'putVoiceConnectorTerminationCredentials_voiceConnectorId' - Undocumented member.
newPutVoiceConnectorTerminationCredentials ::
  -- | 'voiceConnectorId'
  Prelude.Text ->
  PutVoiceConnectorTerminationCredentials
newPutVoiceConnectorTerminationCredentials
  pVoiceConnectorId_ =
    PutVoiceConnectorTerminationCredentials'
      { credentials =
          Prelude.Nothing,
        voiceConnectorId =
          pVoiceConnectorId_
      }

-- | Undocumented member.
putVoiceConnectorTerminationCredentials_credentials :: Lens.Lens' PutVoiceConnectorTerminationCredentials (Prelude.Maybe [Credential])
putVoiceConnectorTerminationCredentials_credentials = Lens.lens (\PutVoiceConnectorTerminationCredentials' {credentials} -> credentials) (\s@PutVoiceConnectorTerminationCredentials' {} a -> s {credentials = a} :: PutVoiceConnectorTerminationCredentials) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
putVoiceConnectorTerminationCredentials_voiceConnectorId :: Lens.Lens' PutVoiceConnectorTerminationCredentials Prelude.Text
putVoiceConnectorTerminationCredentials_voiceConnectorId = Lens.lens (\PutVoiceConnectorTerminationCredentials' {voiceConnectorId} -> voiceConnectorId) (\s@PutVoiceConnectorTerminationCredentials' {} a -> s {voiceConnectorId = a} :: PutVoiceConnectorTerminationCredentials)

instance
  Core.AWSRequest
    PutVoiceConnectorTerminationCredentials
  where
  type
    AWSResponse
      PutVoiceConnectorTerminationCredentials =
      PutVoiceConnectorTerminationCredentialsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      PutVoiceConnectorTerminationCredentialsResponse'

instance
  Prelude.Hashable
    PutVoiceConnectorTerminationCredentials
  where
  hashWithSalt
    _salt
    PutVoiceConnectorTerminationCredentials' {..} =
      _salt
        `Prelude.hashWithSalt` credentials
        `Prelude.hashWithSalt` voiceConnectorId

instance
  Prelude.NFData
    PutVoiceConnectorTerminationCredentials
  where
  rnf PutVoiceConnectorTerminationCredentials' {..} =
    Prelude.rnf credentials
      `Prelude.seq` Prelude.rnf voiceConnectorId

instance
  Data.ToHeaders
    PutVoiceConnectorTerminationCredentials
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    PutVoiceConnectorTerminationCredentials
  where
  toJSON PutVoiceConnectorTerminationCredentials' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Credentials" Data..=) Prelude.<$> credentials]
      )

instance
  Data.ToPath
    PutVoiceConnectorTerminationCredentials
  where
  toPath PutVoiceConnectorTerminationCredentials' {..} =
    Prelude.mconcat
      [ "/voice-connectors/",
        Data.toBS voiceConnectorId,
        "/termination/credentials"
      ]

instance
  Data.ToQuery
    PutVoiceConnectorTerminationCredentials
  where
  toQuery =
    Prelude.const (Prelude.mconcat ["operation=put"])

-- | /See:/ 'newPutVoiceConnectorTerminationCredentialsResponse' smart constructor.
data PutVoiceConnectorTerminationCredentialsResponse = PutVoiceConnectorTerminationCredentialsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutVoiceConnectorTerminationCredentialsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutVoiceConnectorTerminationCredentialsResponse ::
  PutVoiceConnectorTerminationCredentialsResponse
newPutVoiceConnectorTerminationCredentialsResponse =
  PutVoiceConnectorTerminationCredentialsResponse'

instance
  Prelude.NFData
    PutVoiceConnectorTerminationCredentialsResponse
  where
  rnf _ = ()
