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
-- Module      : Amazonka.ChimeSDKMessaging.GetMessagingStreamingConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the data streaming configuration for an @AppInstance@. For
-- more information, see
-- <https://docs.aws.amazon.com/chime-sdk/latest/dg/streaming-export.html Streaming messaging data>
-- in the /Amazon Chime SDK Developer Guide/.
module Amazonka.ChimeSDKMessaging.GetMessagingStreamingConfigurations
  ( -- * Creating a Request
    GetMessagingStreamingConfigurations (..),
    newGetMessagingStreamingConfigurations,

    -- * Request Lenses
    getMessagingStreamingConfigurations_appInstanceArn,

    -- * Destructuring the Response
    GetMessagingStreamingConfigurationsResponse (..),
    newGetMessagingStreamingConfigurationsResponse,

    -- * Response Lenses
    getMessagingStreamingConfigurationsResponse_streamingConfigurations,
    getMessagingStreamingConfigurationsResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMessagingStreamingConfigurations' smart constructor.
data GetMessagingStreamingConfigurations = GetMessagingStreamingConfigurations'
  { -- | The ARN of the streaming configurations.
    appInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMessagingStreamingConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceArn', 'getMessagingStreamingConfigurations_appInstanceArn' - The ARN of the streaming configurations.
newGetMessagingStreamingConfigurations ::
  -- | 'appInstanceArn'
  Prelude.Text ->
  GetMessagingStreamingConfigurations
newGetMessagingStreamingConfigurations
  pAppInstanceArn_ =
    GetMessagingStreamingConfigurations'
      { appInstanceArn =
          pAppInstanceArn_
      }

-- | The ARN of the streaming configurations.
getMessagingStreamingConfigurations_appInstanceArn :: Lens.Lens' GetMessagingStreamingConfigurations Prelude.Text
getMessagingStreamingConfigurations_appInstanceArn = Lens.lens (\GetMessagingStreamingConfigurations' {appInstanceArn} -> appInstanceArn) (\s@GetMessagingStreamingConfigurations' {} a -> s {appInstanceArn = a} :: GetMessagingStreamingConfigurations)

instance
  Core.AWSRequest
    GetMessagingStreamingConfigurations
  where
  type
    AWSResponse GetMessagingStreamingConfigurations =
      GetMessagingStreamingConfigurationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMessagingStreamingConfigurationsResponse'
            Prelude.<$> (x Data..?> "StreamingConfigurations")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetMessagingStreamingConfigurations
  where
  hashWithSalt
    _salt
    GetMessagingStreamingConfigurations' {..} =
      _salt `Prelude.hashWithSalt` appInstanceArn

instance
  Prelude.NFData
    GetMessagingStreamingConfigurations
  where
  rnf GetMessagingStreamingConfigurations' {..} =
    Prelude.rnf appInstanceArn

instance
  Data.ToHeaders
    GetMessagingStreamingConfigurations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GetMessagingStreamingConfigurations
  where
  toPath GetMessagingStreamingConfigurations' {..} =
    Prelude.mconcat
      [ "/app-instances/",
        Data.toBS appInstanceArn,
        "/streaming-configurations"
      ]

instance
  Data.ToQuery
    GetMessagingStreamingConfigurations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMessagingStreamingConfigurationsResponse' smart constructor.
data GetMessagingStreamingConfigurationsResponse = GetMessagingStreamingConfigurationsResponse'
  { -- | The streaming settings.
    streamingConfigurations :: Prelude.Maybe (Prelude.NonEmpty StreamingConfiguration),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMessagingStreamingConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamingConfigurations', 'getMessagingStreamingConfigurationsResponse_streamingConfigurations' - The streaming settings.
--
-- 'httpStatus', 'getMessagingStreamingConfigurationsResponse_httpStatus' - The response's http status code.
newGetMessagingStreamingConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMessagingStreamingConfigurationsResponse
newGetMessagingStreamingConfigurationsResponse
  pHttpStatus_ =
    GetMessagingStreamingConfigurationsResponse'
      { streamingConfigurations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The streaming settings.
getMessagingStreamingConfigurationsResponse_streamingConfigurations :: Lens.Lens' GetMessagingStreamingConfigurationsResponse (Prelude.Maybe (Prelude.NonEmpty StreamingConfiguration))
getMessagingStreamingConfigurationsResponse_streamingConfigurations = Lens.lens (\GetMessagingStreamingConfigurationsResponse' {streamingConfigurations} -> streamingConfigurations) (\s@GetMessagingStreamingConfigurationsResponse' {} a -> s {streamingConfigurations = a} :: GetMessagingStreamingConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getMessagingStreamingConfigurationsResponse_httpStatus :: Lens.Lens' GetMessagingStreamingConfigurationsResponse Prelude.Int
getMessagingStreamingConfigurationsResponse_httpStatus = Lens.lens (\GetMessagingStreamingConfigurationsResponse' {httpStatus} -> httpStatus) (\s@GetMessagingStreamingConfigurationsResponse' {} a -> s {httpStatus = a} :: GetMessagingStreamingConfigurationsResponse)

instance
  Prelude.NFData
    GetMessagingStreamingConfigurationsResponse
  where
  rnf GetMessagingStreamingConfigurationsResponse' {..} =
    Prelude.rnf streamingConfigurations
      `Prelude.seq` Prelude.rnf httpStatus
