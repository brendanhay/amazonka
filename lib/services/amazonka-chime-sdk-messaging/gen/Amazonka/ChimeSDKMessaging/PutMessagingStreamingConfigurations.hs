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
-- Module      : Amazonka.ChimeSDKMessaging.PutMessagingStreamingConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the data streaming configuration for an @AppInstance@. For more
-- information, see
-- <https://docs.aws.amazon.com/chime-sdk/latest/dg/streaming-export.html Streaming messaging data>
-- in the /Amazon Chime SDK Developer Guide/.
module Amazonka.ChimeSDKMessaging.PutMessagingStreamingConfigurations
  ( -- * Creating a Request
    PutMessagingStreamingConfigurations (..),
    newPutMessagingStreamingConfigurations,

    -- * Request Lenses
    putMessagingStreamingConfigurations_appInstanceArn,
    putMessagingStreamingConfigurations_streamingConfigurations,

    -- * Destructuring the Response
    PutMessagingStreamingConfigurationsResponse (..),
    newPutMessagingStreamingConfigurationsResponse,

    -- * Response Lenses
    putMessagingStreamingConfigurationsResponse_streamingConfigurations,
    putMessagingStreamingConfigurationsResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKMessaging.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutMessagingStreamingConfigurations' smart constructor.
data PutMessagingStreamingConfigurations = PutMessagingStreamingConfigurations'
  { -- | The ARN of the streaming configuration.
    appInstanceArn :: Prelude.Text,
    -- | The streaming configurations.
    streamingConfigurations :: Prelude.NonEmpty StreamingConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutMessagingStreamingConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceArn', 'putMessagingStreamingConfigurations_appInstanceArn' - The ARN of the streaming configuration.
--
-- 'streamingConfigurations', 'putMessagingStreamingConfigurations_streamingConfigurations' - The streaming configurations.
newPutMessagingStreamingConfigurations ::
  -- | 'appInstanceArn'
  Prelude.Text ->
  -- | 'streamingConfigurations'
  Prelude.NonEmpty StreamingConfiguration ->
  PutMessagingStreamingConfigurations
newPutMessagingStreamingConfigurations
  pAppInstanceArn_
  pStreamingConfigurations_ =
    PutMessagingStreamingConfigurations'
      { appInstanceArn =
          pAppInstanceArn_,
        streamingConfigurations =
          Lens.coerced
            Lens.# pStreamingConfigurations_
      }

-- | The ARN of the streaming configuration.
putMessagingStreamingConfigurations_appInstanceArn :: Lens.Lens' PutMessagingStreamingConfigurations Prelude.Text
putMessagingStreamingConfigurations_appInstanceArn = Lens.lens (\PutMessagingStreamingConfigurations' {appInstanceArn} -> appInstanceArn) (\s@PutMessagingStreamingConfigurations' {} a -> s {appInstanceArn = a} :: PutMessagingStreamingConfigurations)

-- | The streaming configurations.
putMessagingStreamingConfigurations_streamingConfigurations :: Lens.Lens' PutMessagingStreamingConfigurations (Prelude.NonEmpty StreamingConfiguration)
putMessagingStreamingConfigurations_streamingConfigurations = Lens.lens (\PutMessagingStreamingConfigurations' {streamingConfigurations} -> streamingConfigurations) (\s@PutMessagingStreamingConfigurations' {} a -> s {streamingConfigurations = a} :: PutMessagingStreamingConfigurations) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    PutMessagingStreamingConfigurations
  where
  type
    AWSResponse PutMessagingStreamingConfigurations =
      PutMessagingStreamingConfigurationsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutMessagingStreamingConfigurationsResponse'
            Prelude.<$> (x Data..?> "StreamingConfigurations")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutMessagingStreamingConfigurations
  where
  hashWithSalt
    _salt
    PutMessagingStreamingConfigurations' {..} =
      _salt
        `Prelude.hashWithSalt` appInstanceArn
        `Prelude.hashWithSalt` streamingConfigurations

instance
  Prelude.NFData
    PutMessagingStreamingConfigurations
  where
  rnf PutMessagingStreamingConfigurations' {..} =
    Prelude.rnf appInstanceArn
      `Prelude.seq` Prelude.rnf streamingConfigurations

instance
  Data.ToHeaders
    PutMessagingStreamingConfigurations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    PutMessagingStreamingConfigurations
  where
  toJSON PutMessagingStreamingConfigurations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "StreamingConfigurations"
                  Data..= streamingConfigurations
              )
          ]
      )

instance
  Data.ToPath
    PutMessagingStreamingConfigurations
  where
  toPath PutMessagingStreamingConfigurations' {..} =
    Prelude.mconcat
      [ "/app-instances/",
        Data.toBS appInstanceArn,
        "/streaming-configurations"
      ]

instance
  Data.ToQuery
    PutMessagingStreamingConfigurations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutMessagingStreamingConfigurationsResponse' smart constructor.
data PutMessagingStreamingConfigurationsResponse = PutMessagingStreamingConfigurationsResponse'
  { -- | The requested streaming configurations.
    streamingConfigurations :: Prelude.Maybe (Prelude.NonEmpty StreamingConfiguration),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutMessagingStreamingConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'streamingConfigurations', 'putMessagingStreamingConfigurationsResponse_streamingConfigurations' - The requested streaming configurations.
--
-- 'httpStatus', 'putMessagingStreamingConfigurationsResponse_httpStatus' - The response's http status code.
newPutMessagingStreamingConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutMessagingStreamingConfigurationsResponse
newPutMessagingStreamingConfigurationsResponse
  pHttpStatus_ =
    PutMessagingStreamingConfigurationsResponse'
      { streamingConfigurations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The requested streaming configurations.
putMessagingStreamingConfigurationsResponse_streamingConfigurations :: Lens.Lens' PutMessagingStreamingConfigurationsResponse (Prelude.Maybe (Prelude.NonEmpty StreamingConfiguration))
putMessagingStreamingConfigurationsResponse_streamingConfigurations = Lens.lens (\PutMessagingStreamingConfigurationsResponse' {streamingConfigurations} -> streamingConfigurations) (\s@PutMessagingStreamingConfigurationsResponse' {} a -> s {streamingConfigurations = a} :: PutMessagingStreamingConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
putMessagingStreamingConfigurationsResponse_httpStatus :: Lens.Lens' PutMessagingStreamingConfigurationsResponse Prelude.Int
putMessagingStreamingConfigurationsResponse_httpStatus = Lens.lens (\PutMessagingStreamingConfigurationsResponse' {httpStatus} -> httpStatus) (\s@PutMessagingStreamingConfigurationsResponse' {} a -> s {httpStatus = a} :: PutMessagingStreamingConfigurationsResponse)

instance
  Prelude.NFData
    PutMessagingStreamingConfigurationsResponse
  where
  rnf PutMessagingStreamingConfigurationsResponse' {..} =
    Prelude.rnf streamingConfigurations
      `Prelude.seq` Prelude.rnf httpStatus
