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
-- Module      : Amazonka.Chime.GetAppInstanceStreamingConfigurations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the streaming settings for an @AppInstance@.
module Amazonka.Chime.GetAppInstanceStreamingConfigurations
  ( -- * Creating a Request
    GetAppInstanceStreamingConfigurations (..),
    newGetAppInstanceStreamingConfigurations,

    -- * Request Lenses
    getAppInstanceStreamingConfigurations_appInstanceArn,

    -- * Destructuring the Response
    GetAppInstanceStreamingConfigurationsResponse (..),
    newGetAppInstanceStreamingConfigurationsResponse,

    -- * Response Lenses
    getAppInstanceStreamingConfigurationsResponse_appInstanceStreamingConfigurations,
    getAppInstanceStreamingConfigurationsResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAppInstanceStreamingConfigurations' smart constructor.
data GetAppInstanceStreamingConfigurations = GetAppInstanceStreamingConfigurations'
  { -- | The ARN of the @AppInstance@.
    appInstanceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAppInstanceStreamingConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceArn', 'getAppInstanceStreamingConfigurations_appInstanceArn' - The ARN of the @AppInstance@.
newGetAppInstanceStreamingConfigurations ::
  -- | 'appInstanceArn'
  Prelude.Text ->
  GetAppInstanceStreamingConfigurations
newGetAppInstanceStreamingConfigurations
  pAppInstanceArn_ =
    GetAppInstanceStreamingConfigurations'
      { appInstanceArn =
          pAppInstanceArn_
      }

-- | The ARN of the @AppInstance@.
getAppInstanceStreamingConfigurations_appInstanceArn :: Lens.Lens' GetAppInstanceStreamingConfigurations Prelude.Text
getAppInstanceStreamingConfigurations_appInstanceArn = Lens.lens (\GetAppInstanceStreamingConfigurations' {appInstanceArn} -> appInstanceArn) (\s@GetAppInstanceStreamingConfigurations' {} a -> s {appInstanceArn = a} :: GetAppInstanceStreamingConfigurations)

instance
  Core.AWSRequest
    GetAppInstanceStreamingConfigurations
  where
  type
    AWSResponse
      GetAppInstanceStreamingConfigurations =
      GetAppInstanceStreamingConfigurationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppInstanceStreamingConfigurationsResponse'
            Prelude.<$> (x Data..?> "AppInstanceStreamingConfigurations")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetAppInstanceStreamingConfigurations
  where
  hashWithSalt
    _salt
    GetAppInstanceStreamingConfigurations' {..} =
      _salt `Prelude.hashWithSalt` appInstanceArn

instance
  Prelude.NFData
    GetAppInstanceStreamingConfigurations
  where
  rnf GetAppInstanceStreamingConfigurations' {..} =
    Prelude.rnf appInstanceArn

instance
  Data.ToHeaders
    GetAppInstanceStreamingConfigurations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    GetAppInstanceStreamingConfigurations
  where
  toPath GetAppInstanceStreamingConfigurations' {..} =
    Prelude.mconcat
      [ "/app-instances/",
        Data.toBS appInstanceArn,
        "/streaming-configurations"
      ]

instance
  Data.ToQuery
    GetAppInstanceStreamingConfigurations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAppInstanceStreamingConfigurationsResponse' smart constructor.
data GetAppInstanceStreamingConfigurationsResponse = GetAppInstanceStreamingConfigurationsResponse'
  { -- | The streaming settings.
    appInstanceStreamingConfigurations :: Prelude.Maybe (Prelude.NonEmpty AppInstanceStreamingConfiguration),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAppInstanceStreamingConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceStreamingConfigurations', 'getAppInstanceStreamingConfigurationsResponse_appInstanceStreamingConfigurations' - The streaming settings.
--
-- 'httpStatus', 'getAppInstanceStreamingConfigurationsResponse_httpStatus' - The response's http status code.
newGetAppInstanceStreamingConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAppInstanceStreamingConfigurationsResponse
newGetAppInstanceStreamingConfigurationsResponse
  pHttpStatus_ =
    GetAppInstanceStreamingConfigurationsResponse'
      { appInstanceStreamingConfigurations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The streaming settings.
getAppInstanceStreamingConfigurationsResponse_appInstanceStreamingConfigurations :: Lens.Lens' GetAppInstanceStreamingConfigurationsResponse (Prelude.Maybe (Prelude.NonEmpty AppInstanceStreamingConfiguration))
getAppInstanceStreamingConfigurationsResponse_appInstanceStreamingConfigurations = Lens.lens (\GetAppInstanceStreamingConfigurationsResponse' {appInstanceStreamingConfigurations} -> appInstanceStreamingConfigurations) (\s@GetAppInstanceStreamingConfigurationsResponse' {} a -> s {appInstanceStreamingConfigurations = a} :: GetAppInstanceStreamingConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getAppInstanceStreamingConfigurationsResponse_httpStatus :: Lens.Lens' GetAppInstanceStreamingConfigurationsResponse Prelude.Int
getAppInstanceStreamingConfigurationsResponse_httpStatus = Lens.lens (\GetAppInstanceStreamingConfigurationsResponse' {httpStatus} -> httpStatus) (\s@GetAppInstanceStreamingConfigurationsResponse' {} a -> s {httpStatus = a} :: GetAppInstanceStreamingConfigurationsResponse)

instance
  Prelude.NFData
    GetAppInstanceStreamingConfigurationsResponse
  where
  rnf
    GetAppInstanceStreamingConfigurationsResponse' {..} =
      Prelude.rnf appInstanceStreamingConfigurations
        `Prelude.seq` Prelude.rnf httpStatus
