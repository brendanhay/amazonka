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
-- Module      : Amazonka.SMS.GetAppReplicationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the application replication configuration associated with the
-- specified application.
module Amazonka.SMS.GetAppReplicationConfiguration
  ( -- * Creating a Request
    GetAppReplicationConfiguration (..),
    newGetAppReplicationConfiguration,

    -- * Request Lenses
    getAppReplicationConfiguration_appId,

    -- * Destructuring the Response
    GetAppReplicationConfigurationResponse (..),
    newGetAppReplicationConfigurationResponse,

    -- * Response Lenses
    getAppReplicationConfigurationResponse_serverGroupReplicationConfigurations,
    getAppReplicationConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newGetAppReplicationConfiguration' smart constructor.
data GetAppReplicationConfiguration = GetAppReplicationConfiguration'
  { -- | The ID of the application.
    appId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAppReplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'getAppReplicationConfiguration_appId' - The ID of the application.
newGetAppReplicationConfiguration ::
  GetAppReplicationConfiguration
newGetAppReplicationConfiguration =
  GetAppReplicationConfiguration'
    { appId =
        Prelude.Nothing
    }

-- | The ID of the application.
getAppReplicationConfiguration_appId :: Lens.Lens' GetAppReplicationConfiguration (Prelude.Maybe Prelude.Text)
getAppReplicationConfiguration_appId = Lens.lens (\GetAppReplicationConfiguration' {appId} -> appId) (\s@GetAppReplicationConfiguration' {} a -> s {appId = a} :: GetAppReplicationConfiguration)

instance
  Core.AWSRequest
    GetAppReplicationConfiguration
  where
  type
    AWSResponse GetAppReplicationConfiguration =
      GetAppReplicationConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAppReplicationConfigurationResponse'
            Prelude.<$> ( x
                            Data..?> "serverGroupReplicationConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetAppReplicationConfiguration
  where
  hashWithSalt
    _salt
    GetAppReplicationConfiguration' {..} =
      _salt `Prelude.hashWithSalt` appId

instance
  Prelude.NFData
    GetAppReplicationConfiguration
  where
  rnf GetAppReplicationConfiguration' {..} =
    Prelude.rnf appId

instance
  Data.ToHeaders
    GetAppReplicationConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSServerMigrationService_V2016_10_24.GetAppReplicationConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetAppReplicationConfiguration where
  toJSON GetAppReplicationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [("appId" Data..=) Prelude.<$> appId]
      )

instance Data.ToPath GetAppReplicationConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery GetAppReplicationConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAppReplicationConfigurationResponse' smart constructor.
data GetAppReplicationConfigurationResponse = GetAppReplicationConfigurationResponse'
  { -- | The replication configurations associated with server groups in this
    -- application.
    serverGroupReplicationConfigurations :: Prelude.Maybe [ServerGroupReplicationConfiguration],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAppReplicationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverGroupReplicationConfigurations', 'getAppReplicationConfigurationResponse_serverGroupReplicationConfigurations' - The replication configurations associated with server groups in this
-- application.
--
-- 'httpStatus', 'getAppReplicationConfigurationResponse_httpStatus' - The response's http status code.
newGetAppReplicationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAppReplicationConfigurationResponse
newGetAppReplicationConfigurationResponse
  pHttpStatus_ =
    GetAppReplicationConfigurationResponse'
      { serverGroupReplicationConfigurations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The replication configurations associated with server groups in this
-- application.
getAppReplicationConfigurationResponse_serverGroupReplicationConfigurations :: Lens.Lens' GetAppReplicationConfigurationResponse (Prelude.Maybe [ServerGroupReplicationConfiguration])
getAppReplicationConfigurationResponse_serverGroupReplicationConfigurations = Lens.lens (\GetAppReplicationConfigurationResponse' {serverGroupReplicationConfigurations} -> serverGroupReplicationConfigurations) (\s@GetAppReplicationConfigurationResponse' {} a -> s {serverGroupReplicationConfigurations = a} :: GetAppReplicationConfigurationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getAppReplicationConfigurationResponse_httpStatus :: Lens.Lens' GetAppReplicationConfigurationResponse Prelude.Int
getAppReplicationConfigurationResponse_httpStatus = Lens.lens (\GetAppReplicationConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetAppReplicationConfigurationResponse' {} a -> s {httpStatus = a} :: GetAppReplicationConfigurationResponse)

instance
  Prelude.NFData
    GetAppReplicationConfigurationResponse
  where
  rnf GetAppReplicationConfigurationResponse' {..} =
    Prelude.rnf serverGroupReplicationConfigurations
      `Prelude.seq` Prelude.rnf httpStatus
