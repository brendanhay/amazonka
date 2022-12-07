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
-- Module      : Amazonka.SMS.PutAppReplicationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the replication configuration for the specified
-- application.
module Amazonka.SMS.PutAppReplicationConfiguration
  ( -- * Creating a Request
    PutAppReplicationConfiguration (..),
    newPutAppReplicationConfiguration,

    -- * Request Lenses
    putAppReplicationConfiguration_serverGroupReplicationConfigurations,
    putAppReplicationConfiguration_appId,

    -- * Destructuring the Response
    PutAppReplicationConfigurationResponse (..),
    newPutAppReplicationConfigurationResponse,

    -- * Response Lenses
    putAppReplicationConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newPutAppReplicationConfiguration' smart constructor.
data PutAppReplicationConfiguration = PutAppReplicationConfiguration'
  { -- | Information about the replication configurations for server groups in
    -- the application.
    serverGroupReplicationConfigurations :: Prelude.Maybe [ServerGroupReplicationConfiguration],
    -- | The ID of the application.
    appId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAppReplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverGroupReplicationConfigurations', 'putAppReplicationConfiguration_serverGroupReplicationConfigurations' - Information about the replication configurations for server groups in
-- the application.
--
-- 'appId', 'putAppReplicationConfiguration_appId' - The ID of the application.
newPutAppReplicationConfiguration ::
  PutAppReplicationConfiguration
newPutAppReplicationConfiguration =
  PutAppReplicationConfiguration'
    { serverGroupReplicationConfigurations =
        Prelude.Nothing,
      appId = Prelude.Nothing
    }

-- | Information about the replication configurations for server groups in
-- the application.
putAppReplicationConfiguration_serverGroupReplicationConfigurations :: Lens.Lens' PutAppReplicationConfiguration (Prelude.Maybe [ServerGroupReplicationConfiguration])
putAppReplicationConfiguration_serverGroupReplicationConfigurations = Lens.lens (\PutAppReplicationConfiguration' {serverGroupReplicationConfigurations} -> serverGroupReplicationConfigurations) (\s@PutAppReplicationConfiguration' {} a -> s {serverGroupReplicationConfigurations = a} :: PutAppReplicationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the application.
putAppReplicationConfiguration_appId :: Lens.Lens' PutAppReplicationConfiguration (Prelude.Maybe Prelude.Text)
putAppReplicationConfiguration_appId = Lens.lens (\PutAppReplicationConfiguration' {appId} -> appId) (\s@PutAppReplicationConfiguration' {} a -> s {appId = a} :: PutAppReplicationConfiguration)

instance
  Core.AWSRequest
    PutAppReplicationConfiguration
  where
  type
    AWSResponse PutAppReplicationConfiguration =
      PutAppReplicationConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutAppReplicationConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutAppReplicationConfiguration
  where
  hashWithSalt
    _salt
    PutAppReplicationConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` serverGroupReplicationConfigurations
        `Prelude.hashWithSalt` appId

instance
  Prelude.NFData
    PutAppReplicationConfiguration
  where
  rnf PutAppReplicationConfiguration' {..} =
    Prelude.rnf serverGroupReplicationConfigurations
      `Prelude.seq` Prelude.rnf appId

instance
  Data.ToHeaders
    PutAppReplicationConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSServerMigrationService_V2016_10_24.PutAppReplicationConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutAppReplicationConfiguration where
  toJSON PutAppReplicationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("serverGroupReplicationConfigurations" Data..=)
              Prelude.<$> serverGroupReplicationConfigurations,
            ("appId" Data..=) Prelude.<$> appId
          ]
      )

instance Data.ToPath PutAppReplicationConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery PutAppReplicationConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAppReplicationConfigurationResponse' smart constructor.
data PutAppReplicationConfigurationResponse = PutAppReplicationConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAppReplicationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putAppReplicationConfigurationResponse_httpStatus' - The response's http status code.
newPutAppReplicationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutAppReplicationConfigurationResponse
newPutAppReplicationConfigurationResponse
  pHttpStatus_ =
    PutAppReplicationConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putAppReplicationConfigurationResponse_httpStatus :: Lens.Lens' PutAppReplicationConfigurationResponse Prelude.Int
putAppReplicationConfigurationResponse_httpStatus = Lens.lens (\PutAppReplicationConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutAppReplicationConfigurationResponse' {} a -> s {httpStatus = a} :: PutAppReplicationConfigurationResponse)

instance
  Prelude.NFData
    PutAppReplicationConfigurationResponse
  where
  rnf PutAppReplicationConfigurationResponse' {..} =
    Prelude.rnf httpStatus
