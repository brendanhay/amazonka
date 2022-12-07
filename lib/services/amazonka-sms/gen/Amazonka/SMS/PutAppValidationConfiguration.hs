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
-- Module      : Amazonka.SMS.PutAppValidationConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a validation configuration for the specified
-- application.
module Amazonka.SMS.PutAppValidationConfiguration
  ( -- * Creating a Request
    PutAppValidationConfiguration (..),
    newPutAppValidationConfiguration,

    -- * Request Lenses
    putAppValidationConfiguration_serverGroupValidationConfigurations,
    putAppValidationConfiguration_appValidationConfigurations,
    putAppValidationConfiguration_appId,

    -- * Destructuring the Response
    PutAppValidationConfigurationResponse (..),
    newPutAppValidationConfigurationResponse,

    -- * Response Lenses
    putAppValidationConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newPutAppValidationConfiguration' smart constructor.
data PutAppValidationConfiguration = PutAppValidationConfiguration'
  { -- | The configuration for instance validation.
    serverGroupValidationConfigurations :: Prelude.Maybe [ServerGroupValidationConfiguration],
    -- | The configuration for application validation.
    appValidationConfigurations :: Prelude.Maybe [AppValidationConfiguration],
    -- | The ID of the application.
    appId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAppValidationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverGroupValidationConfigurations', 'putAppValidationConfiguration_serverGroupValidationConfigurations' - The configuration for instance validation.
--
-- 'appValidationConfigurations', 'putAppValidationConfiguration_appValidationConfigurations' - The configuration for application validation.
--
-- 'appId', 'putAppValidationConfiguration_appId' - The ID of the application.
newPutAppValidationConfiguration ::
  -- | 'appId'
  Prelude.Text ->
  PutAppValidationConfiguration
newPutAppValidationConfiguration pAppId_ =
  PutAppValidationConfiguration'
    { serverGroupValidationConfigurations =
        Prelude.Nothing,
      appValidationConfigurations =
        Prelude.Nothing,
      appId = pAppId_
    }

-- | The configuration for instance validation.
putAppValidationConfiguration_serverGroupValidationConfigurations :: Lens.Lens' PutAppValidationConfiguration (Prelude.Maybe [ServerGroupValidationConfiguration])
putAppValidationConfiguration_serverGroupValidationConfigurations = Lens.lens (\PutAppValidationConfiguration' {serverGroupValidationConfigurations} -> serverGroupValidationConfigurations) (\s@PutAppValidationConfiguration' {} a -> s {serverGroupValidationConfigurations = a} :: PutAppValidationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The configuration for application validation.
putAppValidationConfiguration_appValidationConfigurations :: Lens.Lens' PutAppValidationConfiguration (Prelude.Maybe [AppValidationConfiguration])
putAppValidationConfiguration_appValidationConfigurations = Lens.lens (\PutAppValidationConfiguration' {appValidationConfigurations} -> appValidationConfigurations) (\s@PutAppValidationConfiguration' {} a -> s {appValidationConfigurations = a} :: PutAppValidationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the application.
putAppValidationConfiguration_appId :: Lens.Lens' PutAppValidationConfiguration Prelude.Text
putAppValidationConfiguration_appId = Lens.lens (\PutAppValidationConfiguration' {appId} -> appId) (\s@PutAppValidationConfiguration' {} a -> s {appId = a} :: PutAppValidationConfiguration)

instance
  Core.AWSRequest
    PutAppValidationConfiguration
  where
  type
    AWSResponse PutAppValidationConfiguration =
      PutAppValidationConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutAppValidationConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutAppValidationConfiguration
  where
  hashWithSalt _salt PutAppValidationConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` serverGroupValidationConfigurations
      `Prelude.hashWithSalt` appValidationConfigurations
      `Prelude.hashWithSalt` appId

instance Prelude.NFData PutAppValidationConfiguration where
  rnf PutAppValidationConfiguration' {..} =
    Prelude.rnf serverGroupValidationConfigurations
      `Prelude.seq` Prelude.rnf appValidationConfigurations
      `Prelude.seq` Prelude.rnf appId

instance Data.ToHeaders PutAppValidationConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSServerMigrationService_V2016_10_24.PutAppValidationConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutAppValidationConfiguration where
  toJSON PutAppValidationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("serverGroupValidationConfigurations" Data..=)
              Prelude.<$> serverGroupValidationConfigurations,
            ("appValidationConfigurations" Data..=)
              Prelude.<$> appValidationConfigurations,
            Prelude.Just ("appId" Data..= appId)
          ]
      )

instance Data.ToPath PutAppValidationConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery PutAppValidationConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAppValidationConfigurationResponse' smart constructor.
data PutAppValidationConfigurationResponse = PutAppValidationConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutAppValidationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putAppValidationConfigurationResponse_httpStatus' - The response's http status code.
newPutAppValidationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutAppValidationConfigurationResponse
newPutAppValidationConfigurationResponse pHttpStatus_ =
  PutAppValidationConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putAppValidationConfigurationResponse_httpStatus :: Lens.Lens' PutAppValidationConfigurationResponse Prelude.Int
putAppValidationConfigurationResponse_httpStatus = Lens.lens (\PutAppValidationConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutAppValidationConfigurationResponse' {} a -> s {httpStatus = a} :: PutAppValidationConfigurationResponse)

instance
  Prelude.NFData
    PutAppValidationConfigurationResponse
  where
  rnf PutAppValidationConfigurationResponse' {..} =
    Prelude.rnf httpStatus
