{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SMS.PutAppValidationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a validation configuration for the specified
-- application.
module Network.AWS.SMS.PutAppValidationConfiguration
  ( -- * Creating a Request
    PutAppValidationConfiguration (..),
    newPutAppValidationConfiguration,

    -- * Request Lenses
    putAppValidationConfiguration_appValidationConfigurations,
    putAppValidationConfiguration_serverGroupValidationConfigurations,
    putAppValidationConfiguration_appId,

    -- * Destructuring the Response
    PutAppValidationConfigurationResponse (..),
    newPutAppValidationConfigurationResponse,

    -- * Response Lenses
    putAppValidationConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newPutAppValidationConfiguration' smart constructor.
data PutAppValidationConfiguration = PutAppValidationConfiguration'
  { -- | The configuration for application validation.
    appValidationConfigurations :: Prelude.Maybe [AppValidationConfiguration],
    -- | The configuration for instance validation.
    serverGroupValidationConfigurations :: Prelude.Maybe [ServerGroupValidationConfiguration],
    -- | The ID of the application.
    appId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutAppValidationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appValidationConfigurations', 'putAppValidationConfiguration_appValidationConfigurations' - The configuration for application validation.
--
-- 'serverGroupValidationConfigurations', 'putAppValidationConfiguration_serverGroupValidationConfigurations' - The configuration for instance validation.
--
-- 'appId', 'putAppValidationConfiguration_appId' - The ID of the application.
newPutAppValidationConfiguration ::
  -- | 'appId'
  Prelude.Text ->
  PutAppValidationConfiguration
newPutAppValidationConfiguration pAppId_ =
  PutAppValidationConfiguration'
    { appValidationConfigurations =
        Prelude.Nothing,
      serverGroupValidationConfigurations =
        Prelude.Nothing,
      appId = pAppId_
    }

-- | The configuration for application validation.
putAppValidationConfiguration_appValidationConfigurations :: Lens.Lens' PutAppValidationConfiguration (Prelude.Maybe [AppValidationConfiguration])
putAppValidationConfiguration_appValidationConfigurations = Lens.lens (\PutAppValidationConfiguration' {appValidationConfigurations} -> appValidationConfigurations) (\s@PutAppValidationConfiguration' {} a -> s {appValidationConfigurations = a} :: PutAppValidationConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | The configuration for instance validation.
putAppValidationConfiguration_serverGroupValidationConfigurations :: Lens.Lens' PutAppValidationConfiguration (Prelude.Maybe [ServerGroupValidationConfiguration])
putAppValidationConfiguration_serverGroupValidationConfigurations = Lens.lens (\PutAppValidationConfiguration' {serverGroupValidationConfigurations} -> serverGroupValidationConfigurations) (\s@PutAppValidationConfiguration' {} a -> s {serverGroupValidationConfigurations = a} :: PutAppValidationConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the application.
putAppValidationConfiguration_appId :: Lens.Lens' PutAppValidationConfiguration Prelude.Text
putAppValidationConfiguration_appId = Lens.lens (\PutAppValidationConfiguration' {appId} -> appId) (\s@PutAppValidationConfiguration' {} a -> s {appId = a} :: PutAppValidationConfiguration)

instance
  Prelude.AWSRequest
    PutAppValidationConfiguration
  where
  type
    Rs PutAppValidationConfiguration =
      PutAppValidationConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutAppValidationConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    PutAppValidationConfiguration

instance Prelude.NFData PutAppValidationConfiguration

instance
  Prelude.ToHeaders
    PutAppValidationConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSServerMigrationService_V2016_10_24.PutAppValidationConfiguration" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutAppValidationConfiguration where
  toJSON PutAppValidationConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("appValidationConfigurations" Prelude..=)
              Prelude.<$> appValidationConfigurations,
            ("serverGroupValidationConfigurations" Prelude..=)
              Prelude.<$> serverGroupValidationConfigurations,
            Prelude.Just ("appId" Prelude..= appId)
          ]
      )

instance Prelude.ToPath PutAppValidationConfiguration where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    PutAppValidationConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutAppValidationConfigurationResponse' smart constructor.
data PutAppValidationConfigurationResponse = PutAppValidationConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
