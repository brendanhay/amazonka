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
-- Module      : Network.AWS.SMS.PutAppReplicationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the replication configuration for the specified
-- application.
module Network.AWS.SMS.PutAppReplicationConfiguration
  ( -- * Creating a Request
    PutAppReplicationConfiguration (..),
    newPutAppReplicationConfiguration,

    -- * Request Lenses
    putAppReplicationConfiguration_appId,
    putAppReplicationConfiguration_serverGroupReplicationConfigurations,

    -- * Destructuring the Response
    PutAppReplicationConfigurationResponse (..),
    newPutAppReplicationConfigurationResponse,

    -- * Response Lenses
    putAppReplicationConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newPutAppReplicationConfiguration' smart constructor.
data PutAppReplicationConfiguration = PutAppReplicationConfiguration'
  { -- | The ID of the application.
    appId :: Core.Maybe Core.Text,
    -- | Information about the replication configurations for server groups in
    -- the application.
    serverGroupReplicationConfigurations :: Core.Maybe [ServerGroupReplicationConfiguration]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutAppReplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'putAppReplicationConfiguration_appId' - The ID of the application.
--
-- 'serverGroupReplicationConfigurations', 'putAppReplicationConfiguration_serverGroupReplicationConfigurations' - Information about the replication configurations for server groups in
-- the application.
newPutAppReplicationConfiguration ::
  PutAppReplicationConfiguration
newPutAppReplicationConfiguration =
  PutAppReplicationConfiguration'
    { appId =
        Core.Nothing,
      serverGroupReplicationConfigurations =
        Core.Nothing
    }

-- | The ID of the application.
putAppReplicationConfiguration_appId :: Lens.Lens' PutAppReplicationConfiguration (Core.Maybe Core.Text)
putAppReplicationConfiguration_appId = Lens.lens (\PutAppReplicationConfiguration' {appId} -> appId) (\s@PutAppReplicationConfiguration' {} a -> s {appId = a} :: PutAppReplicationConfiguration)

-- | Information about the replication configurations for server groups in
-- the application.
putAppReplicationConfiguration_serverGroupReplicationConfigurations :: Lens.Lens' PutAppReplicationConfiguration (Core.Maybe [ServerGroupReplicationConfiguration])
putAppReplicationConfiguration_serverGroupReplicationConfigurations = Lens.lens (\PutAppReplicationConfiguration' {serverGroupReplicationConfigurations} -> serverGroupReplicationConfigurations) (\s@PutAppReplicationConfiguration' {} a -> s {serverGroupReplicationConfigurations = a} :: PutAppReplicationConfiguration) Core.. Lens.mapping Lens._Coerce

instance
  Core.AWSRequest
    PutAppReplicationConfiguration
  where
  type
    AWSResponse PutAppReplicationConfiguration =
      PutAppReplicationConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutAppReplicationConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutAppReplicationConfiguration

instance Core.NFData PutAppReplicationConfiguration

instance
  Core.ToHeaders
    PutAppReplicationConfiguration
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.PutAppReplicationConfiguration" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutAppReplicationConfiguration where
  toJSON PutAppReplicationConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("appId" Core..=) Core.<$> appId,
            ("serverGroupReplicationConfigurations" Core..=)
              Core.<$> serverGroupReplicationConfigurations
          ]
      )

instance Core.ToPath PutAppReplicationConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery PutAppReplicationConfiguration where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutAppReplicationConfigurationResponse' smart constructor.
data PutAppReplicationConfigurationResponse = PutAppReplicationConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  PutAppReplicationConfigurationResponse
newPutAppReplicationConfigurationResponse
  pHttpStatus_ =
    PutAppReplicationConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
putAppReplicationConfigurationResponse_httpStatus :: Lens.Lens' PutAppReplicationConfigurationResponse Core.Int
putAppReplicationConfigurationResponse_httpStatus = Lens.lens (\PutAppReplicationConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutAppReplicationConfigurationResponse' {} a -> s {httpStatus = a} :: PutAppReplicationConfigurationResponse)

instance
  Core.NFData
    PutAppReplicationConfigurationResponse
