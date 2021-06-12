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
-- Module      : Network.AWS.SMS.DeleteAppReplicationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the replication configuration for the specified application.
module Network.AWS.SMS.DeleteAppReplicationConfiguration
  ( -- * Creating a Request
    DeleteAppReplicationConfiguration (..),
    newDeleteAppReplicationConfiguration,

    -- * Request Lenses
    deleteAppReplicationConfiguration_appId,

    -- * Destructuring the Response
    DeleteAppReplicationConfigurationResponse (..),
    newDeleteAppReplicationConfigurationResponse,

    -- * Response Lenses
    deleteAppReplicationConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newDeleteAppReplicationConfiguration' smart constructor.
data DeleteAppReplicationConfiguration = DeleteAppReplicationConfiguration'
  { -- | The ID of the application.
    appId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAppReplicationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'deleteAppReplicationConfiguration_appId' - The ID of the application.
newDeleteAppReplicationConfiguration ::
  DeleteAppReplicationConfiguration
newDeleteAppReplicationConfiguration =
  DeleteAppReplicationConfiguration'
    { appId =
        Core.Nothing
    }

-- | The ID of the application.
deleteAppReplicationConfiguration_appId :: Lens.Lens' DeleteAppReplicationConfiguration (Core.Maybe Core.Text)
deleteAppReplicationConfiguration_appId = Lens.lens (\DeleteAppReplicationConfiguration' {appId} -> appId) (\s@DeleteAppReplicationConfiguration' {} a -> s {appId = a} :: DeleteAppReplicationConfiguration)

instance
  Core.AWSRequest
    DeleteAppReplicationConfiguration
  where
  type
    AWSResponse DeleteAppReplicationConfiguration =
      DeleteAppReplicationConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAppReplicationConfigurationResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DeleteAppReplicationConfiguration

instance
  Core.NFData
    DeleteAppReplicationConfiguration

instance
  Core.ToHeaders
    DeleteAppReplicationConfiguration
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.DeleteAppReplicationConfiguration" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DeleteAppReplicationConfiguration
  where
  toJSON DeleteAppReplicationConfiguration' {..} =
    Core.object
      (Core.catMaybes [("appId" Core..=) Core.<$> appId])

instance
  Core.ToPath
    DeleteAppReplicationConfiguration
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DeleteAppReplicationConfiguration
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteAppReplicationConfigurationResponse' smart constructor.
data DeleteAppReplicationConfigurationResponse = DeleteAppReplicationConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteAppReplicationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAppReplicationConfigurationResponse_httpStatus' - The response's http status code.
newDeleteAppReplicationConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteAppReplicationConfigurationResponse
newDeleteAppReplicationConfigurationResponse
  pHttpStatus_ =
    DeleteAppReplicationConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteAppReplicationConfigurationResponse_httpStatus :: Lens.Lens' DeleteAppReplicationConfigurationResponse Core.Int
deleteAppReplicationConfigurationResponse_httpStatus = Lens.lens (\DeleteAppReplicationConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteAppReplicationConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteAppReplicationConfigurationResponse)

instance
  Core.NFData
    DeleteAppReplicationConfigurationResponse
