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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newDeleteAppReplicationConfiguration' smart constructor.
data DeleteAppReplicationConfiguration = DeleteAppReplicationConfiguration'
  { -- | The ID of the application.
    appId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | The ID of the application.
deleteAppReplicationConfiguration_appId :: Lens.Lens' DeleteAppReplicationConfiguration (Prelude.Maybe Prelude.Text)
deleteAppReplicationConfiguration_appId = Lens.lens (\DeleteAppReplicationConfiguration' {appId} -> appId) (\s@DeleteAppReplicationConfiguration' {} a -> s {appId = a} :: DeleteAppReplicationConfiguration)

instance
  Prelude.AWSRequest
    DeleteAppReplicationConfiguration
  where
  type
    Rs DeleteAppReplicationConfiguration =
      DeleteAppReplicationConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAppReplicationConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteAppReplicationConfiguration

instance
  Prelude.NFData
    DeleteAppReplicationConfiguration

instance
  Prelude.ToHeaders
    DeleteAppReplicationConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSServerMigrationService_V2016_10_24.DeleteAppReplicationConfiguration" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DeleteAppReplicationConfiguration
  where
  toJSON DeleteAppReplicationConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("appId" Prelude..=) Prelude.<$> appId]
      )

instance
  Prelude.ToPath
    DeleteAppReplicationConfiguration
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteAppReplicationConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAppReplicationConfigurationResponse' smart constructor.
data DeleteAppReplicationConfigurationResponse = DeleteAppReplicationConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteAppReplicationConfigurationResponse
newDeleteAppReplicationConfigurationResponse
  pHttpStatus_ =
    DeleteAppReplicationConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteAppReplicationConfigurationResponse_httpStatus :: Lens.Lens' DeleteAppReplicationConfigurationResponse Prelude.Int
deleteAppReplicationConfigurationResponse_httpStatus = Lens.lens (\DeleteAppReplicationConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteAppReplicationConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteAppReplicationConfigurationResponse)

instance
  Prelude.NFData
    DeleteAppReplicationConfigurationResponse
