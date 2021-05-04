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
-- Module      : Network.AWS.SMS.DeleteAppValidationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the validation configuration for the specified application.
module Network.AWS.SMS.DeleteAppValidationConfiguration
  ( -- * Creating a Request
    DeleteAppValidationConfiguration (..),
    newDeleteAppValidationConfiguration,

    -- * Request Lenses
    deleteAppValidationConfiguration_appId,

    -- * Destructuring the Response
    DeleteAppValidationConfigurationResponse (..),
    newDeleteAppValidationConfigurationResponse,

    -- * Response Lenses
    deleteAppValidationConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SMS.Types

-- | /See:/ 'newDeleteAppValidationConfiguration' smart constructor.
data DeleteAppValidationConfiguration = DeleteAppValidationConfiguration'
  { -- | The ID of the application.
    appId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppValidationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appId', 'deleteAppValidationConfiguration_appId' - The ID of the application.
newDeleteAppValidationConfiguration ::
  -- | 'appId'
  Prelude.Text ->
  DeleteAppValidationConfiguration
newDeleteAppValidationConfiguration pAppId_ =
  DeleteAppValidationConfiguration' {appId = pAppId_}

-- | The ID of the application.
deleteAppValidationConfiguration_appId :: Lens.Lens' DeleteAppValidationConfiguration Prelude.Text
deleteAppValidationConfiguration_appId = Lens.lens (\DeleteAppValidationConfiguration' {appId} -> appId) (\s@DeleteAppValidationConfiguration' {} a -> s {appId = a} :: DeleteAppValidationConfiguration)

instance
  Prelude.AWSRequest
    DeleteAppValidationConfiguration
  where
  type
    Rs DeleteAppValidationConfiguration =
      DeleteAppValidationConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteAppValidationConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteAppValidationConfiguration

instance
  Prelude.NFData
    DeleteAppValidationConfiguration

instance
  Prelude.ToHeaders
    DeleteAppValidationConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSServerMigrationService_V2016_10_24.DeleteAppValidationConfiguration" ::
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
    DeleteAppValidationConfiguration
  where
  toJSON DeleteAppValidationConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("appId" Prelude..= appId)]
      )

instance
  Prelude.ToPath
    DeleteAppValidationConfiguration
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteAppValidationConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAppValidationConfigurationResponse' smart constructor.
data DeleteAppValidationConfigurationResponse = DeleteAppValidationConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteAppValidationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteAppValidationConfigurationResponse_httpStatus' - The response's http status code.
newDeleteAppValidationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAppValidationConfigurationResponse
newDeleteAppValidationConfigurationResponse
  pHttpStatus_ =
    DeleteAppValidationConfigurationResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteAppValidationConfigurationResponse_httpStatus :: Lens.Lens' DeleteAppValidationConfigurationResponse Prelude.Int
deleteAppValidationConfigurationResponse_httpStatus = Lens.lens (\DeleteAppValidationConfigurationResponse' {httpStatus} -> httpStatus) (\s@DeleteAppValidationConfigurationResponse' {} a -> s {httpStatus = a} :: DeleteAppValidationConfigurationResponse)

instance
  Prelude.NFData
    DeleteAppValidationConfigurationResponse
