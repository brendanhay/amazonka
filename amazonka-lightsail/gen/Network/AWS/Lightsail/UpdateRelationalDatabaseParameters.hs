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
-- Module      : Network.AWS.Lightsail.UpdateRelationalDatabaseParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the update of one or more parameters of a database in Amazon
-- Lightsail.
--
-- Parameter updates don\'t cause outages; therefore, their application is
-- not subject to the preferred maintenance window. However, there are two
-- ways in which parameter updates are applied: @dynamic@ or
-- @pending-reboot@. Parameters marked with a @dynamic@ apply type are
-- applied immediately. Parameters marked with a @pending-reboot@ apply
-- type are applied only after the database is rebooted using the
-- @reboot relational database@ operation.
--
-- The @update relational database parameters@ operation supports tag-based
-- access control via resource tags applied to the resource identified by
-- relationalDatabaseName. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.UpdateRelationalDatabaseParameters
  ( -- * Creating a Request
    UpdateRelationalDatabaseParameters (..),
    newUpdateRelationalDatabaseParameters,

    -- * Request Lenses
    updateRelationalDatabaseParameters_relationalDatabaseName,
    updateRelationalDatabaseParameters_parameters,

    -- * Destructuring the Response
    UpdateRelationalDatabaseParametersResponse (..),
    newUpdateRelationalDatabaseParametersResponse,

    -- * Response Lenses
    updateRelationalDatabaseParametersResponse_operations,
    updateRelationalDatabaseParametersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateRelationalDatabaseParameters' smart constructor.
data UpdateRelationalDatabaseParameters = UpdateRelationalDatabaseParameters'
  { -- | The name of your database for which to update parameters.
    relationalDatabaseName :: Core.Text,
    -- | The database parameters to update.
    parameters :: [RelationalDatabaseParameter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateRelationalDatabaseParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relationalDatabaseName', 'updateRelationalDatabaseParameters_relationalDatabaseName' - The name of your database for which to update parameters.
--
-- 'parameters', 'updateRelationalDatabaseParameters_parameters' - The database parameters to update.
newUpdateRelationalDatabaseParameters ::
  -- | 'relationalDatabaseName'
  Core.Text ->
  UpdateRelationalDatabaseParameters
newUpdateRelationalDatabaseParameters
  pRelationalDatabaseName_ =
    UpdateRelationalDatabaseParameters'
      { relationalDatabaseName =
          pRelationalDatabaseName_,
        parameters = Core.mempty
      }

-- | The name of your database for which to update parameters.
updateRelationalDatabaseParameters_relationalDatabaseName :: Lens.Lens' UpdateRelationalDatabaseParameters Core.Text
updateRelationalDatabaseParameters_relationalDatabaseName = Lens.lens (\UpdateRelationalDatabaseParameters' {relationalDatabaseName} -> relationalDatabaseName) (\s@UpdateRelationalDatabaseParameters' {} a -> s {relationalDatabaseName = a} :: UpdateRelationalDatabaseParameters)

-- | The database parameters to update.
updateRelationalDatabaseParameters_parameters :: Lens.Lens' UpdateRelationalDatabaseParameters [RelationalDatabaseParameter]
updateRelationalDatabaseParameters_parameters = Lens.lens (\UpdateRelationalDatabaseParameters' {parameters} -> parameters) (\s@UpdateRelationalDatabaseParameters' {} a -> s {parameters = a} :: UpdateRelationalDatabaseParameters) Core.. Lens._Coerce

instance
  Core.AWSRequest
    UpdateRelationalDatabaseParameters
  where
  type
    AWSResponse UpdateRelationalDatabaseParameters =
      UpdateRelationalDatabaseParametersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRelationalDatabaseParametersResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    UpdateRelationalDatabaseParameters

instance
  Core.NFData
    UpdateRelationalDatabaseParameters

instance
  Core.ToHeaders
    UpdateRelationalDatabaseParameters
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.UpdateRelationalDatabaseParameters" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    UpdateRelationalDatabaseParameters
  where
  toJSON UpdateRelationalDatabaseParameters' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "relationalDatabaseName"
                  Core..= relationalDatabaseName
              ),
            Core.Just ("parameters" Core..= parameters)
          ]
      )

instance
  Core.ToPath
    UpdateRelationalDatabaseParameters
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    UpdateRelationalDatabaseParameters
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateRelationalDatabaseParametersResponse' smart constructor.
data UpdateRelationalDatabaseParametersResponse = UpdateRelationalDatabaseParametersResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateRelationalDatabaseParametersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'updateRelationalDatabaseParametersResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'updateRelationalDatabaseParametersResponse_httpStatus' - The response's http status code.
newUpdateRelationalDatabaseParametersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateRelationalDatabaseParametersResponse
newUpdateRelationalDatabaseParametersResponse
  pHttpStatus_ =
    UpdateRelationalDatabaseParametersResponse'
      { operations =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
updateRelationalDatabaseParametersResponse_operations :: Lens.Lens' UpdateRelationalDatabaseParametersResponse (Core.Maybe [Operation])
updateRelationalDatabaseParametersResponse_operations = Lens.lens (\UpdateRelationalDatabaseParametersResponse' {operations} -> operations) (\s@UpdateRelationalDatabaseParametersResponse' {} a -> s {operations = a} :: UpdateRelationalDatabaseParametersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
updateRelationalDatabaseParametersResponse_httpStatus :: Lens.Lens' UpdateRelationalDatabaseParametersResponse Core.Int
updateRelationalDatabaseParametersResponse_httpStatus = Lens.lens (\UpdateRelationalDatabaseParametersResponse' {httpStatus} -> httpStatus) (\s@UpdateRelationalDatabaseParametersResponse' {} a -> s {httpStatus = a} :: UpdateRelationalDatabaseParametersResponse)

instance
  Core.NFData
    UpdateRelationalDatabaseParametersResponse
