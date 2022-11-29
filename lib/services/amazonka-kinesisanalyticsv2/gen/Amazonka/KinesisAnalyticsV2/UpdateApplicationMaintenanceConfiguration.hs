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
-- Module      : Amazonka.KinesisAnalyticsV2.UpdateApplicationMaintenanceConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the maintenance configuration of the Kinesis Data Analytics
-- application.
--
-- You can invoke this operation on an application that is in one of the
-- two following states: @READY@ or @RUNNING@. If you invoke it when the
-- application is in a state other than these two states, it throws a
-- @ResourceInUseException@. The service makes use of the updated
-- configuration the next time it schedules maintenance for the
-- application. If you invoke this operation after the service schedules
-- maintenance, the service will apply the configuration update the next
-- time it schedules maintenance for the application. This means that you
-- might not see the maintenance configuration update applied to the
-- maintenance process that follows a successful invocation of this
-- operation, but to the following maintenance process instead.
--
-- To see the current maintenance configuration of your application, invoke
-- the DescribeApplication operation.
--
-- For information about application maintenance, see
-- <https://docs.aws.amazon.com/kinesisanalytics/latest/java/maintenance.html Kinesis Data Analytics for Apache Flink Maintenance>.
--
-- This operation is supported only for Amazon Kinesis Data Analytics for
-- Apache Flink.
module Amazonka.KinesisAnalyticsV2.UpdateApplicationMaintenanceConfiguration
  ( -- * Creating a Request
    UpdateApplicationMaintenanceConfiguration (..),
    newUpdateApplicationMaintenanceConfiguration,

    -- * Request Lenses
    updateApplicationMaintenanceConfiguration_applicationName,
    updateApplicationMaintenanceConfiguration_applicationMaintenanceConfigurationUpdate,

    -- * Destructuring the Response
    UpdateApplicationMaintenanceConfigurationResponse (..),
    newUpdateApplicationMaintenanceConfigurationResponse,

    -- * Response Lenses
    updateApplicationMaintenanceConfigurationResponse_applicationARN,
    updateApplicationMaintenanceConfigurationResponse_applicationMaintenanceConfigurationDescription,
    updateApplicationMaintenanceConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisAnalyticsV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateApplicationMaintenanceConfiguration' smart constructor.
data UpdateApplicationMaintenanceConfiguration = UpdateApplicationMaintenanceConfiguration'
  { -- | The name of the application for which you want to update the maintenance
    -- configuration.
    applicationName :: Prelude.Text,
    -- | Describes the application maintenance configuration update.
    applicationMaintenanceConfigurationUpdate :: ApplicationMaintenanceConfigurationUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplicationMaintenanceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'updateApplicationMaintenanceConfiguration_applicationName' - The name of the application for which you want to update the maintenance
-- configuration.
--
-- 'applicationMaintenanceConfigurationUpdate', 'updateApplicationMaintenanceConfiguration_applicationMaintenanceConfigurationUpdate' - Describes the application maintenance configuration update.
newUpdateApplicationMaintenanceConfiguration ::
  -- | 'applicationName'
  Prelude.Text ->
  -- | 'applicationMaintenanceConfigurationUpdate'
  ApplicationMaintenanceConfigurationUpdate ->
  UpdateApplicationMaintenanceConfiguration
newUpdateApplicationMaintenanceConfiguration
  pApplicationName_
  pApplicationMaintenanceConfigurationUpdate_ =
    UpdateApplicationMaintenanceConfiguration'
      { applicationName =
          pApplicationName_,
        applicationMaintenanceConfigurationUpdate =
          pApplicationMaintenanceConfigurationUpdate_
      }

-- | The name of the application for which you want to update the maintenance
-- configuration.
updateApplicationMaintenanceConfiguration_applicationName :: Lens.Lens' UpdateApplicationMaintenanceConfiguration Prelude.Text
updateApplicationMaintenanceConfiguration_applicationName = Lens.lens (\UpdateApplicationMaintenanceConfiguration' {applicationName} -> applicationName) (\s@UpdateApplicationMaintenanceConfiguration' {} a -> s {applicationName = a} :: UpdateApplicationMaintenanceConfiguration)

-- | Describes the application maintenance configuration update.
updateApplicationMaintenanceConfiguration_applicationMaintenanceConfigurationUpdate :: Lens.Lens' UpdateApplicationMaintenanceConfiguration ApplicationMaintenanceConfigurationUpdate
updateApplicationMaintenanceConfiguration_applicationMaintenanceConfigurationUpdate = Lens.lens (\UpdateApplicationMaintenanceConfiguration' {applicationMaintenanceConfigurationUpdate} -> applicationMaintenanceConfigurationUpdate) (\s@UpdateApplicationMaintenanceConfiguration' {} a -> s {applicationMaintenanceConfigurationUpdate = a} :: UpdateApplicationMaintenanceConfiguration)

instance
  Core.AWSRequest
    UpdateApplicationMaintenanceConfiguration
  where
  type
    AWSResponse
      UpdateApplicationMaintenanceConfiguration =
      UpdateApplicationMaintenanceConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApplicationMaintenanceConfigurationResponse'
            Prelude.<$> (x Core..?> "ApplicationARN")
              Prelude.<*> ( x
                              Core..?> "ApplicationMaintenanceConfigurationDescription"
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateApplicationMaintenanceConfiguration
  where
  hashWithSalt
    _salt
    UpdateApplicationMaintenanceConfiguration' {..} =
      _salt `Prelude.hashWithSalt` applicationName
        `Prelude.hashWithSalt` applicationMaintenanceConfigurationUpdate

instance
  Prelude.NFData
    UpdateApplicationMaintenanceConfiguration
  where
  rnf UpdateApplicationMaintenanceConfiguration' {..} =
    Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf applicationMaintenanceConfigurationUpdate

instance
  Core.ToHeaders
    UpdateApplicationMaintenanceConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "KinesisAnalytics_20180523.UpdateApplicationMaintenanceConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    UpdateApplicationMaintenanceConfiguration
  where
  toJSON UpdateApplicationMaintenanceConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ApplicationName" Core..= applicationName),
            Prelude.Just
              ( "ApplicationMaintenanceConfigurationUpdate"
                  Core..= applicationMaintenanceConfigurationUpdate
              )
          ]
      )

instance
  Core.ToPath
    UpdateApplicationMaintenanceConfiguration
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    UpdateApplicationMaintenanceConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateApplicationMaintenanceConfigurationResponse' smart constructor.
data UpdateApplicationMaintenanceConfigurationResponse = UpdateApplicationMaintenanceConfigurationResponse'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationARN :: Prelude.Maybe Prelude.Text,
    -- | The application maintenance configuration description after the update.
    applicationMaintenanceConfigurationDescription :: Prelude.Maybe ApplicationMaintenanceConfigurationDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplicationMaintenanceConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationARN', 'updateApplicationMaintenanceConfigurationResponse_applicationARN' - The Amazon Resource Name (ARN) of the application.
--
-- 'applicationMaintenanceConfigurationDescription', 'updateApplicationMaintenanceConfigurationResponse_applicationMaintenanceConfigurationDescription' - The application maintenance configuration description after the update.
--
-- 'httpStatus', 'updateApplicationMaintenanceConfigurationResponse_httpStatus' - The response's http status code.
newUpdateApplicationMaintenanceConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateApplicationMaintenanceConfigurationResponse
newUpdateApplicationMaintenanceConfigurationResponse
  pHttpStatus_ =
    UpdateApplicationMaintenanceConfigurationResponse'
      { applicationARN =
          Prelude.Nothing,
        applicationMaintenanceConfigurationDescription =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The Amazon Resource Name (ARN) of the application.
updateApplicationMaintenanceConfigurationResponse_applicationARN :: Lens.Lens' UpdateApplicationMaintenanceConfigurationResponse (Prelude.Maybe Prelude.Text)
updateApplicationMaintenanceConfigurationResponse_applicationARN = Lens.lens (\UpdateApplicationMaintenanceConfigurationResponse' {applicationARN} -> applicationARN) (\s@UpdateApplicationMaintenanceConfigurationResponse' {} a -> s {applicationARN = a} :: UpdateApplicationMaintenanceConfigurationResponse)

-- | The application maintenance configuration description after the update.
updateApplicationMaintenanceConfigurationResponse_applicationMaintenanceConfigurationDescription :: Lens.Lens' UpdateApplicationMaintenanceConfigurationResponse (Prelude.Maybe ApplicationMaintenanceConfigurationDescription)
updateApplicationMaintenanceConfigurationResponse_applicationMaintenanceConfigurationDescription = Lens.lens (\UpdateApplicationMaintenanceConfigurationResponse' {applicationMaintenanceConfigurationDescription} -> applicationMaintenanceConfigurationDescription) (\s@UpdateApplicationMaintenanceConfigurationResponse' {} a -> s {applicationMaintenanceConfigurationDescription = a} :: UpdateApplicationMaintenanceConfigurationResponse)

-- | The response's http status code.
updateApplicationMaintenanceConfigurationResponse_httpStatus :: Lens.Lens' UpdateApplicationMaintenanceConfigurationResponse Prelude.Int
updateApplicationMaintenanceConfigurationResponse_httpStatus = Lens.lens (\UpdateApplicationMaintenanceConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateApplicationMaintenanceConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateApplicationMaintenanceConfigurationResponse)

instance
  Prelude.NFData
    UpdateApplicationMaintenanceConfigurationResponse
  where
  rnf
    UpdateApplicationMaintenanceConfigurationResponse' {..} =
      Prelude.rnf applicationARN
        `Prelude.seq` Prelude.rnf
          applicationMaintenanceConfigurationDescription
        `Prelude.seq` Prelude.rnf httpStatus
