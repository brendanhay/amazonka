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
-- Module      : Amazonka.EMRServerless.UpdateApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a specified application. An application has to be in a stopped
-- or created state in order to be updated.
module Amazonka.EMRServerless.UpdateApplication
  ( -- * Creating a Request
    UpdateApplication (..),
    newUpdateApplication,

    -- * Request Lenses
    updateApplication_architecture,
    updateApplication_autoStartConfiguration,
    updateApplication_autoStopConfiguration,
    updateApplication_imageConfiguration,
    updateApplication_initialCapacity,
    updateApplication_maximumCapacity,
    updateApplication_networkConfiguration,
    updateApplication_workerTypeSpecifications,
    updateApplication_applicationId,
    updateApplication_clientToken,

    -- * Destructuring the Response
    UpdateApplicationResponse (..),
    newUpdateApplicationResponse,

    -- * Response Lenses
    updateApplicationResponse_httpStatus,
    updateApplicationResponse_application,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { -- | The CPU architecture of an application.
    architecture :: Prelude.Maybe Architecture,
    -- | The configuration for an application to automatically start on job
    -- submission.
    autoStartConfiguration :: Prelude.Maybe AutoStartConfig,
    -- | The configuration for an application to automatically stop after a
    -- certain amount of time being idle.
    autoStopConfiguration :: Prelude.Maybe AutoStopConfig,
    -- | The image configuration to be used for all worker types. You can either
    -- set this parameter or @imageConfiguration@ for each worker type in
    -- @WorkerTypeSpecificationInput@.
    imageConfiguration :: Prelude.Maybe ImageConfigurationInput,
    -- | The capacity to initialize when the application is updated.
    initialCapacity :: Prelude.Maybe (Prelude.HashMap Prelude.Text InitialCapacityConfig),
    -- | The maximum capacity to allocate when the application is updated. This
    -- is cumulative across all workers at any given point in time during the
    -- lifespan of the application. No new resources will be created once any
    -- one of the defined limits is hit.
    maximumCapacity :: Prelude.Maybe MaximumAllowedResources,
    networkConfiguration :: Prelude.Maybe NetworkConfiguration,
    -- | The key-value pairs that specify worker type to
    -- @WorkerTypeSpecificationInput@. This parameter must contain all valid
    -- worker types for a Spark or Hive application. Valid worker types include
    -- @Driver@ and @Executor@ for Spark applications and @HiveDriver@ and
    -- @TezTask@ for Hive applications. You can either set image details in
    -- this parameter for each worker type, or in @imageConfiguration@ for all
    -- worker types.
    workerTypeSpecifications :: Prelude.Maybe (Prelude.HashMap Prelude.Text WorkerTypeSpecificationInput),
    -- | The ID of the application to update.
    applicationId :: Prelude.Text,
    -- | The client idempotency token of the application to update. Its value
    -- must be unique for each request.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'architecture', 'updateApplication_architecture' - The CPU architecture of an application.
--
-- 'autoStartConfiguration', 'updateApplication_autoStartConfiguration' - The configuration for an application to automatically start on job
-- submission.
--
-- 'autoStopConfiguration', 'updateApplication_autoStopConfiguration' - The configuration for an application to automatically stop after a
-- certain amount of time being idle.
--
-- 'imageConfiguration', 'updateApplication_imageConfiguration' - The image configuration to be used for all worker types. You can either
-- set this parameter or @imageConfiguration@ for each worker type in
-- @WorkerTypeSpecificationInput@.
--
-- 'initialCapacity', 'updateApplication_initialCapacity' - The capacity to initialize when the application is updated.
--
-- 'maximumCapacity', 'updateApplication_maximumCapacity' - The maximum capacity to allocate when the application is updated. This
-- is cumulative across all workers at any given point in time during the
-- lifespan of the application. No new resources will be created once any
-- one of the defined limits is hit.
--
-- 'networkConfiguration', 'updateApplication_networkConfiguration' - Undocumented member.
--
-- 'workerTypeSpecifications', 'updateApplication_workerTypeSpecifications' - The key-value pairs that specify worker type to
-- @WorkerTypeSpecificationInput@. This parameter must contain all valid
-- worker types for a Spark or Hive application. Valid worker types include
-- @Driver@ and @Executor@ for Spark applications and @HiveDriver@ and
-- @TezTask@ for Hive applications. You can either set image details in
-- this parameter for each worker type, or in @imageConfiguration@ for all
-- worker types.
--
-- 'applicationId', 'updateApplication_applicationId' - The ID of the application to update.
--
-- 'clientToken', 'updateApplication_clientToken' - The client idempotency token of the application to update. Its value
-- must be unique for each request.
newUpdateApplication ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  UpdateApplication
newUpdateApplication pApplicationId_ pClientToken_ =
  UpdateApplication'
    { architecture = Prelude.Nothing,
      autoStartConfiguration = Prelude.Nothing,
      autoStopConfiguration = Prelude.Nothing,
      imageConfiguration = Prelude.Nothing,
      initialCapacity = Prelude.Nothing,
      maximumCapacity = Prelude.Nothing,
      networkConfiguration = Prelude.Nothing,
      workerTypeSpecifications = Prelude.Nothing,
      applicationId = pApplicationId_,
      clientToken = pClientToken_
    }

-- | The CPU architecture of an application.
updateApplication_architecture :: Lens.Lens' UpdateApplication (Prelude.Maybe Architecture)
updateApplication_architecture = Lens.lens (\UpdateApplication' {architecture} -> architecture) (\s@UpdateApplication' {} a -> s {architecture = a} :: UpdateApplication)

-- | The configuration for an application to automatically start on job
-- submission.
updateApplication_autoStartConfiguration :: Lens.Lens' UpdateApplication (Prelude.Maybe AutoStartConfig)
updateApplication_autoStartConfiguration = Lens.lens (\UpdateApplication' {autoStartConfiguration} -> autoStartConfiguration) (\s@UpdateApplication' {} a -> s {autoStartConfiguration = a} :: UpdateApplication)

-- | The configuration for an application to automatically stop after a
-- certain amount of time being idle.
updateApplication_autoStopConfiguration :: Lens.Lens' UpdateApplication (Prelude.Maybe AutoStopConfig)
updateApplication_autoStopConfiguration = Lens.lens (\UpdateApplication' {autoStopConfiguration} -> autoStopConfiguration) (\s@UpdateApplication' {} a -> s {autoStopConfiguration = a} :: UpdateApplication)

-- | The image configuration to be used for all worker types. You can either
-- set this parameter or @imageConfiguration@ for each worker type in
-- @WorkerTypeSpecificationInput@.
updateApplication_imageConfiguration :: Lens.Lens' UpdateApplication (Prelude.Maybe ImageConfigurationInput)
updateApplication_imageConfiguration = Lens.lens (\UpdateApplication' {imageConfiguration} -> imageConfiguration) (\s@UpdateApplication' {} a -> s {imageConfiguration = a} :: UpdateApplication)

-- | The capacity to initialize when the application is updated.
updateApplication_initialCapacity :: Lens.Lens' UpdateApplication (Prelude.Maybe (Prelude.HashMap Prelude.Text InitialCapacityConfig))
updateApplication_initialCapacity = Lens.lens (\UpdateApplication' {initialCapacity} -> initialCapacity) (\s@UpdateApplication' {} a -> s {initialCapacity = a} :: UpdateApplication) Prelude.. Lens.mapping Lens.coerced

-- | The maximum capacity to allocate when the application is updated. This
-- is cumulative across all workers at any given point in time during the
-- lifespan of the application. No new resources will be created once any
-- one of the defined limits is hit.
updateApplication_maximumCapacity :: Lens.Lens' UpdateApplication (Prelude.Maybe MaximumAllowedResources)
updateApplication_maximumCapacity = Lens.lens (\UpdateApplication' {maximumCapacity} -> maximumCapacity) (\s@UpdateApplication' {} a -> s {maximumCapacity = a} :: UpdateApplication)

-- | Undocumented member.
updateApplication_networkConfiguration :: Lens.Lens' UpdateApplication (Prelude.Maybe NetworkConfiguration)
updateApplication_networkConfiguration = Lens.lens (\UpdateApplication' {networkConfiguration} -> networkConfiguration) (\s@UpdateApplication' {} a -> s {networkConfiguration = a} :: UpdateApplication)

-- | The key-value pairs that specify worker type to
-- @WorkerTypeSpecificationInput@. This parameter must contain all valid
-- worker types for a Spark or Hive application. Valid worker types include
-- @Driver@ and @Executor@ for Spark applications and @HiveDriver@ and
-- @TezTask@ for Hive applications. You can either set image details in
-- this parameter for each worker type, or in @imageConfiguration@ for all
-- worker types.
updateApplication_workerTypeSpecifications :: Lens.Lens' UpdateApplication (Prelude.Maybe (Prelude.HashMap Prelude.Text WorkerTypeSpecificationInput))
updateApplication_workerTypeSpecifications = Lens.lens (\UpdateApplication' {workerTypeSpecifications} -> workerTypeSpecifications) (\s@UpdateApplication' {} a -> s {workerTypeSpecifications = a} :: UpdateApplication) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the application to update.
updateApplication_applicationId :: Lens.Lens' UpdateApplication Prelude.Text
updateApplication_applicationId = Lens.lens (\UpdateApplication' {applicationId} -> applicationId) (\s@UpdateApplication' {} a -> s {applicationId = a} :: UpdateApplication)

-- | The client idempotency token of the application to update. Its value
-- must be unique for each request.
updateApplication_clientToken :: Lens.Lens' UpdateApplication Prelude.Text
updateApplication_clientToken = Lens.lens (\UpdateApplication' {clientToken} -> clientToken) (\s@UpdateApplication' {} a -> s {clientToken = a} :: UpdateApplication)

instance Core.AWSRequest UpdateApplication where
  type
    AWSResponse UpdateApplication =
      UpdateApplicationResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "application")
      )

instance Prelude.Hashable UpdateApplication where
  hashWithSalt _salt UpdateApplication' {..} =
    _salt
      `Prelude.hashWithSalt` architecture
      `Prelude.hashWithSalt` autoStartConfiguration
      `Prelude.hashWithSalt` autoStopConfiguration
      `Prelude.hashWithSalt` imageConfiguration
      `Prelude.hashWithSalt` initialCapacity
      `Prelude.hashWithSalt` maximumCapacity
      `Prelude.hashWithSalt` networkConfiguration
      `Prelude.hashWithSalt` workerTypeSpecifications
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData UpdateApplication where
  rnf UpdateApplication' {..} =
    Prelude.rnf architecture
      `Prelude.seq` Prelude.rnf autoStartConfiguration
      `Prelude.seq` Prelude.rnf autoStopConfiguration
      `Prelude.seq` Prelude.rnf imageConfiguration
      `Prelude.seq` Prelude.rnf initialCapacity
      `Prelude.seq` Prelude.rnf maximumCapacity
      `Prelude.seq` Prelude.rnf networkConfiguration
      `Prelude.seq` Prelude.rnf workerTypeSpecifications
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders UpdateApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateApplication where
  toJSON UpdateApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("architecture" Data..=) Prelude.<$> architecture,
            ("autoStartConfiguration" Data..=)
              Prelude.<$> autoStartConfiguration,
            ("autoStopConfiguration" Data..=)
              Prelude.<$> autoStopConfiguration,
            ("imageConfiguration" Data..=)
              Prelude.<$> imageConfiguration,
            ("initialCapacity" Data..=)
              Prelude.<$> initialCapacity,
            ("maximumCapacity" Data..=)
              Prelude.<$> maximumCapacity,
            ("networkConfiguration" Data..=)
              Prelude.<$> networkConfiguration,
            ("workerTypeSpecifications" Data..=)
              Prelude.<$> workerTypeSpecifications,
            Prelude.Just ("clientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath UpdateApplication where
  toPath UpdateApplication' {..} =
    Prelude.mconcat
      ["/applications/", Data.toBS applicationId]

instance Data.ToQuery UpdateApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateApplicationResponse' smart constructor.
data UpdateApplicationResponse = UpdateApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about the updated application.
    application :: Application
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateApplicationResponse_httpStatus' - The response's http status code.
--
-- 'application', 'updateApplicationResponse_application' - Information about the updated application.
newUpdateApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'application'
  Application ->
  UpdateApplicationResponse
newUpdateApplicationResponse
  pHttpStatus_
  pApplication_ =
    UpdateApplicationResponse'
      { httpStatus =
          pHttpStatus_,
        application = pApplication_
      }

-- | The response's http status code.
updateApplicationResponse_httpStatus :: Lens.Lens' UpdateApplicationResponse Prelude.Int
updateApplicationResponse_httpStatus = Lens.lens (\UpdateApplicationResponse' {httpStatus} -> httpStatus) (\s@UpdateApplicationResponse' {} a -> s {httpStatus = a} :: UpdateApplicationResponse)

-- | Information about the updated application.
updateApplicationResponse_application :: Lens.Lens' UpdateApplicationResponse Application
updateApplicationResponse_application = Lens.lens (\UpdateApplicationResponse' {application} -> application) (\s@UpdateApplicationResponse' {} a -> s {application = a} :: UpdateApplicationResponse)

instance Prelude.NFData UpdateApplicationResponse where
  rnf UpdateApplicationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf application
