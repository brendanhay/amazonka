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
-- Module      : Amazonka.EMRServerless.CreateApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application.
module Amazonka.EMRServerless.CreateApplication
  ( -- * Creating a Request
    CreateApplication (..),
    newCreateApplication,

    -- * Request Lenses
    createApplication_architecture,
    createApplication_autoStartConfiguration,
    createApplication_autoStopConfiguration,
    createApplication_imageConfiguration,
    createApplication_initialCapacity,
    createApplication_maximumCapacity,
    createApplication_name,
    createApplication_networkConfiguration,
    createApplication_tags,
    createApplication_workerTypeSpecifications,
    createApplication_releaseLabel,
    createApplication_type,
    createApplication_clientToken,

    -- * Destructuring the Response
    CreateApplicationResponse (..),
    newCreateApplicationResponse,

    -- * Response Lenses
    createApplicationResponse_name,
    createApplicationResponse_httpStatus,
    createApplicationResponse_applicationId,
    createApplicationResponse_arn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMRServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { -- | The CPU architecture of an application.
    architecture :: Prelude.Maybe Architecture,
    -- | The configuration for an application to automatically start on job
    -- submission.
    autoStartConfiguration :: Prelude.Maybe AutoStartConfig,
    -- | The configuration for an application to automatically stop after a
    -- certain amount of time being idle.
    autoStopConfiguration :: Prelude.Maybe AutoStopConfig,
    -- | The image configuration for all worker types. You can either set this
    -- parameter or @imageConfiguration@ for each worker type in
    -- @workerTypeSpecifications@.
    imageConfiguration :: Prelude.Maybe ImageConfigurationInput,
    -- | The capacity to initialize when the application is created.
    initialCapacity :: Prelude.Maybe (Prelude.HashMap Prelude.Text InitialCapacityConfig),
    -- | The maximum capacity to allocate when the application is created. This
    -- is cumulative across all workers at any given point in time, not just
    -- when an application is created. No new resources will be created once
    -- any one of the defined limits is hit.
    maximumCapacity :: Prelude.Maybe MaximumAllowedResources,
    -- | The name of the application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The network configuration for customer VPC connectivity.
    networkConfiguration :: Prelude.Maybe NetworkConfiguration,
    -- | The tags assigned to the application.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The key-value pairs that specify worker type to
    -- @WorkerTypeSpecificationInput@. This parameter must contain all valid
    -- worker types for a Spark or Hive application. Valid worker types include
    -- @Driver@ and @Executor@ for Spark applications and @HiveDriver@ and
    -- @TezTask@ for Hive applications. You can either set image details in
    -- this parameter for each worker type, or in @imageConfiguration@ for all
    -- worker types.
    workerTypeSpecifications :: Prelude.Maybe (Prelude.HashMap Prelude.Text WorkerTypeSpecificationInput),
    -- | The EMR release associated with the application.
    releaseLabel :: Prelude.Text,
    -- | The type of application you want to start, such as Spark or Hive.
    type' :: Prelude.Text,
    -- | The client idempotency token of the application to create. Its value
    -- must be unique for each request.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'architecture', 'createApplication_architecture' - The CPU architecture of an application.
--
-- 'autoStartConfiguration', 'createApplication_autoStartConfiguration' - The configuration for an application to automatically start on job
-- submission.
--
-- 'autoStopConfiguration', 'createApplication_autoStopConfiguration' - The configuration for an application to automatically stop after a
-- certain amount of time being idle.
--
-- 'imageConfiguration', 'createApplication_imageConfiguration' - The image configuration for all worker types. You can either set this
-- parameter or @imageConfiguration@ for each worker type in
-- @workerTypeSpecifications@.
--
-- 'initialCapacity', 'createApplication_initialCapacity' - The capacity to initialize when the application is created.
--
-- 'maximumCapacity', 'createApplication_maximumCapacity' - The maximum capacity to allocate when the application is created. This
-- is cumulative across all workers at any given point in time, not just
-- when an application is created. No new resources will be created once
-- any one of the defined limits is hit.
--
-- 'name', 'createApplication_name' - The name of the application.
--
-- 'networkConfiguration', 'createApplication_networkConfiguration' - The network configuration for customer VPC connectivity.
--
-- 'tags', 'createApplication_tags' - The tags assigned to the application.
--
-- 'workerTypeSpecifications', 'createApplication_workerTypeSpecifications' - The key-value pairs that specify worker type to
-- @WorkerTypeSpecificationInput@. This parameter must contain all valid
-- worker types for a Spark or Hive application. Valid worker types include
-- @Driver@ and @Executor@ for Spark applications and @HiveDriver@ and
-- @TezTask@ for Hive applications. You can either set image details in
-- this parameter for each worker type, or in @imageConfiguration@ for all
-- worker types.
--
-- 'releaseLabel', 'createApplication_releaseLabel' - The EMR release associated with the application.
--
-- 'type'', 'createApplication_type' - The type of application you want to start, such as Spark or Hive.
--
-- 'clientToken', 'createApplication_clientToken' - The client idempotency token of the application to create. Its value
-- must be unique for each request.
newCreateApplication ::
  -- | 'releaseLabel'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  CreateApplication
newCreateApplication
  pReleaseLabel_
  pType_
  pClientToken_ =
    CreateApplication'
      { architecture = Prelude.Nothing,
        autoStartConfiguration = Prelude.Nothing,
        autoStopConfiguration = Prelude.Nothing,
        imageConfiguration = Prelude.Nothing,
        initialCapacity = Prelude.Nothing,
        maximumCapacity = Prelude.Nothing,
        name = Prelude.Nothing,
        networkConfiguration = Prelude.Nothing,
        tags = Prelude.Nothing,
        workerTypeSpecifications = Prelude.Nothing,
        releaseLabel = pReleaseLabel_,
        type' = pType_,
        clientToken = pClientToken_
      }

-- | The CPU architecture of an application.
createApplication_architecture :: Lens.Lens' CreateApplication (Prelude.Maybe Architecture)
createApplication_architecture = Lens.lens (\CreateApplication' {architecture} -> architecture) (\s@CreateApplication' {} a -> s {architecture = a} :: CreateApplication)

-- | The configuration for an application to automatically start on job
-- submission.
createApplication_autoStartConfiguration :: Lens.Lens' CreateApplication (Prelude.Maybe AutoStartConfig)
createApplication_autoStartConfiguration = Lens.lens (\CreateApplication' {autoStartConfiguration} -> autoStartConfiguration) (\s@CreateApplication' {} a -> s {autoStartConfiguration = a} :: CreateApplication)

-- | The configuration for an application to automatically stop after a
-- certain amount of time being idle.
createApplication_autoStopConfiguration :: Lens.Lens' CreateApplication (Prelude.Maybe AutoStopConfig)
createApplication_autoStopConfiguration = Lens.lens (\CreateApplication' {autoStopConfiguration} -> autoStopConfiguration) (\s@CreateApplication' {} a -> s {autoStopConfiguration = a} :: CreateApplication)

-- | The image configuration for all worker types. You can either set this
-- parameter or @imageConfiguration@ for each worker type in
-- @workerTypeSpecifications@.
createApplication_imageConfiguration :: Lens.Lens' CreateApplication (Prelude.Maybe ImageConfigurationInput)
createApplication_imageConfiguration = Lens.lens (\CreateApplication' {imageConfiguration} -> imageConfiguration) (\s@CreateApplication' {} a -> s {imageConfiguration = a} :: CreateApplication)

-- | The capacity to initialize when the application is created.
createApplication_initialCapacity :: Lens.Lens' CreateApplication (Prelude.Maybe (Prelude.HashMap Prelude.Text InitialCapacityConfig))
createApplication_initialCapacity = Lens.lens (\CreateApplication' {initialCapacity} -> initialCapacity) (\s@CreateApplication' {} a -> s {initialCapacity = a} :: CreateApplication) Prelude.. Lens.mapping Lens.coerced

-- | The maximum capacity to allocate when the application is created. This
-- is cumulative across all workers at any given point in time, not just
-- when an application is created. No new resources will be created once
-- any one of the defined limits is hit.
createApplication_maximumCapacity :: Lens.Lens' CreateApplication (Prelude.Maybe MaximumAllowedResources)
createApplication_maximumCapacity = Lens.lens (\CreateApplication' {maximumCapacity} -> maximumCapacity) (\s@CreateApplication' {} a -> s {maximumCapacity = a} :: CreateApplication)

-- | The name of the application.
createApplication_name :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_name = Lens.lens (\CreateApplication' {name} -> name) (\s@CreateApplication' {} a -> s {name = a} :: CreateApplication)

-- | The network configuration for customer VPC connectivity.
createApplication_networkConfiguration :: Lens.Lens' CreateApplication (Prelude.Maybe NetworkConfiguration)
createApplication_networkConfiguration = Lens.lens (\CreateApplication' {networkConfiguration} -> networkConfiguration) (\s@CreateApplication' {} a -> s {networkConfiguration = a} :: CreateApplication)

-- | The tags assigned to the application.
createApplication_tags :: Lens.Lens' CreateApplication (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createApplication_tags = Lens.lens (\CreateApplication' {tags} -> tags) (\s@CreateApplication' {} a -> s {tags = a} :: CreateApplication) Prelude.. Lens.mapping Lens.coerced

-- | The key-value pairs that specify worker type to
-- @WorkerTypeSpecificationInput@. This parameter must contain all valid
-- worker types for a Spark or Hive application. Valid worker types include
-- @Driver@ and @Executor@ for Spark applications and @HiveDriver@ and
-- @TezTask@ for Hive applications. You can either set image details in
-- this parameter for each worker type, or in @imageConfiguration@ for all
-- worker types.
createApplication_workerTypeSpecifications :: Lens.Lens' CreateApplication (Prelude.Maybe (Prelude.HashMap Prelude.Text WorkerTypeSpecificationInput))
createApplication_workerTypeSpecifications = Lens.lens (\CreateApplication' {workerTypeSpecifications} -> workerTypeSpecifications) (\s@CreateApplication' {} a -> s {workerTypeSpecifications = a} :: CreateApplication) Prelude.. Lens.mapping Lens.coerced

-- | The EMR release associated with the application.
createApplication_releaseLabel :: Lens.Lens' CreateApplication Prelude.Text
createApplication_releaseLabel = Lens.lens (\CreateApplication' {releaseLabel} -> releaseLabel) (\s@CreateApplication' {} a -> s {releaseLabel = a} :: CreateApplication)

-- | The type of application you want to start, such as Spark or Hive.
createApplication_type :: Lens.Lens' CreateApplication Prelude.Text
createApplication_type = Lens.lens (\CreateApplication' {type'} -> type') (\s@CreateApplication' {} a -> s {type' = a} :: CreateApplication)

-- | The client idempotency token of the application to create. Its value
-- must be unique for each request.
createApplication_clientToken :: Lens.Lens' CreateApplication Prelude.Text
createApplication_clientToken = Lens.lens (\CreateApplication' {clientToken} -> clientToken) (\s@CreateApplication' {} a -> s {clientToken = a} :: CreateApplication)

instance Core.AWSRequest CreateApplication where
  type
    AWSResponse CreateApplication =
      CreateApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApplicationResponse'
            Prelude.<$> (x Data..?> "name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "applicationId")
            Prelude.<*> (x Data..:> "arn")
      )

instance Prelude.Hashable CreateApplication where
  hashWithSalt _salt CreateApplication' {..} =
    _salt
      `Prelude.hashWithSalt` architecture
      `Prelude.hashWithSalt` autoStartConfiguration
      `Prelude.hashWithSalt` autoStopConfiguration
      `Prelude.hashWithSalt` imageConfiguration
      `Prelude.hashWithSalt` initialCapacity
      `Prelude.hashWithSalt` maximumCapacity
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` networkConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` workerTypeSpecifications
      `Prelude.hashWithSalt` releaseLabel
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateApplication where
  rnf CreateApplication' {..} =
    Prelude.rnf architecture
      `Prelude.seq` Prelude.rnf autoStartConfiguration
      `Prelude.seq` Prelude.rnf autoStopConfiguration
      `Prelude.seq` Prelude.rnf imageConfiguration
      `Prelude.seq` Prelude.rnf initialCapacity
      `Prelude.seq` Prelude.rnf maximumCapacity
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf networkConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf workerTypeSpecifications
      `Prelude.seq` Prelude.rnf releaseLabel
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders CreateApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateApplication where
  toJSON CreateApplication' {..} =
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
            ("name" Data..=) Prelude.<$> name,
            ("networkConfiguration" Data..=)
              Prelude.<$> networkConfiguration,
            ("tags" Data..=) Prelude.<$> tags,
            ("workerTypeSpecifications" Data..=)
              Prelude.<$> workerTypeSpecifications,
            Prelude.Just ("releaseLabel" Data..= releaseLabel),
            Prelude.Just ("type" Data..= type'),
            Prelude.Just ("clientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath CreateApplication where
  toPath = Prelude.const "/applications"

instance Data.ToQuery CreateApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { -- | The output contains the name of the application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The output contains the application ID.
    applicationId :: Prelude.Text,
    -- | The output contains the ARN of the application.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createApplicationResponse_name' - The output contains the name of the application.
--
-- 'httpStatus', 'createApplicationResponse_httpStatus' - The response's http status code.
--
-- 'applicationId', 'createApplicationResponse_applicationId' - The output contains the application ID.
--
-- 'arn', 'createApplicationResponse_arn' - The output contains the ARN of the application.
newCreateApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  CreateApplicationResponse
newCreateApplicationResponse
  pHttpStatus_
  pApplicationId_
  pArn_ =
    CreateApplicationResponse'
      { name = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        applicationId = pApplicationId_,
        arn = pArn_
      }

-- | The output contains the name of the application.
createApplicationResponse_name :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Prelude.Text)
createApplicationResponse_name = Lens.lens (\CreateApplicationResponse' {name} -> name) (\s@CreateApplicationResponse' {} a -> s {name = a} :: CreateApplicationResponse)

-- | The response's http status code.
createApplicationResponse_httpStatus :: Lens.Lens' CreateApplicationResponse Prelude.Int
createApplicationResponse_httpStatus = Lens.lens (\CreateApplicationResponse' {httpStatus} -> httpStatus) (\s@CreateApplicationResponse' {} a -> s {httpStatus = a} :: CreateApplicationResponse)

-- | The output contains the application ID.
createApplicationResponse_applicationId :: Lens.Lens' CreateApplicationResponse Prelude.Text
createApplicationResponse_applicationId = Lens.lens (\CreateApplicationResponse' {applicationId} -> applicationId) (\s@CreateApplicationResponse' {} a -> s {applicationId = a} :: CreateApplicationResponse)

-- | The output contains the ARN of the application.
createApplicationResponse_arn :: Lens.Lens' CreateApplicationResponse Prelude.Text
createApplicationResponse_arn = Lens.lens (\CreateApplicationResponse' {arn} -> arn) (\s@CreateApplicationResponse' {} a -> s {arn = a} :: CreateApplicationResponse)

instance Prelude.NFData CreateApplicationResponse where
  rnf CreateApplicationResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf arn
