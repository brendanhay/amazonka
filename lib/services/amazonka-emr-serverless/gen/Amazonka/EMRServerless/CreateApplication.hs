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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application.
module Amazonka.EMRServerless.CreateApplication
  ( -- * Creating a Request
    CreateApplication (..),
    newCreateApplication,

    -- * Request Lenses
    createApplication_tags,
    createApplication_name,
    createApplication_autoStopConfiguration,
    createApplication_initialCapacity,
    createApplication_networkConfiguration,
    createApplication_autoStartConfiguration,
    createApplication_maximumCapacity,
    createApplication_architecture,
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
import Amazonka.EMRServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { -- | The tags assigned to the application.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The configuration for an application to automatically stop after a
    -- certain amount of time being idle.
    autoStopConfiguration :: Prelude.Maybe AutoStopConfig,
    -- | The capacity to initialize when the application is created.
    initialCapacity :: Prelude.Maybe (Prelude.HashMap Prelude.Text InitialCapacityConfig),
    -- | The network configuration for customer VPC connectivity.
    networkConfiguration :: Prelude.Maybe NetworkConfiguration,
    -- | The configuration for an application to automatically start on job
    -- submission.
    autoStartConfiguration :: Prelude.Maybe AutoStartConfig,
    -- | The maximum capacity to allocate when the application is created. This
    -- is cumulative across all workers at any given point in time, not just
    -- when an application is created. No new resources will be created once
    -- any one of the defined limits is hit.
    maximumCapacity :: Prelude.Maybe MaximumAllowedResources,
    -- | The CPU architecture of an application.
    architecture :: Prelude.Maybe Architecture,
    -- | The EMR release version associated with the application.
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
-- 'tags', 'createApplication_tags' - The tags assigned to the application.
--
-- 'name', 'createApplication_name' - The name of the application.
--
-- 'autoStopConfiguration', 'createApplication_autoStopConfiguration' - The configuration for an application to automatically stop after a
-- certain amount of time being idle.
--
-- 'initialCapacity', 'createApplication_initialCapacity' - The capacity to initialize when the application is created.
--
-- 'networkConfiguration', 'createApplication_networkConfiguration' - The network configuration for customer VPC connectivity.
--
-- 'autoStartConfiguration', 'createApplication_autoStartConfiguration' - The configuration for an application to automatically start on job
-- submission.
--
-- 'maximumCapacity', 'createApplication_maximumCapacity' - The maximum capacity to allocate when the application is created. This
-- is cumulative across all workers at any given point in time, not just
-- when an application is created. No new resources will be created once
-- any one of the defined limits is hit.
--
-- 'architecture', 'createApplication_architecture' - The CPU architecture of an application.
--
-- 'releaseLabel', 'createApplication_releaseLabel' - The EMR release version associated with the application.
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
      { tags = Prelude.Nothing,
        name = Prelude.Nothing,
        autoStopConfiguration = Prelude.Nothing,
        initialCapacity = Prelude.Nothing,
        networkConfiguration = Prelude.Nothing,
        autoStartConfiguration = Prelude.Nothing,
        maximumCapacity = Prelude.Nothing,
        architecture = Prelude.Nothing,
        releaseLabel = pReleaseLabel_,
        type' = pType_,
        clientToken = pClientToken_
      }

-- | The tags assigned to the application.
createApplication_tags :: Lens.Lens' CreateApplication (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createApplication_tags = Lens.lens (\CreateApplication' {tags} -> tags) (\s@CreateApplication' {} a -> s {tags = a} :: CreateApplication) Prelude.. Lens.mapping Lens.coerced

-- | The name of the application.
createApplication_name :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_name = Lens.lens (\CreateApplication' {name} -> name) (\s@CreateApplication' {} a -> s {name = a} :: CreateApplication)

-- | The configuration for an application to automatically stop after a
-- certain amount of time being idle.
createApplication_autoStopConfiguration :: Lens.Lens' CreateApplication (Prelude.Maybe AutoStopConfig)
createApplication_autoStopConfiguration = Lens.lens (\CreateApplication' {autoStopConfiguration} -> autoStopConfiguration) (\s@CreateApplication' {} a -> s {autoStopConfiguration = a} :: CreateApplication)

-- | The capacity to initialize when the application is created.
createApplication_initialCapacity :: Lens.Lens' CreateApplication (Prelude.Maybe (Prelude.HashMap Prelude.Text InitialCapacityConfig))
createApplication_initialCapacity = Lens.lens (\CreateApplication' {initialCapacity} -> initialCapacity) (\s@CreateApplication' {} a -> s {initialCapacity = a} :: CreateApplication) Prelude.. Lens.mapping Lens.coerced

-- | The network configuration for customer VPC connectivity.
createApplication_networkConfiguration :: Lens.Lens' CreateApplication (Prelude.Maybe NetworkConfiguration)
createApplication_networkConfiguration = Lens.lens (\CreateApplication' {networkConfiguration} -> networkConfiguration) (\s@CreateApplication' {} a -> s {networkConfiguration = a} :: CreateApplication)

-- | The configuration for an application to automatically start on job
-- submission.
createApplication_autoStartConfiguration :: Lens.Lens' CreateApplication (Prelude.Maybe AutoStartConfig)
createApplication_autoStartConfiguration = Lens.lens (\CreateApplication' {autoStartConfiguration} -> autoStartConfiguration) (\s@CreateApplication' {} a -> s {autoStartConfiguration = a} :: CreateApplication)

-- | The maximum capacity to allocate when the application is created. This
-- is cumulative across all workers at any given point in time, not just
-- when an application is created. No new resources will be created once
-- any one of the defined limits is hit.
createApplication_maximumCapacity :: Lens.Lens' CreateApplication (Prelude.Maybe MaximumAllowedResources)
createApplication_maximumCapacity = Lens.lens (\CreateApplication' {maximumCapacity} -> maximumCapacity) (\s@CreateApplication' {} a -> s {maximumCapacity = a} :: CreateApplication)

-- | The CPU architecture of an application.
createApplication_architecture :: Lens.Lens' CreateApplication (Prelude.Maybe Architecture)
createApplication_architecture = Lens.lens (\CreateApplication' {architecture} -> architecture) (\s@CreateApplication' {} a -> s {architecture = a} :: CreateApplication)

-- | The EMR release version associated with the application.
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
            Prelude.<$> (x Core..?> "name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "applicationId")
            Prelude.<*> (x Core..:> "arn")
      )

instance Prelude.Hashable CreateApplication where
  hashWithSalt _salt CreateApplication' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` autoStopConfiguration
      `Prelude.hashWithSalt` initialCapacity
      `Prelude.hashWithSalt` networkConfiguration
      `Prelude.hashWithSalt` autoStartConfiguration
      `Prelude.hashWithSalt` maximumCapacity
      `Prelude.hashWithSalt` architecture
      `Prelude.hashWithSalt` releaseLabel
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData CreateApplication where
  rnf CreateApplication' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf autoStopConfiguration
      `Prelude.seq` Prelude.rnf initialCapacity
      `Prelude.seq` Prelude.rnf networkConfiguration
      `Prelude.seq` Prelude.rnf autoStartConfiguration
      `Prelude.seq` Prelude.rnf maximumCapacity
      `Prelude.seq` Prelude.rnf architecture
      `Prelude.seq` Prelude.rnf releaseLabel
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf clientToken

instance Core.ToHeaders CreateApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateApplication where
  toJSON CreateApplication' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tags" Core..=) Prelude.<$> tags,
            ("name" Core..=) Prelude.<$> name,
            ("autoStopConfiguration" Core..=)
              Prelude.<$> autoStopConfiguration,
            ("initialCapacity" Core..=)
              Prelude.<$> initialCapacity,
            ("networkConfiguration" Core..=)
              Prelude.<$> networkConfiguration,
            ("autoStartConfiguration" Core..=)
              Prelude.<$> autoStartConfiguration,
            ("maximumCapacity" Core..=)
              Prelude.<$> maximumCapacity,
            ("architecture" Core..=) Prelude.<$> architecture,
            Prelude.Just ("releaseLabel" Core..= releaseLabel),
            Prelude.Just ("type" Core..= type'),
            Prelude.Just ("clientToken" Core..= clientToken)
          ]
      )

instance Core.ToPath CreateApplication where
  toPath = Prelude.const "/applications"

instance Core.ToQuery CreateApplication where
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
