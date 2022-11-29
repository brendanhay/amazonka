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
-- Module      : Amazonka.RobOMaker.CreateSimulationApplicationVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a simulation application with a specific revision id.
module Amazonka.RobOMaker.CreateSimulationApplicationVersion
  ( -- * Creating a Request
    CreateSimulationApplicationVersion (..),
    newCreateSimulationApplicationVersion,

    -- * Request Lenses
    createSimulationApplicationVersion_s3Etags,
    createSimulationApplicationVersion_currentRevisionId,
    createSimulationApplicationVersion_imageDigest,
    createSimulationApplicationVersion_application,

    -- * Destructuring the Response
    CreateSimulationApplicationVersionResponse (..),
    newCreateSimulationApplicationVersionResponse,

    -- * Response Lenses
    createSimulationApplicationVersionResponse_name,
    createSimulationApplicationVersionResponse_sources,
    createSimulationApplicationVersionResponse_environment,
    createSimulationApplicationVersionResponse_renderingEngine,
    createSimulationApplicationVersionResponse_lastUpdatedAt,
    createSimulationApplicationVersionResponse_arn,
    createSimulationApplicationVersionResponse_robotSoftwareSuite,
    createSimulationApplicationVersionResponse_simulationSoftwareSuite,
    createSimulationApplicationVersionResponse_revisionId,
    createSimulationApplicationVersionResponse_version,
    createSimulationApplicationVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newCreateSimulationApplicationVersion' smart constructor.
data CreateSimulationApplicationVersion = CreateSimulationApplicationVersion'
  { -- | The Amazon S3 eTag identifier for the zip file bundle that you use to
    -- create the simulation application.
    s3Etags :: Prelude.Maybe [Prelude.Text],
    -- | The current revision id for the simulation application. If you provide a
    -- value and it matches the latest revision ID, a new version will be
    -- created.
    currentRevisionId :: Prelude.Maybe Prelude.Text,
    -- | The SHA256 digest used to identify the Docker image URI used to created
    -- the simulation application.
    imageDigest :: Prelude.Maybe Prelude.Text,
    -- | The application information for the simulation application.
    application :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSimulationApplicationVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Etags', 'createSimulationApplicationVersion_s3Etags' - The Amazon S3 eTag identifier for the zip file bundle that you use to
-- create the simulation application.
--
-- 'currentRevisionId', 'createSimulationApplicationVersion_currentRevisionId' - The current revision id for the simulation application. If you provide a
-- value and it matches the latest revision ID, a new version will be
-- created.
--
-- 'imageDigest', 'createSimulationApplicationVersion_imageDigest' - The SHA256 digest used to identify the Docker image URI used to created
-- the simulation application.
--
-- 'application', 'createSimulationApplicationVersion_application' - The application information for the simulation application.
newCreateSimulationApplicationVersion ::
  -- | 'application'
  Prelude.Text ->
  CreateSimulationApplicationVersion
newCreateSimulationApplicationVersion pApplication_ =
  CreateSimulationApplicationVersion'
    { s3Etags =
        Prelude.Nothing,
      currentRevisionId = Prelude.Nothing,
      imageDigest = Prelude.Nothing,
      application = pApplication_
    }

-- | The Amazon S3 eTag identifier for the zip file bundle that you use to
-- create the simulation application.
createSimulationApplicationVersion_s3Etags :: Lens.Lens' CreateSimulationApplicationVersion (Prelude.Maybe [Prelude.Text])
createSimulationApplicationVersion_s3Etags = Lens.lens (\CreateSimulationApplicationVersion' {s3Etags} -> s3Etags) (\s@CreateSimulationApplicationVersion' {} a -> s {s3Etags = a} :: CreateSimulationApplicationVersion) Prelude.. Lens.mapping Lens.coerced

-- | The current revision id for the simulation application. If you provide a
-- value and it matches the latest revision ID, a new version will be
-- created.
createSimulationApplicationVersion_currentRevisionId :: Lens.Lens' CreateSimulationApplicationVersion (Prelude.Maybe Prelude.Text)
createSimulationApplicationVersion_currentRevisionId = Lens.lens (\CreateSimulationApplicationVersion' {currentRevisionId} -> currentRevisionId) (\s@CreateSimulationApplicationVersion' {} a -> s {currentRevisionId = a} :: CreateSimulationApplicationVersion)

-- | The SHA256 digest used to identify the Docker image URI used to created
-- the simulation application.
createSimulationApplicationVersion_imageDigest :: Lens.Lens' CreateSimulationApplicationVersion (Prelude.Maybe Prelude.Text)
createSimulationApplicationVersion_imageDigest = Lens.lens (\CreateSimulationApplicationVersion' {imageDigest} -> imageDigest) (\s@CreateSimulationApplicationVersion' {} a -> s {imageDigest = a} :: CreateSimulationApplicationVersion)

-- | The application information for the simulation application.
createSimulationApplicationVersion_application :: Lens.Lens' CreateSimulationApplicationVersion Prelude.Text
createSimulationApplicationVersion_application = Lens.lens (\CreateSimulationApplicationVersion' {application} -> application) (\s@CreateSimulationApplicationVersion' {} a -> s {application = a} :: CreateSimulationApplicationVersion)

instance
  Core.AWSRequest
    CreateSimulationApplicationVersion
  where
  type
    AWSResponse CreateSimulationApplicationVersion =
      CreateSimulationApplicationVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSimulationApplicationVersionResponse'
            Prelude.<$> (x Core..?> "name")
              Prelude.<*> (x Core..?> "sources" Core..!@ Prelude.mempty)
              Prelude.<*> (x Core..?> "environment")
              Prelude.<*> (x Core..?> "renderingEngine")
              Prelude.<*> (x Core..?> "lastUpdatedAt")
              Prelude.<*> (x Core..?> "arn")
              Prelude.<*> (x Core..?> "robotSoftwareSuite")
              Prelude.<*> (x Core..?> "simulationSoftwareSuite")
              Prelude.<*> (x Core..?> "revisionId")
              Prelude.<*> (x Core..?> "version")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateSimulationApplicationVersion
  where
  hashWithSalt
    _salt
    CreateSimulationApplicationVersion' {..} =
      _salt `Prelude.hashWithSalt` s3Etags
        `Prelude.hashWithSalt` currentRevisionId
        `Prelude.hashWithSalt` imageDigest
        `Prelude.hashWithSalt` application

instance
  Prelude.NFData
    CreateSimulationApplicationVersion
  where
  rnf CreateSimulationApplicationVersion' {..} =
    Prelude.rnf s3Etags
      `Prelude.seq` Prelude.rnf currentRevisionId
      `Prelude.seq` Prelude.rnf imageDigest
      `Prelude.seq` Prelude.rnf application

instance
  Core.ToHeaders
    CreateSimulationApplicationVersion
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    CreateSimulationApplicationVersion
  where
  toJSON CreateSimulationApplicationVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("s3Etags" Core..=) Prelude.<$> s3Etags,
            ("currentRevisionId" Core..=)
              Prelude.<$> currentRevisionId,
            ("imageDigest" Core..=) Prelude.<$> imageDigest,
            Prelude.Just ("application" Core..= application)
          ]
      )

instance
  Core.ToPath
    CreateSimulationApplicationVersion
  where
  toPath =
    Prelude.const "/createSimulationApplicationVersion"

instance
  Core.ToQuery
    CreateSimulationApplicationVersion
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSimulationApplicationVersionResponse' smart constructor.
data CreateSimulationApplicationVersionResponse = CreateSimulationApplicationVersionResponse'
  { -- | The name of the simulation application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The sources of the simulation application.
    sources :: Prelude.Maybe [Source],
    -- | The object that contains the Docker image URI used to create the
    -- simulation application.
    environment :: Prelude.Maybe Environment,
    -- | The rendering engine for the simulation application.
    renderingEngine :: Prelude.Maybe RenderingEngine,
    -- | The time, in milliseconds since the epoch, when the simulation
    -- application was last updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the simulation application.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Information about the robot software suite (ROS distribution).
    robotSoftwareSuite :: Prelude.Maybe RobotSoftwareSuite,
    -- | The simulation software suite used by the simulation application.
    simulationSoftwareSuite :: Prelude.Maybe SimulationSoftwareSuite,
    -- | The revision ID of the simulation application.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The version of the simulation application.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSimulationApplicationVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createSimulationApplicationVersionResponse_name' - The name of the simulation application.
--
-- 'sources', 'createSimulationApplicationVersionResponse_sources' - The sources of the simulation application.
--
-- 'environment', 'createSimulationApplicationVersionResponse_environment' - The object that contains the Docker image URI used to create the
-- simulation application.
--
-- 'renderingEngine', 'createSimulationApplicationVersionResponse_renderingEngine' - The rendering engine for the simulation application.
--
-- 'lastUpdatedAt', 'createSimulationApplicationVersionResponse_lastUpdatedAt' - The time, in milliseconds since the epoch, when the simulation
-- application was last updated.
--
-- 'arn', 'createSimulationApplicationVersionResponse_arn' - The Amazon Resource Name (ARN) of the simulation application.
--
-- 'robotSoftwareSuite', 'createSimulationApplicationVersionResponse_robotSoftwareSuite' - Information about the robot software suite (ROS distribution).
--
-- 'simulationSoftwareSuite', 'createSimulationApplicationVersionResponse_simulationSoftwareSuite' - The simulation software suite used by the simulation application.
--
-- 'revisionId', 'createSimulationApplicationVersionResponse_revisionId' - The revision ID of the simulation application.
--
-- 'version', 'createSimulationApplicationVersionResponse_version' - The version of the simulation application.
--
-- 'httpStatus', 'createSimulationApplicationVersionResponse_httpStatus' - The response's http status code.
newCreateSimulationApplicationVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSimulationApplicationVersionResponse
newCreateSimulationApplicationVersionResponse
  pHttpStatus_ =
    CreateSimulationApplicationVersionResponse'
      { name =
          Prelude.Nothing,
        sources = Prelude.Nothing,
        environment = Prelude.Nothing,
        renderingEngine =
          Prelude.Nothing,
        lastUpdatedAt = Prelude.Nothing,
        arn = Prelude.Nothing,
        robotSoftwareSuite =
          Prelude.Nothing,
        simulationSoftwareSuite =
          Prelude.Nothing,
        revisionId = Prelude.Nothing,
        version = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The name of the simulation application.
createSimulationApplicationVersionResponse_name :: Lens.Lens' CreateSimulationApplicationVersionResponse (Prelude.Maybe Prelude.Text)
createSimulationApplicationVersionResponse_name = Lens.lens (\CreateSimulationApplicationVersionResponse' {name} -> name) (\s@CreateSimulationApplicationVersionResponse' {} a -> s {name = a} :: CreateSimulationApplicationVersionResponse)

-- | The sources of the simulation application.
createSimulationApplicationVersionResponse_sources :: Lens.Lens' CreateSimulationApplicationVersionResponse (Prelude.Maybe [Source])
createSimulationApplicationVersionResponse_sources = Lens.lens (\CreateSimulationApplicationVersionResponse' {sources} -> sources) (\s@CreateSimulationApplicationVersionResponse' {} a -> s {sources = a} :: CreateSimulationApplicationVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The object that contains the Docker image URI used to create the
-- simulation application.
createSimulationApplicationVersionResponse_environment :: Lens.Lens' CreateSimulationApplicationVersionResponse (Prelude.Maybe Environment)
createSimulationApplicationVersionResponse_environment = Lens.lens (\CreateSimulationApplicationVersionResponse' {environment} -> environment) (\s@CreateSimulationApplicationVersionResponse' {} a -> s {environment = a} :: CreateSimulationApplicationVersionResponse)

-- | The rendering engine for the simulation application.
createSimulationApplicationVersionResponse_renderingEngine :: Lens.Lens' CreateSimulationApplicationVersionResponse (Prelude.Maybe RenderingEngine)
createSimulationApplicationVersionResponse_renderingEngine = Lens.lens (\CreateSimulationApplicationVersionResponse' {renderingEngine} -> renderingEngine) (\s@CreateSimulationApplicationVersionResponse' {} a -> s {renderingEngine = a} :: CreateSimulationApplicationVersionResponse)

-- | The time, in milliseconds since the epoch, when the simulation
-- application was last updated.
createSimulationApplicationVersionResponse_lastUpdatedAt :: Lens.Lens' CreateSimulationApplicationVersionResponse (Prelude.Maybe Prelude.UTCTime)
createSimulationApplicationVersionResponse_lastUpdatedAt = Lens.lens (\CreateSimulationApplicationVersionResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@CreateSimulationApplicationVersionResponse' {} a -> s {lastUpdatedAt = a} :: CreateSimulationApplicationVersionResponse) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the simulation application.
createSimulationApplicationVersionResponse_arn :: Lens.Lens' CreateSimulationApplicationVersionResponse (Prelude.Maybe Prelude.Text)
createSimulationApplicationVersionResponse_arn = Lens.lens (\CreateSimulationApplicationVersionResponse' {arn} -> arn) (\s@CreateSimulationApplicationVersionResponse' {} a -> s {arn = a} :: CreateSimulationApplicationVersionResponse)

-- | Information about the robot software suite (ROS distribution).
createSimulationApplicationVersionResponse_robotSoftwareSuite :: Lens.Lens' CreateSimulationApplicationVersionResponse (Prelude.Maybe RobotSoftwareSuite)
createSimulationApplicationVersionResponse_robotSoftwareSuite = Lens.lens (\CreateSimulationApplicationVersionResponse' {robotSoftwareSuite} -> robotSoftwareSuite) (\s@CreateSimulationApplicationVersionResponse' {} a -> s {robotSoftwareSuite = a} :: CreateSimulationApplicationVersionResponse)

-- | The simulation software suite used by the simulation application.
createSimulationApplicationVersionResponse_simulationSoftwareSuite :: Lens.Lens' CreateSimulationApplicationVersionResponse (Prelude.Maybe SimulationSoftwareSuite)
createSimulationApplicationVersionResponse_simulationSoftwareSuite = Lens.lens (\CreateSimulationApplicationVersionResponse' {simulationSoftwareSuite} -> simulationSoftwareSuite) (\s@CreateSimulationApplicationVersionResponse' {} a -> s {simulationSoftwareSuite = a} :: CreateSimulationApplicationVersionResponse)

-- | The revision ID of the simulation application.
createSimulationApplicationVersionResponse_revisionId :: Lens.Lens' CreateSimulationApplicationVersionResponse (Prelude.Maybe Prelude.Text)
createSimulationApplicationVersionResponse_revisionId = Lens.lens (\CreateSimulationApplicationVersionResponse' {revisionId} -> revisionId) (\s@CreateSimulationApplicationVersionResponse' {} a -> s {revisionId = a} :: CreateSimulationApplicationVersionResponse)

-- | The version of the simulation application.
createSimulationApplicationVersionResponse_version :: Lens.Lens' CreateSimulationApplicationVersionResponse (Prelude.Maybe Prelude.Text)
createSimulationApplicationVersionResponse_version = Lens.lens (\CreateSimulationApplicationVersionResponse' {version} -> version) (\s@CreateSimulationApplicationVersionResponse' {} a -> s {version = a} :: CreateSimulationApplicationVersionResponse)

-- | The response's http status code.
createSimulationApplicationVersionResponse_httpStatus :: Lens.Lens' CreateSimulationApplicationVersionResponse Prelude.Int
createSimulationApplicationVersionResponse_httpStatus = Lens.lens (\CreateSimulationApplicationVersionResponse' {httpStatus} -> httpStatus) (\s@CreateSimulationApplicationVersionResponse' {} a -> s {httpStatus = a} :: CreateSimulationApplicationVersionResponse)

instance
  Prelude.NFData
    CreateSimulationApplicationVersionResponse
  where
  rnf CreateSimulationApplicationVersionResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf renderingEngine
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf robotSoftwareSuite
      `Prelude.seq` Prelude.rnf simulationSoftwareSuite
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
