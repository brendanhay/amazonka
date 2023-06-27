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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    createSimulationApplicationVersion_currentRevisionId,
    createSimulationApplicationVersion_imageDigest,
    createSimulationApplicationVersion_s3Etags,
    createSimulationApplicationVersion_application,

    -- * Destructuring the Response
    CreateSimulationApplicationVersionResponse (..),
    newCreateSimulationApplicationVersionResponse,

    -- * Response Lenses
    createSimulationApplicationVersionResponse_arn,
    createSimulationApplicationVersionResponse_environment,
    createSimulationApplicationVersionResponse_lastUpdatedAt,
    createSimulationApplicationVersionResponse_name,
    createSimulationApplicationVersionResponse_renderingEngine,
    createSimulationApplicationVersionResponse_revisionId,
    createSimulationApplicationVersionResponse_robotSoftwareSuite,
    createSimulationApplicationVersionResponse_simulationSoftwareSuite,
    createSimulationApplicationVersionResponse_sources,
    createSimulationApplicationVersionResponse_version,
    createSimulationApplicationVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newCreateSimulationApplicationVersion' smart constructor.
data CreateSimulationApplicationVersion = CreateSimulationApplicationVersion'
  { -- | The current revision id for the simulation application. If you provide a
    -- value and it matches the latest revision ID, a new version will be
    -- created.
    currentRevisionId :: Prelude.Maybe Prelude.Text,
    -- | The SHA256 digest used to identify the Docker image URI used to created
    -- the simulation application.
    imageDigest :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 eTag identifier for the zip file bundle that you use to
    -- create the simulation application.
    s3Etags :: Prelude.Maybe [Prelude.Text],
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
-- 'currentRevisionId', 'createSimulationApplicationVersion_currentRevisionId' - The current revision id for the simulation application. If you provide a
-- value and it matches the latest revision ID, a new version will be
-- created.
--
-- 'imageDigest', 'createSimulationApplicationVersion_imageDigest' - The SHA256 digest used to identify the Docker image URI used to created
-- the simulation application.
--
-- 's3Etags', 'createSimulationApplicationVersion_s3Etags' - The Amazon S3 eTag identifier for the zip file bundle that you use to
-- create the simulation application.
--
-- 'application', 'createSimulationApplicationVersion_application' - The application information for the simulation application.
newCreateSimulationApplicationVersion ::
  -- | 'application'
  Prelude.Text ->
  CreateSimulationApplicationVersion
newCreateSimulationApplicationVersion pApplication_ =
  CreateSimulationApplicationVersion'
    { currentRevisionId =
        Prelude.Nothing,
      imageDigest = Prelude.Nothing,
      s3Etags = Prelude.Nothing,
      application = pApplication_
    }

-- | The current revision id for the simulation application. If you provide a
-- value and it matches the latest revision ID, a new version will be
-- created.
createSimulationApplicationVersion_currentRevisionId :: Lens.Lens' CreateSimulationApplicationVersion (Prelude.Maybe Prelude.Text)
createSimulationApplicationVersion_currentRevisionId = Lens.lens (\CreateSimulationApplicationVersion' {currentRevisionId} -> currentRevisionId) (\s@CreateSimulationApplicationVersion' {} a -> s {currentRevisionId = a} :: CreateSimulationApplicationVersion)

-- | The SHA256 digest used to identify the Docker image URI used to created
-- the simulation application.
createSimulationApplicationVersion_imageDigest :: Lens.Lens' CreateSimulationApplicationVersion (Prelude.Maybe Prelude.Text)
createSimulationApplicationVersion_imageDigest = Lens.lens (\CreateSimulationApplicationVersion' {imageDigest} -> imageDigest) (\s@CreateSimulationApplicationVersion' {} a -> s {imageDigest = a} :: CreateSimulationApplicationVersion)

-- | The Amazon S3 eTag identifier for the zip file bundle that you use to
-- create the simulation application.
createSimulationApplicationVersion_s3Etags :: Lens.Lens' CreateSimulationApplicationVersion (Prelude.Maybe [Prelude.Text])
createSimulationApplicationVersion_s3Etags = Lens.lens (\CreateSimulationApplicationVersion' {s3Etags} -> s3Etags) (\s@CreateSimulationApplicationVersion' {} a -> s {s3Etags = a} :: CreateSimulationApplicationVersion) Prelude.. Lens.mapping Lens.coerced

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
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "environment")
            Prelude.<*> (x Data..?> "lastUpdatedAt")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "renderingEngine")
            Prelude.<*> (x Data..?> "revisionId")
            Prelude.<*> (x Data..?> "robotSoftwareSuite")
            Prelude.<*> (x Data..?> "simulationSoftwareSuite")
            Prelude.<*> (x Data..?> "sources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateSimulationApplicationVersion
  where
  hashWithSalt
    _salt
    CreateSimulationApplicationVersion' {..} =
      _salt
        `Prelude.hashWithSalt` currentRevisionId
        `Prelude.hashWithSalt` imageDigest
        `Prelude.hashWithSalt` s3Etags
        `Prelude.hashWithSalt` application

instance
  Prelude.NFData
    CreateSimulationApplicationVersion
  where
  rnf CreateSimulationApplicationVersion' {..} =
    Prelude.rnf currentRevisionId
      `Prelude.seq` Prelude.rnf imageDigest
      `Prelude.seq` Prelude.rnf s3Etags
      `Prelude.seq` Prelude.rnf application

instance
  Data.ToHeaders
    CreateSimulationApplicationVersion
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    CreateSimulationApplicationVersion
  where
  toJSON CreateSimulationApplicationVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("currentRevisionId" Data..=)
              Prelude.<$> currentRevisionId,
            ("imageDigest" Data..=) Prelude.<$> imageDigest,
            ("s3Etags" Data..=) Prelude.<$> s3Etags,
            Prelude.Just ("application" Data..= application)
          ]
      )

instance
  Data.ToPath
    CreateSimulationApplicationVersion
  where
  toPath =
    Prelude.const "/createSimulationApplicationVersion"

instance
  Data.ToQuery
    CreateSimulationApplicationVersion
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSimulationApplicationVersionResponse' smart constructor.
data CreateSimulationApplicationVersionResponse = CreateSimulationApplicationVersionResponse'
  { -- | The Amazon Resource Name (ARN) of the simulation application.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The object that contains the Docker image URI used to create the
    -- simulation application.
    environment :: Prelude.Maybe Environment,
    -- | The time, in milliseconds since the epoch, when the simulation
    -- application was last updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The name of the simulation application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The rendering engine for the simulation application.
    renderingEngine :: Prelude.Maybe RenderingEngine,
    -- | The revision ID of the simulation application.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | Information about the robot software suite (ROS distribution).
    robotSoftwareSuite :: Prelude.Maybe RobotSoftwareSuite,
    -- | The simulation software suite used by the simulation application.
    simulationSoftwareSuite :: Prelude.Maybe SimulationSoftwareSuite,
    -- | The sources of the simulation application.
    sources :: Prelude.Maybe [Source],
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
-- 'arn', 'createSimulationApplicationVersionResponse_arn' - The Amazon Resource Name (ARN) of the simulation application.
--
-- 'environment', 'createSimulationApplicationVersionResponse_environment' - The object that contains the Docker image URI used to create the
-- simulation application.
--
-- 'lastUpdatedAt', 'createSimulationApplicationVersionResponse_lastUpdatedAt' - The time, in milliseconds since the epoch, when the simulation
-- application was last updated.
--
-- 'name', 'createSimulationApplicationVersionResponse_name' - The name of the simulation application.
--
-- 'renderingEngine', 'createSimulationApplicationVersionResponse_renderingEngine' - The rendering engine for the simulation application.
--
-- 'revisionId', 'createSimulationApplicationVersionResponse_revisionId' - The revision ID of the simulation application.
--
-- 'robotSoftwareSuite', 'createSimulationApplicationVersionResponse_robotSoftwareSuite' - Information about the robot software suite (ROS distribution).
--
-- 'simulationSoftwareSuite', 'createSimulationApplicationVersionResponse_simulationSoftwareSuite' - The simulation software suite used by the simulation application.
--
-- 'sources', 'createSimulationApplicationVersionResponse_sources' - The sources of the simulation application.
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
      { arn =
          Prelude.Nothing,
        environment = Prelude.Nothing,
        lastUpdatedAt = Prelude.Nothing,
        name = Prelude.Nothing,
        renderingEngine =
          Prelude.Nothing,
        revisionId = Prelude.Nothing,
        robotSoftwareSuite =
          Prelude.Nothing,
        simulationSoftwareSuite =
          Prelude.Nothing,
        sources = Prelude.Nothing,
        version = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The Amazon Resource Name (ARN) of the simulation application.
createSimulationApplicationVersionResponse_arn :: Lens.Lens' CreateSimulationApplicationVersionResponse (Prelude.Maybe Prelude.Text)
createSimulationApplicationVersionResponse_arn = Lens.lens (\CreateSimulationApplicationVersionResponse' {arn} -> arn) (\s@CreateSimulationApplicationVersionResponse' {} a -> s {arn = a} :: CreateSimulationApplicationVersionResponse)

-- | The object that contains the Docker image URI used to create the
-- simulation application.
createSimulationApplicationVersionResponse_environment :: Lens.Lens' CreateSimulationApplicationVersionResponse (Prelude.Maybe Environment)
createSimulationApplicationVersionResponse_environment = Lens.lens (\CreateSimulationApplicationVersionResponse' {environment} -> environment) (\s@CreateSimulationApplicationVersionResponse' {} a -> s {environment = a} :: CreateSimulationApplicationVersionResponse)

-- | The time, in milliseconds since the epoch, when the simulation
-- application was last updated.
createSimulationApplicationVersionResponse_lastUpdatedAt :: Lens.Lens' CreateSimulationApplicationVersionResponse (Prelude.Maybe Prelude.UTCTime)
createSimulationApplicationVersionResponse_lastUpdatedAt = Lens.lens (\CreateSimulationApplicationVersionResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@CreateSimulationApplicationVersionResponse' {} a -> s {lastUpdatedAt = a} :: CreateSimulationApplicationVersionResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the simulation application.
createSimulationApplicationVersionResponse_name :: Lens.Lens' CreateSimulationApplicationVersionResponse (Prelude.Maybe Prelude.Text)
createSimulationApplicationVersionResponse_name = Lens.lens (\CreateSimulationApplicationVersionResponse' {name} -> name) (\s@CreateSimulationApplicationVersionResponse' {} a -> s {name = a} :: CreateSimulationApplicationVersionResponse)

-- | The rendering engine for the simulation application.
createSimulationApplicationVersionResponse_renderingEngine :: Lens.Lens' CreateSimulationApplicationVersionResponse (Prelude.Maybe RenderingEngine)
createSimulationApplicationVersionResponse_renderingEngine = Lens.lens (\CreateSimulationApplicationVersionResponse' {renderingEngine} -> renderingEngine) (\s@CreateSimulationApplicationVersionResponse' {} a -> s {renderingEngine = a} :: CreateSimulationApplicationVersionResponse)

-- | The revision ID of the simulation application.
createSimulationApplicationVersionResponse_revisionId :: Lens.Lens' CreateSimulationApplicationVersionResponse (Prelude.Maybe Prelude.Text)
createSimulationApplicationVersionResponse_revisionId = Lens.lens (\CreateSimulationApplicationVersionResponse' {revisionId} -> revisionId) (\s@CreateSimulationApplicationVersionResponse' {} a -> s {revisionId = a} :: CreateSimulationApplicationVersionResponse)

-- | Information about the robot software suite (ROS distribution).
createSimulationApplicationVersionResponse_robotSoftwareSuite :: Lens.Lens' CreateSimulationApplicationVersionResponse (Prelude.Maybe RobotSoftwareSuite)
createSimulationApplicationVersionResponse_robotSoftwareSuite = Lens.lens (\CreateSimulationApplicationVersionResponse' {robotSoftwareSuite} -> robotSoftwareSuite) (\s@CreateSimulationApplicationVersionResponse' {} a -> s {robotSoftwareSuite = a} :: CreateSimulationApplicationVersionResponse)

-- | The simulation software suite used by the simulation application.
createSimulationApplicationVersionResponse_simulationSoftwareSuite :: Lens.Lens' CreateSimulationApplicationVersionResponse (Prelude.Maybe SimulationSoftwareSuite)
createSimulationApplicationVersionResponse_simulationSoftwareSuite = Lens.lens (\CreateSimulationApplicationVersionResponse' {simulationSoftwareSuite} -> simulationSoftwareSuite) (\s@CreateSimulationApplicationVersionResponse' {} a -> s {simulationSoftwareSuite = a} :: CreateSimulationApplicationVersionResponse)

-- | The sources of the simulation application.
createSimulationApplicationVersionResponse_sources :: Lens.Lens' CreateSimulationApplicationVersionResponse (Prelude.Maybe [Source])
createSimulationApplicationVersionResponse_sources = Lens.lens (\CreateSimulationApplicationVersionResponse' {sources} -> sources) (\s@CreateSimulationApplicationVersionResponse' {} a -> s {sources = a} :: CreateSimulationApplicationVersionResponse) Prelude.. Lens.mapping Lens.coerced

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
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf renderingEngine
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf robotSoftwareSuite
      `Prelude.seq` Prelude.rnf simulationSoftwareSuite
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
