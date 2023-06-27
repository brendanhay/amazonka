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
-- Module      : Amazonka.RobOMaker.CreateSimulationApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a simulation application.
module Amazonka.RobOMaker.CreateSimulationApplication
  ( -- * Creating a Request
    CreateSimulationApplication (..),
    newCreateSimulationApplication,

    -- * Request Lenses
    createSimulationApplication_environment,
    createSimulationApplication_renderingEngine,
    createSimulationApplication_sources,
    createSimulationApplication_tags,
    createSimulationApplication_name,
    createSimulationApplication_simulationSoftwareSuite,
    createSimulationApplication_robotSoftwareSuite,

    -- * Destructuring the Response
    CreateSimulationApplicationResponse (..),
    newCreateSimulationApplicationResponse,

    -- * Response Lenses
    createSimulationApplicationResponse_arn,
    createSimulationApplicationResponse_environment,
    createSimulationApplicationResponse_lastUpdatedAt,
    createSimulationApplicationResponse_name,
    createSimulationApplicationResponse_renderingEngine,
    createSimulationApplicationResponse_revisionId,
    createSimulationApplicationResponse_robotSoftwareSuite,
    createSimulationApplicationResponse_simulationSoftwareSuite,
    createSimulationApplicationResponse_sources,
    createSimulationApplicationResponse_tags,
    createSimulationApplicationResponse_version,
    createSimulationApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newCreateSimulationApplication' smart constructor.
data CreateSimulationApplication = CreateSimulationApplication'
  { -- | The object that contains the Docker image URI used to create your
    -- simulation application.
    environment :: Prelude.Maybe Environment,
    -- | The rendering engine for the simulation application.
    renderingEngine :: Prelude.Maybe RenderingEngine,
    -- | The sources of the simulation application.
    sources :: Prelude.Maybe [SourceConfig],
    -- | A map that contains tag keys and tag values that are attached to the
    -- simulation application.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the simulation application.
    name :: Prelude.Text,
    -- | The simulation software suite used by the simulation application.
    simulationSoftwareSuite :: SimulationSoftwareSuite,
    -- | The robot software suite (ROS distribution) used by the simulation
    -- application.
    robotSoftwareSuite :: RobotSoftwareSuite
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSimulationApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environment', 'createSimulationApplication_environment' - The object that contains the Docker image URI used to create your
-- simulation application.
--
-- 'renderingEngine', 'createSimulationApplication_renderingEngine' - The rendering engine for the simulation application.
--
-- 'sources', 'createSimulationApplication_sources' - The sources of the simulation application.
--
-- 'tags', 'createSimulationApplication_tags' - A map that contains tag keys and tag values that are attached to the
-- simulation application.
--
-- 'name', 'createSimulationApplication_name' - The name of the simulation application.
--
-- 'simulationSoftwareSuite', 'createSimulationApplication_simulationSoftwareSuite' - The simulation software suite used by the simulation application.
--
-- 'robotSoftwareSuite', 'createSimulationApplication_robotSoftwareSuite' - The robot software suite (ROS distribution) used by the simulation
-- application.
newCreateSimulationApplication ::
  -- | 'name'
  Prelude.Text ->
  -- | 'simulationSoftwareSuite'
  SimulationSoftwareSuite ->
  -- | 'robotSoftwareSuite'
  RobotSoftwareSuite ->
  CreateSimulationApplication
newCreateSimulationApplication
  pName_
  pSimulationSoftwareSuite_
  pRobotSoftwareSuite_ =
    CreateSimulationApplication'
      { environment =
          Prelude.Nothing,
        renderingEngine = Prelude.Nothing,
        sources = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        simulationSoftwareSuite =
          pSimulationSoftwareSuite_,
        robotSoftwareSuite = pRobotSoftwareSuite_
      }

-- | The object that contains the Docker image URI used to create your
-- simulation application.
createSimulationApplication_environment :: Lens.Lens' CreateSimulationApplication (Prelude.Maybe Environment)
createSimulationApplication_environment = Lens.lens (\CreateSimulationApplication' {environment} -> environment) (\s@CreateSimulationApplication' {} a -> s {environment = a} :: CreateSimulationApplication)

-- | The rendering engine for the simulation application.
createSimulationApplication_renderingEngine :: Lens.Lens' CreateSimulationApplication (Prelude.Maybe RenderingEngine)
createSimulationApplication_renderingEngine = Lens.lens (\CreateSimulationApplication' {renderingEngine} -> renderingEngine) (\s@CreateSimulationApplication' {} a -> s {renderingEngine = a} :: CreateSimulationApplication)

-- | The sources of the simulation application.
createSimulationApplication_sources :: Lens.Lens' CreateSimulationApplication (Prelude.Maybe [SourceConfig])
createSimulationApplication_sources = Lens.lens (\CreateSimulationApplication' {sources} -> sources) (\s@CreateSimulationApplication' {} a -> s {sources = a} :: CreateSimulationApplication) Prelude.. Lens.mapping Lens.coerced

-- | A map that contains tag keys and tag values that are attached to the
-- simulation application.
createSimulationApplication_tags :: Lens.Lens' CreateSimulationApplication (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSimulationApplication_tags = Lens.lens (\CreateSimulationApplication' {tags} -> tags) (\s@CreateSimulationApplication' {} a -> s {tags = a} :: CreateSimulationApplication) Prelude.. Lens.mapping Lens.coerced

-- | The name of the simulation application.
createSimulationApplication_name :: Lens.Lens' CreateSimulationApplication Prelude.Text
createSimulationApplication_name = Lens.lens (\CreateSimulationApplication' {name} -> name) (\s@CreateSimulationApplication' {} a -> s {name = a} :: CreateSimulationApplication)

-- | The simulation software suite used by the simulation application.
createSimulationApplication_simulationSoftwareSuite :: Lens.Lens' CreateSimulationApplication SimulationSoftwareSuite
createSimulationApplication_simulationSoftwareSuite = Lens.lens (\CreateSimulationApplication' {simulationSoftwareSuite} -> simulationSoftwareSuite) (\s@CreateSimulationApplication' {} a -> s {simulationSoftwareSuite = a} :: CreateSimulationApplication)

-- | The robot software suite (ROS distribution) used by the simulation
-- application.
createSimulationApplication_robotSoftwareSuite :: Lens.Lens' CreateSimulationApplication RobotSoftwareSuite
createSimulationApplication_robotSoftwareSuite = Lens.lens (\CreateSimulationApplication' {robotSoftwareSuite} -> robotSoftwareSuite) (\s@CreateSimulationApplication' {} a -> s {robotSoftwareSuite = a} :: CreateSimulationApplication)

instance Core.AWSRequest CreateSimulationApplication where
  type
    AWSResponse CreateSimulationApplication =
      CreateSimulationApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSimulationApplicationResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "environment")
            Prelude.<*> (x Data..?> "lastUpdatedAt")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "renderingEngine")
            Prelude.<*> (x Data..?> "revisionId")
            Prelude.<*> (x Data..?> "robotSoftwareSuite")
            Prelude.<*> (x Data..?> "simulationSoftwareSuite")
            Prelude.<*> (x Data..?> "sources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSimulationApplication where
  hashWithSalt _salt CreateSimulationApplication' {..} =
    _salt
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` renderingEngine
      `Prelude.hashWithSalt` sources
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` simulationSoftwareSuite
      `Prelude.hashWithSalt` robotSoftwareSuite

instance Prelude.NFData CreateSimulationApplication where
  rnf CreateSimulationApplication' {..} =
    Prelude.rnf environment
      `Prelude.seq` Prelude.rnf renderingEngine
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf simulationSoftwareSuite
      `Prelude.seq` Prelude.rnf robotSoftwareSuite

instance Data.ToHeaders CreateSimulationApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateSimulationApplication where
  toJSON CreateSimulationApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("environment" Data..=) Prelude.<$> environment,
            ("renderingEngine" Data..=)
              Prelude.<$> renderingEngine,
            ("sources" Data..=) Prelude.<$> sources,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ( "simulationSoftwareSuite"
                  Data..= simulationSoftwareSuite
              ),
            Prelude.Just
              ("robotSoftwareSuite" Data..= robotSoftwareSuite)
          ]
      )

instance Data.ToPath CreateSimulationApplication where
  toPath = Prelude.const "/createSimulationApplication"

instance Data.ToQuery CreateSimulationApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSimulationApplicationResponse' smart constructor.
data CreateSimulationApplicationResponse = CreateSimulationApplicationResponse'
  { -- | The Amazon Resource Name (ARN) of the simulation application.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The object that contains the Docker image URI that you used to create
    -- your simulation application.
    environment :: Prelude.Maybe Environment,
    -- | The time, in milliseconds since the epoch, when the simulation
    -- application was last updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The name of the simulation application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The rendering engine for the simulation application.
    renderingEngine :: Prelude.Maybe RenderingEngine,
    -- | The revision id of the simulation application.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | Information about the robot software suite (ROS distribution).
    robotSoftwareSuite :: Prelude.Maybe RobotSoftwareSuite,
    -- | The simulation software suite used by the simulation application.
    simulationSoftwareSuite :: Prelude.Maybe SimulationSoftwareSuite,
    -- | The sources of the simulation application.
    sources :: Prelude.Maybe [Source],
    -- | The list of all tags added to the simulation application.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The version of the simulation application.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSimulationApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createSimulationApplicationResponse_arn' - The Amazon Resource Name (ARN) of the simulation application.
--
-- 'environment', 'createSimulationApplicationResponse_environment' - The object that contains the Docker image URI that you used to create
-- your simulation application.
--
-- 'lastUpdatedAt', 'createSimulationApplicationResponse_lastUpdatedAt' - The time, in milliseconds since the epoch, when the simulation
-- application was last updated.
--
-- 'name', 'createSimulationApplicationResponse_name' - The name of the simulation application.
--
-- 'renderingEngine', 'createSimulationApplicationResponse_renderingEngine' - The rendering engine for the simulation application.
--
-- 'revisionId', 'createSimulationApplicationResponse_revisionId' - The revision id of the simulation application.
--
-- 'robotSoftwareSuite', 'createSimulationApplicationResponse_robotSoftwareSuite' - Information about the robot software suite (ROS distribution).
--
-- 'simulationSoftwareSuite', 'createSimulationApplicationResponse_simulationSoftwareSuite' - The simulation software suite used by the simulation application.
--
-- 'sources', 'createSimulationApplicationResponse_sources' - The sources of the simulation application.
--
-- 'tags', 'createSimulationApplicationResponse_tags' - The list of all tags added to the simulation application.
--
-- 'version', 'createSimulationApplicationResponse_version' - The version of the simulation application.
--
-- 'httpStatus', 'createSimulationApplicationResponse_httpStatus' - The response's http status code.
newCreateSimulationApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSimulationApplicationResponse
newCreateSimulationApplicationResponse pHttpStatus_ =
  CreateSimulationApplicationResponse'
    { arn =
        Prelude.Nothing,
      environment = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      name = Prelude.Nothing,
      renderingEngine = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      robotSoftwareSuite = Prelude.Nothing,
      simulationSoftwareSuite =
        Prelude.Nothing,
      sources = Prelude.Nothing,
      tags = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the simulation application.
createSimulationApplicationResponse_arn :: Lens.Lens' CreateSimulationApplicationResponse (Prelude.Maybe Prelude.Text)
createSimulationApplicationResponse_arn = Lens.lens (\CreateSimulationApplicationResponse' {arn} -> arn) (\s@CreateSimulationApplicationResponse' {} a -> s {arn = a} :: CreateSimulationApplicationResponse)

-- | The object that contains the Docker image URI that you used to create
-- your simulation application.
createSimulationApplicationResponse_environment :: Lens.Lens' CreateSimulationApplicationResponse (Prelude.Maybe Environment)
createSimulationApplicationResponse_environment = Lens.lens (\CreateSimulationApplicationResponse' {environment} -> environment) (\s@CreateSimulationApplicationResponse' {} a -> s {environment = a} :: CreateSimulationApplicationResponse)

-- | The time, in milliseconds since the epoch, when the simulation
-- application was last updated.
createSimulationApplicationResponse_lastUpdatedAt :: Lens.Lens' CreateSimulationApplicationResponse (Prelude.Maybe Prelude.UTCTime)
createSimulationApplicationResponse_lastUpdatedAt = Lens.lens (\CreateSimulationApplicationResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@CreateSimulationApplicationResponse' {} a -> s {lastUpdatedAt = a} :: CreateSimulationApplicationResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the simulation application.
createSimulationApplicationResponse_name :: Lens.Lens' CreateSimulationApplicationResponse (Prelude.Maybe Prelude.Text)
createSimulationApplicationResponse_name = Lens.lens (\CreateSimulationApplicationResponse' {name} -> name) (\s@CreateSimulationApplicationResponse' {} a -> s {name = a} :: CreateSimulationApplicationResponse)

-- | The rendering engine for the simulation application.
createSimulationApplicationResponse_renderingEngine :: Lens.Lens' CreateSimulationApplicationResponse (Prelude.Maybe RenderingEngine)
createSimulationApplicationResponse_renderingEngine = Lens.lens (\CreateSimulationApplicationResponse' {renderingEngine} -> renderingEngine) (\s@CreateSimulationApplicationResponse' {} a -> s {renderingEngine = a} :: CreateSimulationApplicationResponse)

-- | The revision id of the simulation application.
createSimulationApplicationResponse_revisionId :: Lens.Lens' CreateSimulationApplicationResponse (Prelude.Maybe Prelude.Text)
createSimulationApplicationResponse_revisionId = Lens.lens (\CreateSimulationApplicationResponse' {revisionId} -> revisionId) (\s@CreateSimulationApplicationResponse' {} a -> s {revisionId = a} :: CreateSimulationApplicationResponse)

-- | Information about the robot software suite (ROS distribution).
createSimulationApplicationResponse_robotSoftwareSuite :: Lens.Lens' CreateSimulationApplicationResponse (Prelude.Maybe RobotSoftwareSuite)
createSimulationApplicationResponse_robotSoftwareSuite = Lens.lens (\CreateSimulationApplicationResponse' {robotSoftwareSuite} -> robotSoftwareSuite) (\s@CreateSimulationApplicationResponse' {} a -> s {robotSoftwareSuite = a} :: CreateSimulationApplicationResponse)

-- | The simulation software suite used by the simulation application.
createSimulationApplicationResponse_simulationSoftwareSuite :: Lens.Lens' CreateSimulationApplicationResponse (Prelude.Maybe SimulationSoftwareSuite)
createSimulationApplicationResponse_simulationSoftwareSuite = Lens.lens (\CreateSimulationApplicationResponse' {simulationSoftwareSuite} -> simulationSoftwareSuite) (\s@CreateSimulationApplicationResponse' {} a -> s {simulationSoftwareSuite = a} :: CreateSimulationApplicationResponse)

-- | The sources of the simulation application.
createSimulationApplicationResponse_sources :: Lens.Lens' CreateSimulationApplicationResponse (Prelude.Maybe [Source])
createSimulationApplicationResponse_sources = Lens.lens (\CreateSimulationApplicationResponse' {sources} -> sources) (\s@CreateSimulationApplicationResponse' {} a -> s {sources = a} :: CreateSimulationApplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of all tags added to the simulation application.
createSimulationApplicationResponse_tags :: Lens.Lens' CreateSimulationApplicationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createSimulationApplicationResponse_tags = Lens.lens (\CreateSimulationApplicationResponse' {tags} -> tags) (\s@CreateSimulationApplicationResponse' {} a -> s {tags = a} :: CreateSimulationApplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The version of the simulation application.
createSimulationApplicationResponse_version :: Lens.Lens' CreateSimulationApplicationResponse (Prelude.Maybe Prelude.Text)
createSimulationApplicationResponse_version = Lens.lens (\CreateSimulationApplicationResponse' {version} -> version) (\s@CreateSimulationApplicationResponse' {} a -> s {version = a} :: CreateSimulationApplicationResponse)

-- | The response's http status code.
createSimulationApplicationResponse_httpStatus :: Lens.Lens' CreateSimulationApplicationResponse Prelude.Int
createSimulationApplicationResponse_httpStatus = Lens.lens (\CreateSimulationApplicationResponse' {httpStatus} -> httpStatus) (\s@CreateSimulationApplicationResponse' {} a -> s {httpStatus = a} :: CreateSimulationApplicationResponse)

instance
  Prelude.NFData
    CreateSimulationApplicationResponse
  where
  rnf CreateSimulationApplicationResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf renderingEngine
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf robotSoftwareSuite
      `Prelude.seq` Prelude.rnf simulationSoftwareSuite
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
