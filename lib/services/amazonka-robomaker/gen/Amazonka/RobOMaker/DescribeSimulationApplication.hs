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
-- Module      : Amazonka.RobOMaker.DescribeSimulationApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a simulation application.
module Amazonka.RobOMaker.DescribeSimulationApplication
  ( -- * Creating a Request
    DescribeSimulationApplication (..),
    newDescribeSimulationApplication,

    -- * Request Lenses
    describeSimulationApplication_applicationVersion,
    describeSimulationApplication_application,

    -- * Destructuring the Response
    DescribeSimulationApplicationResponse (..),
    newDescribeSimulationApplicationResponse,

    -- * Response Lenses
    describeSimulationApplicationResponse_arn,
    describeSimulationApplicationResponse_environment,
    describeSimulationApplicationResponse_imageDigest,
    describeSimulationApplicationResponse_lastUpdatedAt,
    describeSimulationApplicationResponse_name,
    describeSimulationApplicationResponse_renderingEngine,
    describeSimulationApplicationResponse_revisionId,
    describeSimulationApplicationResponse_robotSoftwareSuite,
    describeSimulationApplicationResponse_simulationSoftwareSuite,
    describeSimulationApplicationResponse_sources,
    describeSimulationApplicationResponse_tags,
    describeSimulationApplicationResponse_version,
    describeSimulationApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newDescribeSimulationApplication' smart constructor.
data DescribeSimulationApplication = DescribeSimulationApplication'
  { -- | The version of the simulation application to describe.
    applicationVersion :: Prelude.Maybe Prelude.Text,
    -- | The application information for the simulation application.
    application :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSimulationApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationVersion', 'describeSimulationApplication_applicationVersion' - The version of the simulation application to describe.
--
-- 'application', 'describeSimulationApplication_application' - The application information for the simulation application.
newDescribeSimulationApplication ::
  -- | 'application'
  Prelude.Text ->
  DescribeSimulationApplication
newDescribeSimulationApplication pApplication_ =
  DescribeSimulationApplication'
    { applicationVersion =
        Prelude.Nothing,
      application = pApplication_
    }

-- | The version of the simulation application to describe.
describeSimulationApplication_applicationVersion :: Lens.Lens' DescribeSimulationApplication (Prelude.Maybe Prelude.Text)
describeSimulationApplication_applicationVersion = Lens.lens (\DescribeSimulationApplication' {applicationVersion} -> applicationVersion) (\s@DescribeSimulationApplication' {} a -> s {applicationVersion = a} :: DescribeSimulationApplication)

-- | The application information for the simulation application.
describeSimulationApplication_application :: Lens.Lens' DescribeSimulationApplication Prelude.Text
describeSimulationApplication_application = Lens.lens (\DescribeSimulationApplication' {application} -> application) (\s@DescribeSimulationApplication' {} a -> s {application = a} :: DescribeSimulationApplication)

instance
  Core.AWSRequest
    DescribeSimulationApplication
  where
  type
    AWSResponse DescribeSimulationApplication =
      DescribeSimulationApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeSimulationApplicationResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "environment")
            Prelude.<*> (x Data..?> "imageDigest")
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

instance
  Prelude.Hashable
    DescribeSimulationApplication
  where
  hashWithSalt _salt DescribeSimulationApplication' {..} =
    _salt `Prelude.hashWithSalt` applicationVersion
      `Prelude.hashWithSalt` application

instance Prelude.NFData DescribeSimulationApplication where
  rnf DescribeSimulationApplication' {..} =
    Prelude.rnf applicationVersion
      `Prelude.seq` Prelude.rnf application

instance Data.ToHeaders DescribeSimulationApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeSimulationApplication where
  toJSON DescribeSimulationApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("applicationVersion" Data..=)
              Prelude.<$> applicationVersion,
            Prelude.Just ("application" Data..= application)
          ]
      )

instance Data.ToPath DescribeSimulationApplication where
  toPath =
    Prelude.const "/describeSimulationApplication"

instance Data.ToQuery DescribeSimulationApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeSimulationApplicationResponse' smart constructor.
data DescribeSimulationApplicationResponse = DescribeSimulationApplicationResponse'
  { -- | The Amazon Resource Name (ARN) of the robot simulation application.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The object that contains the Docker image URI used to create the
    -- simulation application.
    environment :: Prelude.Maybe Environment,
    -- | A SHA256 identifier for the Docker image that you use for your
    -- simulation application.
    imageDigest :: Prelude.Maybe Prelude.Text,
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
    -- | The list of all tags added to the specified simulation application.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The version of the simulation application.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSimulationApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeSimulationApplicationResponse_arn' - The Amazon Resource Name (ARN) of the robot simulation application.
--
-- 'environment', 'describeSimulationApplicationResponse_environment' - The object that contains the Docker image URI used to create the
-- simulation application.
--
-- 'imageDigest', 'describeSimulationApplicationResponse_imageDigest' - A SHA256 identifier for the Docker image that you use for your
-- simulation application.
--
-- 'lastUpdatedAt', 'describeSimulationApplicationResponse_lastUpdatedAt' - The time, in milliseconds since the epoch, when the simulation
-- application was last updated.
--
-- 'name', 'describeSimulationApplicationResponse_name' - The name of the simulation application.
--
-- 'renderingEngine', 'describeSimulationApplicationResponse_renderingEngine' - The rendering engine for the simulation application.
--
-- 'revisionId', 'describeSimulationApplicationResponse_revisionId' - The revision id of the simulation application.
--
-- 'robotSoftwareSuite', 'describeSimulationApplicationResponse_robotSoftwareSuite' - Information about the robot software suite (ROS distribution).
--
-- 'simulationSoftwareSuite', 'describeSimulationApplicationResponse_simulationSoftwareSuite' - The simulation software suite used by the simulation application.
--
-- 'sources', 'describeSimulationApplicationResponse_sources' - The sources of the simulation application.
--
-- 'tags', 'describeSimulationApplicationResponse_tags' - The list of all tags added to the specified simulation application.
--
-- 'version', 'describeSimulationApplicationResponse_version' - The version of the simulation application.
--
-- 'httpStatus', 'describeSimulationApplicationResponse_httpStatus' - The response's http status code.
newDescribeSimulationApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSimulationApplicationResponse
newDescribeSimulationApplicationResponse pHttpStatus_ =
  DescribeSimulationApplicationResponse'
    { arn =
        Prelude.Nothing,
      environment = Prelude.Nothing,
      imageDigest = Prelude.Nothing,
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

-- | The Amazon Resource Name (ARN) of the robot simulation application.
describeSimulationApplicationResponse_arn :: Lens.Lens' DescribeSimulationApplicationResponse (Prelude.Maybe Prelude.Text)
describeSimulationApplicationResponse_arn = Lens.lens (\DescribeSimulationApplicationResponse' {arn} -> arn) (\s@DescribeSimulationApplicationResponse' {} a -> s {arn = a} :: DescribeSimulationApplicationResponse)

-- | The object that contains the Docker image URI used to create the
-- simulation application.
describeSimulationApplicationResponse_environment :: Lens.Lens' DescribeSimulationApplicationResponse (Prelude.Maybe Environment)
describeSimulationApplicationResponse_environment = Lens.lens (\DescribeSimulationApplicationResponse' {environment} -> environment) (\s@DescribeSimulationApplicationResponse' {} a -> s {environment = a} :: DescribeSimulationApplicationResponse)

-- | A SHA256 identifier for the Docker image that you use for your
-- simulation application.
describeSimulationApplicationResponse_imageDigest :: Lens.Lens' DescribeSimulationApplicationResponse (Prelude.Maybe Prelude.Text)
describeSimulationApplicationResponse_imageDigest = Lens.lens (\DescribeSimulationApplicationResponse' {imageDigest} -> imageDigest) (\s@DescribeSimulationApplicationResponse' {} a -> s {imageDigest = a} :: DescribeSimulationApplicationResponse)

-- | The time, in milliseconds since the epoch, when the simulation
-- application was last updated.
describeSimulationApplicationResponse_lastUpdatedAt :: Lens.Lens' DescribeSimulationApplicationResponse (Prelude.Maybe Prelude.UTCTime)
describeSimulationApplicationResponse_lastUpdatedAt = Lens.lens (\DescribeSimulationApplicationResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@DescribeSimulationApplicationResponse' {} a -> s {lastUpdatedAt = a} :: DescribeSimulationApplicationResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the simulation application.
describeSimulationApplicationResponse_name :: Lens.Lens' DescribeSimulationApplicationResponse (Prelude.Maybe Prelude.Text)
describeSimulationApplicationResponse_name = Lens.lens (\DescribeSimulationApplicationResponse' {name} -> name) (\s@DescribeSimulationApplicationResponse' {} a -> s {name = a} :: DescribeSimulationApplicationResponse)

-- | The rendering engine for the simulation application.
describeSimulationApplicationResponse_renderingEngine :: Lens.Lens' DescribeSimulationApplicationResponse (Prelude.Maybe RenderingEngine)
describeSimulationApplicationResponse_renderingEngine = Lens.lens (\DescribeSimulationApplicationResponse' {renderingEngine} -> renderingEngine) (\s@DescribeSimulationApplicationResponse' {} a -> s {renderingEngine = a} :: DescribeSimulationApplicationResponse)

-- | The revision id of the simulation application.
describeSimulationApplicationResponse_revisionId :: Lens.Lens' DescribeSimulationApplicationResponse (Prelude.Maybe Prelude.Text)
describeSimulationApplicationResponse_revisionId = Lens.lens (\DescribeSimulationApplicationResponse' {revisionId} -> revisionId) (\s@DescribeSimulationApplicationResponse' {} a -> s {revisionId = a} :: DescribeSimulationApplicationResponse)

-- | Information about the robot software suite (ROS distribution).
describeSimulationApplicationResponse_robotSoftwareSuite :: Lens.Lens' DescribeSimulationApplicationResponse (Prelude.Maybe RobotSoftwareSuite)
describeSimulationApplicationResponse_robotSoftwareSuite = Lens.lens (\DescribeSimulationApplicationResponse' {robotSoftwareSuite} -> robotSoftwareSuite) (\s@DescribeSimulationApplicationResponse' {} a -> s {robotSoftwareSuite = a} :: DescribeSimulationApplicationResponse)

-- | The simulation software suite used by the simulation application.
describeSimulationApplicationResponse_simulationSoftwareSuite :: Lens.Lens' DescribeSimulationApplicationResponse (Prelude.Maybe SimulationSoftwareSuite)
describeSimulationApplicationResponse_simulationSoftwareSuite = Lens.lens (\DescribeSimulationApplicationResponse' {simulationSoftwareSuite} -> simulationSoftwareSuite) (\s@DescribeSimulationApplicationResponse' {} a -> s {simulationSoftwareSuite = a} :: DescribeSimulationApplicationResponse)

-- | The sources of the simulation application.
describeSimulationApplicationResponse_sources :: Lens.Lens' DescribeSimulationApplicationResponse (Prelude.Maybe [Source])
describeSimulationApplicationResponse_sources = Lens.lens (\DescribeSimulationApplicationResponse' {sources} -> sources) (\s@DescribeSimulationApplicationResponse' {} a -> s {sources = a} :: DescribeSimulationApplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of all tags added to the specified simulation application.
describeSimulationApplicationResponse_tags :: Lens.Lens' DescribeSimulationApplicationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeSimulationApplicationResponse_tags = Lens.lens (\DescribeSimulationApplicationResponse' {tags} -> tags) (\s@DescribeSimulationApplicationResponse' {} a -> s {tags = a} :: DescribeSimulationApplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The version of the simulation application.
describeSimulationApplicationResponse_version :: Lens.Lens' DescribeSimulationApplicationResponse (Prelude.Maybe Prelude.Text)
describeSimulationApplicationResponse_version = Lens.lens (\DescribeSimulationApplicationResponse' {version} -> version) (\s@DescribeSimulationApplicationResponse' {} a -> s {version = a} :: DescribeSimulationApplicationResponse)

-- | The response's http status code.
describeSimulationApplicationResponse_httpStatus :: Lens.Lens' DescribeSimulationApplicationResponse Prelude.Int
describeSimulationApplicationResponse_httpStatus = Lens.lens (\DescribeSimulationApplicationResponse' {httpStatus} -> httpStatus) (\s@DescribeSimulationApplicationResponse' {} a -> s {httpStatus = a} :: DescribeSimulationApplicationResponse)

instance
  Prelude.NFData
    DescribeSimulationApplicationResponse
  where
  rnf DescribeSimulationApplicationResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf imageDigest
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
