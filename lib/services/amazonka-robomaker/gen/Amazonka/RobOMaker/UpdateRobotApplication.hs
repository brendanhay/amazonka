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
-- Module      : Amazonka.RobOMaker.UpdateRobotApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a robot application.
module Amazonka.RobOMaker.UpdateRobotApplication
  ( -- * Creating a Request
    UpdateRobotApplication (..),
    newUpdateRobotApplication,

    -- * Request Lenses
    updateRobotApplication_currentRevisionId,
    updateRobotApplication_environment,
    updateRobotApplication_sources,
    updateRobotApplication_application,
    updateRobotApplication_robotSoftwareSuite,

    -- * Destructuring the Response
    UpdateRobotApplicationResponse (..),
    newUpdateRobotApplicationResponse,

    -- * Response Lenses
    updateRobotApplicationResponse_arn,
    updateRobotApplicationResponse_environment,
    updateRobotApplicationResponse_lastUpdatedAt,
    updateRobotApplicationResponse_name,
    updateRobotApplicationResponse_revisionId,
    updateRobotApplicationResponse_robotSoftwareSuite,
    updateRobotApplicationResponse_sources,
    updateRobotApplicationResponse_version,
    updateRobotApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newUpdateRobotApplication' smart constructor.
data UpdateRobotApplication = UpdateRobotApplication'
  { -- | The revision id for the robot application.
    currentRevisionId :: Prelude.Maybe Prelude.Text,
    -- | The object that contains the Docker image URI for your robot
    -- application.
    environment :: Prelude.Maybe Environment,
    -- | The sources of the robot application.
    sources :: Prelude.Maybe [SourceConfig],
    -- | The application information for the robot application.
    application :: Prelude.Text,
    -- | The robot software suite (ROS distribution) used by the robot
    -- application.
    robotSoftwareSuite :: RobotSoftwareSuite
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRobotApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentRevisionId', 'updateRobotApplication_currentRevisionId' - The revision id for the robot application.
--
-- 'environment', 'updateRobotApplication_environment' - The object that contains the Docker image URI for your robot
-- application.
--
-- 'sources', 'updateRobotApplication_sources' - The sources of the robot application.
--
-- 'application', 'updateRobotApplication_application' - The application information for the robot application.
--
-- 'robotSoftwareSuite', 'updateRobotApplication_robotSoftwareSuite' - The robot software suite (ROS distribution) used by the robot
-- application.
newUpdateRobotApplication ::
  -- | 'application'
  Prelude.Text ->
  -- | 'robotSoftwareSuite'
  RobotSoftwareSuite ->
  UpdateRobotApplication
newUpdateRobotApplication
  pApplication_
  pRobotSoftwareSuite_ =
    UpdateRobotApplication'
      { currentRevisionId =
          Prelude.Nothing,
        environment = Prelude.Nothing,
        sources = Prelude.Nothing,
        application = pApplication_,
        robotSoftwareSuite = pRobotSoftwareSuite_
      }

-- | The revision id for the robot application.
updateRobotApplication_currentRevisionId :: Lens.Lens' UpdateRobotApplication (Prelude.Maybe Prelude.Text)
updateRobotApplication_currentRevisionId = Lens.lens (\UpdateRobotApplication' {currentRevisionId} -> currentRevisionId) (\s@UpdateRobotApplication' {} a -> s {currentRevisionId = a} :: UpdateRobotApplication)

-- | The object that contains the Docker image URI for your robot
-- application.
updateRobotApplication_environment :: Lens.Lens' UpdateRobotApplication (Prelude.Maybe Environment)
updateRobotApplication_environment = Lens.lens (\UpdateRobotApplication' {environment} -> environment) (\s@UpdateRobotApplication' {} a -> s {environment = a} :: UpdateRobotApplication)

-- | The sources of the robot application.
updateRobotApplication_sources :: Lens.Lens' UpdateRobotApplication (Prelude.Maybe [SourceConfig])
updateRobotApplication_sources = Lens.lens (\UpdateRobotApplication' {sources} -> sources) (\s@UpdateRobotApplication' {} a -> s {sources = a} :: UpdateRobotApplication) Prelude.. Lens.mapping Lens.coerced

-- | The application information for the robot application.
updateRobotApplication_application :: Lens.Lens' UpdateRobotApplication Prelude.Text
updateRobotApplication_application = Lens.lens (\UpdateRobotApplication' {application} -> application) (\s@UpdateRobotApplication' {} a -> s {application = a} :: UpdateRobotApplication)

-- | The robot software suite (ROS distribution) used by the robot
-- application.
updateRobotApplication_robotSoftwareSuite :: Lens.Lens' UpdateRobotApplication RobotSoftwareSuite
updateRobotApplication_robotSoftwareSuite = Lens.lens (\UpdateRobotApplication' {robotSoftwareSuite} -> robotSoftwareSuite) (\s@UpdateRobotApplication' {} a -> s {robotSoftwareSuite = a} :: UpdateRobotApplication)

instance Core.AWSRequest UpdateRobotApplication where
  type
    AWSResponse UpdateRobotApplication =
      UpdateRobotApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRobotApplicationResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "environment")
            Prelude.<*> (x Data..?> "lastUpdatedAt")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "revisionId")
            Prelude.<*> (x Data..?> "robotSoftwareSuite")
            Prelude.<*> (x Data..?> "sources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRobotApplication where
  hashWithSalt _salt UpdateRobotApplication' {..} =
    _salt
      `Prelude.hashWithSalt` currentRevisionId
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` sources
      `Prelude.hashWithSalt` application
      `Prelude.hashWithSalt` robotSoftwareSuite

instance Prelude.NFData UpdateRobotApplication where
  rnf UpdateRobotApplication' {..} =
    Prelude.rnf currentRevisionId
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf application
      `Prelude.seq` Prelude.rnf robotSoftwareSuite

instance Data.ToHeaders UpdateRobotApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRobotApplication where
  toJSON UpdateRobotApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("currentRevisionId" Data..=)
              Prelude.<$> currentRevisionId,
            ("environment" Data..=) Prelude.<$> environment,
            ("sources" Data..=) Prelude.<$> sources,
            Prelude.Just ("application" Data..= application),
            Prelude.Just
              ("robotSoftwareSuite" Data..= robotSoftwareSuite)
          ]
      )

instance Data.ToPath UpdateRobotApplication where
  toPath = Prelude.const "/updateRobotApplication"

instance Data.ToQuery UpdateRobotApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRobotApplicationResponse' smart constructor.
data UpdateRobotApplicationResponse = UpdateRobotApplicationResponse'
  { -- | The Amazon Resource Name (ARN) of the updated robot application.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The object that contains the Docker image URI for your robot
    -- application.
    environment :: Prelude.Maybe Environment,
    -- | The time, in milliseconds since the epoch, when the robot application
    -- was last updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The name of the robot application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The revision id of the robot application.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The robot software suite (ROS distribution) used by the robot
    -- application.
    robotSoftwareSuite :: Prelude.Maybe RobotSoftwareSuite,
    -- | The sources of the robot application.
    sources :: Prelude.Maybe [Source],
    -- | The version of the robot application.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRobotApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateRobotApplicationResponse_arn' - The Amazon Resource Name (ARN) of the updated robot application.
--
-- 'environment', 'updateRobotApplicationResponse_environment' - The object that contains the Docker image URI for your robot
-- application.
--
-- 'lastUpdatedAt', 'updateRobotApplicationResponse_lastUpdatedAt' - The time, in milliseconds since the epoch, when the robot application
-- was last updated.
--
-- 'name', 'updateRobotApplicationResponse_name' - The name of the robot application.
--
-- 'revisionId', 'updateRobotApplicationResponse_revisionId' - The revision id of the robot application.
--
-- 'robotSoftwareSuite', 'updateRobotApplicationResponse_robotSoftwareSuite' - The robot software suite (ROS distribution) used by the robot
-- application.
--
-- 'sources', 'updateRobotApplicationResponse_sources' - The sources of the robot application.
--
-- 'version', 'updateRobotApplicationResponse_version' - The version of the robot application.
--
-- 'httpStatus', 'updateRobotApplicationResponse_httpStatus' - The response's http status code.
newUpdateRobotApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRobotApplicationResponse
newUpdateRobotApplicationResponse pHttpStatus_ =
  UpdateRobotApplicationResponse'
    { arn =
        Prelude.Nothing,
      environment = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      name = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      robotSoftwareSuite = Prelude.Nothing,
      sources = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the updated robot application.
updateRobotApplicationResponse_arn :: Lens.Lens' UpdateRobotApplicationResponse (Prelude.Maybe Prelude.Text)
updateRobotApplicationResponse_arn = Lens.lens (\UpdateRobotApplicationResponse' {arn} -> arn) (\s@UpdateRobotApplicationResponse' {} a -> s {arn = a} :: UpdateRobotApplicationResponse)

-- | The object that contains the Docker image URI for your robot
-- application.
updateRobotApplicationResponse_environment :: Lens.Lens' UpdateRobotApplicationResponse (Prelude.Maybe Environment)
updateRobotApplicationResponse_environment = Lens.lens (\UpdateRobotApplicationResponse' {environment} -> environment) (\s@UpdateRobotApplicationResponse' {} a -> s {environment = a} :: UpdateRobotApplicationResponse)

-- | The time, in milliseconds since the epoch, when the robot application
-- was last updated.
updateRobotApplicationResponse_lastUpdatedAt :: Lens.Lens' UpdateRobotApplicationResponse (Prelude.Maybe Prelude.UTCTime)
updateRobotApplicationResponse_lastUpdatedAt = Lens.lens (\UpdateRobotApplicationResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@UpdateRobotApplicationResponse' {} a -> s {lastUpdatedAt = a} :: UpdateRobotApplicationResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the robot application.
updateRobotApplicationResponse_name :: Lens.Lens' UpdateRobotApplicationResponse (Prelude.Maybe Prelude.Text)
updateRobotApplicationResponse_name = Lens.lens (\UpdateRobotApplicationResponse' {name} -> name) (\s@UpdateRobotApplicationResponse' {} a -> s {name = a} :: UpdateRobotApplicationResponse)

-- | The revision id of the robot application.
updateRobotApplicationResponse_revisionId :: Lens.Lens' UpdateRobotApplicationResponse (Prelude.Maybe Prelude.Text)
updateRobotApplicationResponse_revisionId = Lens.lens (\UpdateRobotApplicationResponse' {revisionId} -> revisionId) (\s@UpdateRobotApplicationResponse' {} a -> s {revisionId = a} :: UpdateRobotApplicationResponse)

-- | The robot software suite (ROS distribution) used by the robot
-- application.
updateRobotApplicationResponse_robotSoftwareSuite :: Lens.Lens' UpdateRobotApplicationResponse (Prelude.Maybe RobotSoftwareSuite)
updateRobotApplicationResponse_robotSoftwareSuite = Lens.lens (\UpdateRobotApplicationResponse' {robotSoftwareSuite} -> robotSoftwareSuite) (\s@UpdateRobotApplicationResponse' {} a -> s {robotSoftwareSuite = a} :: UpdateRobotApplicationResponse)

-- | The sources of the robot application.
updateRobotApplicationResponse_sources :: Lens.Lens' UpdateRobotApplicationResponse (Prelude.Maybe [Source])
updateRobotApplicationResponse_sources = Lens.lens (\UpdateRobotApplicationResponse' {sources} -> sources) (\s@UpdateRobotApplicationResponse' {} a -> s {sources = a} :: UpdateRobotApplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The version of the robot application.
updateRobotApplicationResponse_version :: Lens.Lens' UpdateRobotApplicationResponse (Prelude.Maybe Prelude.Text)
updateRobotApplicationResponse_version = Lens.lens (\UpdateRobotApplicationResponse' {version} -> version) (\s@UpdateRobotApplicationResponse' {} a -> s {version = a} :: UpdateRobotApplicationResponse)

-- | The response's http status code.
updateRobotApplicationResponse_httpStatus :: Lens.Lens' UpdateRobotApplicationResponse Prelude.Int
updateRobotApplicationResponse_httpStatus = Lens.lens (\UpdateRobotApplicationResponse' {httpStatus} -> httpStatus) (\s@UpdateRobotApplicationResponse' {} a -> s {httpStatus = a} :: UpdateRobotApplicationResponse)

instance
  Prelude.NFData
    UpdateRobotApplicationResponse
  where
  rnf UpdateRobotApplicationResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf robotSoftwareSuite
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
