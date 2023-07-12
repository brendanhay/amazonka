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
-- Module      : Amazonka.RobOMaker.CreateRobotApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a robot application.
module Amazonka.RobOMaker.CreateRobotApplication
  ( -- * Creating a Request
    CreateRobotApplication (..),
    newCreateRobotApplication,

    -- * Request Lenses
    createRobotApplication_environment,
    createRobotApplication_sources,
    createRobotApplication_tags,
    createRobotApplication_name,
    createRobotApplication_robotSoftwareSuite,

    -- * Destructuring the Response
    CreateRobotApplicationResponse (..),
    newCreateRobotApplicationResponse,

    -- * Response Lenses
    createRobotApplicationResponse_arn,
    createRobotApplicationResponse_environment,
    createRobotApplicationResponse_lastUpdatedAt,
    createRobotApplicationResponse_name,
    createRobotApplicationResponse_revisionId,
    createRobotApplicationResponse_robotSoftwareSuite,
    createRobotApplicationResponse_sources,
    createRobotApplicationResponse_tags,
    createRobotApplicationResponse_version,
    createRobotApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newCreateRobotApplication' smart constructor.
data CreateRobotApplication = CreateRobotApplication'
  { -- | The object that contains that URI of the Docker image that you use for
    -- your robot application.
    environment :: Prelude.Maybe Environment,
    -- | The sources of the robot application.
    sources :: Prelude.Maybe [SourceConfig],
    -- | A map that contains tag keys and tag values that are attached to the
    -- robot application.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the robot application.
    name :: Prelude.Text,
    -- | The robot software suite (ROS distribuition) used by the robot
    -- application.
    robotSoftwareSuite :: RobotSoftwareSuite
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRobotApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environment', 'createRobotApplication_environment' - The object that contains that URI of the Docker image that you use for
-- your robot application.
--
-- 'sources', 'createRobotApplication_sources' - The sources of the robot application.
--
-- 'tags', 'createRobotApplication_tags' - A map that contains tag keys and tag values that are attached to the
-- robot application.
--
-- 'name', 'createRobotApplication_name' - The name of the robot application.
--
-- 'robotSoftwareSuite', 'createRobotApplication_robotSoftwareSuite' - The robot software suite (ROS distribuition) used by the robot
-- application.
newCreateRobotApplication ::
  -- | 'name'
  Prelude.Text ->
  -- | 'robotSoftwareSuite'
  RobotSoftwareSuite ->
  CreateRobotApplication
newCreateRobotApplication pName_ pRobotSoftwareSuite_ =
  CreateRobotApplication'
    { environment =
        Prelude.Nothing,
      sources = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      robotSoftwareSuite = pRobotSoftwareSuite_
    }

-- | The object that contains that URI of the Docker image that you use for
-- your robot application.
createRobotApplication_environment :: Lens.Lens' CreateRobotApplication (Prelude.Maybe Environment)
createRobotApplication_environment = Lens.lens (\CreateRobotApplication' {environment} -> environment) (\s@CreateRobotApplication' {} a -> s {environment = a} :: CreateRobotApplication)

-- | The sources of the robot application.
createRobotApplication_sources :: Lens.Lens' CreateRobotApplication (Prelude.Maybe [SourceConfig])
createRobotApplication_sources = Lens.lens (\CreateRobotApplication' {sources} -> sources) (\s@CreateRobotApplication' {} a -> s {sources = a} :: CreateRobotApplication) Prelude.. Lens.mapping Lens.coerced

-- | A map that contains tag keys and tag values that are attached to the
-- robot application.
createRobotApplication_tags :: Lens.Lens' CreateRobotApplication (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRobotApplication_tags = Lens.lens (\CreateRobotApplication' {tags} -> tags) (\s@CreateRobotApplication' {} a -> s {tags = a} :: CreateRobotApplication) Prelude.. Lens.mapping Lens.coerced

-- | The name of the robot application.
createRobotApplication_name :: Lens.Lens' CreateRobotApplication Prelude.Text
createRobotApplication_name = Lens.lens (\CreateRobotApplication' {name} -> name) (\s@CreateRobotApplication' {} a -> s {name = a} :: CreateRobotApplication)

-- | The robot software suite (ROS distribuition) used by the robot
-- application.
createRobotApplication_robotSoftwareSuite :: Lens.Lens' CreateRobotApplication RobotSoftwareSuite
createRobotApplication_robotSoftwareSuite = Lens.lens (\CreateRobotApplication' {robotSoftwareSuite} -> robotSoftwareSuite) (\s@CreateRobotApplication' {} a -> s {robotSoftwareSuite = a} :: CreateRobotApplication)

instance Core.AWSRequest CreateRobotApplication where
  type
    AWSResponse CreateRobotApplication =
      CreateRobotApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRobotApplicationResponse'
            Prelude.<$> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "environment")
            Prelude.<*> (x Data..?> "lastUpdatedAt")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "revisionId")
            Prelude.<*> (x Data..?> "robotSoftwareSuite")
            Prelude.<*> (x Data..?> "sources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateRobotApplication where
  hashWithSalt _salt CreateRobotApplication' {..} =
    _salt
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` sources
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` robotSoftwareSuite

instance Prelude.NFData CreateRobotApplication where
  rnf CreateRobotApplication' {..} =
    Prelude.rnf environment
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf robotSoftwareSuite

instance Data.ToHeaders CreateRobotApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateRobotApplication where
  toJSON CreateRobotApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("environment" Data..=) Prelude.<$> environment,
            ("sources" Data..=) Prelude.<$> sources,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("robotSoftwareSuite" Data..= robotSoftwareSuite)
          ]
      )

instance Data.ToPath CreateRobotApplication where
  toPath = Prelude.const "/createRobotApplication"

instance Data.ToQuery CreateRobotApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRobotApplicationResponse' smart constructor.
data CreateRobotApplicationResponse = CreateRobotApplicationResponse'
  { -- | The Amazon Resource Name (ARN) of the robot application.
    arn :: Prelude.Maybe Prelude.Text,
    -- | An object that contains the Docker image URI used to a create your robot
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
    -- | The list of all tags added to the robot application.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The version of the robot application.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRobotApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createRobotApplicationResponse_arn' - The Amazon Resource Name (ARN) of the robot application.
--
-- 'environment', 'createRobotApplicationResponse_environment' - An object that contains the Docker image URI used to a create your robot
-- application.
--
-- 'lastUpdatedAt', 'createRobotApplicationResponse_lastUpdatedAt' - The time, in milliseconds since the epoch, when the robot application
-- was last updated.
--
-- 'name', 'createRobotApplicationResponse_name' - The name of the robot application.
--
-- 'revisionId', 'createRobotApplicationResponse_revisionId' - The revision id of the robot application.
--
-- 'robotSoftwareSuite', 'createRobotApplicationResponse_robotSoftwareSuite' - The robot software suite (ROS distribution) used by the robot
-- application.
--
-- 'sources', 'createRobotApplicationResponse_sources' - The sources of the robot application.
--
-- 'tags', 'createRobotApplicationResponse_tags' - The list of all tags added to the robot application.
--
-- 'version', 'createRobotApplicationResponse_version' - The version of the robot application.
--
-- 'httpStatus', 'createRobotApplicationResponse_httpStatus' - The response's http status code.
newCreateRobotApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateRobotApplicationResponse
newCreateRobotApplicationResponse pHttpStatus_ =
  CreateRobotApplicationResponse'
    { arn =
        Prelude.Nothing,
      environment = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      name = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      robotSoftwareSuite = Prelude.Nothing,
      sources = Prelude.Nothing,
      tags = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the robot application.
createRobotApplicationResponse_arn :: Lens.Lens' CreateRobotApplicationResponse (Prelude.Maybe Prelude.Text)
createRobotApplicationResponse_arn = Lens.lens (\CreateRobotApplicationResponse' {arn} -> arn) (\s@CreateRobotApplicationResponse' {} a -> s {arn = a} :: CreateRobotApplicationResponse)

-- | An object that contains the Docker image URI used to a create your robot
-- application.
createRobotApplicationResponse_environment :: Lens.Lens' CreateRobotApplicationResponse (Prelude.Maybe Environment)
createRobotApplicationResponse_environment = Lens.lens (\CreateRobotApplicationResponse' {environment} -> environment) (\s@CreateRobotApplicationResponse' {} a -> s {environment = a} :: CreateRobotApplicationResponse)

-- | The time, in milliseconds since the epoch, when the robot application
-- was last updated.
createRobotApplicationResponse_lastUpdatedAt :: Lens.Lens' CreateRobotApplicationResponse (Prelude.Maybe Prelude.UTCTime)
createRobotApplicationResponse_lastUpdatedAt = Lens.lens (\CreateRobotApplicationResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@CreateRobotApplicationResponse' {} a -> s {lastUpdatedAt = a} :: CreateRobotApplicationResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the robot application.
createRobotApplicationResponse_name :: Lens.Lens' CreateRobotApplicationResponse (Prelude.Maybe Prelude.Text)
createRobotApplicationResponse_name = Lens.lens (\CreateRobotApplicationResponse' {name} -> name) (\s@CreateRobotApplicationResponse' {} a -> s {name = a} :: CreateRobotApplicationResponse)

-- | The revision id of the robot application.
createRobotApplicationResponse_revisionId :: Lens.Lens' CreateRobotApplicationResponse (Prelude.Maybe Prelude.Text)
createRobotApplicationResponse_revisionId = Lens.lens (\CreateRobotApplicationResponse' {revisionId} -> revisionId) (\s@CreateRobotApplicationResponse' {} a -> s {revisionId = a} :: CreateRobotApplicationResponse)

-- | The robot software suite (ROS distribution) used by the robot
-- application.
createRobotApplicationResponse_robotSoftwareSuite :: Lens.Lens' CreateRobotApplicationResponse (Prelude.Maybe RobotSoftwareSuite)
createRobotApplicationResponse_robotSoftwareSuite = Lens.lens (\CreateRobotApplicationResponse' {robotSoftwareSuite} -> robotSoftwareSuite) (\s@CreateRobotApplicationResponse' {} a -> s {robotSoftwareSuite = a} :: CreateRobotApplicationResponse)

-- | The sources of the robot application.
createRobotApplicationResponse_sources :: Lens.Lens' CreateRobotApplicationResponse (Prelude.Maybe [Source])
createRobotApplicationResponse_sources = Lens.lens (\CreateRobotApplicationResponse' {sources} -> sources) (\s@CreateRobotApplicationResponse' {} a -> s {sources = a} :: CreateRobotApplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The list of all tags added to the robot application.
createRobotApplicationResponse_tags :: Lens.Lens' CreateRobotApplicationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRobotApplicationResponse_tags = Lens.lens (\CreateRobotApplicationResponse' {tags} -> tags) (\s@CreateRobotApplicationResponse' {} a -> s {tags = a} :: CreateRobotApplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The version of the robot application.
createRobotApplicationResponse_version :: Lens.Lens' CreateRobotApplicationResponse (Prelude.Maybe Prelude.Text)
createRobotApplicationResponse_version = Lens.lens (\CreateRobotApplicationResponse' {version} -> version) (\s@CreateRobotApplicationResponse' {} a -> s {version = a} :: CreateRobotApplicationResponse)

-- | The response's http status code.
createRobotApplicationResponse_httpStatus :: Lens.Lens' CreateRobotApplicationResponse Prelude.Int
createRobotApplicationResponse_httpStatus = Lens.lens (\CreateRobotApplicationResponse' {httpStatus} -> httpStatus) (\s@CreateRobotApplicationResponse' {} a -> s {httpStatus = a} :: CreateRobotApplicationResponse)

instance
  Prelude.NFData
    CreateRobotApplicationResponse
  where
  rnf CreateRobotApplicationResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf robotSoftwareSuite
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
