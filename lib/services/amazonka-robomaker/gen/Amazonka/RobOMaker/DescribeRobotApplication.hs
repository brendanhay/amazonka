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
-- Module      : Amazonka.RobOMaker.DescribeRobotApplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a robot application.
module Amazonka.RobOMaker.DescribeRobotApplication
  ( -- * Creating a Request
    DescribeRobotApplication (..),
    newDescribeRobotApplication,

    -- * Request Lenses
    describeRobotApplication_applicationVersion,
    describeRobotApplication_application,

    -- * Destructuring the Response
    DescribeRobotApplicationResponse (..),
    newDescribeRobotApplicationResponse,

    -- * Response Lenses
    describeRobotApplicationResponse_tags,
    describeRobotApplicationResponse_name,
    describeRobotApplicationResponse_sources,
    describeRobotApplicationResponse_environment,
    describeRobotApplicationResponse_lastUpdatedAt,
    describeRobotApplicationResponse_arn,
    describeRobotApplicationResponse_robotSoftwareSuite,
    describeRobotApplicationResponse_revisionId,
    describeRobotApplicationResponse_imageDigest,
    describeRobotApplicationResponse_version,
    describeRobotApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newDescribeRobotApplication' smart constructor.
data DescribeRobotApplication = DescribeRobotApplication'
  { -- | The version of the robot application to describe.
    applicationVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the robot application.
    application :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRobotApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationVersion', 'describeRobotApplication_applicationVersion' - The version of the robot application to describe.
--
-- 'application', 'describeRobotApplication_application' - The Amazon Resource Name (ARN) of the robot application.
newDescribeRobotApplication ::
  -- | 'application'
  Prelude.Text ->
  DescribeRobotApplication
newDescribeRobotApplication pApplication_ =
  DescribeRobotApplication'
    { applicationVersion =
        Prelude.Nothing,
      application = pApplication_
    }

-- | The version of the robot application to describe.
describeRobotApplication_applicationVersion :: Lens.Lens' DescribeRobotApplication (Prelude.Maybe Prelude.Text)
describeRobotApplication_applicationVersion = Lens.lens (\DescribeRobotApplication' {applicationVersion} -> applicationVersion) (\s@DescribeRobotApplication' {} a -> s {applicationVersion = a} :: DescribeRobotApplication)

-- | The Amazon Resource Name (ARN) of the robot application.
describeRobotApplication_application :: Lens.Lens' DescribeRobotApplication Prelude.Text
describeRobotApplication_application = Lens.lens (\DescribeRobotApplication' {application} -> application) (\s@DescribeRobotApplication' {} a -> s {application = a} :: DescribeRobotApplication)

instance Core.AWSRequest DescribeRobotApplication where
  type
    AWSResponse DescribeRobotApplication =
      DescribeRobotApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeRobotApplicationResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "sources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "environment")
            Prelude.<*> (x Data..?> "lastUpdatedAt")
            Prelude.<*> (x Data..?> "arn")
            Prelude.<*> (x Data..?> "robotSoftwareSuite")
            Prelude.<*> (x Data..?> "revisionId")
            Prelude.<*> (x Data..?> "imageDigest")
            Prelude.<*> (x Data..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeRobotApplication where
  hashWithSalt _salt DescribeRobotApplication' {..} =
    _salt `Prelude.hashWithSalt` applicationVersion
      `Prelude.hashWithSalt` application

instance Prelude.NFData DescribeRobotApplication where
  rnf DescribeRobotApplication' {..} =
    Prelude.rnf applicationVersion
      `Prelude.seq` Prelude.rnf application

instance Data.ToHeaders DescribeRobotApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeRobotApplication where
  toJSON DescribeRobotApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("applicationVersion" Data..=)
              Prelude.<$> applicationVersion,
            Prelude.Just ("application" Data..= application)
          ]
      )

instance Data.ToPath DescribeRobotApplication where
  toPath = Prelude.const "/describeRobotApplication"

instance Data.ToQuery DescribeRobotApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeRobotApplicationResponse' smart constructor.
data DescribeRobotApplicationResponse = DescribeRobotApplicationResponse'
  { -- | The list of all tags added to the specified robot application.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the robot application.
    name :: Prelude.Maybe Prelude.Text,
    -- | The sources of the robot application.
    sources :: Prelude.Maybe [Source],
    -- | The object that contains the Docker image URI used to create the robot
    -- application.
    environment :: Prelude.Maybe Environment,
    -- | The time, in milliseconds since the epoch, when the robot application
    -- was last updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of the robot application.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The robot software suite (ROS distribution) used by the robot
    -- application.
    robotSoftwareSuite :: Prelude.Maybe RobotSoftwareSuite,
    -- | The revision id of the robot application.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | A SHA256 identifier for the Docker image that you use for your robot
    -- application.
    imageDigest :: Prelude.Maybe Prelude.Text,
    -- | The version of the robot application.
    version :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeRobotApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'describeRobotApplicationResponse_tags' - The list of all tags added to the specified robot application.
--
-- 'name', 'describeRobotApplicationResponse_name' - The name of the robot application.
--
-- 'sources', 'describeRobotApplicationResponse_sources' - The sources of the robot application.
--
-- 'environment', 'describeRobotApplicationResponse_environment' - The object that contains the Docker image URI used to create the robot
-- application.
--
-- 'lastUpdatedAt', 'describeRobotApplicationResponse_lastUpdatedAt' - The time, in milliseconds since the epoch, when the robot application
-- was last updated.
--
-- 'arn', 'describeRobotApplicationResponse_arn' - The Amazon Resource Name (ARN) of the robot application.
--
-- 'robotSoftwareSuite', 'describeRobotApplicationResponse_robotSoftwareSuite' - The robot software suite (ROS distribution) used by the robot
-- application.
--
-- 'revisionId', 'describeRobotApplicationResponse_revisionId' - The revision id of the robot application.
--
-- 'imageDigest', 'describeRobotApplicationResponse_imageDigest' - A SHA256 identifier for the Docker image that you use for your robot
-- application.
--
-- 'version', 'describeRobotApplicationResponse_version' - The version of the robot application.
--
-- 'httpStatus', 'describeRobotApplicationResponse_httpStatus' - The response's http status code.
newDescribeRobotApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeRobotApplicationResponse
newDescribeRobotApplicationResponse pHttpStatus_ =
  DescribeRobotApplicationResponse'
    { tags =
        Prelude.Nothing,
      name = Prelude.Nothing,
      sources = Prelude.Nothing,
      environment = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      robotSoftwareSuite = Prelude.Nothing,
      revisionId = Prelude.Nothing,
      imageDigest = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of all tags added to the specified robot application.
describeRobotApplicationResponse_tags :: Lens.Lens' DescribeRobotApplicationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeRobotApplicationResponse_tags = Lens.lens (\DescribeRobotApplicationResponse' {tags} -> tags) (\s@DescribeRobotApplicationResponse' {} a -> s {tags = a} :: DescribeRobotApplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the robot application.
describeRobotApplicationResponse_name :: Lens.Lens' DescribeRobotApplicationResponse (Prelude.Maybe Prelude.Text)
describeRobotApplicationResponse_name = Lens.lens (\DescribeRobotApplicationResponse' {name} -> name) (\s@DescribeRobotApplicationResponse' {} a -> s {name = a} :: DescribeRobotApplicationResponse)

-- | The sources of the robot application.
describeRobotApplicationResponse_sources :: Lens.Lens' DescribeRobotApplicationResponse (Prelude.Maybe [Source])
describeRobotApplicationResponse_sources = Lens.lens (\DescribeRobotApplicationResponse' {sources} -> sources) (\s@DescribeRobotApplicationResponse' {} a -> s {sources = a} :: DescribeRobotApplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The object that contains the Docker image URI used to create the robot
-- application.
describeRobotApplicationResponse_environment :: Lens.Lens' DescribeRobotApplicationResponse (Prelude.Maybe Environment)
describeRobotApplicationResponse_environment = Lens.lens (\DescribeRobotApplicationResponse' {environment} -> environment) (\s@DescribeRobotApplicationResponse' {} a -> s {environment = a} :: DescribeRobotApplicationResponse)

-- | The time, in milliseconds since the epoch, when the robot application
-- was last updated.
describeRobotApplicationResponse_lastUpdatedAt :: Lens.Lens' DescribeRobotApplicationResponse (Prelude.Maybe Prelude.UTCTime)
describeRobotApplicationResponse_lastUpdatedAt = Lens.lens (\DescribeRobotApplicationResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@DescribeRobotApplicationResponse' {} a -> s {lastUpdatedAt = a} :: DescribeRobotApplicationResponse) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of the robot application.
describeRobotApplicationResponse_arn :: Lens.Lens' DescribeRobotApplicationResponse (Prelude.Maybe Prelude.Text)
describeRobotApplicationResponse_arn = Lens.lens (\DescribeRobotApplicationResponse' {arn} -> arn) (\s@DescribeRobotApplicationResponse' {} a -> s {arn = a} :: DescribeRobotApplicationResponse)

-- | The robot software suite (ROS distribution) used by the robot
-- application.
describeRobotApplicationResponse_robotSoftwareSuite :: Lens.Lens' DescribeRobotApplicationResponse (Prelude.Maybe RobotSoftwareSuite)
describeRobotApplicationResponse_robotSoftwareSuite = Lens.lens (\DescribeRobotApplicationResponse' {robotSoftwareSuite} -> robotSoftwareSuite) (\s@DescribeRobotApplicationResponse' {} a -> s {robotSoftwareSuite = a} :: DescribeRobotApplicationResponse)

-- | The revision id of the robot application.
describeRobotApplicationResponse_revisionId :: Lens.Lens' DescribeRobotApplicationResponse (Prelude.Maybe Prelude.Text)
describeRobotApplicationResponse_revisionId = Lens.lens (\DescribeRobotApplicationResponse' {revisionId} -> revisionId) (\s@DescribeRobotApplicationResponse' {} a -> s {revisionId = a} :: DescribeRobotApplicationResponse)

-- | A SHA256 identifier for the Docker image that you use for your robot
-- application.
describeRobotApplicationResponse_imageDigest :: Lens.Lens' DescribeRobotApplicationResponse (Prelude.Maybe Prelude.Text)
describeRobotApplicationResponse_imageDigest = Lens.lens (\DescribeRobotApplicationResponse' {imageDigest} -> imageDigest) (\s@DescribeRobotApplicationResponse' {} a -> s {imageDigest = a} :: DescribeRobotApplicationResponse)

-- | The version of the robot application.
describeRobotApplicationResponse_version :: Lens.Lens' DescribeRobotApplicationResponse (Prelude.Maybe Prelude.Text)
describeRobotApplicationResponse_version = Lens.lens (\DescribeRobotApplicationResponse' {version} -> version) (\s@DescribeRobotApplicationResponse' {} a -> s {version = a} :: DescribeRobotApplicationResponse)

-- | The response's http status code.
describeRobotApplicationResponse_httpStatus :: Lens.Lens' DescribeRobotApplicationResponse Prelude.Int
describeRobotApplicationResponse_httpStatus = Lens.lens (\DescribeRobotApplicationResponse' {httpStatus} -> httpStatus) (\s@DescribeRobotApplicationResponse' {} a -> s {httpStatus = a} :: DescribeRobotApplicationResponse)

instance
  Prelude.NFData
    DescribeRobotApplicationResponse
  where
  rnf DescribeRobotApplicationResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf robotSoftwareSuite
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf imageDigest
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
