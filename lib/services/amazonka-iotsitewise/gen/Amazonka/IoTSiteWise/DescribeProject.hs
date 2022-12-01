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
-- Module      : Amazonka.IoTSiteWise.DescribeProject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a project.
module Amazonka.IoTSiteWise.DescribeProject
  ( -- * Creating a Request
    DescribeProject (..),
    newDescribeProject,

    -- * Request Lenses
    describeProject_projectId,

    -- * Destructuring the Response
    DescribeProjectResponse (..),
    newDescribeProjectResponse,

    -- * Response Lenses
    describeProjectResponse_projectDescription,
    describeProjectResponse_httpStatus,
    describeProjectResponse_projectId,
    describeProjectResponse_projectArn,
    describeProjectResponse_projectName,
    describeProjectResponse_portalId,
    describeProjectResponse_projectCreationDate,
    describeProjectResponse_projectLastUpdateDate,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeProject' smart constructor.
data DescribeProject = DescribeProject'
  { -- | The ID of the project.
    projectId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectId', 'describeProject_projectId' - The ID of the project.
newDescribeProject ::
  -- | 'projectId'
  Prelude.Text ->
  DescribeProject
newDescribeProject pProjectId_ =
  DescribeProject' {projectId = pProjectId_}

-- | The ID of the project.
describeProject_projectId :: Lens.Lens' DescribeProject Prelude.Text
describeProject_projectId = Lens.lens (\DescribeProject' {projectId} -> projectId) (\s@DescribeProject' {} a -> s {projectId = a} :: DescribeProject)

instance Core.AWSRequest DescribeProject where
  type
    AWSResponse DescribeProject =
      DescribeProjectResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProjectResponse'
            Prelude.<$> (x Core..?> "projectDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "projectId")
            Prelude.<*> (x Core..:> "projectArn")
            Prelude.<*> (x Core..:> "projectName")
            Prelude.<*> (x Core..:> "portalId")
            Prelude.<*> (x Core..:> "projectCreationDate")
            Prelude.<*> (x Core..:> "projectLastUpdateDate")
      )

instance Prelude.Hashable DescribeProject where
  hashWithSalt _salt DescribeProject' {..} =
    _salt `Prelude.hashWithSalt` projectId

instance Prelude.NFData DescribeProject where
  rnf DescribeProject' {..} = Prelude.rnf projectId

instance Core.ToHeaders DescribeProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeProject where
  toPath DescribeProject' {..} =
    Prelude.mconcat ["/projects/", Core.toBS projectId]

instance Core.ToQuery DescribeProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProjectResponse' smart constructor.
data DescribeProjectResponse = DescribeProjectResponse'
  { -- | The project\'s description.
    projectDescription :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the project.
    projectId :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the project, which has the following format.
    --
    -- @arn:${Partition}:iotsitewise:${Region}:${Account}:project\/${ProjectId}@
    projectArn :: Prelude.Text,
    -- | The name of the project.
    projectName :: Prelude.Text,
    -- | The ID of the portal that the project is in.
    portalId :: Prelude.Text,
    -- | The date the project was created, in Unix epoch time.
    projectCreationDate :: Core.POSIX,
    -- | The date the project was last updated, in Unix epoch time.
    projectLastUpdateDate :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectDescription', 'describeProjectResponse_projectDescription' - The project\'s description.
--
-- 'httpStatus', 'describeProjectResponse_httpStatus' - The response's http status code.
--
-- 'projectId', 'describeProjectResponse_projectId' - The ID of the project.
--
-- 'projectArn', 'describeProjectResponse_projectArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the project, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:project\/${ProjectId}@
--
-- 'projectName', 'describeProjectResponse_projectName' - The name of the project.
--
-- 'portalId', 'describeProjectResponse_portalId' - The ID of the portal that the project is in.
--
-- 'projectCreationDate', 'describeProjectResponse_projectCreationDate' - The date the project was created, in Unix epoch time.
--
-- 'projectLastUpdateDate', 'describeProjectResponse_projectLastUpdateDate' - The date the project was last updated, in Unix epoch time.
newDescribeProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'projectId'
  Prelude.Text ->
  -- | 'projectArn'
  Prelude.Text ->
  -- | 'projectName'
  Prelude.Text ->
  -- | 'portalId'
  Prelude.Text ->
  -- | 'projectCreationDate'
  Prelude.UTCTime ->
  -- | 'projectLastUpdateDate'
  Prelude.UTCTime ->
  DescribeProjectResponse
newDescribeProjectResponse
  pHttpStatus_
  pProjectId_
  pProjectArn_
  pProjectName_
  pPortalId_
  pProjectCreationDate_
  pProjectLastUpdateDate_ =
    DescribeProjectResponse'
      { projectDescription =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        projectId = pProjectId_,
        projectArn = pProjectArn_,
        projectName = pProjectName_,
        portalId = pPortalId_,
        projectCreationDate =
          Core._Time Lens.# pProjectCreationDate_,
        projectLastUpdateDate =
          Core._Time Lens.# pProjectLastUpdateDate_
      }

-- | The project\'s description.
describeProjectResponse_projectDescription :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Prelude.Text)
describeProjectResponse_projectDescription = Lens.lens (\DescribeProjectResponse' {projectDescription} -> projectDescription) (\s@DescribeProjectResponse' {} a -> s {projectDescription = a} :: DescribeProjectResponse)

-- | The response's http status code.
describeProjectResponse_httpStatus :: Lens.Lens' DescribeProjectResponse Prelude.Int
describeProjectResponse_httpStatus = Lens.lens (\DescribeProjectResponse' {httpStatus} -> httpStatus) (\s@DescribeProjectResponse' {} a -> s {httpStatus = a} :: DescribeProjectResponse)

-- | The ID of the project.
describeProjectResponse_projectId :: Lens.Lens' DescribeProjectResponse Prelude.Text
describeProjectResponse_projectId = Lens.lens (\DescribeProjectResponse' {projectId} -> projectId) (\s@DescribeProjectResponse' {} a -> s {projectId = a} :: DescribeProjectResponse)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the project, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:project\/${ProjectId}@
describeProjectResponse_projectArn :: Lens.Lens' DescribeProjectResponse Prelude.Text
describeProjectResponse_projectArn = Lens.lens (\DescribeProjectResponse' {projectArn} -> projectArn) (\s@DescribeProjectResponse' {} a -> s {projectArn = a} :: DescribeProjectResponse)

-- | The name of the project.
describeProjectResponse_projectName :: Lens.Lens' DescribeProjectResponse Prelude.Text
describeProjectResponse_projectName = Lens.lens (\DescribeProjectResponse' {projectName} -> projectName) (\s@DescribeProjectResponse' {} a -> s {projectName = a} :: DescribeProjectResponse)

-- | The ID of the portal that the project is in.
describeProjectResponse_portalId :: Lens.Lens' DescribeProjectResponse Prelude.Text
describeProjectResponse_portalId = Lens.lens (\DescribeProjectResponse' {portalId} -> portalId) (\s@DescribeProjectResponse' {} a -> s {portalId = a} :: DescribeProjectResponse)

-- | The date the project was created, in Unix epoch time.
describeProjectResponse_projectCreationDate :: Lens.Lens' DescribeProjectResponse Prelude.UTCTime
describeProjectResponse_projectCreationDate = Lens.lens (\DescribeProjectResponse' {projectCreationDate} -> projectCreationDate) (\s@DescribeProjectResponse' {} a -> s {projectCreationDate = a} :: DescribeProjectResponse) Prelude.. Core._Time

-- | The date the project was last updated, in Unix epoch time.
describeProjectResponse_projectLastUpdateDate :: Lens.Lens' DescribeProjectResponse Prelude.UTCTime
describeProjectResponse_projectLastUpdateDate = Lens.lens (\DescribeProjectResponse' {projectLastUpdateDate} -> projectLastUpdateDate) (\s@DescribeProjectResponse' {} a -> s {projectLastUpdateDate = a} :: DescribeProjectResponse) Prelude.. Core._Time

instance Prelude.NFData DescribeProjectResponse where
  rnf DescribeProjectResponse' {..} =
    Prelude.rnf projectDescription
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf projectId
      `Prelude.seq` Prelude.rnf projectArn
      `Prelude.seq` Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf portalId
      `Prelude.seq` Prelude.rnf projectCreationDate
      `Prelude.seq` Prelude.rnf projectLastUpdateDate
