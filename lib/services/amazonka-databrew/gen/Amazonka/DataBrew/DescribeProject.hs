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
-- Module      : Amazonka.DataBrew.DescribeProject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the definition of a specific DataBrew project.
module Amazonka.DataBrew.DescribeProject
  ( -- * Creating a Request
    DescribeProject (..),
    newDescribeProject,

    -- * Request Lenses
    describeProject_name,

    -- * Destructuring the Response
    DescribeProjectResponse (..),
    newDescribeProjectResponse,

    -- * Response Lenses
    describeProjectResponse_createDate,
    describeProjectResponse_createdBy,
    describeProjectResponse_datasetName,
    describeProjectResponse_lastModifiedBy,
    describeProjectResponse_lastModifiedDate,
    describeProjectResponse_openDate,
    describeProjectResponse_openedBy,
    describeProjectResponse_recipeName,
    describeProjectResponse_resourceArn,
    describeProjectResponse_roleArn,
    describeProjectResponse_sample,
    describeProjectResponse_sessionStatus,
    describeProjectResponse_tags,
    describeProjectResponse_httpStatus,
    describeProjectResponse_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeProject' smart constructor.
data DescribeProject = DescribeProject'
  { -- | The name of the project to be described.
    name :: Prelude.Text
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
-- 'name', 'describeProject_name' - The name of the project to be described.
newDescribeProject ::
  -- | 'name'
  Prelude.Text ->
  DescribeProject
newDescribeProject pName_ =
  DescribeProject' {name = pName_}

-- | The name of the project to be described.
describeProject_name :: Lens.Lens' DescribeProject Prelude.Text
describeProject_name = Lens.lens (\DescribeProject' {name} -> name) (\s@DescribeProject' {} a -> s {name = a} :: DescribeProject)

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
            Prelude.<$> (x Data..?> "CreateDate")
            Prelude.<*> (x Data..?> "CreatedBy")
            Prelude.<*> (x Data..?> "DatasetName")
            Prelude.<*> (x Data..?> "LastModifiedBy")
            Prelude.<*> (x Data..?> "LastModifiedDate")
            Prelude.<*> (x Data..?> "OpenDate")
            Prelude.<*> (x Data..?> "OpenedBy")
            Prelude.<*> (x Data..?> "RecipeName")
            Prelude.<*> (x Data..?> "ResourceArn")
            Prelude.<*> (x Data..?> "RoleArn")
            Prelude.<*> (x Data..?> "Sample")
            Prelude.<*> (x Data..?> "SessionStatus")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Name")
      )

instance Prelude.Hashable DescribeProject where
  hashWithSalt _salt DescribeProject' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DescribeProject where
  rnf DescribeProject' {..} = Prelude.rnf name

instance Data.ToHeaders DescribeProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeProject where
  toPath DescribeProject' {..} =
    Prelude.mconcat ["/projects/", Data.toBS name]

instance Data.ToQuery DescribeProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProjectResponse' smart constructor.
data DescribeProjectResponse = DescribeProjectResponse'
  { -- | The date and time that the project was created.
    createDate :: Prelude.Maybe Data.POSIX,
    -- | The identifier (user name) of the user who created the project.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The dataset associated with the project.
    datasetName :: Prelude.Maybe Prelude.Text,
    -- | The identifier (user name) of the user who last modified the project.
    lastModifiedBy :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the project was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The date and time when the project was opened.
    openDate :: Prelude.Maybe Data.POSIX,
    -- | The identifier (user name) of the user that opened the project for use.
    openedBy :: Prelude.Maybe Prelude.Text,
    -- | The recipe associated with this job.
    recipeName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the project.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Identity and Access Management (IAM) role to be assumed
    -- when DataBrew runs the job.
    roleArn :: Prelude.Maybe Prelude.Text,
    sample :: Prelude.Maybe Sample,
    -- | Describes the current state of the session:
    --
    -- -   @PROVISIONING@ - allocating resources for the session.
    --
    -- -   @INITIALIZING@ - getting the session ready for first use.
    --
    -- -   @ASSIGNED@ - the session is ready for use.
    sessionStatus :: Prelude.Maybe SessionStatus,
    -- | Metadata tags associated with this project.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the project.
    name :: Prelude.Text
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
-- 'createDate', 'describeProjectResponse_createDate' - The date and time that the project was created.
--
-- 'createdBy', 'describeProjectResponse_createdBy' - The identifier (user name) of the user who created the project.
--
-- 'datasetName', 'describeProjectResponse_datasetName' - The dataset associated with the project.
--
-- 'lastModifiedBy', 'describeProjectResponse_lastModifiedBy' - The identifier (user name) of the user who last modified the project.
--
-- 'lastModifiedDate', 'describeProjectResponse_lastModifiedDate' - The date and time that the project was last modified.
--
-- 'openDate', 'describeProjectResponse_openDate' - The date and time when the project was opened.
--
-- 'openedBy', 'describeProjectResponse_openedBy' - The identifier (user name) of the user that opened the project for use.
--
-- 'recipeName', 'describeProjectResponse_recipeName' - The recipe associated with this job.
--
-- 'resourceArn', 'describeProjectResponse_resourceArn' - The Amazon Resource Name (ARN) of the project.
--
-- 'roleArn', 'describeProjectResponse_roleArn' - The ARN of the Identity and Access Management (IAM) role to be assumed
-- when DataBrew runs the job.
--
-- 'sample', 'describeProjectResponse_sample' - Undocumented member.
--
-- 'sessionStatus', 'describeProjectResponse_sessionStatus' - Describes the current state of the session:
--
-- -   @PROVISIONING@ - allocating resources for the session.
--
-- -   @INITIALIZING@ - getting the session ready for first use.
--
-- -   @ASSIGNED@ - the session is ready for use.
--
-- 'tags', 'describeProjectResponse_tags' - Metadata tags associated with this project.
--
-- 'httpStatus', 'describeProjectResponse_httpStatus' - The response's http status code.
--
-- 'name', 'describeProjectResponse_name' - The name of the project.
newDescribeProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  DescribeProjectResponse
newDescribeProjectResponse pHttpStatus_ pName_ =
  DescribeProjectResponse'
    { createDate =
        Prelude.Nothing,
      createdBy = Prelude.Nothing,
      datasetName = Prelude.Nothing,
      lastModifiedBy = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      openDate = Prelude.Nothing,
      openedBy = Prelude.Nothing,
      recipeName = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      sample = Prelude.Nothing,
      sessionStatus = Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      name = pName_
    }

-- | The date and time that the project was created.
describeProjectResponse_createDate :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Prelude.UTCTime)
describeProjectResponse_createDate = Lens.lens (\DescribeProjectResponse' {createDate} -> createDate) (\s@DescribeProjectResponse' {} a -> s {createDate = a} :: DescribeProjectResponse) Prelude.. Lens.mapping Data._Time

-- | The identifier (user name) of the user who created the project.
describeProjectResponse_createdBy :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Prelude.Text)
describeProjectResponse_createdBy = Lens.lens (\DescribeProjectResponse' {createdBy} -> createdBy) (\s@DescribeProjectResponse' {} a -> s {createdBy = a} :: DescribeProjectResponse)

-- | The dataset associated with the project.
describeProjectResponse_datasetName :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Prelude.Text)
describeProjectResponse_datasetName = Lens.lens (\DescribeProjectResponse' {datasetName} -> datasetName) (\s@DescribeProjectResponse' {} a -> s {datasetName = a} :: DescribeProjectResponse)

-- | The identifier (user name) of the user who last modified the project.
describeProjectResponse_lastModifiedBy :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Prelude.Text)
describeProjectResponse_lastModifiedBy = Lens.lens (\DescribeProjectResponse' {lastModifiedBy} -> lastModifiedBy) (\s@DescribeProjectResponse' {} a -> s {lastModifiedBy = a} :: DescribeProjectResponse)

-- | The date and time that the project was last modified.
describeProjectResponse_lastModifiedDate :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Prelude.UTCTime)
describeProjectResponse_lastModifiedDate = Lens.lens (\DescribeProjectResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeProjectResponse' {} a -> s {lastModifiedDate = a} :: DescribeProjectResponse) Prelude.. Lens.mapping Data._Time

-- | The date and time when the project was opened.
describeProjectResponse_openDate :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Prelude.UTCTime)
describeProjectResponse_openDate = Lens.lens (\DescribeProjectResponse' {openDate} -> openDate) (\s@DescribeProjectResponse' {} a -> s {openDate = a} :: DescribeProjectResponse) Prelude.. Lens.mapping Data._Time

-- | The identifier (user name) of the user that opened the project for use.
describeProjectResponse_openedBy :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Prelude.Text)
describeProjectResponse_openedBy = Lens.lens (\DescribeProjectResponse' {openedBy} -> openedBy) (\s@DescribeProjectResponse' {} a -> s {openedBy = a} :: DescribeProjectResponse)

-- | The recipe associated with this job.
describeProjectResponse_recipeName :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Prelude.Text)
describeProjectResponse_recipeName = Lens.lens (\DescribeProjectResponse' {recipeName} -> recipeName) (\s@DescribeProjectResponse' {} a -> s {recipeName = a} :: DescribeProjectResponse)

-- | The Amazon Resource Name (ARN) of the project.
describeProjectResponse_resourceArn :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Prelude.Text)
describeProjectResponse_resourceArn = Lens.lens (\DescribeProjectResponse' {resourceArn} -> resourceArn) (\s@DescribeProjectResponse' {} a -> s {resourceArn = a} :: DescribeProjectResponse)

-- | The ARN of the Identity and Access Management (IAM) role to be assumed
-- when DataBrew runs the job.
describeProjectResponse_roleArn :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Prelude.Text)
describeProjectResponse_roleArn = Lens.lens (\DescribeProjectResponse' {roleArn} -> roleArn) (\s@DescribeProjectResponse' {} a -> s {roleArn = a} :: DescribeProjectResponse)

-- | Undocumented member.
describeProjectResponse_sample :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe Sample)
describeProjectResponse_sample = Lens.lens (\DescribeProjectResponse' {sample} -> sample) (\s@DescribeProjectResponse' {} a -> s {sample = a} :: DescribeProjectResponse)

-- | Describes the current state of the session:
--
-- -   @PROVISIONING@ - allocating resources for the session.
--
-- -   @INITIALIZING@ - getting the session ready for first use.
--
-- -   @ASSIGNED@ - the session is ready for use.
describeProjectResponse_sessionStatus :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe SessionStatus)
describeProjectResponse_sessionStatus = Lens.lens (\DescribeProjectResponse' {sessionStatus} -> sessionStatus) (\s@DescribeProjectResponse' {} a -> s {sessionStatus = a} :: DescribeProjectResponse)

-- | Metadata tags associated with this project.
describeProjectResponse_tags :: Lens.Lens' DescribeProjectResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeProjectResponse_tags = Lens.lens (\DescribeProjectResponse' {tags} -> tags) (\s@DescribeProjectResponse' {} a -> s {tags = a} :: DescribeProjectResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeProjectResponse_httpStatus :: Lens.Lens' DescribeProjectResponse Prelude.Int
describeProjectResponse_httpStatus = Lens.lens (\DescribeProjectResponse' {httpStatus} -> httpStatus) (\s@DescribeProjectResponse' {} a -> s {httpStatus = a} :: DescribeProjectResponse)

-- | The name of the project.
describeProjectResponse_name :: Lens.Lens' DescribeProjectResponse Prelude.Text
describeProjectResponse_name = Lens.lens (\DescribeProjectResponse' {name} -> name) (\s@DescribeProjectResponse' {} a -> s {name = a} :: DescribeProjectResponse)

instance Prelude.NFData DescribeProjectResponse where
  rnf DescribeProjectResponse' {..} =
    Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf datasetName
      `Prelude.seq` Prelude.rnf lastModifiedBy
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf openDate
      `Prelude.seq` Prelude.rnf openedBy
      `Prelude.seq` Prelude.rnf recipeName
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf sample
      `Prelude.seq` Prelude.rnf sessionStatus
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
