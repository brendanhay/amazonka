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
-- Module      : Network.AWS.CodeStar.DescribeProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a project and its resources.
module Network.AWS.CodeStar.DescribeProject
  ( -- * Creating a Request
    DescribeProject (..),
    newDescribeProject,

    -- * Request Lenses
    describeProject_id,

    -- * Destructuring the Response
    DescribeProjectResponse (..),
    newDescribeProjectResponse,

    -- * Response Lenses
    describeProjectResponse_status,
    describeProjectResponse_createdTimeStamp,
    describeProjectResponse_stackId,
    describeProjectResponse_id,
    describeProjectResponse_arn,
    describeProjectResponse_name,
    describeProjectResponse_description,
    describeProjectResponse_clientRequestToken,
    describeProjectResponse_projectTemplateId,
    describeProjectResponse_httpStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeProject' smart constructor.
data DescribeProject = DescribeProject'
  { -- | The ID of the project.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'describeProject_id' - The ID of the project.
newDescribeProject ::
  -- | 'id'
  Core.Text ->
  DescribeProject
newDescribeProject pId_ = DescribeProject' {id = pId_}

-- | The ID of the project.
describeProject_id :: Lens.Lens' DescribeProject Core.Text
describeProject_id = Lens.lens (\DescribeProject' {id} -> id) (\s@DescribeProject' {} a -> s {id = a} :: DescribeProject)

instance Core.AWSRequest DescribeProject where
  type
    AWSResponse DescribeProject =
      DescribeProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProjectResponse'
            Core.<$> (x Core..?> "status")
            Core.<*> (x Core..?> "createdTimeStamp")
            Core.<*> (x Core..?> "stackId")
            Core.<*> (x Core..?> "id")
            Core.<*> (x Core..?> "arn")
            Core.<*> (x Core..?> "name")
            Core.<*> (x Core..?> "description")
            Core.<*> (x Core..?> "clientRequestToken")
            Core.<*> (x Core..?> "projectTemplateId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeProject

instance Core.NFData DescribeProject

instance Core.ToHeaders DescribeProject where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeStar_20170419.DescribeProject" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeProject where
  toJSON DescribeProject' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("id" Core..= id)])

instance Core.ToPath DescribeProject where
  toPath = Core.const "/"

instance Core.ToQuery DescribeProject where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeProjectResponse' smart constructor.
data DescribeProjectResponse = DescribeProjectResponse'
  { -- | The project creation or deletion status.
    status :: Core.Maybe ProjectStatus,
    -- | The date and time the project was created, in timestamp format.
    createdTimeStamp :: Core.Maybe Core.POSIX,
    -- | The ID of the primary stack in AWS CloudFormation used to generate
    -- resources for the project.
    stackId :: Core.Maybe Core.Text,
    -- | The ID of the project.
    id :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) for the project.
    arn :: Core.Maybe Core.Text,
    -- | The display name for the project.
    name :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The description of the project, if any.
    description :: Core.Maybe (Core.Sensitive Core.Text),
    -- | A user- or system-generated token that identifies the entity that
    -- requested project creation.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The ID for the AWS CodeStar project template used to create the project.
    projectTemplateId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'describeProjectResponse_status' - The project creation or deletion status.
--
-- 'createdTimeStamp', 'describeProjectResponse_createdTimeStamp' - The date and time the project was created, in timestamp format.
--
-- 'stackId', 'describeProjectResponse_stackId' - The ID of the primary stack in AWS CloudFormation used to generate
-- resources for the project.
--
-- 'id', 'describeProjectResponse_id' - The ID of the project.
--
-- 'arn', 'describeProjectResponse_arn' - The Amazon Resource Name (ARN) for the project.
--
-- 'name', 'describeProjectResponse_name' - The display name for the project.
--
-- 'description', 'describeProjectResponse_description' - The description of the project, if any.
--
-- 'clientRequestToken', 'describeProjectResponse_clientRequestToken' - A user- or system-generated token that identifies the entity that
-- requested project creation.
--
-- 'projectTemplateId', 'describeProjectResponse_projectTemplateId' - The ID for the AWS CodeStar project template used to create the project.
--
-- 'httpStatus', 'describeProjectResponse_httpStatus' - The response's http status code.
newDescribeProjectResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeProjectResponse
newDescribeProjectResponse pHttpStatus_ =
  DescribeProjectResponse'
    { status = Core.Nothing,
      createdTimeStamp = Core.Nothing,
      stackId = Core.Nothing,
      id = Core.Nothing,
      arn = Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing,
      clientRequestToken = Core.Nothing,
      projectTemplateId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The project creation or deletion status.
describeProjectResponse_status :: Lens.Lens' DescribeProjectResponse (Core.Maybe ProjectStatus)
describeProjectResponse_status = Lens.lens (\DescribeProjectResponse' {status} -> status) (\s@DescribeProjectResponse' {} a -> s {status = a} :: DescribeProjectResponse)

-- | The date and time the project was created, in timestamp format.
describeProjectResponse_createdTimeStamp :: Lens.Lens' DescribeProjectResponse (Core.Maybe Core.UTCTime)
describeProjectResponse_createdTimeStamp = Lens.lens (\DescribeProjectResponse' {createdTimeStamp} -> createdTimeStamp) (\s@DescribeProjectResponse' {} a -> s {createdTimeStamp = a} :: DescribeProjectResponse) Core.. Lens.mapping Core._Time

-- | The ID of the primary stack in AWS CloudFormation used to generate
-- resources for the project.
describeProjectResponse_stackId :: Lens.Lens' DescribeProjectResponse (Core.Maybe Core.Text)
describeProjectResponse_stackId = Lens.lens (\DescribeProjectResponse' {stackId} -> stackId) (\s@DescribeProjectResponse' {} a -> s {stackId = a} :: DescribeProjectResponse)

-- | The ID of the project.
describeProjectResponse_id :: Lens.Lens' DescribeProjectResponse (Core.Maybe Core.Text)
describeProjectResponse_id = Lens.lens (\DescribeProjectResponse' {id} -> id) (\s@DescribeProjectResponse' {} a -> s {id = a} :: DescribeProjectResponse)

-- | The Amazon Resource Name (ARN) for the project.
describeProjectResponse_arn :: Lens.Lens' DescribeProjectResponse (Core.Maybe Core.Text)
describeProjectResponse_arn = Lens.lens (\DescribeProjectResponse' {arn} -> arn) (\s@DescribeProjectResponse' {} a -> s {arn = a} :: DescribeProjectResponse)

-- | The display name for the project.
describeProjectResponse_name :: Lens.Lens' DescribeProjectResponse (Core.Maybe Core.Text)
describeProjectResponse_name = Lens.lens (\DescribeProjectResponse' {name} -> name) (\s@DescribeProjectResponse' {} a -> s {name = a} :: DescribeProjectResponse) Core.. Lens.mapping Core._Sensitive

-- | The description of the project, if any.
describeProjectResponse_description :: Lens.Lens' DescribeProjectResponse (Core.Maybe Core.Text)
describeProjectResponse_description = Lens.lens (\DescribeProjectResponse' {description} -> description) (\s@DescribeProjectResponse' {} a -> s {description = a} :: DescribeProjectResponse) Core.. Lens.mapping Core._Sensitive

-- | A user- or system-generated token that identifies the entity that
-- requested project creation.
describeProjectResponse_clientRequestToken :: Lens.Lens' DescribeProjectResponse (Core.Maybe Core.Text)
describeProjectResponse_clientRequestToken = Lens.lens (\DescribeProjectResponse' {clientRequestToken} -> clientRequestToken) (\s@DescribeProjectResponse' {} a -> s {clientRequestToken = a} :: DescribeProjectResponse)

-- | The ID for the AWS CodeStar project template used to create the project.
describeProjectResponse_projectTemplateId :: Lens.Lens' DescribeProjectResponse (Core.Maybe Core.Text)
describeProjectResponse_projectTemplateId = Lens.lens (\DescribeProjectResponse' {projectTemplateId} -> projectTemplateId) (\s@DescribeProjectResponse' {} a -> s {projectTemplateId = a} :: DescribeProjectResponse)

-- | The response's http status code.
describeProjectResponse_httpStatus :: Lens.Lens' DescribeProjectResponse Core.Int
describeProjectResponse_httpStatus = Lens.lens (\DescribeProjectResponse' {httpStatus} -> httpStatus) (\s@DescribeProjectResponse' {} a -> s {httpStatus = a} :: DescribeProjectResponse)

instance Core.NFData DescribeProjectResponse
