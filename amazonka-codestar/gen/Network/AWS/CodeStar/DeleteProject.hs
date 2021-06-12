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
-- Module      : Network.AWS.CodeStar.DeleteProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a project, including project resources. Does not delete users
-- associated with the project, but does delete the IAM roles that allowed
-- access to the project.
module Network.AWS.CodeStar.DeleteProject
  ( -- * Creating a Request
    DeleteProject (..),
    newDeleteProject,

    -- * Request Lenses
    deleteProject_deleteStack,
    deleteProject_clientRequestToken,
    deleteProject_id,

    -- * Destructuring the Response
    DeleteProjectResponse (..),
    newDeleteProjectResponse,

    -- * Response Lenses
    deleteProjectResponse_stackId,
    deleteProjectResponse_projectArn,
    deleteProjectResponse_httpStatus,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteProject' smart constructor.
data DeleteProject = DeleteProject'
  { -- | Whether to send a delete request for the primary stack in AWS
    -- CloudFormation originally used to generate the project and its
    -- resources. This option will delete all AWS resources for the project
    -- (except for any buckets in Amazon S3) as well as deleting the project
    -- itself. Recommended for most use cases.
    deleteStack :: Core.Maybe Core.Bool,
    -- | A user- or system-generated token that identifies the entity that
    -- requested project deletion. This token can be used to repeat the
    -- request.
    clientRequestToken :: Core.Maybe Core.Text,
    -- | The ID of the project to be deleted in AWS CodeStar.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteStack', 'deleteProject_deleteStack' - Whether to send a delete request for the primary stack in AWS
-- CloudFormation originally used to generate the project and its
-- resources. This option will delete all AWS resources for the project
-- (except for any buckets in Amazon S3) as well as deleting the project
-- itself. Recommended for most use cases.
--
-- 'clientRequestToken', 'deleteProject_clientRequestToken' - A user- or system-generated token that identifies the entity that
-- requested project deletion. This token can be used to repeat the
-- request.
--
-- 'id', 'deleteProject_id' - The ID of the project to be deleted in AWS CodeStar.
newDeleteProject ::
  -- | 'id'
  Core.Text ->
  DeleteProject
newDeleteProject pId_ =
  DeleteProject'
    { deleteStack = Core.Nothing,
      clientRequestToken = Core.Nothing,
      id = pId_
    }

-- | Whether to send a delete request for the primary stack in AWS
-- CloudFormation originally used to generate the project and its
-- resources. This option will delete all AWS resources for the project
-- (except for any buckets in Amazon S3) as well as deleting the project
-- itself. Recommended for most use cases.
deleteProject_deleteStack :: Lens.Lens' DeleteProject (Core.Maybe Core.Bool)
deleteProject_deleteStack = Lens.lens (\DeleteProject' {deleteStack} -> deleteStack) (\s@DeleteProject' {} a -> s {deleteStack = a} :: DeleteProject)

-- | A user- or system-generated token that identifies the entity that
-- requested project deletion. This token can be used to repeat the
-- request.
deleteProject_clientRequestToken :: Lens.Lens' DeleteProject (Core.Maybe Core.Text)
deleteProject_clientRequestToken = Lens.lens (\DeleteProject' {clientRequestToken} -> clientRequestToken) (\s@DeleteProject' {} a -> s {clientRequestToken = a} :: DeleteProject)

-- | The ID of the project to be deleted in AWS CodeStar.
deleteProject_id :: Lens.Lens' DeleteProject Core.Text
deleteProject_id = Lens.lens (\DeleteProject' {id} -> id) (\s@DeleteProject' {} a -> s {id = a} :: DeleteProject)

instance Core.AWSRequest DeleteProject where
  type
    AWSResponse DeleteProject =
      DeleteProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteProjectResponse'
            Core.<$> (x Core..?> "stackId")
            Core.<*> (x Core..?> "projectArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteProject

instance Core.NFData DeleteProject

instance Core.ToHeaders DeleteProject where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeStar_20170419.DeleteProject" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteProject where
  toJSON DeleteProject' {..} =
    Core.object
      ( Core.catMaybes
          [ ("deleteStack" Core..=) Core.<$> deleteStack,
            ("clientRequestToken" Core..=)
              Core.<$> clientRequestToken,
            Core.Just ("id" Core..= id)
          ]
      )

instance Core.ToPath DeleteProject where
  toPath = Core.const "/"

instance Core.ToQuery DeleteProject where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteProjectResponse' smart constructor.
data DeleteProjectResponse = DeleteProjectResponse'
  { -- | The ID of the primary stack in AWS CloudFormation that will be deleted
    -- as part of deleting the project and its resources.
    stackId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the deleted project.
    projectArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackId', 'deleteProjectResponse_stackId' - The ID of the primary stack in AWS CloudFormation that will be deleted
-- as part of deleting the project and its resources.
--
-- 'projectArn', 'deleteProjectResponse_projectArn' - The Amazon Resource Name (ARN) of the deleted project.
--
-- 'httpStatus', 'deleteProjectResponse_httpStatus' - The response's http status code.
newDeleteProjectResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteProjectResponse
newDeleteProjectResponse pHttpStatus_ =
  DeleteProjectResponse'
    { stackId = Core.Nothing,
      projectArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the primary stack in AWS CloudFormation that will be deleted
-- as part of deleting the project and its resources.
deleteProjectResponse_stackId :: Lens.Lens' DeleteProjectResponse (Core.Maybe Core.Text)
deleteProjectResponse_stackId = Lens.lens (\DeleteProjectResponse' {stackId} -> stackId) (\s@DeleteProjectResponse' {} a -> s {stackId = a} :: DeleteProjectResponse)

-- | The Amazon Resource Name (ARN) of the deleted project.
deleteProjectResponse_projectArn :: Lens.Lens' DeleteProjectResponse (Core.Maybe Core.Text)
deleteProjectResponse_projectArn = Lens.lens (\DeleteProjectResponse' {projectArn} -> projectArn) (\s@DeleteProjectResponse' {} a -> s {projectArn = a} :: DeleteProjectResponse)

-- | The response's http status code.
deleteProjectResponse_httpStatus :: Lens.Lens' DeleteProjectResponse Core.Int
deleteProjectResponse_httpStatus = Lens.lens (\DeleteProjectResponse' {httpStatus} -> httpStatus) (\s@DeleteProjectResponse' {} a -> s {httpStatus = a} :: DeleteProjectResponse)

instance Core.NFData DeleteProjectResponse
