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
-- Module      : Amazonka.CodeStar.DeleteProject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a project, including project resources. Does not delete users
-- associated with the project, but does delete the IAM roles that allowed
-- access to the project.
module Amazonka.CodeStar.DeleteProject
  ( -- * Creating a Request
    DeleteProject (..),
    newDeleteProject,

    -- * Request Lenses
    deleteProject_clientRequestToken,
    deleteProject_deleteStack,
    deleteProject_id,

    -- * Destructuring the Response
    DeleteProjectResponse (..),
    newDeleteProjectResponse,

    -- * Response Lenses
    deleteProjectResponse_projectArn,
    deleteProjectResponse_stackId,
    deleteProjectResponse_httpStatus,
  )
where

import Amazonka.CodeStar.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteProject' smart constructor.
data DeleteProject = DeleteProject'
  { -- | A user- or system-generated token that identifies the entity that
    -- requested project deletion. This token can be used to repeat the
    -- request.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Whether to send a delete request for the primary stack in AWS
    -- CloudFormation originally used to generate the project and its
    -- resources. This option will delete all AWS resources for the project
    -- (except for any buckets in Amazon S3) as well as deleting the project
    -- itself. Recommended for most use cases.
    deleteStack :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the project to be deleted in AWS CodeStar.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'deleteProject_clientRequestToken' - A user- or system-generated token that identifies the entity that
-- requested project deletion. This token can be used to repeat the
-- request.
--
-- 'deleteStack', 'deleteProject_deleteStack' - Whether to send a delete request for the primary stack in AWS
-- CloudFormation originally used to generate the project and its
-- resources. This option will delete all AWS resources for the project
-- (except for any buckets in Amazon S3) as well as deleting the project
-- itself. Recommended for most use cases.
--
-- 'id', 'deleteProject_id' - The ID of the project to be deleted in AWS CodeStar.
newDeleteProject ::
  -- | 'id'
  Prelude.Text ->
  DeleteProject
newDeleteProject pId_ =
  DeleteProject'
    { clientRequestToken =
        Prelude.Nothing,
      deleteStack = Prelude.Nothing,
      id = pId_
    }

-- | A user- or system-generated token that identifies the entity that
-- requested project deletion. This token can be used to repeat the
-- request.
deleteProject_clientRequestToken :: Lens.Lens' DeleteProject (Prelude.Maybe Prelude.Text)
deleteProject_clientRequestToken = Lens.lens (\DeleteProject' {clientRequestToken} -> clientRequestToken) (\s@DeleteProject' {} a -> s {clientRequestToken = a} :: DeleteProject)

-- | Whether to send a delete request for the primary stack in AWS
-- CloudFormation originally used to generate the project and its
-- resources. This option will delete all AWS resources for the project
-- (except for any buckets in Amazon S3) as well as deleting the project
-- itself. Recommended for most use cases.
deleteProject_deleteStack :: Lens.Lens' DeleteProject (Prelude.Maybe Prelude.Bool)
deleteProject_deleteStack = Lens.lens (\DeleteProject' {deleteStack} -> deleteStack) (\s@DeleteProject' {} a -> s {deleteStack = a} :: DeleteProject)

-- | The ID of the project to be deleted in AWS CodeStar.
deleteProject_id :: Lens.Lens' DeleteProject Prelude.Text
deleteProject_id = Lens.lens (\DeleteProject' {id} -> id) (\s@DeleteProject' {} a -> s {id = a} :: DeleteProject)

instance Core.AWSRequest DeleteProject where
  type
    AWSResponse DeleteProject =
      DeleteProjectResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteProjectResponse'
            Prelude.<$> (x Data..?> "projectArn")
            Prelude.<*> (x Data..?> "stackId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteProject where
  hashWithSalt _salt DeleteProject' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` deleteStack
      `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteProject where
  rnf DeleteProject' {..} =
    Prelude.rnf clientRequestToken `Prelude.seq`
      Prelude.rnf deleteStack `Prelude.seq`
        Prelude.rnf id

instance Data.ToHeaders DeleteProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeStar_20170419.DeleteProject" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteProject where
  toJSON DeleteProject' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("deleteStack" Data..=) Prelude.<$> deleteStack,
            Prelude.Just ("id" Data..= id)
          ]
      )

instance Data.ToPath DeleteProject where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProjectResponse' smart constructor.
data DeleteProjectResponse = DeleteProjectResponse'
  { -- | The Amazon Resource Name (ARN) of the deleted project.
    projectArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the primary stack in AWS CloudFormation that will be deleted
    -- as part of deleting the project and its resources.
    stackId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectArn', 'deleteProjectResponse_projectArn' - The Amazon Resource Name (ARN) of the deleted project.
--
-- 'stackId', 'deleteProjectResponse_stackId' - The ID of the primary stack in AWS CloudFormation that will be deleted
-- as part of deleting the project and its resources.
--
-- 'httpStatus', 'deleteProjectResponse_httpStatus' - The response's http status code.
newDeleteProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteProjectResponse
newDeleteProjectResponse pHttpStatus_ =
  DeleteProjectResponse'
    { projectArn =
        Prelude.Nothing,
      stackId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the deleted project.
deleteProjectResponse_projectArn :: Lens.Lens' DeleteProjectResponse (Prelude.Maybe Prelude.Text)
deleteProjectResponse_projectArn = Lens.lens (\DeleteProjectResponse' {projectArn} -> projectArn) (\s@DeleteProjectResponse' {} a -> s {projectArn = a} :: DeleteProjectResponse)

-- | The ID of the primary stack in AWS CloudFormation that will be deleted
-- as part of deleting the project and its resources.
deleteProjectResponse_stackId :: Lens.Lens' DeleteProjectResponse (Prelude.Maybe Prelude.Text)
deleteProjectResponse_stackId = Lens.lens (\DeleteProjectResponse' {stackId} -> stackId) (\s@DeleteProjectResponse' {} a -> s {stackId = a} :: DeleteProjectResponse)

-- | The response's http status code.
deleteProjectResponse_httpStatus :: Lens.Lens' DeleteProjectResponse Prelude.Int
deleteProjectResponse_httpStatus = Lens.lens (\DeleteProjectResponse' {httpStatus} -> httpStatus) (\s@DeleteProjectResponse' {} a -> s {httpStatus = a} :: DeleteProjectResponse)

instance Prelude.NFData DeleteProjectResponse where
  rnf DeleteProjectResponse' {..} =
    Prelude.rnf projectArn `Prelude.seq`
      Prelude.rnf stackId `Prelude.seq`
        Prelude.rnf httpStatus
