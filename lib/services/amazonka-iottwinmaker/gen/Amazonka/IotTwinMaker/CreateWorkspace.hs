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
-- Module      : Amazonka.IotTwinMaker.CreateWorkspace
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a workplace.
module Amazonka.IotTwinMaker.CreateWorkspace
  ( -- * Creating a Request
    CreateWorkspace (..),
    newCreateWorkspace,

    -- * Request Lenses
    createWorkspace_description,
    createWorkspace_tags,
    createWorkspace_workspaceId,
    createWorkspace_s3Location,
    createWorkspace_role,

    -- * Destructuring the Response
    CreateWorkspaceResponse (..),
    newCreateWorkspaceResponse,

    -- * Response Lenses
    createWorkspaceResponse_httpStatus,
    createWorkspaceResponse_arn,
    createWorkspaceResponse_creationDateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateWorkspace' smart constructor.
data CreateWorkspace = CreateWorkspace'
  { -- | The description of the workspace.
    description :: Prelude.Maybe Prelude.Text,
    -- | Metadata that you can use to manage the workspace
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the workspace.
    workspaceId :: Prelude.Text,
    -- | The ARN of the S3 bucket where resources associated with the workspace
    -- are stored.
    s3Location :: Prelude.Text,
    -- | The ARN of the execution role associated with the workspace.
    role' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkspace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createWorkspace_description' - The description of the workspace.
--
-- 'tags', 'createWorkspace_tags' - Metadata that you can use to manage the workspace
--
-- 'workspaceId', 'createWorkspace_workspaceId' - The ID of the workspace.
--
-- 's3Location', 'createWorkspace_s3Location' - The ARN of the S3 bucket where resources associated with the workspace
-- are stored.
--
-- 'role'', 'createWorkspace_role' - The ARN of the execution role associated with the workspace.
newCreateWorkspace ::
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 's3Location'
  Prelude.Text ->
  -- | 'role''
  Prelude.Text ->
  CreateWorkspace
newCreateWorkspace pWorkspaceId_ pS3Location_ pRole_ =
  CreateWorkspace'
    { description = Prelude.Nothing,
      tags = Prelude.Nothing,
      workspaceId = pWorkspaceId_,
      s3Location = pS3Location_,
      role' = pRole_
    }

-- | The description of the workspace.
createWorkspace_description :: Lens.Lens' CreateWorkspace (Prelude.Maybe Prelude.Text)
createWorkspace_description = Lens.lens (\CreateWorkspace' {description} -> description) (\s@CreateWorkspace' {} a -> s {description = a} :: CreateWorkspace)

-- | Metadata that you can use to manage the workspace
createWorkspace_tags :: Lens.Lens' CreateWorkspace (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createWorkspace_tags = Lens.lens (\CreateWorkspace' {tags} -> tags) (\s@CreateWorkspace' {} a -> s {tags = a} :: CreateWorkspace) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the workspace.
createWorkspace_workspaceId :: Lens.Lens' CreateWorkspace Prelude.Text
createWorkspace_workspaceId = Lens.lens (\CreateWorkspace' {workspaceId} -> workspaceId) (\s@CreateWorkspace' {} a -> s {workspaceId = a} :: CreateWorkspace)

-- | The ARN of the S3 bucket where resources associated with the workspace
-- are stored.
createWorkspace_s3Location :: Lens.Lens' CreateWorkspace Prelude.Text
createWorkspace_s3Location = Lens.lens (\CreateWorkspace' {s3Location} -> s3Location) (\s@CreateWorkspace' {} a -> s {s3Location = a} :: CreateWorkspace)

-- | The ARN of the execution role associated with the workspace.
createWorkspace_role :: Lens.Lens' CreateWorkspace Prelude.Text
createWorkspace_role = Lens.lens (\CreateWorkspace' {role'} -> role') (\s@CreateWorkspace' {} a -> s {role' = a} :: CreateWorkspace)

instance Core.AWSRequest CreateWorkspace where
  type
    AWSResponse CreateWorkspace =
      CreateWorkspaceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorkspaceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "creationDateTime")
      )

instance Prelude.Hashable CreateWorkspace where
  hashWithSalt _salt CreateWorkspace' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` workspaceId
      `Prelude.hashWithSalt` s3Location
      `Prelude.hashWithSalt` role'

instance Prelude.NFData CreateWorkspace where
  rnf CreateWorkspace' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf workspaceId
      `Prelude.seq` Prelude.rnf s3Location
      `Prelude.seq` Prelude.rnf role'

instance Data.ToHeaders CreateWorkspace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateWorkspace where
  toJSON CreateWorkspace' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("s3Location" Data..= s3Location),
            Prelude.Just ("role" Data..= role')
          ]
      )

instance Data.ToPath CreateWorkspace where
  toPath CreateWorkspace' {..} =
    Prelude.mconcat
      ["/workspaces/", Data.toBS workspaceId]

instance Data.ToQuery CreateWorkspace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorkspaceResponse' smart constructor.
data CreateWorkspaceResponse = CreateWorkspaceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the workspace.
    arn :: Prelude.Text,
    -- | The date and time when the workspace was created.
    creationDateTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkspaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createWorkspaceResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'createWorkspaceResponse_arn' - The ARN of the workspace.
--
-- 'creationDateTime', 'createWorkspaceResponse_creationDateTime' - The date and time when the workspace was created.
newCreateWorkspaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationDateTime'
  Prelude.UTCTime ->
  CreateWorkspaceResponse
newCreateWorkspaceResponse
  pHttpStatus_
  pArn_
  pCreationDateTime_ =
    CreateWorkspaceResponse'
      { httpStatus = pHttpStatus_,
        arn = pArn_,
        creationDateTime =
          Data._Time Lens.# pCreationDateTime_
      }

-- | The response's http status code.
createWorkspaceResponse_httpStatus :: Lens.Lens' CreateWorkspaceResponse Prelude.Int
createWorkspaceResponse_httpStatus = Lens.lens (\CreateWorkspaceResponse' {httpStatus} -> httpStatus) (\s@CreateWorkspaceResponse' {} a -> s {httpStatus = a} :: CreateWorkspaceResponse)

-- | The ARN of the workspace.
createWorkspaceResponse_arn :: Lens.Lens' CreateWorkspaceResponse Prelude.Text
createWorkspaceResponse_arn = Lens.lens (\CreateWorkspaceResponse' {arn} -> arn) (\s@CreateWorkspaceResponse' {} a -> s {arn = a} :: CreateWorkspaceResponse)

-- | The date and time when the workspace was created.
createWorkspaceResponse_creationDateTime :: Lens.Lens' CreateWorkspaceResponse Prelude.UTCTime
createWorkspaceResponse_creationDateTime = Lens.lens (\CreateWorkspaceResponse' {creationDateTime} -> creationDateTime) (\s@CreateWorkspaceResponse' {} a -> s {creationDateTime = a} :: CreateWorkspaceResponse) Prelude.. Data._Time

instance Prelude.NFData CreateWorkspaceResponse where
  rnf CreateWorkspaceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDateTime
