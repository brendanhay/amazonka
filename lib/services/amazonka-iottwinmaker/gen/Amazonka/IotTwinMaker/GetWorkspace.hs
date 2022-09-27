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
-- Module      : Amazonka.IotTwinMaker.GetWorkspace
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a workspace.
module Amazonka.IotTwinMaker.GetWorkspace
  ( -- * Creating a Request
    GetWorkspace (..),
    newGetWorkspace,

    -- * Request Lenses
    getWorkspace_workspaceId,

    -- * Destructuring the Response
    GetWorkspaceResponse (..),
    newGetWorkspaceResponse,

    -- * Response Lenses
    getWorkspaceResponse_description,
    getWorkspaceResponse_httpStatus,
    getWorkspaceResponse_arn,
    getWorkspaceResponse_creationDateTime,
    getWorkspaceResponse_role,
    getWorkspaceResponse_s3Location,
    getWorkspaceResponse_updateDateTime,
    getWorkspaceResponse_workspaceId,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IotTwinMaker.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWorkspace' smart constructor.
data GetWorkspace = GetWorkspace'
  { -- | The ID of the workspace.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkspace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workspaceId', 'getWorkspace_workspaceId' - The ID of the workspace.
newGetWorkspace ::
  -- | 'workspaceId'
  Prelude.Text ->
  GetWorkspace
newGetWorkspace pWorkspaceId_ =
  GetWorkspace' {workspaceId = pWorkspaceId_}

-- | The ID of the workspace.
getWorkspace_workspaceId :: Lens.Lens' GetWorkspace Prelude.Text
getWorkspace_workspaceId = Lens.lens (\GetWorkspace' {workspaceId} -> workspaceId) (\s@GetWorkspace' {} a -> s {workspaceId = a} :: GetWorkspace)

instance Core.AWSRequest GetWorkspace where
  type AWSResponse GetWorkspace = GetWorkspaceResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkspaceResponse'
            Prelude.<$> (x Core..?> "description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "arn")
            Prelude.<*> (x Core..:> "creationDateTime")
            Prelude.<*> (x Core..:> "role")
            Prelude.<*> (x Core..:> "s3Location")
            Prelude.<*> (x Core..:> "updateDateTime")
            Prelude.<*> (x Core..:> "workspaceId")
      )

instance Prelude.Hashable GetWorkspace where
  hashWithSalt _salt GetWorkspace' {..} =
    _salt `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData GetWorkspace where
  rnf GetWorkspace' {..} = Prelude.rnf workspaceId

instance Core.ToHeaders GetWorkspace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetWorkspace where
  toPath GetWorkspace' {..} =
    Prelude.mconcat
      ["/workspaces/", Core.toBS workspaceId]

instance Core.ToQuery GetWorkspace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWorkspaceResponse' smart constructor.
data GetWorkspaceResponse = GetWorkspaceResponse'
  { -- | The description of the workspace.
    description :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the workspace.
    arn :: Prelude.Text,
    -- | The date and time when the workspace was created.
    creationDateTime :: Core.POSIX,
    -- | The ARN of the execution role associated with the workspace.
    role' :: Prelude.Text,
    -- | The ARN of the S3 bucket where resources associated with the workspace
    -- are stored.
    s3Location :: Prelude.Text,
    -- | The date and time when the workspace was last updated.
    updateDateTime :: Core.POSIX,
    -- | The ID of the workspace.
    workspaceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkspaceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'getWorkspaceResponse_description' - The description of the workspace.
--
-- 'httpStatus', 'getWorkspaceResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'getWorkspaceResponse_arn' - The ARN of the workspace.
--
-- 'creationDateTime', 'getWorkspaceResponse_creationDateTime' - The date and time when the workspace was created.
--
-- 'role'', 'getWorkspaceResponse_role' - The ARN of the execution role associated with the workspace.
--
-- 's3Location', 'getWorkspaceResponse_s3Location' - The ARN of the S3 bucket where resources associated with the workspace
-- are stored.
--
-- 'updateDateTime', 'getWorkspaceResponse_updateDateTime' - The date and time when the workspace was last updated.
--
-- 'workspaceId', 'getWorkspaceResponse_workspaceId' - The ID of the workspace.
newGetWorkspaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationDateTime'
  Prelude.UTCTime ->
  -- | 'role''
  Prelude.Text ->
  -- | 's3Location'
  Prelude.Text ->
  -- | 'updateDateTime'
  Prelude.UTCTime ->
  -- | 'workspaceId'
  Prelude.Text ->
  GetWorkspaceResponse
newGetWorkspaceResponse
  pHttpStatus_
  pArn_
  pCreationDateTime_
  pRole_
  pS3Location_
  pUpdateDateTime_
  pWorkspaceId_ =
    GetWorkspaceResponse'
      { description =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        creationDateTime =
          Core._Time Lens.# pCreationDateTime_,
        role' = pRole_,
        s3Location = pS3Location_,
        updateDateTime = Core._Time Lens.# pUpdateDateTime_,
        workspaceId = pWorkspaceId_
      }

-- | The description of the workspace.
getWorkspaceResponse_description :: Lens.Lens' GetWorkspaceResponse (Prelude.Maybe Prelude.Text)
getWorkspaceResponse_description = Lens.lens (\GetWorkspaceResponse' {description} -> description) (\s@GetWorkspaceResponse' {} a -> s {description = a} :: GetWorkspaceResponse)

-- | The response's http status code.
getWorkspaceResponse_httpStatus :: Lens.Lens' GetWorkspaceResponse Prelude.Int
getWorkspaceResponse_httpStatus = Lens.lens (\GetWorkspaceResponse' {httpStatus} -> httpStatus) (\s@GetWorkspaceResponse' {} a -> s {httpStatus = a} :: GetWorkspaceResponse)

-- | The ARN of the workspace.
getWorkspaceResponse_arn :: Lens.Lens' GetWorkspaceResponse Prelude.Text
getWorkspaceResponse_arn = Lens.lens (\GetWorkspaceResponse' {arn} -> arn) (\s@GetWorkspaceResponse' {} a -> s {arn = a} :: GetWorkspaceResponse)

-- | The date and time when the workspace was created.
getWorkspaceResponse_creationDateTime :: Lens.Lens' GetWorkspaceResponse Prelude.UTCTime
getWorkspaceResponse_creationDateTime = Lens.lens (\GetWorkspaceResponse' {creationDateTime} -> creationDateTime) (\s@GetWorkspaceResponse' {} a -> s {creationDateTime = a} :: GetWorkspaceResponse) Prelude.. Core._Time

-- | The ARN of the execution role associated with the workspace.
getWorkspaceResponse_role :: Lens.Lens' GetWorkspaceResponse Prelude.Text
getWorkspaceResponse_role = Lens.lens (\GetWorkspaceResponse' {role'} -> role') (\s@GetWorkspaceResponse' {} a -> s {role' = a} :: GetWorkspaceResponse)

-- | The ARN of the S3 bucket where resources associated with the workspace
-- are stored.
getWorkspaceResponse_s3Location :: Lens.Lens' GetWorkspaceResponse Prelude.Text
getWorkspaceResponse_s3Location = Lens.lens (\GetWorkspaceResponse' {s3Location} -> s3Location) (\s@GetWorkspaceResponse' {} a -> s {s3Location = a} :: GetWorkspaceResponse)

-- | The date and time when the workspace was last updated.
getWorkspaceResponse_updateDateTime :: Lens.Lens' GetWorkspaceResponse Prelude.UTCTime
getWorkspaceResponse_updateDateTime = Lens.lens (\GetWorkspaceResponse' {updateDateTime} -> updateDateTime) (\s@GetWorkspaceResponse' {} a -> s {updateDateTime = a} :: GetWorkspaceResponse) Prelude.. Core._Time

-- | The ID of the workspace.
getWorkspaceResponse_workspaceId :: Lens.Lens' GetWorkspaceResponse Prelude.Text
getWorkspaceResponse_workspaceId = Lens.lens (\GetWorkspaceResponse' {workspaceId} -> workspaceId) (\s@GetWorkspaceResponse' {} a -> s {workspaceId = a} :: GetWorkspaceResponse)

instance Prelude.NFData GetWorkspaceResponse where
  rnf GetWorkspaceResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf s3Location
      `Prelude.seq` Prelude.rnf updateDateTime
      `Prelude.seq` Prelude.rnf workspaceId
