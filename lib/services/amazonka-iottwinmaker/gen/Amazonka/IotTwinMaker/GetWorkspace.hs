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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    getWorkspaceResponse_workspaceId,
    getWorkspaceResponse_arn,
    getWorkspaceResponse_s3Location,
    getWorkspaceResponse_role,
    getWorkspaceResponse_creationDateTime,
    getWorkspaceResponse_updateDateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IotTwinMaker.Types
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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkspaceResponse'
            Prelude.<$> (x Data..?> "description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "workspaceId")
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "s3Location")
            Prelude.<*> (x Data..:> "role")
            Prelude.<*> (x Data..:> "creationDateTime")
            Prelude.<*> (x Data..:> "updateDateTime")
      )

instance Prelude.Hashable GetWorkspace where
  hashWithSalt _salt GetWorkspace' {..} =
    _salt `Prelude.hashWithSalt` workspaceId

instance Prelude.NFData GetWorkspace where
  rnf GetWorkspace' {..} = Prelude.rnf workspaceId

instance Data.ToHeaders GetWorkspace where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetWorkspace where
  toPath GetWorkspace' {..} =
    Prelude.mconcat
      ["/workspaces/", Data.toBS workspaceId]

instance Data.ToQuery GetWorkspace where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWorkspaceResponse' smart constructor.
data GetWorkspaceResponse = GetWorkspaceResponse'
  { -- | The description of the workspace.
    description :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the workspace.
    workspaceId :: Prelude.Text,
    -- | The ARN of the workspace.
    arn :: Prelude.Text,
    -- | The ARN of the S3 bucket where resources associated with the workspace
    -- are stored.
    s3Location :: Prelude.Text,
    -- | The ARN of the execution role associated with the workspace.
    role' :: Prelude.Text,
    -- | The date and time when the workspace was created.
    creationDateTime :: Data.POSIX,
    -- | The date and time when the workspace was last updated.
    updateDateTime :: Data.POSIX
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
-- 'workspaceId', 'getWorkspaceResponse_workspaceId' - The ID of the workspace.
--
-- 'arn', 'getWorkspaceResponse_arn' - The ARN of the workspace.
--
-- 's3Location', 'getWorkspaceResponse_s3Location' - The ARN of the S3 bucket where resources associated with the workspace
-- are stored.
--
-- 'role'', 'getWorkspaceResponse_role' - The ARN of the execution role associated with the workspace.
--
-- 'creationDateTime', 'getWorkspaceResponse_creationDateTime' - The date and time when the workspace was created.
--
-- 'updateDateTime', 'getWorkspaceResponse_updateDateTime' - The date and time when the workspace was last updated.
newGetWorkspaceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workspaceId'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 's3Location'
  Prelude.Text ->
  -- | 'role''
  Prelude.Text ->
  -- | 'creationDateTime'
  Prelude.UTCTime ->
  -- | 'updateDateTime'
  Prelude.UTCTime ->
  GetWorkspaceResponse
newGetWorkspaceResponse
  pHttpStatus_
  pWorkspaceId_
  pArn_
  pS3Location_
  pRole_
  pCreationDateTime_
  pUpdateDateTime_ =
    GetWorkspaceResponse'
      { description =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        workspaceId = pWorkspaceId_,
        arn = pArn_,
        s3Location = pS3Location_,
        role' = pRole_,
        creationDateTime =
          Data._Time Lens.# pCreationDateTime_,
        updateDateTime = Data._Time Lens.# pUpdateDateTime_
      }

-- | The description of the workspace.
getWorkspaceResponse_description :: Lens.Lens' GetWorkspaceResponse (Prelude.Maybe Prelude.Text)
getWorkspaceResponse_description = Lens.lens (\GetWorkspaceResponse' {description} -> description) (\s@GetWorkspaceResponse' {} a -> s {description = a} :: GetWorkspaceResponse)

-- | The response's http status code.
getWorkspaceResponse_httpStatus :: Lens.Lens' GetWorkspaceResponse Prelude.Int
getWorkspaceResponse_httpStatus = Lens.lens (\GetWorkspaceResponse' {httpStatus} -> httpStatus) (\s@GetWorkspaceResponse' {} a -> s {httpStatus = a} :: GetWorkspaceResponse)

-- | The ID of the workspace.
getWorkspaceResponse_workspaceId :: Lens.Lens' GetWorkspaceResponse Prelude.Text
getWorkspaceResponse_workspaceId = Lens.lens (\GetWorkspaceResponse' {workspaceId} -> workspaceId) (\s@GetWorkspaceResponse' {} a -> s {workspaceId = a} :: GetWorkspaceResponse)

-- | The ARN of the workspace.
getWorkspaceResponse_arn :: Lens.Lens' GetWorkspaceResponse Prelude.Text
getWorkspaceResponse_arn = Lens.lens (\GetWorkspaceResponse' {arn} -> arn) (\s@GetWorkspaceResponse' {} a -> s {arn = a} :: GetWorkspaceResponse)

-- | The ARN of the S3 bucket where resources associated with the workspace
-- are stored.
getWorkspaceResponse_s3Location :: Lens.Lens' GetWorkspaceResponse Prelude.Text
getWorkspaceResponse_s3Location = Lens.lens (\GetWorkspaceResponse' {s3Location} -> s3Location) (\s@GetWorkspaceResponse' {} a -> s {s3Location = a} :: GetWorkspaceResponse)

-- | The ARN of the execution role associated with the workspace.
getWorkspaceResponse_role :: Lens.Lens' GetWorkspaceResponse Prelude.Text
getWorkspaceResponse_role = Lens.lens (\GetWorkspaceResponse' {role'} -> role') (\s@GetWorkspaceResponse' {} a -> s {role' = a} :: GetWorkspaceResponse)

-- | The date and time when the workspace was created.
getWorkspaceResponse_creationDateTime :: Lens.Lens' GetWorkspaceResponse Prelude.UTCTime
getWorkspaceResponse_creationDateTime = Lens.lens (\GetWorkspaceResponse' {creationDateTime} -> creationDateTime) (\s@GetWorkspaceResponse' {} a -> s {creationDateTime = a} :: GetWorkspaceResponse) Prelude.. Data._Time

-- | The date and time when the workspace was last updated.
getWorkspaceResponse_updateDateTime :: Lens.Lens' GetWorkspaceResponse Prelude.UTCTime
getWorkspaceResponse_updateDateTime = Lens.lens (\GetWorkspaceResponse' {updateDateTime} -> updateDateTime) (\s@GetWorkspaceResponse' {} a -> s {updateDateTime = a} :: GetWorkspaceResponse) Prelude.. Data._Time

instance Prelude.NFData GetWorkspaceResponse where
  rnf GetWorkspaceResponse' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf workspaceId `Prelude.seq`
          Prelude.rnf arn `Prelude.seq`
            Prelude.rnf s3Location `Prelude.seq`
              Prelude.rnf role' `Prelude.seq`
                Prelude.rnf creationDateTime `Prelude.seq`
                  Prelude.rnf updateDateTime
