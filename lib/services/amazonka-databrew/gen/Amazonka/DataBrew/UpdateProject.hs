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
-- Module      : Amazonka.DataBrew.UpdateProject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the definition of an existing DataBrew project.
module Amazonka.DataBrew.UpdateProject
  ( -- * Creating a Request
    UpdateProject (..),
    newUpdateProject,

    -- * Request Lenses
    updateProject_sample,
    updateProject_roleArn,
    updateProject_name,

    -- * Destructuring the Response
    UpdateProjectResponse (..),
    newUpdateProjectResponse,

    -- * Response Lenses
    updateProjectResponse_lastModifiedDate,
    updateProjectResponse_httpStatus,
    updateProjectResponse_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { sample :: Prelude.Maybe Sample,
    -- | The Amazon Resource Name (ARN) of the IAM role to be assumed for this
    -- request.
    roleArn :: Prelude.Text,
    -- | The name of the project to be updated.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sample', 'updateProject_sample' - Undocumented member.
--
-- 'roleArn', 'updateProject_roleArn' - The Amazon Resource Name (ARN) of the IAM role to be assumed for this
-- request.
--
-- 'name', 'updateProject_name' - The name of the project to be updated.
newUpdateProject ::
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  UpdateProject
newUpdateProject pRoleArn_ pName_ =
  UpdateProject'
    { sample = Prelude.Nothing,
      roleArn = pRoleArn_,
      name = pName_
    }

-- | Undocumented member.
updateProject_sample :: Lens.Lens' UpdateProject (Prelude.Maybe Sample)
updateProject_sample = Lens.lens (\UpdateProject' {sample} -> sample) (\s@UpdateProject' {} a -> s {sample = a} :: UpdateProject)

-- | The Amazon Resource Name (ARN) of the IAM role to be assumed for this
-- request.
updateProject_roleArn :: Lens.Lens' UpdateProject Prelude.Text
updateProject_roleArn = Lens.lens (\UpdateProject' {roleArn} -> roleArn) (\s@UpdateProject' {} a -> s {roleArn = a} :: UpdateProject)

-- | The name of the project to be updated.
updateProject_name :: Lens.Lens' UpdateProject Prelude.Text
updateProject_name = Lens.lens (\UpdateProject' {name} -> name) (\s@UpdateProject' {} a -> s {name = a} :: UpdateProject)

instance Core.AWSRequest UpdateProject where
  type
    AWSResponse UpdateProject =
      UpdateProjectResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProjectResponse'
            Prelude.<$> (x Data..?> "LastModifiedDate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Name")
      )

instance Prelude.Hashable UpdateProject where
  hashWithSalt _salt UpdateProject' {..} =
    _salt
      `Prelude.hashWithSalt` sample
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateProject where
  rnf UpdateProject' {..} =
    Prelude.rnf sample `Prelude.seq`
      Prelude.rnf roleArn `Prelude.seq`
        Prelude.rnf name

instance Data.ToHeaders UpdateProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateProject where
  toJSON UpdateProject' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Sample" Data..=) Prelude.<$> sample,
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath UpdateProject where
  toPath UpdateProject' {..} =
    Prelude.mconcat ["/projects/", Data.toBS name]

instance Data.ToQuery UpdateProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProjectResponse' smart constructor.
data UpdateProjectResponse = UpdateProjectResponse'
  { -- | The date and time that the project was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the project that you updated.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'updateProjectResponse_lastModifiedDate' - The date and time that the project was last modified.
--
-- 'httpStatus', 'updateProjectResponse_httpStatus' - The response's http status code.
--
-- 'name', 'updateProjectResponse_name' - The name of the project that you updated.
newUpdateProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  UpdateProjectResponse
newUpdateProjectResponse pHttpStatus_ pName_ =
  UpdateProjectResponse'
    { lastModifiedDate =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      name = pName_
    }

-- | The date and time that the project was last modified.
updateProjectResponse_lastModifiedDate :: Lens.Lens' UpdateProjectResponse (Prelude.Maybe Prelude.UTCTime)
updateProjectResponse_lastModifiedDate = Lens.lens (\UpdateProjectResponse' {lastModifiedDate} -> lastModifiedDate) (\s@UpdateProjectResponse' {} a -> s {lastModifiedDate = a} :: UpdateProjectResponse) Prelude.. Lens.mapping Data._Time

-- | The response's http status code.
updateProjectResponse_httpStatus :: Lens.Lens' UpdateProjectResponse Prelude.Int
updateProjectResponse_httpStatus = Lens.lens (\UpdateProjectResponse' {httpStatus} -> httpStatus) (\s@UpdateProjectResponse' {} a -> s {httpStatus = a} :: UpdateProjectResponse)

-- | The name of the project that you updated.
updateProjectResponse_name :: Lens.Lens' UpdateProjectResponse Prelude.Text
updateProjectResponse_name = Lens.lens (\UpdateProjectResponse' {name} -> name) (\s@UpdateProjectResponse' {} a -> s {name = a} :: UpdateProjectResponse)

instance Prelude.NFData UpdateProjectResponse where
  rnf UpdateProjectResponse' {..} =
    Prelude.rnf lastModifiedDate `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf name
