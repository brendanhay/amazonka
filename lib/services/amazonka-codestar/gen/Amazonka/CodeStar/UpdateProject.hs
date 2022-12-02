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
-- Module      : Amazonka.CodeStar.UpdateProject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a project in AWS CodeStar.
module Amazonka.CodeStar.UpdateProject
  ( -- * Creating a Request
    UpdateProject (..),
    newUpdateProject,

    -- * Request Lenses
    updateProject_name,
    updateProject_description,
    updateProject_id,

    -- * Destructuring the Response
    UpdateProjectResponse (..),
    newUpdateProjectResponse,

    -- * Response Lenses
    updateProjectResponse_httpStatus,
  )
where

import Amazonka.CodeStar.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { -- | The name of the project you want to update.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The description of the project, if any.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the project you want to update.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateProject_name' - The name of the project you want to update.
--
-- 'description', 'updateProject_description' - The description of the project, if any.
--
-- 'id', 'updateProject_id' - The ID of the project you want to update.
newUpdateProject ::
  -- | 'id'
  Prelude.Text ->
  UpdateProject
newUpdateProject pId_ =
  UpdateProject'
    { name = Prelude.Nothing,
      description = Prelude.Nothing,
      id = pId_
    }

-- | The name of the project you want to update.
updateProject_name :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Text)
updateProject_name = Lens.lens (\UpdateProject' {name} -> name) (\s@UpdateProject' {} a -> s {name = a} :: UpdateProject) Prelude.. Lens.mapping Data._Sensitive

-- | The description of the project, if any.
updateProject_description :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.Text)
updateProject_description = Lens.lens (\UpdateProject' {description} -> description) (\s@UpdateProject' {} a -> s {description = a} :: UpdateProject) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the project you want to update.
updateProject_id :: Lens.Lens' UpdateProject Prelude.Text
updateProject_id = Lens.lens (\UpdateProject' {id} -> id) (\s@UpdateProject' {} a -> s {id = a} :: UpdateProject)

instance Core.AWSRequest UpdateProject where
  type
    AWSResponse UpdateProject =
      UpdateProjectResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateProjectResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateProject where
  hashWithSalt _salt UpdateProject' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id

instance Prelude.NFData UpdateProject where
  rnf UpdateProject' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders UpdateProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeStar_20170419.UpdateProject" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateProject where
  toJSON UpdateProject' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("id" Data..= id)
          ]
      )

instance Data.ToPath UpdateProject where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProjectResponse' smart constructor.
data UpdateProjectResponse = UpdateProjectResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'httpStatus', 'updateProjectResponse_httpStatus' - The response's http status code.
newUpdateProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateProjectResponse
newUpdateProjectResponse pHttpStatus_ =
  UpdateProjectResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateProjectResponse_httpStatus :: Lens.Lens' UpdateProjectResponse Prelude.Int
updateProjectResponse_httpStatus = Lens.lens (\UpdateProjectResponse' {httpStatus} -> httpStatus) (\s@UpdateProjectResponse' {} a -> s {httpStatus = a} :: UpdateProjectResponse)

instance Prelude.NFData UpdateProjectResponse where
  rnf UpdateProjectResponse' {..} =
    Prelude.rnf httpStatus
