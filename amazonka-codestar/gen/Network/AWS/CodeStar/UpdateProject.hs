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
-- Module      : Network.AWS.CodeStar.UpdateProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a project in AWS CodeStar.
module Network.AWS.CodeStar.UpdateProject
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

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { -- | The name of the project you want to update.
    name :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The description of the project, if any.
    description :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The ID of the project you want to update.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Text ->
  UpdateProject
newUpdateProject pId_ =
  UpdateProject'
    { name = Core.Nothing,
      description = Core.Nothing,
      id = pId_
    }

-- | The name of the project you want to update.
updateProject_name :: Lens.Lens' UpdateProject (Core.Maybe Core.Text)
updateProject_name = Lens.lens (\UpdateProject' {name} -> name) (\s@UpdateProject' {} a -> s {name = a} :: UpdateProject) Core.. Lens.mapping Core._Sensitive

-- | The description of the project, if any.
updateProject_description :: Lens.Lens' UpdateProject (Core.Maybe Core.Text)
updateProject_description = Lens.lens (\UpdateProject' {description} -> description) (\s@UpdateProject' {} a -> s {description = a} :: UpdateProject) Core.. Lens.mapping Core._Sensitive

-- | The ID of the project you want to update.
updateProject_id :: Lens.Lens' UpdateProject Core.Text
updateProject_id = Lens.lens (\UpdateProject' {id} -> id) (\s@UpdateProject' {} a -> s {id = a} :: UpdateProject)

instance Core.AWSRequest UpdateProject where
  type
    AWSResponse UpdateProject =
      UpdateProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateProjectResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateProject

instance Core.NFData UpdateProject

instance Core.ToHeaders UpdateProject where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeStar_20170419.UpdateProject" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateProject where
  toJSON UpdateProject' {..} =
    Core.object
      ( Core.catMaybes
          [ ("name" Core..=) Core.<$> name,
            ("description" Core..=) Core.<$> description,
            Core.Just ("id" Core..= id)
          ]
      )

instance Core.ToPath UpdateProject where
  toPath = Core.const "/"

instance Core.ToQuery UpdateProject where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateProjectResponse' smart constructor.
data UpdateProjectResponse = UpdateProjectResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateProjectResponse
newUpdateProjectResponse pHttpStatus_ =
  UpdateProjectResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateProjectResponse_httpStatus :: Lens.Lens' UpdateProjectResponse Core.Int
updateProjectResponse_httpStatus = Lens.lens (\UpdateProjectResponse' {httpStatus} -> httpStatus) (\s@UpdateProjectResponse' {} a -> s {httpStatus = a} :: UpdateProjectResponse)

instance Core.NFData UpdateProjectResponse
