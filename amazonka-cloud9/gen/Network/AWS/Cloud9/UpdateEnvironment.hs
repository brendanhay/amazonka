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
-- Module      : Network.AWS.Cloud9.UpdateEnvironment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the settings of an existing AWS Cloud9 development environment.
module Network.AWS.Cloud9.UpdateEnvironment
  ( -- * Creating a Request
    UpdateEnvironment (..),
    newUpdateEnvironment,

    -- * Request Lenses
    updateEnvironment_name,
    updateEnvironment_description,
    updateEnvironment_environmentId,

    -- * Destructuring the Response
    UpdateEnvironmentResponse (..),
    newUpdateEnvironmentResponse,

    -- * Response Lenses
    updateEnvironmentResponse_httpStatus,
  )
where

import Network.AWS.Cloud9.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateEnvironment' smart constructor.
data UpdateEnvironment = UpdateEnvironment'
  { -- | A replacement name for the environment.
    name :: Core.Maybe Core.Text,
    -- | Any new or replacement description for the environment.
    description :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The ID of the environment to change settings.
    environmentId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateEnvironment_name' - A replacement name for the environment.
--
-- 'description', 'updateEnvironment_description' - Any new or replacement description for the environment.
--
-- 'environmentId', 'updateEnvironment_environmentId' - The ID of the environment to change settings.
newUpdateEnvironment ::
  -- | 'environmentId'
  Core.Text ->
  UpdateEnvironment
newUpdateEnvironment pEnvironmentId_ =
  UpdateEnvironment'
    { name = Core.Nothing,
      description = Core.Nothing,
      environmentId = pEnvironmentId_
    }

-- | A replacement name for the environment.
updateEnvironment_name :: Lens.Lens' UpdateEnvironment (Core.Maybe Core.Text)
updateEnvironment_name = Lens.lens (\UpdateEnvironment' {name} -> name) (\s@UpdateEnvironment' {} a -> s {name = a} :: UpdateEnvironment)

-- | Any new or replacement description for the environment.
updateEnvironment_description :: Lens.Lens' UpdateEnvironment (Core.Maybe Core.Text)
updateEnvironment_description = Lens.lens (\UpdateEnvironment' {description} -> description) (\s@UpdateEnvironment' {} a -> s {description = a} :: UpdateEnvironment) Core.. Lens.mapping Core._Sensitive

-- | The ID of the environment to change settings.
updateEnvironment_environmentId :: Lens.Lens' UpdateEnvironment Core.Text
updateEnvironment_environmentId = Lens.lens (\UpdateEnvironment' {environmentId} -> environmentId) (\s@UpdateEnvironment' {} a -> s {environmentId = a} :: UpdateEnvironment)

instance Core.AWSRequest UpdateEnvironment where
  type
    AWSResponse UpdateEnvironment =
      UpdateEnvironmentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateEnvironmentResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateEnvironment

instance Core.NFData UpdateEnvironment

instance Core.ToHeaders UpdateEnvironment where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCloud9WorkspaceManagementService.UpdateEnvironment" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateEnvironment where
  toJSON UpdateEnvironment' {..} =
    Core.object
      ( Core.catMaybes
          [ ("name" Core..=) Core.<$> name,
            ("description" Core..=) Core.<$> description,
            Core.Just ("environmentId" Core..= environmentId)
          ]
      )

instance Core.ToPath UpdateEnvironment where
  toPath = Core.const "/"

instance Core.ToQuery UpdateEnvironment where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateEnvironmentResponse' smart constructor.
data UpdateEnvironmentResponse = UpdateEnvironmentResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEnvironmentResponse_httpStatus' - The response's http status code.
newUpdateEnvironmentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateEnvironmentResponse
newUpdateEnvironmentResponse pHttpStatus_ =
  UpdateEnvironmentResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateEnvironmentResponse_httpStatus :: Lens.Lens' UpdateEnvironmentResponse Core.Int
updateEnvironmentResponse_httpStatus = Lens.lens (\UpdateEnvironmentResponse' {httpStatus} -> httpStatus) (\s@UpdateEnvironmentResponse' {} a -> s {httpStatus = a} :: UpdateEnvironmentResponse)

instance Core.NFData UpdateEnvironmentResponse
