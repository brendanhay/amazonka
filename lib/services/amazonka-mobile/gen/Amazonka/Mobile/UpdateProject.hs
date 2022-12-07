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
-- Module      : Amazonka.Mobile.UpdateProject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an existing project.
module Amazonka.Mobile.UpdateProject
  ( -- * Creating a Request
    UpdateProject (..),
    newUpdateProject,

    -- * Request Lenses
    updateProject_contents,
    updateProject_projectId,

    -- * Destructuring the Response
    UpdateProjectResponse (..),
    newUpdateProjectResponse,

    -- * Response Lenses
    updateProjectResponse_details,
    updateProjectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Mobile.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request structure used for requests to update project configuration.
--
-- /See:/ 'newUpdateProject' smart constructor.
data UpdateProject = UpdateProject'
  { -- | ZIP or YAML file which contains project configuration to be updated.
    -- This should be the contents of the file downloaded from the URL provided
    -- in an export project operation.
    contents :: Prelude.Maybe Prelude.ByteString,
    -- | Unique project identifier.
    projectId :: Prelude.Text
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
-- 'contents', 'updateProject_contents' - ZIP or YAML file which contains project configuration to be updated.
-- This should be the contents of the file downloaded from the URL provided
-- in an export project operation.
--
-- 'projectId', 'updateProject_projectId' - Unique project identifier.
newUpdateProject ::
  -- | 'projectId'
  Prelude.Text ->
  UpdateProject
newUpdateProject pProjectId_ =
  UpdateProject'
    { contents = Prelude.Nothing,
      projectId = pProjectId_
    }

-- | ZIP or YAML file which contains project configuration to be updated.
-- This should be the contents of the file downloaded from the URL provided
-- in an export project operation.
updateProject_contents :: Lens.Lens' UpdateProject (Prelude.Maybe Prelude.ByteString)
updateProject_contents = Lens.lens (\UpdateProject' {contents} -> contents) (\s@UpdateProject' {} a -> s {contents = a} :: UpdateProject)

-- | Unique project identifier.
updateProject_projectId :: Lens.Lens' UpdateProject Prelude.Text
updateProject_projectId = Lens.lens (\UpdateProject' {projectId} -> projectId) (\s@UpdateProject' {} a -> s {projectId = a} :: UpdateProject)

instance Core.AWSRequest UpdateProject where
  type
    AWSResponse UpdateProject =
      UpdateProjectResponse
  request overrides =
    Request.postBody (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateProjectResponse'
            Prelude.<$> (x Data..?> "details")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateProject where
  hashWithSalt _salt UpdateProject' {..} =
    _salt `Prelude.hashWithSalt` contents
      `Prelude.hashWithSalt` projectId

instance Prelude.NFData UpdateProject where
  rnf UpdateProject' {..} =
    Prelude.rnf contents
      `Prelude.seq` Prelude.rnf projectId

instance Data.ToBody UpdateProject where
  toBody UpdateProject' {..} = Data.toBody contents

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

instance Data.ToPath UpdateProject where
  toPath = Prelude.const "/update"

instance Data.ToQuery UpdateProject where
  toQuery UpdateProject' {..} =
    Prelude.mconcat ["projectId" Data.=: projectId]

-- | Result structure used for requests to updated project configuration.
--
-- /See:/ 'newUpdateProjectResponse' smart constructor.
data UpdateProjectResponse = UpdateProjectResponse'
  { -- | Detailed information about the updated AWS Mobile Hub project.
    details :: Prelude.Maybe ProjectDetails,
    -- | The response's http status code.
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
-- 'details', 'updateProjectResponse_details' - Detailed information about the updated AWS Mobile Hub project.
--
-- 'httpStatus', 'updateProjectResponse_httpStatus' - The response's http status code.
newUpdateProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateProjectResponse
newUpdateProjectResponse pHttpStatus_ =
  UpdateProjectResponse'
    { details = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Detailed information about the updated AWS Mobile Hub project.
updateProjectResponse_details :: Lens.Lens' UpdateProjectResponse (Prelude.Maybe ProjectDetails)
updateProjectResponse_details = Lens.lens (\UpdateProjectResponse' {details} -> details) (\s@UpdateProjectResponse' {} a -> s {details = a} :: UpdateProjectResponse)

-- | The response's http status code.
updateProjectResponse_httpStatus :: Lens.Lens' UpdateProjectResponse Prelude.Int
updateProjectResponse_httpStatus = Lens.lens (\UpdateProjectResponse' {httpStatus} -> httpStatus) (\s@UpdateProjectResponse' {} a -> s {httpStatus = a} :: UpdateProjectResponse)

instance Prelude.NFData UpdateProjectResponse where
  rnf UpdateProjectResponse' {..} =
    Prelude.rnf details
      `Prelude.seq` Prelude.rnf httpStatus
