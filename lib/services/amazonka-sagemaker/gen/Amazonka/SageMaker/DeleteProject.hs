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
-- Module      : Amazonka.SageMaker.DeleteProject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the specified project.
module Amazonka.SageMaker.DeleteProject
  ( -- * Creating a Request
    DeleteProject (..),
    newDeleteProject,

    -- * Request Lenses
    deleteProject_projectName,

    -- * Destructuring the Response
    DeleteProjectResponse (..),
    newDeleteProjectResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteProject' smart constructor.
data DeleteProject = DeleteProject'
  { -- | The name of the project to delete.
    projectName :: Prelude.Text
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
-- 'projectName', 'deleteProject_projectName' - The name of the project to delete.
newDeleteProject ::
  -- | 'projectName'
  Prelude.Text ->
  DeleteProject
newDeleteProject pProjectName_ =
  DeleteProject' {projectName = pProjectName_}

-- | The name of the project to delete.
deleteProject_projectName :: Lens.Lens' DeleteProject Prelude.Text
deleteProject_projectName = Lens.lens (\DeleteProject' {projectName} -> projectName) (\s@DeleteProject' {} a -> s {projectName = a} :: DeleteProject)

instance Core.AWSRequest DeleteProject where
  type
    AWSResponse DeleteProject =
      DeleteProjectResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteProjectResponse'

instance Prelude.Hashable DeleteProject where
  hashWithSalt _salt DeleteProject' {..} =
    _salt `Prelude.hashWithSalt` projectName

instance Prelude.NFData DeleteProject where
  rnf DeleteProject' {..} = Prelude.rnf projectName

instance Data.ToHeaders DeleteProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.DeleteProject" :: Prelude.ByteString),
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
          [Prelude.Just ("ProjectName" Data..= projectName)]
      )

instance Data.ToPath DeleteProject where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProjectResponse' smart constructor.
data DeleteProjectResponse = DeleteProjectResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteProjectResponse ::
  DeleteProjectResponse
newDeleteProjectResponse = DeleteProjectResponse'

instance Prelude.NFData DeleteProjectResponse where
  rnf _ = ()
