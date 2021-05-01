{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.DeleteProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the specified project.
module Network.AWS.SageMaker.DeleteProject
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteProject' smart constructor.
data DeleteProject = DeleteProject'
  { -- | The name of the project to delete.
    projectName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteProject where
  type Rs DeleteProject = DeleteProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteProjectResponse'

instance Prelude.Hashable DeleteProject

instance Prelude.NFData DeleteProject

instance Prelude.ToHeaders DeleteProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.DeleteProject" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteProject where
  toJSON DeleteProject' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ProjectName" Prelude..= projectName)
          ]
      )

instance Prelude.ToPath DeleteProject where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProjectResponse' smart constructor.
data DeleteProjectResponse = DeleteProjectResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteProjectResponse ::
  DeleteProjectResponse
newDeleteProjectResponse = DeleteProjectResponse'

instance Prelude.NFData DeleteProjectResponse
