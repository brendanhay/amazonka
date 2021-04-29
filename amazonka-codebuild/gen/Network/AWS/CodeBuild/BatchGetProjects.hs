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
-- Module      : Network.AWS.CodeBuild.BatchGetProjects
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more build projects.
module Network.AWS.CodeBuild.BatchGetProjects
  ( -- * Creating a Request
    BatchGetProjects (..),
    newBatchGetProjects,

    -- * Request Lenses
    batchGetProjects_names,

    -- * Destructuring the Response
    BatchGetProjectsResponse (..),
    newBatchGetProjectsResponse,

    -- * Response Lenses
    batchGetProjectsResponse_projects,
    batchGetProjectsResponse_projectsNotFound,
    batchGetProjectsResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchGetProjects' smart constructor.
data BatchGetProjects = BatchGetProjects'
  { -- | The names or ARNs of the build projects. To get information about a
    -- project shared with your AWS account, its ARN must be specified. You
    -- cannot specify a shared project using its name.
    names :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchGetProjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'names', 'batchGetProjects_names' - The names or ARNs of the build projects. To get information about a
-- project shared with your AWS account, its ARN must be specified. You
-- cannot specify a shared project using its name.
newBatchGetProjects ::
  -- | 'names'
  Prelude.NonEmpty Prelude.Text ->
  BatchGetProjects
newBatchGetProjects pNames_ =
  BatchGetProjects'
    { names =
        Prelude._Coerce Lens.# pNames_
    }

-- | The names or ARNs of the build projects. To get information about a
-- project shared with your AWS account, its ARN must be specified. You
-- cannot specify a shared project using its name.
batchGetProjects_names :: Lens.Lens' BatchGetProjects (Prelude.NonEmpty Prelude.Text)
batchGetProjects_names = Lens.lens (\BatchGetProjects' {names} -> names) (\s@BatchGetProjects' {} a -> s {names = a} :: BatchGetProjects) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest BatchGetProjects where
  type Rs BatchGetProjects = BatchGetProjectsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetProjectsResponse'
            Prelude.<$> (x Prelude..?> "projects" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "projectsNotFound")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetProjects

instance Prelude.NFData BatchGetProjects

instance Prelude.ToHeaders BatchGetProjects where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeBuild_20161006.BatchGetProjects" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON BatchGetProjects where
  toJSON BatchGetProjects' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("names" Prelude..= names)]
      )

instance Prelude.ToPath BatchGetProjects where
  toPath = Prelude.const "/"

instance Prelude.ToQuery BatchGetProjects where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetProjectsResponse' smart constructor.
data BatchGetProjectsResponse = BatchGetProjectsResponse'
  { -- | Information about the requested build projects.
    projects :: Prelude.Maybe [Project],
    -- | The names of build projects for which information could not be found.
    projectsNotFound :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BatchGetProjectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projects', 'batchGetProjectsResponse_projects' - Information about the requested build projects.
--
-- 'projectsNotFound', 'batchGetProjectsResponse_projectsNotFound' - The names of build projects for which information could not be found.
--
-- 'httpStatus', 'batchGetProjectsResponse_httpStatus' - The response's http status code.
newBatchGetProjectsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetProjectsResponse
newBatchGetProjectsResponse pHttpStatus_ =
  BatchGetProjectsResponse'
    { projects =
        Prelude.Nothing,
      projectsNotFound = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the requested build projects.
batchGetProjectsResponse_projects :: Lens.Lens' BatchGetProjectsResponse (Prelude.Maybe [Project])
batchGetProjectsResponse_projects = Lens.lens (\BatchGetProjectsResponse' {projects} -> projects) (\s@BatchGetProjectsResponse' {} a -> s {projects = a} :: BatchGetProjectsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The names of build projects for which information could not be found.
batchGetProjectsResponse_projectsNotFound :: Lens.Lens' BatchGetProjectsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
batchGetProjectsResponse_projectsNotFound = Lens.lens (\BatchGetProjectsResponse' {projectsNotFound} -> projectsNotFound) (\s@BatchGetProjectsResponse' {} a -> s {projectsNotFound = a} :: BatchGetProjectsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
batchGetProjectsResponse_httpStatus :: Lens.Lens' BatchGetProjectsResponse Prelude.Int
batchGetProjectsResponse_httpStatus = Lens.lens (\BatchGetProjectsResponse' {httpStatus} -> httpStatus) (\s@BatchGetProjectsResponse' {} a -> s {httpStatus = a} :: BatchGetProjectsResponse)

instance Prelude.NFData BatchGetProjectsResponse
