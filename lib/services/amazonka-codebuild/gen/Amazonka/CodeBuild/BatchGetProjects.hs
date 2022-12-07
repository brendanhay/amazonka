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
-- Module      : Amazonka.CodeBuild.BatchGetProjects
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more build projects.
module Amazonka.CodeBuild.BatchGetProjects
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

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetProjects' smart constructor.
data BatchGetProjects = BatchGetProjects'
  { -- | The names or ARNs of the build projects. To get information about a
    -- project shared with your Amazon Web Services account, its ARN must be
    -- specified. You cannot specify a shared project using its name.
    names :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetProjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'names', 'batchGetProjects_names' - The names or ARNs of the build projects. To get information about a
-- project shared with your Amazon Web Services account, its ARN must be
-- specified. You cannot specify a shared project using its name.
newBatchGetProjects ::
  -- | 'names'
  Prelude.NonEmpty Prelude.Text ->
  BatchGetProjects
newBatchGetProjects pNames_ =
  BatchGetProjects'
    { names =
        Lens.coerced Lens.# pNames_
    }

-- | The names or ARNs of the build projects. To get information about a
-- project shared with your Amazon Web Services account, its ARN must be
-- specified. You cannot specify a shared project using its name.
batchGetProjects_names :: Lens.Lens' BatchGetProjects (Prelude.NonEmpty Prelude.Text)
batchGetProjects_names = Lens.lens (\BatchGetProjects' {names} -> names) (\s@BatchGetProjects' {} a -> s {names = a} :: BatchGetProjects) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetProjects where
  type
    AWSResponse BatchGetProjects =
      BatchGetProjectsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetProjectsResponse'
            Prelude.<$> (x Data..?> "projects" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "projectsNotFound")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetProjects where
  hashWithSalt _salt BatchGetProjects' {..} =
    _salt `Prelude.hashWithSalt` names

instance Prelude.NFData BatchGetProjects where
  rnf BatchGetProjects' {..} = Prelude.rnf names

instance Data.ToHeaders BatchGetProjects where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.BatchGetProjects" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetProjects where
  toJSON BatchGetProjects' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("names" Data..= names)]
      )

instance Data.ToPath BatchGetProjects where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetProjects where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
batchGetProjectsResponse_projects = Lens.lens (\BatchGetProjectsResponse' {projects} -> projects) (\s@BatchGetProjectsResponse' {} a -> s {projects = a} :: BatchGetProjectsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The names of build projects for which information could not be found.
batchGetProjectsResponse_projectsNotFound :: Lens.Lens' BatchGetProjectsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
batchGetProjectsResponse_projectsNotFound = Lens.lens (\BatchGetProjectsResponse' {projectsNotFound} -> projectsNotFound) (\s@BatchGetProjectsResponse' {} a -> s {projectsNotFound = a} :: BatchGetProjectsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetProjectsResponse_httpStatus :: Lens.Lens' BatchGetProjectsResponse Prelude.Int
batchGetProjectsResponse_httpStatus = Lens.lens (\BatchGetProjectsResponse' {httpStatus} -> httpStatus) (\s@BatchGetProjectsResponse' {} a -> s {httpStatus = a} :: BatchGetProjectsResponse)

instance Prelude.NFData BatchGetProjectsResponse where
  rnf BatchGetProjectsResponse' {..} =
    Prelude.rnf projects
      `Prelude.seq` Prelude.rnf projectsNotFound
      `Prelude.seq` Prelude.rnf httpStatus
