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
-- Module      : Amazonka.Mobile.CreateProject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Mobile Hub project.
module Amazonka.Mobile.CreateProject
  ( -- * Creating a Request
    CreateProject (..),
    newCreateProject,

    -- * Request Lenses
    createProject_contents,
    createProject_name,
    createProject_region,
    createProject_snapshotId,

    -- * Destructuring the Response
    CreateProjectResponse (..),
    newCreateProjectResponse,

    -- * Response Lenses
    createProjectResponse_details,
    createProjectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Mobile.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request structure used to request a project be created.
--
-- /See:/ 'newCreateProject' smart constructor.
data CreateProject = CreateProject'
  { -- | ZIP or YAML file which contains configuration settings to be used when
    -- creating the project. This may be the contents of the file downloaded
    -- from the URL provided in an export project operation.
    contents :: Prelude.Maybe Prelude.ByteString,
    -- | Name of the project.
    name :: Prelude.Maybe Prelude.Text,
    -- | Default region where project resources should be created.
    region :: Prelude.Maybe Prelude.Text,
    -- | Unique identifier for an exported snapshot of project configuration.
    -- This snapshot identifier is included in the share URL when a project is
    -- exported.
    snapshotId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contents', 'createProject_contents' - ZIP or YAML file which contains configuration settings to be used when
-- creating the project. This may be the contents of the file downloaded
-- from the URL provided in an export project operation.
--
-- 'name', 'createProject_name' - Name of the project.
--
-- 'region', 'createProject_region' - Default region where project resources should be created.
--
-- 'snapshotId', 'createProject_snapshotId' - Unique identifier for an exported snapshot of project configuration.
-- This snapshot identifier is included in the share URL when a project is
-- exported.
newCreateProject ::
  CreateProject
newCreateProject =
  CreateProject'
    { contents = Prelude.Nothing,
      name = Prelude.Nothing,
      region = Prelude.Nothing,
      snapshotId = Prelude.Nothing
    }

-- | ZIP or YAML file which contains configuration settings to be used when
-- creating the project. This may be the contents of the file downloaded
-- from the URL provided in an export project operation.
createProject_contents :: Lens.Lens' CreateProject (Prelude.Maybe Prelude.ByteString)
createProject_contents = Lens.lens (\CreateProject' {contents} -> contents) (\s@CreateProject' {} a -> s {contents = a} :: CreateProject)

-- | Name of the project.
createProject_name :: Lens.Lens' CreateProject (Prelude.Maybe Prelude.Text)
createProject_name = Lens.lens (\CreateProject' {name} -> name) (\s@CreateProject' {} a -> s {name = a} :: CreateProject)

-- | Default region where project resources should be created.
createProject_region :: Lens.Lens' CreateProject (Prelude.Maybe Prelude.Text)
createProject_region = Lens.lens (\CreateProject' {region} -> region) (\s@CreateProject' {} a -> s {region = a} :: CreateProject)

-- | Unique identifier for an exported snapshot of project configuration.
-- This snapshot identifier is included in the share URL when a project is
-- exported.
createProject_snapshotId :: Lens.Lens' CreateProject (Prelude.Maybe Prelude.Text)
createProject_snapshotId = Lens.lens (\CreateProject' {snapshotId} -> snapshotId) (\s@CreateProject' {} a -> s {snapshotId = a} :: CreateProject)

instance Core.AWSRequest CreateProject where
  type
    AWSResponse CreateProject =
      CreateProjectResponse
  request overrides =
    Request.postBody (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProjectResponse'
            Prelude.<$> (x Data..?> "details")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProject where
  hashWithSalt _salt CreateProject' {..} =
    _salt
      `Prelude.hashWithSalt` contents
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` snapshotId

instance Prelude.NFData CreateProject where
  rnf CreateProject' {..} =
    Prelude.rnf contents
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf snapshotId

instance Data.ToBody CreateProject where
  toBody CreateProject' {..} = Data.toBody contents

instance Data.ToHeaders CreateProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath CreateProject where
  toPath = Prelude.const "/projects"

instance Data.ToQuery CreateProject where
  toQuery CreateProject' {..} =
    Prelude.mconcat
      [ "name" Data.=: name,
        "region" Data.=: region,
        "snapshotId" Data.=: snapshotId
      ]

-- | Result structure used in response to a request to create a project.
--
-- /See:/ 'newCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { -- | Detailed information about the created AWS Mobile Hub project.
    details :: Prelude.Maybe ProjectDetails,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'createProjectResponse_details' - Detailed information about the created AWS Mobile Hub project.
--
-- 'httpStatus', 'createProjectResponse_httpStatus' - The response's http status code.
newCreateProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProjectResponse
newCreateProjectResponse pHttpStatus_ =
  CreateProjectResponse'
    { details = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Detailed information about the created AWS Mobile Hub project.
createProjectResponse_details :: Lens.Lens' CreateProjectResponse (Prelude.Maybe ProjectDetails)
createProjectResponse_details = Lens.lens (\CreateProjectResponse' {details} -> details) (\s@CreateProjectResponse' {} a -> s {details = a} :: CreateProjectResponse)

-- | The response's http status code.
createProjectResponse_httpStatus :: Lens.Lens' CreateProjectResponse Prelude.Int
createProjectResponse_httpStatus = Lens.lens (\CreateProjectResponse' {httpStatus} -> httpStatus) (\s@CreateProjectResponse' {} a -> s {httpStatus = a} :: CreateProjectResponse)

instance Prelude.NFData CreateProjectResponse where
  rnf CreateProjectResponse' {..} =
    Prelude.rnf details
      `Prelude.seq` Prelude.rnf httpStatus
