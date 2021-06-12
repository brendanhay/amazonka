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
-- Module      : Network.AWS.Mobile.CreateProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Mobile Hub project.
module Network.AWS.Mobile.CreateProject
  ( -- * Creating a Request
    CreateProject (..),
    newCreateProject,

    -- * Request Lenses
    createProject_contents,
    createProject_name,
    createProject_snapshotId,
    createProject_region,

    -- * Destructuring the Response
    CreateProjectResponse (..),
    newCreateProjectResponse,

    -- * Response Lenses
    createProjectResponse_details,
    createProjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Mobile.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request structure used to request a project be created.
--
-- /See:/ 'newCreateProject' smart constructor.
data CreateProject = CreateProject'
  { -- | ZIP or YAML file which contains configuration settings to be used when
    -- creating the project. This may be the contents of the file downloaded
    -- from the URL provided in an export project operation.
    contents :: Core.Maybe Core.ByteString,
    -- | Name of the project.
    name :: Core.Maybe Core.Text,
    -- | Unique identifier for an exported snapshot of project configuration.
    -- This snapshot identifier is included in the share URL when a project is
    -- exported.
    snapshotId :: Core.Maybe Core.Text,
    -- | Default region where project resources should be created.
    region :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'snapshotId', 'createProject_snapshotId' - Unique identifier for an exported snapshot of project configuration.
-- This snapshot identifier is included in the share URL when a project is
-- exported.
--
-- 'region', 'createProject_region' - Default region where project resources should be created.
newCreateProject ::
  CreateProject
newCreateProject =
  CreateProject'
    { contents = Core.Nothing,
      name = Core.Nothing,
      snapshotId = Core.Nothing,
      region = Core.Nothing
    }

-- | ZIP or YAML file which contains configuration settings to be used when
-- creating the project. This may be the contents of the file downloaded
-- from the URL provided in an export project operation.
createProject_contents :: Lens.Lens' CreateProject (Core.Maybe Core.ByteString)
createProject_contents = Lens.lens (\CreateProject' {contents} -> contents) (\s@CreateProject' {} a -> s {contents = a} :: CreateProject)

-- | Name of the project.
createProject_name :: Lens.Lens' CreateProject (Core.Maybe Core.Text)
createProject_name = Lens.lens (\CreateProject' {name} -> name) (\s@CreateProject' {} a -> s {name = a} :: CreateProject)

-- | Unique identifier for an exported snapshot of project configuration.
-- This snapshot identifier is included in the share URL when a project is
-- exported.
createProject_snapshotId :: Lens.Lens' CreateProject (Core.Maybe Core.Text)
createProject_snapshotId = Lens.lens (\CreateProject' {snapshotId} -> snapshotId) (\s@CreateProject' {} a -> s {snapshotId = a} :: CreateProject)

-- | Default region where project resources should be created.
createProject_region :: Lens.Lens' CreateProject (Core.Maybe Core.Text)
createProject_region = Lens.lens (\CreateProject' {region} -> region) (\s@CreateProject' {} a -> s {region = a} :: CreateProject)

instance Core.AWSRequest CreateProject where
  type
    AWSResponse CreateProject =
      CreateProjectResponse
  request = Request.postBody defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProjectResponse'
            Core.<$> (x Core..?> "details")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateProject

instance Core.NFData CreateProject

instance Core.ToBody CreateProject where
  toBody CreateProject' {..} = Core.toBody contents

instance Core.ToHeaders CreateProject where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath CreateProject where
  toPath = Core.const "/projects"

instance Core.ToQuery CreateProject where
  toQuery CreateProject' {..} =
    Core.mconcat
      [ "name" Core.=: name,
        "snapshotId" Core.=: snapshotId,
        "region" Core.=: region
      ]

-- | Result structure used in response to a request to create a project.
--
-- /See:/ 'newCreateProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { -- | Detailed information about the created AWS Mobile Hub project.
    details :: Core.Maybe ProjectDetails,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateProjectResponse
newCreateProjectResponse pHttpStatus_ =
  CreateProjectResponse'
    { details = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Detailed information about the created AWS Mobile Hub project.
createProjectResponse_details :: Lens.Lens' CreateProjectResponse (Core.Maybe ProjectDetails)
createProjectResponse_details = Lens.lens (\CreateProjectResponse' {details} -> details) (\s@CreateProjectResponse' {} a -> s {details = a} :: CreateProjectResponse)

-- | The response's http status code.
createProjectResponse_httpStatus :: Lens.Lens' CreateProjectResponse Core.Int
createProjectResponse_httpStatus = Lens.lens (\CreateProjectResponse' {httpStatus} -> httpStatus) (\s@CreateProjectResponse' {} a -> s {httpStatus = a} :: CreateProjectResponse)

instance Core.NFData CreateProjectResponse
