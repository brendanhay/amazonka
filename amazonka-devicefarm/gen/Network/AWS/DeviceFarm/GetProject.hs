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
-- Module      : Network.AWS.DeviceFarm.GetProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a project.
module Network.AWS.DeviceFarm.GetProject
  ( -- * Creating a Request
    GetProject (..),
    newGetProject,

    -- * Request Lenses
    getProject_arn,

    -- * Destructuring the Response
    GetProjectResponse (..),
    newGetProjectResponse,

    -- * Response Lenses
    getProjectResponse_project,
    getProjectResponse_httpStatus,
  )
where

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the get project operation.
--
-- /See:/ 'newGetProject' smart constructor.
data GetProject = GetProject'
  { -- | The project\'s ARN.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getProject_arn' - The project\'s ARN.
newGetProject ::
  -- | 'arn'
  Prelude.Text ->
  GetProject
newGetProject pArn_ = GetProject' {arn = pArn_}

-- | The project\'s ARN.
getProject_arn :: Lens.Lens' GetProject Prelude.Text
getProject_arn = Lens.lens (\GetProject' {arn} -> arn) (\s@GetProject' {} a -> s {arn = a} :: GetProject)

instance Prelude.AWSRequest GetProject where
  type Rs GetProject = GetProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProjectResponse'
            Prelude.<$> (x Prelude..?> "project")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetProject

instance Prelude.NFData GetProject

instance Prelude.ToHeaders GetProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DeviceFarm_20150623.GetProject" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetProject where
  toJSON GetProject' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Prelude..= arn)]
      )

instance Prelude.ToPath GetProject where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetProject where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of a get project request.
--
-- /See:/ 'newGetProjectResponse' smart constructor.
data GetProjectResponse = GetProjectResponse'
  { -- | The project to get information about.
    project :: Prelude.Maybe Project,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'project', 'getProjectResponse_project' - The project to get information about.
--
-- 'httpStatus', 'getProjectResponse_httpStatus' - The response's http status code.
newGetProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetProjectResponse
newGetProjectResponse pHttpStatus_ =
  GetProjectResponse'
    { project = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The project to get information about.
getProjectResponse_project :: Lens.Lens' GetProjectResponse (Prelude.Maybe Project)
getProjectResponse_project = Lens.lens (\GetProjectResponse' {project} -> project) (\s@GetProjectResponse' {} a -> s {project = a} :: GetProjectResponse)

-- | The response's http status code.
getProjectResponse_httpStatus :: Lens.Lens' GetProjectResponse Prelude.Int
getProjectResponse_httpStatus = Lens.lens (\GetProjectResponse' {httpStatus} -> httpStatus) (\s@GetProjectResponse' {} a -> s {httpStatus = a} :: GetProjectResponse)

instance Prelude.NFData GetProjectResponse
