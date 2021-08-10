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
-- Module      : Network.AWS.DeviceFarm.GetTestGridProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a Selenium testing project.
module Network.AWS.DeviceFarm.GetTestGridProject
  ( -- * Creating a Request
    GetTestGridProject (..),
    newGetTestGridProject,

    -- * Request Lenses
    getTestGridProject_projectArn,

    -- * Destructuring the Response
    GetTestGridProjectResponse (..),
    newGetTestGridProjectResponse,

    -- * Response Lenses
    getTestGridProjectResponse_testGridProject,
    getTestGridProjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTestGridProject' smart constructor.
data GetTestGridProject = GetTestGridProject'
  { -- | The ARN of the Selenium testing project, from either
    -- CreateTestGridProject or ListTestGridProjects.
    projectArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTestGridProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectArn', 'getTestGridProject_projectArn' - The ARN of the Selenium testing project, from either
-- CreateTestGridProject or ListTestGridProjects.
newGetTestGridProject ::
  -- | 'projectArn'
  Prelude.Text ->
  GetTestGridProject
newGetTestGridProject pProjectArn_ =
  GetTestGridProject' {projectArn = pProjectArn_}

-- | The ARN of the Selenium testing project, from either
-- CreateTestGridProject or ListTestGridProjects.
getTestGridProject_projectArn :: Lens.Lens' GetTestGridProject Prelude.Text
getTestGridProject_projectArn = Lens.lens (\GetTestGridProject' {projectArn} -> projectArn) (\s@GetTestGridProject' {} a -> s {projectArn = a} :: GetTestGridProject)

instance Core.AWSRequest GetTestGridProject where
  type
    AWSResponse GetTestGridProject =
      GetTestGridProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTestGridProjectResponse'
            Prelude.<$> (x Core..?> "testGridProject")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTestGridProject

instance Prelude.NFData GetTestGridProject

instance Core.ToHeaders GetTestGridProject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.GetTestGridProject" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetTestGridProject where
  toJSON GetTestGridProject' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("projectArn" Core..= projectArn)]
      )

instance Core.ToPath GetTestGridProject where
  toPath = Prelude.const "/"

instance Core.ToQuery GetTestGridProject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTestGridProjectResponse' smart constructor.
data GetTestGridProjectResponse = GetTestGridProjectResponse'
  { -- | A TestGridProject.
    testGridProject :: Prelude.Maybe TestGridProject,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTestGridProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testGridProject', 'getTestGridProjectResponse_testGridProject' - A TestGridProject.
--
-- 'httpStatus', 'getTestGridProjectResponse_httpStatus' - The response's http status code.
newGetTestGridProjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTestGridProjectResponse
newGetTestGridProjectResponse pHttpStatus_ =
  GetTestGridProjectResponse'
    { testGridProject =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A TestGridProject.
getTestGridProjectResponse_testGridProject :: Lens.Lens' GetTestGridProjectResponse (Prelude.Maybe TestGridProject)
getTestGridProjectResponse_testGridProject = Lens.lens (\GetTestGridProjectResponse' {testGridProject} -> testGridProject) (\s@GetTestGridProjectResponse' {} a -> s {testGridProject = a} :: GetTestGridProjectResponse)

-- | The response's http status code.
getTestGridProjectResponse_httpStatus :: Lens.Lens' GetTestGridProjectResponse Prelude.Int
getTestGridProjectResponse_httpStatus = Lens.lens (\GetTestGridProjectResponse' {httpStatus} -> httpStatus) (\s@GetTestGridProjectResponse' {} a -> s {httpStatus = a} :: GetTestGridProjectResponse)

instance Prelude.NFData GetTestGridProjectResponse
