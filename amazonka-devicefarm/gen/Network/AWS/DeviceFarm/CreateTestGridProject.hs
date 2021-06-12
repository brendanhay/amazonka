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
-- Module      : Network.AWS.DeviceFarm.CreateTestGridProject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Selenium testing project. Projects are used to track
-- TestGridSession instances.
module Network.AWS.DeviceFarm.CreateTestGridProject
  ( -- * Creating a Request
    CreateTestGridProject (..),
    newCreateTestGridProject,

    -- * Request Lenses
    createTestGridProject_description,
    createTestGridProject_name,

    -- * Destructuring the Response
    CreateTestGridProjectResponse (..),
    newCreateTestGridProjectResponse,

    -- * Response Lenses
    createTestGridProjectResponse_testGridProject,
    createTestGridProjectResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTestGridProject' smart constructor.
data CreateTestGridProject = CreateTestGridProject'
  { -- | Human-readable description of the project.
    description :: Core.Maybe Core.Text,
    -- | Human-readable name of the Selenium testing project.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTestGridProject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createTestGridProject_description' - Human-readable description of the project.
--
-- 'name', 'createTestGridProject_name' - Human-readable name of the Selenium testing project.
newCreateTestGridProject ::
  -- | 'name'
  Core.Text ->
  CreateTestGridProject
newCreateTestGridProject pName_ =
  CreateTestGridProject'
    { description = Core.Nothing,
      name = pName_
    }

-- | Human-readable description of the project.
createTestGridProject_description :: Lens.Lens' CreateTestGridProject (Core.Maybe Core.Text)
createTestGridProject_description = Lens.lens (\CreateTestGridProject' {description} -> description) (\s@CreateTestGridProject' {} a -> s {description = a} :: CreateTestGridProject)

-- | Human-readable name of the Selenium testing project.
createTestGridProject_name :: Lens.Lens' CreateTestGridProject Core.Text
createTestGridProject_name = Lens.lens (\CreateTestGridProject' {name} -> name) (\s@CreateTestGridProject' {} a -> s {name = a} :: CreateTestGridProject)

instance Core.AWSRequest CreateTestGridProject where
  type
    AWSResponse CreateTestGridProject =
      CreateTestGridProjectResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTestGridProjectResponse'
            Core.<$> (x Core..?> "testGridProject")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateTestGridProject

instance Core.NFData CreateTestGridProject

instance Core.ToHeaders CreateTestGridProject where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.CreateTestGridProject" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateTestGridProject where
  toJSON CreateTestGridProject' {..} =
    Core.object
      ( Core.catMaybes
          [ ("description" Core..=) Core.<$> description,
            Core.Just ("name" Core..= name)
          ]
      )

instance Core.ToPath CreateTestGridProject where
  toPath = Core.const "/"

instance Core.ToQuery CreateTestGridProject where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateTestGridProjectResponse' smart constructor.
data CreateTestGridProjectResponse = CreateTestGridProjectResponse'
  { -- | ARN of the Selenium testing project that was created.
    testGridProject :: Core.Maybe TestGridProject,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateTestGridProjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testGridProject', 'createTestGridProjectResponse_testGridProject' - ARN of the Selenium testing project that was created.
--
-- 'httpStatus', 'createTestGridProjectResponse_httpStatus' - The response's http status code.
newCreateTestGridProjectResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateTestGridProjectResponse
newCreateTestGridProjectResponse pHttpStatus_ =
  CreateTestGridProjectResponse'
    { testGridProject =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | ARN of the Selenium testing project that was created.
createTestGridProjectResponse_testGridProject :: Lens.Lens' CreateTestGridProjectResponse (Core.Maybe TestGridProject)
createTestGridProjectResponse_testGridProject = Lens.lens (\CreateTestGridProjectResponse' {testGridProject} -> testGridProject) (\s@CreateTestGridProjectResponse' {} a -> s {testGridProject = a} :: CreateTestGridProjectResponse)

-- | The response's http status code.
createTestGridProjectResponse_httpStatus :: Lens.Lens' CreateTestGridProjectResponse Core.Int
createTestGridProjectResponse_httpStatus = Lens.lens (\CreateTestGridProjectResponse' {httpStatus} -> httpStatus) (\s@CreateTestGridProjectResponse' {} a -> s {httpStatus = a} :: CreateTestGridProjectResponse)

instance Core.NFData CreateTestGridProjectResponse
