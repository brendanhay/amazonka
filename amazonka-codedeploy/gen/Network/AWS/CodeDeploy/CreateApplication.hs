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
-- Module      : Network.AWS.CodeDeploy.CreateApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application.
module Network.AWS.CodeDeploy.CreateApplication
  ( -- * Creating a Request
    CreateApplication (..),
    newCreateApplication,

    -- * Request Lenses
    createApplication_tags,
    createApplication_computePlatform,
    createApplication_applicationName,

    -- * Destructuring the Response
    CreateApplicationResponse (..),
    newCreateApplicationResponse,

    -- * Response Lenses
    createApplicationResponse_applicationId,
    createApplicationResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @CreateApplication@ operation.
--
-- /See:/ 'newCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { -- | The metadata that you apply to CodeDeploy applications to help you
    -- organize and categorize them. Each tag consists of a key and an optional
    -- value, both of which you define.
    tags :: Core.Maybe [Tag],
    -- | The destination platform type for the deployment (@Lambda@, @Server@, or
    -- @ECS@).
    computePlatform :: Core.Maybe ComputePlatform,
    -- | The name of the application. This name must be unique with the
    -- applicable IAM user or AWS account.
    applicationName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createApplication_tags' - The metadata that you apply to CodeDeploy applications to help you
-- organize and categorize them. Each tag consists of a key and an optional
-- value, both of which you define.
--
-- 'computePlatform', 'createApplication_computePlatform' - The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
--
-- 'applicationName', 'createApplication_applicationName' - The name of the application. This name must be unique with the
-- applicable IAM user or AWS account.
newCreateApplication ::
  -- | 'applicationName'
  Core.Text ->
  CreateApplication
newCreateApplication pApplicationName_ =
  CreateApplication'
    { tags = Core.Nothing,
      computePlatform = Core.Nothing,
      applicationName = pApplicationName_
    }

-- | The metadata that you apply to CodeDeploy applications to help you
-- organize and categorize them. Each tag consists of a key and an optional
-- value, both of which you define.
createApplication_tags :: Lens.Lens' CreateApplication (Core.Maybe [Tag])
createApplication_tags = Lens.lens (\CreateApplication' {tags} -> tags) (\s@CreateApplication' {} a -> s {tags = a} :: CreateApplication) Core.. Lens.mapping Lens._Coerce

-- | The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
createApplication_computePlatform :: Lens.Lens' CreateApplication (Core.Maybe ComputePlatform)
createApplication_computePlatform = Lens.lens (\CreateApplication' {computePlatform} -> computePlatform) (\s@CreateApplication' {} a -> s {computePlatform = a} :: CreateApplication)

-- | The name of the application. This name must be unique with the
-- applicable IAM user or AWS account.
createApplication_applicationName :: Lens.Lens' CreateApplication Core.Text
createApplication_applicationName = Lens.lens (\CreateApplication' {applicationName} -> applicationName) (\s@CreateApplication' {} a -> s {applicationName = a} :: CreateApplication)

instance Core.AWSRequest CreateApplication where
  type
    AWSResponse CreateApplication =
      CreateApplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApplicationResponse'
            Core.<$> (x Core..?> "applicationId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateApplication

instance Core.NFData CreateApplication

instance Core.ToHeaders CreateApplication where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.CreateApplication" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateApplication where
  toJSON CreateApplication' {..} =
    Core.object
      ( Core.catMaybes
          [ ("tags" Core..=) Core.<$> tags,
            ("computePlatform" Core..=) Core.<$> computePlatform,
            Core.Just
              ("applicationName" Core..= applicationName)
          ]
      )

instance Core.ToPath CreateApplication where
  toPath = Core.const "/"

instance Core.ToQuery CreateApplication where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @CreateApplication@ operation.
--
-- /See:/ 'newCreateApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { -- | A unique application ID.
    applicationId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'createApplicationResponse_applicationId' - A unique application ID.
--
-- 'httpStatus', 'createApplicationResponse_httpStatus' - The response's http status code.
newCreateApplicationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateApplicationResponse
newCreateApplicationResponse pHttpStatus_ =
  CreateApplicationResponse'
    { applicationId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique application ID.
createApplicationResponse_applicationId :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
createApplicationResponse_applicationId = Lens.lens (\CreateApplicationResponse' {applicationId} -> applicationId) (\s@CreateApplicationResponse' {} a -> s {applicationId = a} :: CreateApplicationResponse)

-- | The response's http status code.
createApplicationResponse_httpStatus :: Lens.Lens' CreateApplicationResponse Core.Int
createApplicationResponse_httpStatus = Lens.lens (\CreateApplicationResponse' {httpStatus} -> httpStatus) (\s@CreateApplicationResponse' {} a -> s {httpStatus = a} :: CreateApplicationResponse)

instance Core.NFData CreateApplicationResponse
