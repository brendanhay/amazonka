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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @CreateApplication@ operation.
--
-- /See:/ 'newCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { -- | The metadata that you apply to CodeDeploy applications to help you
    -- organize and categorize them. Each tag consists of a key and an optional
    -- value, both of which you define.
    tags :: Prelude.Maybe [Tag],
    -- | The destination platform type for the deployment (@Lambda@, @Server@, or
    -- @ECS@).
    computePlatform :: Prelude.Maybe ComputePlatform,
    -- | The name of the application. This name must be unique with the
    -- applicable IAM user or AWS account.
    applicationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  CreateApplication
newCreateApplication pApplicationName_ =
  CreateApplication'
    { tags = Prelude.Nothing,
      computePlatform = Prelude.Nothing,
      applicationName = pApplicationName_
    }

-- | The metadata that you apply to CodeDeploy applications to help you
-- organize and categorize them. Each tag consists of a key and an optional
-- value, both of which you define.
createApplication_tags :: Lens.Lens' CreateApplication (Prelude.Maybe [Tag])
createApplication_tags = Lens.lens (\CreateApplication' {tags} -> tags) (\s@CreateApplication' {} a -> s {tags = a} :: CreateApplication) Prelude.. Lens.mapping Prelude._Coerce

-- | The destination platform type for the deployment (@Lambda@, @Server@, or
-- @ECS@).
createApplication_computePlatform :: Lens.Lens' CreateApplication (Prelude.Maybe ComputePlatform)
createApplication_computePlatform = Lens.lens (\CreateApplication' {computePlatform} -> computePlatform) (\s@CreateApplication' {} a -> s {computePlatform = a} :: CreateApplication)

-- | The name of the application. This name must be unique with the
-- applicable IAM user or AWS account.
createApplication_applicationName :: Lens.Lens' CreateApplication Prelude.Text
createApplication_applicationName = Lens.lens (\CreateApplication' {applicationName} -> applicationName) (\s@CreateApplication' {} a -> s {applicationName = a} :: CreateApplication)

instance Prelude.AWSRequest CreateApplication where
  type Rs CreateApplication = CreateApplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApplicationResponse'
            Prelude.<$> (x Prelude..?> "applicationId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateApplication

instance Prelude.NFData CreateApplication

instance Prelude.ToHeaders CreateApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeDeploy_20141006.CreateApplication" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateApplication where
  toJSON CreateApplication' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("tags" Prelude..=) Prelude.<$> tags,
            ("computePlatform" Prelude..=)
              Prelude.<$> computePlatform,
            Prelude.Just
              ("applicationName" Prelude..= applicationName)
          ]
      )

instance Prelude.ToPath CreateApplication where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateApplication where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @CreateApplication@ operation.
--
-- /See:/ 'newCreateApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { -- | A unique application ID.
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateApplicationResponse
newCreateApplicationResponse pHttpStatus_ =
  CreateApplicationResponse'
    { applicationId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique application ID.
createApplicationResponse_applicationId :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Prelude.Text)
createApplicationResponse_applicationId = Lens.lens (\CreateApplicationResponse' {applicationId} -> applicationId) (\s@CreateApplicationResponse' {} a -> s {applicationId = a} :: CreateApplicationResponse)

-- | The response's http status code.
createApplicationResponse_httpStatus :: Lens.Lens' CreateApplicationResponse Prelude.Int
createApplicationResponse_httpStatus = Lens.lens (\CreateApplicationResponse' {httpStatus} -> httpStatus) (\s@CreateApplicationResponse' {} a -> s {httpStatus = a} :: CreateApplicationResponse)

instance Prelude.NFData CreateApplicationResponse
