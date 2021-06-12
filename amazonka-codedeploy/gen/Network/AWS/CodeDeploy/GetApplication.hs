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
-- Module      : Network.AWS.CodeDeploy.GetApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an application.
module Network.AWS.CodeDeploy.GetApplication
  ( -- * Creating a Request
    GetApplication (..),
    newGetApplication,

    -- * Request Lenses
    getApplication_applicationName,

    -- * Destructuring the Response
    GetApplicationResponse (..),
    newGetApplicationResponse,

    -- * Response Lenses
    getApplicationResponse_application,
    getApplicationResponse_httpStatus,
  )
where

import Network.AWS.CodeDeploy.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetApplication@ operation.
--
-- /See:/ 'newGetApplication' smart constructor.
data GetApplication = GetApplication'
  { -- | The name of an AWS CodeDeploy application associated with the IAM user
    -- or AWS account.
    applicationName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationName', 'getApplication_applicationName' - The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
newGetApplication ::
  -- | 'applicationName'
  Core.Text ->
  GetApplication
newGetApplication pApplicationName_ =
  GetApplication'
    { applicationName =
        pApplicationName_
    }

-- | The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
getApplication_applicationName :: Lens.Lens' GetApplication Core.Text
getApplication_applicationName = Lens.lens (\GetApplication' {applicationName} -> applicationName) (\s@GetApplication' {} a -> s {applicationName = a} :: GetApplication)

instance Core.AWSRequest GetApplication where
  type
    AWSResponse GetApplication =
      GetApplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApplicationResponse'
            Core.<$> (x Core..?> "application")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetApplication

instance Core.NFData GetApplication

instance Core.ToHeaders GetApplication where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.GetApplication" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetApplication where
  toJSON GetApplication' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("applicationName" Core..= applicationName)
          ]
      )

instance Core.ToPath GetApplication where
  toPath = Core.const "/"

instance Core.ToQuery GetApplication where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @GetApplication@ operation.
--
-- /See:/ 'newGetApplicationResponse' smart constructor.
data GetApplicationResponse = GetApplicationResponse'
  { -- | Information about the application.
    application :: Core.Maybe ApplicationInfo,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'application', 'getApplicationResponse_application' - Information about the application.
--
-- 'httpStatus', 'getApplicationResponse_httpStatus' - The response's http status code.
newGetApplicationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetApplicationResponse
newGetApplicationResponse pHttpStatus_ =
  GetApplicationResponse'
    { application = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the application.
getApplicationResponse_application :: Lens.Lens' GetApplicationResponse (Core.Maybe ApplicationInfo)
getApplicationResponse_application = Lens.lens (\GetApplicationResponse' {application} -> application) (\s@GetApplicationResponse' {} a -> s {application = a} :: GetApplicationResponse)

-- | The response's http status code.
getApplicationResponse_httpStatus :: Lens.Lens' GetApplicationResponse Core.Int
getApplicationResponse_httpStatus = Lens.lens (\GetApplicationResponse' {httpStatus} -> httpStatus) (\s@GetApplicationResponse' {} a -> s {httpStatus = a} :: GetApplicationResponse)

instance Core.NFData GetApplicationResponse
