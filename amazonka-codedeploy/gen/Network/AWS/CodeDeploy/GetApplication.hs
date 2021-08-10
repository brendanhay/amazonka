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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @GetApplication@ operation.
--
-- /See:/ 'newGetApplication' smart constructor.
data GetApplication = GetApplication'
  { -- | The name of an AWS CodeDeploy application associated with the IAM user
    -- or AWS account.
    applicationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetApplication
newGetApplication pApplicationName_ =
  GetApplication'
    { applicationName =
        pApplicationName_
    }

-- | The name of an AWS CodeDeploy application associated with the IAM user
-- or AWS account.
getApplication_applicationName :: Lens.Lens' GetApplication Prelude.Text
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
            Prelude.<$> (x Core..?> "application")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetApplication

instance Prelude.NFData GetApplication

instance Core.ToHeaders GetApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeDeploy_20141006.GetApplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetApplication where
  toJSON GetApplication' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("applicationName" Core..= applicationName)
          ]
      )

instance Core.ToPath GetApplication where
  toPath = Prelude.const "/"

instance Core.ToQuery GetApplication where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @GetApplication@ operation.
--
-- /See:/ 'newGetApplicationResponse' smart constructor.
data GetApplicationResponse = GetApplicationResponse'
  { -- | Information about the application.
    application :: Prelude.Maybe ApplicationInfo,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetApplicationResponse
newGetApplicationResponse pHttpStatus_ =
  GetApplicationResponse'
    { application =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the application.
getApplicationResponse_application :: Lens.Lens' GetApplicationResponse (Prelude.Maybe ApplicationInfo)
getApplicationResponse_application = Lens.lens (\GetApplicationResponse' {application} -> application) (\s@GetApplicationResponse' {} a -> s {application = a} :: GetApplicationResponse)

-- | The response's http status code.
getApplicationResponse_httpStatus :: Lens.Lens' GetApplicationResponse Prelude.Int
getApplicationResponse_httpStatus = Lens.lens (\GetApplicationResponse' {httpStatus} -> httpStatus) (\s@GetApplicationResponse' {} a -> s {httpStatus = a} :: GetApplicationResponse)

instance Prelude.NFData GetApplicationResponse
