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
-- Module      : Amazonka.CodeDeploy.GetApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an application.
module Amazonka.CodeDeploy.GetApplication
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

import Amazonka.CodeDeploy.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @GetApplication@ operation.
--
-- /See:/ 'newGetApplication' smart constructor.
data GetApplication = GetApplication'
  { -- | The name of an CodeDeploy application associated with the IAM user or
    -- Amazon Web Services account.
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
-- 'applicationName', 'getApplication_applicationName' - The name of an CodeDeploy application associated with the IAM user or
-- Amazon Web Services account.
newGetApplication ::
  -- | 'applicationName'
  Prelude.Text ->
  GetApplication
newGetApplication pApplicationName_ =
  GetApplication'
    { applicationName =
        pApplicationName_
    }

-- | The name of an CodeDeploy application associated with the IAM user or
-- Amazon Web Services account.
getApplication_applicationName :: Lens.Lens' GetApplication Prelude.Text
getApplication_applicationName = Lens.lens (\GetApplication' {applicationName} -> applicationName) (\s@GetApplication' {} a -> s {applicationName = a} :: GetApplication)

instance Core.AWSRequest GetApplication where
  type
    AWSResponse GetApplication =
      GetApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApplicationResponse'
            Prelude.<$> (x Data..?> "application")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetApplication where
  hashWithSalt _salt GetApplication' {..} =
    _salt `Prelude.hashWithSalt` applicationName

instance Prelude.NFData GetApplication where
  rnf GetApplication' {..} = Prelude.rnf applicationName

instance Data.ToHeaders GetApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeDeploy_20141006.GetApplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetApplication where
  toJSON GetApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("applicationName" Data..= applicationName)
          ]
      )

instance Data.ToPath GetApplication where
  toPath = Prelude.const "/"

instance Data.ToQuery GetApplication where
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

instance Prelude.NFData GetApplicationResponse where
  rnf GetApplicationResponse' {..} =
    Prelude.rnf application
      `Prelude.seq` Prelude.rnf httpStatus
