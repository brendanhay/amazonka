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
-- Module      : Amazonka.SSMSAP.GetApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an application registered with AWS Systems Manager for SAP. It also
-- returns the components of the application.
module Amazonka.SSMSAP.GetApplication
  ( -- * Creating a Request
    GetApplication (..),
    newGetApplication,

    -- * Request Lenses
    getApplication_appRegistryArn,
    getApplication_applicationArn,
    getApplication_applicationId,

    -- * Destructuring the Response
    GetApplicationResponse (..),
    newGetApplicationResponse,

    -- * Response Lenses
    getApplicationResponse_application,
    getApplicationResponse_tags,
    getApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMSAP.Types

-- | /See:/ 'newGetApplication' smart constructor.
data GetApplication = GetApplication'
  { -- | The Amazon Resource Name (ARN) of the application registry.
    appRegistryArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the application.
    applicationArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the application.
    applicationId :: Prelude.Maybe Prelude.Text
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
-- 'appRegistryArn', 'getApplication_appRegistryArn' - The Amazon Resource Name (ARN) of the application registry.
--
-- 'applicationArn', 'getApplication_applicationArn' - The Amazon Resource Name (ARN) of the application.
--
-- 'applicationId', 'getApplication_applicationId' - The ID of the application.
newGetApplication ::
  GetApplication
newGetApplication =
  GetApplication'
    { appRegistryArn = Prelude.Nothing,
      applicationArn = Prelude.Nothing,
      applicationId = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the application registry.
getApplication_appRegistryArn :: Lens.Lens' GetApplication (Prelude.Maybe Prelude.Text)
getApplication_appRegistryArn = Lens.lens (\GetApplication' {appRegistryArn} -> appRegistryArn) (\s@GetApplication' {} a -> s {appRegistryArn = a} :: GetApplication)

-- | The Amazon Resource Name (ARN) of the application.
getApplication_applicationArn :: Lens.Lens' GetApplication (Prelude.Maybe Prelude.Text)
getApplication_applicationArn = Lens.lens (\GetApplication' {applicationArn} -> applicationArn) (\s@GetApplication' {} a -> s {applicationArn = a} :: GetApplication)

-- | The ID of the application.
getApplication_applicationId :: Lens.Lens' GetApplication (Prelude.Maybe Prelude.Text)
getApplication_applicationId = Lens.lens (\GetApplication' {applicationId} -> applicationId) (\s@GetApplication' {} a -> s {applicationId = a} :: GetApplication)

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
            Prelude.<$> (x Data..?> "Application")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetApplication where
  hashWithSalt _salt GetApplication' {..} =
    _salt
      `Prelude.hashWithSalt` appRegistryArn
      `Prelude.hashWithSalt` applicationArn
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData GetApplication where
  rnf GetApplication' {..} =
    Prelude.rnf appRegistryArn
      `Prelude.seq` Prelude.rnf applicationArn
      `Prelude.seq` Prelude.rnf applicationId

instance Data.ToHeaders GetApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetApplication where
  toJSON GetApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AppRegistryArn" Data..=)
              Prelude.<$> appRegistryArn,
            ("ApplicationArn" Data..=)
              Prelude.<$> applicationArn,
            ("ApplicationId" Data..=) Prelude.<$> applicationId
          ]
      )

instance Data.ToPath GetApplication where
  toPath = Prelude.const "/get-application"

instance Data.ToQuery GetApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetApplicationResponse' smart constructor.
data GetApplicationResponse = GetApplicationResponse'
  { -- | Returns all of the metadata of an application registered with AWS
    -- Systems Manager for SAP.
    application :: Prelude.Maybe Application,
    -- | The tags of a registered application.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
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
-- 'application', 'getApplicationResponse_application' - Returns all of the metadata of an application registered with AWS
-- Systems Manager for SAP.
--
-- 'tags', 'getApplicationResponse_tags' - The tags of a registered application.
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
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns all of the metadata of an application registered with AWS
-- Systems Manager for SAP.
getApplicationResponse_application :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Application)
getApplicationResponse_application = Lens.lens (\GetApplicationResponse' {application} -> application) (\s@GetApplicationResponse' {} a -> s {application = a} :: GetApplicationResponse)

-- | The tags of a registered application.
getApplicationResponse_tags :: Lens.Lens' GetApplicationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getApplicationResponse_tags = Lens.lens (\GetApplicationResponse' {tags} -> tags) (\s@GetApplicationResponse' {} a -> s {tags = a} :: GetApplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getApplicationResponse_httpStatus :: Lens.Lens' GetApplicationResponse Prelude.Int
getApplicationResponse_httpStatus = Lens.lens (\GetApplicationResponse' {httpStatus} -> httpStatus) (\s@GetApplicationResponse' {} a -> s {httpStatus = a} :: GetApplicationResponse)

instance Prelude.NFData GetApplicationResponse where
  rnf GetApplicationResponse' {..} =
    Prelude.rnf application
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
