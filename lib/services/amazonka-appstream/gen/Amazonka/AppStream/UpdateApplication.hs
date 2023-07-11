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
-- Module      : Amazonka.AppStream.UpdateApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified application.
module Amazonka.AppStream.UpdateApplication
  ( -- * Creating a Request
    UpdateApplication (..),
    newUpdateApplication,

    -- * Request Lenses
    updateApplication_appBlockArn,
    updateApplication_attributesToDelete,
    updateApplication_description,
    updateApplication_displayName,
    updateApplication_iconS3Location,
    updateApplication_launchParameters,
    updateApplication_launchPath,
    updateApplication_workingDirectory,
    updateApplication_name,

    -- * Destructuring the Response
    UpdateApplicationResponse (..),
    newUpdateApplicationResponse,

    -- * Response Lenses
    updateApplicationResponse_application,
    updateApplicationResponse_httpStatus,
  )
where

import Amazonka.AppStream.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { -- | The ARN of the app block.
    appBlockArn :: Prelude.Maybe Prelude.Text,
    -- | The attributes to delete for an application.
    attributesToDelete :: Prelude.Maybe [ApplicationAttribute],
    -- | The description of the application.
    description :: Prelude.Maybe Prelude.Text,
    -- | The display name of the application. This name is visible to users in
    -- the application catalog.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The icon S3 location of the application.
    iconS3Location :: Prelude.Maybe S3Location,
    -- | The launch parameters of the application.
    launchParameters :: Prelude.Maybe Prelude.Text,
    -- | The launch path of the application.
    launchPath :: Prelude.Maybe Prelude.Text,
    -- | The working directory of the application.
    workingDirectory :: Prelude.Maybe Prelude.Text,
    -- | The name of the application. This name is visible to users when display
    -- name is not specified.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appBlockArn', 'updateApplication_appBlockArn' - The ARN of the app block.
--
-- 'attributesToDelete', 'updateApplication_attributesToDelete' - The attributes to delete for an application.
--
-- 'description', 'updateApplication_description' - The description of the application.
--
-- 'displayName', 'updateApplication_displayName' - The display name of the application. This name is visible to users in
-- the application catalog.
--
-- 'iconS3Location', 'updateApplication_iconS3Location' - The icon S3 location of the application.
--
-- 'launchParameters', 'updateApplication_launchParameters' - The launch parameters of the application.
--
-- 'launchPath', 'updateApplication_launchPath' - The launch path of the application.
--
-- 'workingDirectory', 'updateApplication_workingDirectory' - The working directory of the application.
--
-- 'name', 'updateApplication_name' - The name of the application. This name is visible to users when display
-- name is not specified.
newUpdateApplication ::
  -- | 'name'
  Prelude.Text ->
  UpdateApplication
newUpdateApplication pName_ =
  UpdateApplication'
    { appBlockArn = Prelude.Nothing,
      attributesToDelete = Prelude.Nothing,
      description = Prelude.Nothing,
      displayName = Prelude.Nothing,
      iconS3Location = Prelude.Nothing,
      launchParameters = Prelude.Nothing,
      launchPath = Prelude.Nothing,
      workingDirectory = Prelude.Nothing,
      name = pName_
    }

-- | The ARN of the app block.
updateApplication_appBlockArn :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_appBlockArn = Lens.lens (\UpdateApplication' {appBlockArn} -> appBlockArn) (\s@UpdateApplication' {} a -> s {appBlockArn = a} :: UpdateApplication)

-- | The attributes to delete for an application.
updateApplication_attributesToDelete :: Lens.Lens' UpdateApplication (Prelude.Maybe [ApplicationAttribute])
updateApplication_attributesToDelete = Lens.lens (\UpdateApplication' {attributesToDelete} -> attributesToDelete) (\s@UpdateApplication' {} a -> s {attributesToDelete = a} :: UpdateApplication) Prelude.. Lens.mapping Lens.coerced

-- | The description of the application.
updateApplication_description :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_description = Lens.lens (\UpdateApplication' {description} -> description) (\s@UpdateApplication' {} a -> s {description = a} :: UpdateApplication)

-- | The display name of the application. This name is visible to users in
-- the application catalog.
updateApplication_displayName :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_displayName = Lens.lens (\UpdateApplication' {displayName} -> displayName) (\s@UpdateApplication' {} a -> s {displayName = a} :: UpdateApplication)

-- | The icon S3 location of the application.
updateApplication_iconS3Location :: Lens.Lens' UpdateApplication (Prelude.Maybe S3Location)
updateApplication_iconS3Location = Lens.lens (\UpdateApplication' {iconS3Location} -> iconS3Location) (\s@UpdateApplication' {} a -> s {iconS3Location = a} :: UpdateApplication)

-- | The launch parameters of the application.
updateApplication_launchParameters :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_launchParameters = Lens.lens (\UpdateApplication' {launchParameters} -> launchParameters) (\s@UpdateApplication' {} a -> s {launchParameters = a} :: UpdateApplication)

-- | The launch path of the application.
updateApplication_launchPath :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_launchPath = Lens.lens (\UpdateApplication' {launchPath} -> launchPath) (\s@UpdateApplication' {} a -> s {launchPath = a} :: UpdateApplication)

-- | The working directory of the application.
updateApplication_workingDirectory :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_workingDirectory = Lens.lens (\UpdateApplication' {workingDirectory} -> workingDirectory) (\s@UpdateApplication' {} a -> s {workingDirectory = a} :: UpdateApplication)

-- | The name of the application. This name is visible to users when display
-- name is not specified.
updateApplication_name :: Lens.Lens' UpdateApplication Prelude.Text
updateApplication_name = Lens.lens (\UpdateApplication' {name} -> name) (\s@UpdateApplication' {} a -> s {name = a} :: UpdateApplication)

instance Core.AWSRequest UpdateApplication where
  type
    AWSResponse UpdateApplication =
      UpdateApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApplicationResponse'
            Prelude.<$> (x Data..?> "Application")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateApplication where
  hashWithSalt _salt UpdateApplication' {..} =
    _salt
      `Prelude.hashWithSalt` appBlockArn
      `Prelude.hashWithSalt` attributesToDelete
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` iconS3Location
      `Prelude.hashWithSalt` launchParameters
      `Prelude.hashWithSalt` launchPath
      `Prelude.hashWithSalt` workingDirectory
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateApplication where
  rnf UpdateApplication' {..} =
    Prelude.rnf appBlockArn
      `Prelude.seq` Prelude.rnf attributesToDelete
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf iconS3Location
      `Prelude.seq` Prelude.rnf launchParameters
      `Prelude.seq` Prelude.rnf launchPath
      `Prelude.seq` Prelude.rnf workingDirectory
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "PhotonAdminProxyService.UpdateApplication" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateApplication where
  toJSON UpdateApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AppBlockArn" Data..=) Prelude.<$> appBlockArn,
            ("AttributesToDelete" Data..=)
              Prelude.<$> attributesToDelete,
            ("Description" Data..=) Prelude.<$> description,
            ("DisplayName" Data..=) Prelude.<$> displayName,
            ("IconS3Location" Data..=)
              Prelude.<$> iconS3Location,
            ("LaunchParameters" Data..=)
              Prelude.<$> launchParameters,
            ("LaunchPath" Data..=) Prelude.<$> launchPath,
            ("WorkingDirectory" Data..=)
              Prelude.<$> workingDirectory,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath UpdateApplication where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateApplicationResponse' smart constructor.
data UpdateApplicationResponse = UpdateApplicationResponse'
  { application :: Prelude.Maybe Application,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'application', 'updateApplicationResponse_application' - Undocumented member.
--
-- 'httpStatus', 'updateApplicationResponse_httpStatus' - The response's http status code.
newUpdateApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateApplicationResponse
newUpdateApplicationResponse pHttpStatus_ =
  UpdateApplicationResponse'
    { application =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateApplicationResponse_application :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Application)
updateApplicationResponse_application = Lens.lens (\UpdateApplicationResponse' {application} -> application) (\s@UpdateApplicationResponse' {} a -> s {application = a} :: UpdateApplicationResponse)

-- | The response's http status code.
updateApplicationResponse_httpStatus :: Lens.Lens' UpdateApplicationResponse Prelude.Int
updateApplicationResponse_httpStatus = Lens.lens (\UpdateApplicationResponse' {httpStatus} -> httpStatus) (\s@UpdateApplicationResponse' {} a -> s {httpStatus = a} :: UpdateApplicationResponse)

instance Prelude.NFData UpdateApplicationResponse where
  rnf UpdateApplicationResponse' {..} =
    Prelude.rnf application
      `Prelude.seq` Prelude.rnf httpStatus
