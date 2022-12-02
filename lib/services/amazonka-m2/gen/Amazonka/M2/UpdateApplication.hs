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
-- Module      : Amazonka.M2.UpdateApplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an application and creates a new version.
module Amazonka.M2.UpdateApplication
  ( -- * Creating a Request
    UpdateApplication (..),
    newUpdateApplication,

    -- * Request Lenses
    updateApplication_description,
    updateApplication_definition,
    updateApplication_applicationId,
    updateApplication_currentApplicationVersion,

    -- * Destructuring the Response
    UpdateApplicationResponse (..),
    newUpdateApplicationResponse,

    -- * Response Lenses
    updateApplicationResponse_httpStatus,
    updateApplicationResponse_applicationVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.M2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { -- | The description of the application to update.
    description :: Prelude.Maybe Prelude.Text,
    -- | The application definition for this application. You can specify either
    -- inline JSON or an S3 bucket location.
    definition :: Prelude.Maybe Definition,
    -- | The unique identifier of the application you want to update.
    applicationId :: Prelude.Text,
    -- | The current version of the application to update.
    currentApplicationVersion :: Prelude.Natural
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
-- 'description', 'updateApplication_description' - The description of the application to update.
--
-- 'definition', 'updateApplication_definition' - The application definition for this application. You can specify either
-- inline JSON or an S3 bucket location.
--
-- 'applicationId', 'updateApplication_applicationId' - The unique identifier of the application you want to update.
--
-- 'currentApplicationVersion', 'updateApplication_currentApplicationVersion' - The current version of the application to update.
newUpdateApplication ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'currentApplicationVersion'
  Prelude.Natural ->
  UpdateApplication
newUpdateApplication
  pApplicationId_
  pCurrentApplicationVersion_ =
    UpdateApplication'
      { description = Prelude.Nothing,
        definition = Prelude.Nothing,
        applicationId = pApplicationId_,
        currentApplicationVersion =
          pCurrentApplicationVersion_
      }

-- | The description of the application to update.
updateApplication_description :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_description = Lens.lens (\UpdateApplication' {description} -> description) (\s@UpdateApplication' {} a -> s {description = a} :: UpdateApplication)

-- | The application definition for this application. You can specify either
-- inline JSON or an S3 bucket location.
updateApplication_definition :: Lens.Lens' UpdateApplication (Prelude.Maybe Definition)
updateApplication_definition = Lens.lens (\UpdateApplication' {definition} -> definition) (\s@UpdateApplication' {} a -> s {definition = a} :: UpdateApplication)

-- | The unique identifier of the application you want to update.
updateApplication_applicationId :: Lens.Lens' UpdateApplication Prelude.Text
updateApplication_applicationId = Lens.lens (\UpdateApplication' {applicationId} -> applicationId) (\s@UpdateApplication' {} a -> s {applicationId = a} :: UpdateApplication)

-- | The current version of the application to update.
updateApplication_currentApplicationVersion :: Lens.Lens' UpdateApplication Prelude.Natural
updateApplication_currentApplicationVersion = Lens.lens (\UpdateApplication' {currentApplicationVersion} -> currentApplicationVersion) (\s@UpdateApplication' {} a -> s {currentApplicationVersion = a} :: UpdateApplication)

instance Core.AWSRequest UpdateApplication where
  type
    AWSResponse UpdateApplication =
      UpdateApplicationResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApplicationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "applicationVersion")
      )

instance Prelude.Hashable UpdateApplication where
  hashWithSalt _salt UpdateApplication' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` definition
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` currentApplicationVersion

instance Prelude.NFData UpdateApplication where
  rnf UpdateApplication' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf definition
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf currentApplicationVersion

instance Data.ToHeaders UpdateApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateApplication where
  toJSON UpdateApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("definition" Data..=) Prelude.<$> definition,
            Prelude.Just
              ( "currentApplicationVersion"
                  Data..= currentApplicationVersion
              )
          ]
      )

instance Data.ToPath UpdateApplication where
  toPath UpdateApplication' {..} =
    Prelude.mconcat
      ["/applications/", Data.toBS applicationId]

instance Data.ToQuery UpdateApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateApplicationResponse' smart constructor.
data UpdateApplicationResponse = UpdateApplicationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The new version of the application.
    applicationVersion :: Prelude.Natural
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
-- 'httpStatus', 'updateApplicationResponse_httpStatus' - The response's http status code.
--
-- 'applicationVersion', 'updateApplicationResponse_applicationVersion' - The new version of the application.
newUpdateApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'applicationVersion'
  Prelude.Natural ->
  UpdateApplicationResponse
newUpdateApplicationResponse
  pHttpStatus_
  pApplicationVersion_ =
    UpdateApplicationResponse'
      { httpStatus =
          pHttpStatus_,
        applicationVersion = pApplicationVersion_
      }

-- | The response's http status code.
updateApplicationResponse_httpStatus :: Lens.Lens' UpdateApplicationResponse Prelude.Int
updateApplicationResponse_httpStatus = Lens.lens (\UpdateApplicationResponse' {httpStatus} -> httpStatus) (\s@UpdateApplicationResponse' {} a -> s {httpStatus = a} :: UpdateApplicationResponse)

-- | The new version of the application.
updateApplicationResponse_applicationVersion :: Lens.Lens' UpdateApplicationResponse Prelude.Natural
updateApplicationResponse_applicationVersion = Lens.lens (\UpdateApplicationResponse' {applicationVersion} -> applicationVersion) (\s@UpdateApplicationResponse' {} a -> s {applicationVersion = a} :: UpdateApplicationResponse)

instance Prelude.NFData UpdateApplicationResponse where
  rnf UpdateApplicationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf applicationVersion
