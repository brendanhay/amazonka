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
-- Module      : Amazonka.Proton.UpdateEnvironmentTemplateVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a major or minor version of an environment template.
module Amazonka.Proton.UpdateEnvironmentTemplateVersion
  ( -- * Creating a Request
    UpdateEnvironmentTemplateVersion (..),
    newUpdateEnvironmentTemplateVersion,

    -- * Request Lenses
    updateEnvironmentTemplateVersion_status,
    updateEnvironmentTemplateVersion_description,
    updateEnvironmentTemplateVersion_majorVersion,
    updateEnvironmentTemplateVersion_minorVersion,
    updateEnvironmentTemplateVersion_templateName,

    -- * Destructuring the Response
    UpdateEnvironmentTemplateVersionResponse (..),
    newUpdateEnvironmentTemplateVersionResponse,

    -- * Response Lenses
    updateEnvironmentTemplateVersionResponse_httpStatus,
    updateEnvironmentTemplateVersionResponse_environmentTemplateVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEnvironmentTemplateVersion' smart constructor.
data UpdateEnvironmentTemplateVersion = UpdateEnvironmentTemplateVersion'
  { -- | The status of the environment template minor version to update.
    status :: Prelude.Maybe TemplateVersionStatus,
    -- | A description of environment template version to update.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | To update a major version of an environment template, include
    -- @major Version@.
    majorVersion :: Prelude.Text,
    -- | To update a minor version of an environment template, include
    -- @minorVersion@.
    minorVersion :: Prelude.Text,
    -- | The name of the environment template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEnvironmentTemplateVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'updateEnvironmentTemplateVersion_status' - The status of the environment template minor version to update.
--
-- 'description', 'updateEnvironmentTemplateVersion_description' - A description of environment template version to update.
--
-- 'majorVersion', 'updateEnvironmentTemplateVersion_majorVersion' - To update a major version of an environment template, include
-- @major Version@.
--
-- 'minorVersion', 'updateEnvironmentTemplateVersion_minorVersion' - To update a minor version of an environment template, include
-- @minorVersion@.
--
-- 'templateName', 'updateEnvironmentTemplateVersion_templateName' - The name of the environment template.
newUpdateEnvironmentTemplateVersion ::
  -- | 'majorVersion'
  Prelude.Text ->
  -- | 'minorVersion'
  Prelude.Text ->
  -- | 'templateName'
  Prelude.Text ->
  UpdateEnvironmentTemplateVersion
newUpdateEnvironmentTemplateVersion
  pMajorVersion_
  pMinorVersion_
  pTemplateName_ =
    UpdateEnvironmentTemplateVersion'
      { status =
          Prelude.Nothing,
        description = Prelude.Nothing,
        majorVersion = pMajorVersion_,
        minorVersion = pMinorVersion_,
        templateName = pTemplateName_
      }

-- | The status of the environment template minor version to update.
updateEnvironmentTemplateVersion_status :: Lens.Lens' UpdateEnvironmentTemplateVersion (Prelude.Maybe TemplateVersionStatus)
updateEnvironmentTemplateVersion_status = Lens.lens (\UpdateEnvironmentTemplateVersion' {status} -> status) (\s@UpdateEnvironmentTemplateVersion' {} a -> s {status = a} :: UpdateEnvironmentTemplateVersion)

-- | A description of environment template version to update.
updateEnvironmentTemplateVersion_description :: Lens.Lens' UpdateEnvironmentTemplateVersion (Prelude.Maybe Prelude.Text)
updateEnvironmentTemplateVersion_description = Lens.lens (\UpdateEnvironmentTemplateVersion' {description} -> description) (\s@UpdateEnvironmentTemplateVersion' {} a -> s {description = a} :: UpdateEnvironmentTemplateVersion) Prelude.. Lens.mapping Data._Sensitive

-- | To update a major version of an environment template, include
-- @major Version@.
updateEnvironmentTemplateVersion_majorVersion :: Lens.Lens' UpdateEnvironmentTemplateVersion Prelude.Text
updateEnvironmentTemplateVersion_majorVersion = Lens.lens (\UpdateEnvironmentTemplateVersion' {majorVersion} -> majorVersion) (\s@UpdateEnvironmentTemplateVersion' {} a -> s {majorVersion = a} :: UpdateEnvironmentTemplateVersion)

-- | To update a minor version of an environment template, include
-- @minorVersion@.
updateEnvironmentTemplateVersion_minorVersion :: Lens.Lens' UpdateEnvironmentTemplateVersion Prelude.Text
updateEnvironmentTemplateVersion_minorVersion = Lens.lens (\UpdateEnvironmentTemplateVersion' {minorVersion} -> minorVersion) (\s@UpdateEnvironmentTemplateVersion' {} a -> s {minorVersion = a} :: UpdateEnvironmentTemplateVersion)

-- | The name of the environment template.
updateEnvironmentTemplateVersion_templateName :: Lens.Lens' UpdateEnvironmentTemplateVersion Prelude.Text
updateEnvironmentTemplateVersion_templateName = Lens.lens (\UpdateEnvironmentTemplateVersion' {templateName} -> templateName) (\s@UpdateEnvironmentTemplateVersion' {} a -> s {templateName = a} :: UpdateEnvironmentTemplateVersion)

instance
  Core.AWSRequest
    UpdateEnvironmentTemplateVersion
  where
  type
    AWSResponse UpdateEnvironmentTemplateVersion =
      UpdateEnvironmentTemplateVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEnvironmentTemplateVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "environmentTemplateVersion")
      )

instance
  Prelude.Hashable
    UpdateEnvironmentTemplateVersion
  where
  hashWithSalt
    _salt
    UpdateEnvironmentTemplateVersion' {..} =
      _salt `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` majorVersion
        `Prelude.hashWithSalt` minorVersion
        `Prelude.hashWithSalt` templateName

instance
  Prelude.NFData
    UpdateEnvironmentTemplateVersion
  where
  rnf UpdateEnvironmentTemplateVersion' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf majorVersion
      `Prelude.seq` Prelude.rnf minorVersion
      `Prelude.seq` Prelude.rnf templateName

instance
  Data.ToHeaders
    UpdateEnvironmentTemplateVersion
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.UpdateEnvironmentTemplateVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateEnvironmentTemplateVersion where
  toJSON UpdateEnvironmentTemplateVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("status" Data..=) Prelude.<$> status,
            ("description" Data..=) Prelude.<$> description,
            Prelude.Just ("majorVersion" Data..= majorVersion),
            Prelude.Just ("minorVersion" Data..= minorVersion),
            Prelude.Just ("templateName" Data..= templateName)
          ]
      )

instance Data.ToPath UpdateEnvironmentTemplateVersion where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    UpdateEnvironmentTemplateVersion
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEnvironmentTemplateVersionResponse' smart constructor.
data UpdateEnvironmentTemplateVersionResponse = UpdateEnvironmentTemplateVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The environment template version detail data that\'s returned by Proton.
    environmentTemplateVersion :: EnvironmentTemplateVersion
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEnvironmentTemplateVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEnvironmentTemplateVersionResponse_httpStatus' - The response's http status code.
--
-- 'environmentTemplateVersion', 'updateEnvironmentTemplateVersionResponse_environmentTemplateVersion' - The environment template version detail data that\'s returned by Proton.
newUpdateEnvironmentTemplateVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'environmentTemplateVersion'
  EnvironmentTemplateVersion ->
  UpdateEnvironmentTemplateVersionResponse
newUpdateEnvironmentTemplateVersionResponse
  pHttpStatus_
  pEnvironmentTemplateVersion_ =
    UpdateEnvironmentTemplateVersionResponse'
      { httpStatus =
          pHttpStatus_,
        environmentTemplateVersion =
          pEnvironmentTemplateVersion_
      }

-- | The response's http status code.
updateEnvironmentTemplateVersionResponse_httpStatus :: Lens.Lens' UpdateEnvironmentTemplateVersionResponse Prelude.Int
updateEnvironmentTemplateVersionResponse_httpStatus = Lens.lens (\UpdateEnvironmentTemplateVersionResponse' {httpStatus} -> httpStatus) (\s@UpdateEnvironmentTemplateVersionResponse' {} a -> s {httpStatus = a} :: UpdateEnvironmentTemplateVersionResponse)

-- | The environment template version detail data that\'s returned by Proton.
updateEnvironmentTemplateVersionResponse_environmentTemplateVersion :: Lens.Lens' UpdateEnvironmentTemplateVersionResponse EnvironmentTemplateVersion
updateEnvironmentTemplateVersionResponse_environmentTemplateVersion = Lens.lens (\UpdateEnvironmentTemplateVersionResponse' {environmentTemplateVersion} -> environmentTemplateVersion) (\s@UpdateEnvironmentTemplateVersionResponse' {} a -> s {environmentTemplateVersion = a} :: UpdateEnvironmentTemplateVersionResponse)

instance
  Prelude.NFData
    UpdateEnvironmentTemplateVersionResponse
  where
  rnf UpdateEnvironmentTemplateVersionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf environmentTemplateVersion
