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
-- Module      : Amazonka.Proton.UpdateServiceTemplateVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a major or minor version of a service template.
module Amazonka.Proton.UpdateServiceTemplateVersion
  ( -- * Creating a Request
    UpdateServiceTemplateVersion (..),
    newUpdateServiceTemplateVersion,

    -- * Request Lenses
    updateServiceTemplateVersion_status,
    updateServiceTemplateVersion_compatibleEnvironmentTemplates,
    updateServiceTemplateVersion_description,
    updateServiceTemplateVersion_majorVersion,
    updateServiceTemplateVersion_minorVersion,
    updateServiceTemplateVersion_templateName,

    -- * Destructuring the Response
    UpdateServiceTemplateVersionResponse (..),
    newUpdateServiceTemplateVersionResponse,

    -- * Response Lenses
    updateServiceTemplateVersionResponse_httpStatus,
    updateServiceTemplateVersionResponse_serviceTemplateVersion,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateServiceTemplateVersion' smart constructor.
data UpdateServiceTemplateVersion = UpdateServiceTemplateVersion'
  { -- | The status of the service template minor version to update.
    status :: Prelude.Maybe TemplateVersionStatus,
    -- | An array of compatible environment names for a service template major or
    -- minor version to update.
    compatibleEnvironmentTemplates :: Prelude.Maybe (Prelude.NonEmpty CompatibleEnvironmentTemplateInput),
    -- | A description of a service template version to update.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | To update a major version of a service template, include @majorVersion@.
    majorVersion :: Prelude.Text,
    -- | To update a minor version of a service template, include @minorVersion@.
    minorVersion :: Prelude.Text,
    -- | The name of the service template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceTemplateVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'updateServiceTemplateVersion_status' - The status of the service template minor version to update.
--
-- 'compatibleEnvironmentTemplates', 'updateServiceTemplateVersion_compatibleEnvironmentTemplates' - An array of compatible environment names for a service template major or
-- minor version to update.
--
-- 'description', 'updateServiceTemplateVersion_description' - A description of a service template version to update.
--
-- 'majorVersion', 'updateServiceTemplateVersion_majorVersion' - To update a major version of a service template, include @majorVersion@.
--
-- 'minorVersion', 'updateServiceTemplateVersion_minorVersion' - To update a minor version of a service template, include @minorVersion@.
--
-- 'templateName', 'updateServiceTemplateVersion_templateName' - The name of the service template.
newUpdateServiceTemplateVersion ::
  -- | 'majorVersion'
  Prelude.Text ->
  -- | 'minorVersion'
  Prelude.Text ->
  -- | 'templateName'
  Prelude.Text ->
  UpdateServiceTemplateVersion
newUpdateServiceTemplateVersion
  pMajorVersion_
  pMinorVersion_
  pTemplateName_ =
    UpdateServiceTemplateVersion'
      { status =
          Prelude.Nothing,
        compatibleEnvironmentTemplates =
          Prelude.Nothing,
        description = Prelude.Nothing,
        majorVersion = pMajorVersion_,
        minorVersion = pMinorVersion_,
        templateName = pTemplateName_
      }

-- | The status of the service template minor version to update.
updateServiceTemplateVersion_status :: Lens.Lens' UpdateServiceTemplateVersion (Prelude.Maybe TemplateVersionStatus)
updateServiceTemplateVersion_status = Lens.lens (\UpdateServiceTemplateVersion' {status} -> status) (\s@UpdateServiceTemplateVersion' {} a -> s {status = a} :: UpdateServiceTemplateVersion)

-- | An array of compatible environment names for a service template major or
-- minor version to update.
updateServiceTemplateVersion_compatibleEnvironmentTemplates :: Lens.Lens' UpdateServiceTemplateVersion (Prelude.Maybe (Prelude.NonEmpty CompatibleEnvironmentTemplateInput))
updateServiceTemplateVersion_compatibleEnvironmentTemplates = Lens.lens (\UpdateServiceTemplateVersion' {compatibleEnvironmentTemplates} -> compatibleEnvironmentTemplates) (\s@UpdateServiceTemplateVersion' {} a -> s {compatibleEnvironmentTemplates = a} :: UpdateServiceTemplateVersion) Prelude.. Lens.mapping Lens.coerced

-- | A description of a service template version to update.
updateServiceTemplateVersion_description :: Lens.Lens' UpdateServiceTemplateVersion (Prelude.Maybe Prelude.Text)
updateServiceTemplateVersion_description = Lens.lens (\UpdateServiceTemplateVersion' {description} -> description) (\s@UpdateServiceTemplateVersion' {} a -> s {description = a} :: UpdateServiceTemplateVersion) Prelude.. Lens.mapping Core._Sensitive

-- | To update a major version of a service template, include @majorVersion@.
updateServiceTemplateVersion_majorVersion :: Lens.Lens' UpdateServiceTemplateVersion Prelude.Text
updateServiceTemplateVersion_majorVersion = Lens.lens (\UpdateServiceTemplateVersion' {majorVersion} -> majorVersion) (\s@UpdateServiceTemplateVersion' {} a -> s {majorVersion = a} :: UpdateServiceTemplateVersion)

-- | To update a minor version of a service template, include @minorVersion@.
updateServiceTemplateVersion_minorVersion :: Lens.Lens' UpdateServiceTemplateVersion Prelude.Text
updateServiceTemplateVersion_minorVersion = Lens.lens (\UpdateServiceTemplateVersion' {minorVersion} -> minorVersion) (\s@UpdateServiceTemplateVersion' {} a -> s {minorVersion = a} :: UpdateServiceTemplateVersion)

-- | The name of the service template.
updateServiceTemplateVersion_templateName :: Lens.Lens' UpdateServiceTemplateVersion Prelude.Text
updateServiceTemplateVersion_templateName = Lens.lens (\UpdateServiceTemplateVersion' {templateName} -> templateName) (\s@UpdateServiceTemplateVersion' {} a -> s {templateName = a} :: UpdateServiceTemplateVersion)

instance Core.AWSRequest UpdateServiceTemplateVersion where
  type
    AWSResponse UpdateServiceTemplateVersion =
      UpdateServiceTemplateVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateServiceTemplateVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "serviceTemplateVersion")
      )

instance
  Prelude.Hashable
    UpdateServiceTemplateVersion
  where
  hashWithSalt salt' UpdateServiceTemplateVersion' {..} =
    salt' `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` minorVersion
      `Prelude.hashWithSalt` majorVersion
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` compatibleEnvironmentTemplates
      `Prelude.hashWithSalt` status

instance Prelude.NFData UpdateServiceTemplateVersion where
  rnf UpdateServiceTemplateVersion' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf minorVersion
      `Prelude.seq` Prelude.rnf majorVersion
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf compatibleEnvironmentTemplates

instance Core.ToHeaders UpdateServiceTemplateVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AwsProton20200720.UpdateServiceTemplateVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateServiceTemplateVersion where
  toJSON UpdateServiceTemplateVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("status" Core..=) Prelude.<$> status,
            ("compatibleEnvironmentTemplates" Core..=)
              Prelude.<$> compatibleEnvironmentTemplates,
            ("description" Core..=) Prelude.<$> description,
            Prelude.Just ("majorVersion" Core..= majorVersion),
            Prelude.Just ("minorVersion" Core..= minorVersion),
            Prelude.Just ("templateName" Core..= templateName)
          ]
      )

instance Core.ToPath UpdateServiceTemplateVersion where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateServiceTemplateVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateServiceTemplateVersionResponse' smart constructor.
data UpdateServiceTemplateVersionResponse = UpdateServiceTemplateVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The service template version detail data that\'s returned by AWS Proton.
    serviceTemplateVersion :: ServiceTemplateVersion
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateServiceTemplateVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateServiceTemplateVersionResponse_httpStatus' - The response's http status code.
--
-- 'serviceTemplateVersion', 'updateServiceTemplateVersionResponse_serviceTemplateVersion' - The service template version detail data that\'s returned by AWS Proton.
newUpdateServiceTemplateVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serviceTemplateVersion'
  ServiceTemplateVersion ->
  UpdateServiceTemplateVersionResponse
newUpdateServiceTemplateVersionResponse
  pHttpStatus_
  pServiceTemplateVersion_ =
    UpdateServiceTemplateVersionResponse'
      { httpStatus =
          pHttpStatus_,
        serviceTemplateVersion =
          pServiceTemplateVersion_
      }

-- | The response's http status code.
updateServiceTemplateVersionResponse_httpStatus :: Lens.Lens' UpdateServiceTemplateVersionResponse Prelude.Int
updateServiceTemplateVersionResponse_httpStatus = Lens.lens (\UpdateServiceTemplateVersionResponse' {httpStatus} -> httpStatus) (\s@UpdateServiceTemplateVersionResponse' {} a -> s {httpStatus = a} :: UpdateServiceTemplateVersionResponse)

-- | The service template version detail data that\'s returned by AWS Proton.
updateServiceTemplateVersionResponse_serviceTemplateVersion :: Lens.Lens' UpdateServiceTemplateVersionResponse ServiceTemplateVersion
updateServiceTemplateVersionResponse_serviceTemplateVersion = Lens.lens (\UpdateServiceTemplateVersionResponse' {serviceTemplateVersion} -> serviceTemplateVersion) (\s@UpdateServiceTemplateVersionResponse' {} a -> s {serviceTemplateVersion = a} :: UpdateServiceTemplateVersionResponse)

instance
  Prelude.NFData
    UpdateServiceTemplateVersionResponse
  where
  rnf UpdateServiceTemplateVersionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serviceTemplateVersion
