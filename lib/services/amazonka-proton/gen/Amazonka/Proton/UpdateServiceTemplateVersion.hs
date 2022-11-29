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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    updateServiceTemplateVersion_supportedComponentSources,
    updateServiceTemplateVersion_status,
    updateServiceTemplateVersion_description,
    updateServiceTemplateVersion_compatibleEnvironmentTemplates,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateServiceTemplateVersion' smart constructor.
data UpdateServiceTemplateVersion = UpdateServiceTemplateVersion'
  { -- | An array of supported component sources. Components with supported
    -- sources can be attached to service instances based on this service
    -- template version.
    --
    -- A change to @supportedComponentSources@ doesn\'t impact existing
    -- component attachments to instances based on this template version. A
    -- change only affects later associations.
    --
    -- For more information about components, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
    -- in the /Proton User Guide/.
    supportedComponentSources :: Prelude.Maybe [ServiceTemplateSupportedComponentSourceType],
    -- | The status of the service template minor version to update.
    status :: Prelude.Maybe TemplateVersionStatus,
    -- | A description of a service template version to update.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | An array of environment template objects that are compatible with this
    -- service template version. A service instance based on this service
    -- template version can run in environments based on compatible templates.
    compatibleEnvironmentTemplates :: Prelude.Maybe (Prelude.NonEmpty CompatibleEnvironmentTemplateInput),
    -- | To update a major version of a service template, include
    -- @major Version@.
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
-- 'supportedComponentSources', 'updateServiceTemplateVersion_supportedComponentSources' - An array of supported component sources. Components with supported
-- sources can be attached to service instances based on this service
-- template version.
--
-- A change to @supportedComponentSources@ doesn\'t impact existing
-- component attachments to instances based on this template version. A
-- change only affects later associations.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
--
-- 'status', 'updateServiceTemplateVersion_status' - The status of the service template minor version to update.
--
-- 'description', 'updateServiceTemplateVersion_description' - A description of a service template version to update.
--
-- 'compatibleEnvironmentTemplates', 'updateServiceTemplateVersion_compatibleEnvironmentTemplates' - An array of environment template objects that are compatible with this
-- service template version. A service instance based on this service
-- template version can run in environments based on compatible templates.
--
-- 'majorVersion', 'updateServiceTemplateVersion_majorVersion' - To update a major version of a service template, include
-- @major Version@.
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
      { supportedComponentSources =
          Prelude.Nothing,
        status = Prelude.Nothing,
        description = Prelude.Nothing,
        compatibleEnvironmentTemplates =
          Prelude.Nothing,
        majorVersion = pMajorVersion_,
        minorVersion = pMinorVersion_,
        templateName = pTemplateName_
      }

-- | An array of supported component sources. Components with supported
-- sources can be attached to service instances based on this service
-- template version.
--
-- A change to @supportedComponentSources@ doesn\'t impact existing
-- component attachments to instances based on this template version. A
-- change only affects later associations.
--
-- For more information about components, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-components.html Proton components>
-- in the /Proton User Guide/.
updateServiceTemplateVersion_supportedComponentSources :: Lens.Lens' UpdateServiceTemplateVersion (Prelude.Maybe [ServiceTemplateSupportedComponentSourceType])
updateServiceTemplateVersion_supportedComponentSources = Lens.lens (\UpdateServiceTemplateVersion' {supportedComponentSources} -> supportedComponentSources) (\s@UpdateServiceTemplateVersion' {} a -> s {supportedComponentSources = a} :: UpdateServiceTemplateVersion) Prelude.. Lens.mapping Lens.coerced

-- | The status of the service template minor version to update.
updateServiceTemplateVersion_status :: Lens.Lens' UpdateServiceTemplateVersion (Prelude.Maybe TemplateVersionStatus)
updateServiceTemplateVersion_status = Lens.lens (\UpdateServiceTemplateVersion' {status} -> status) (\s@UpdateServiceTemplateVersion' {} a -> s {status = a} :: UpdateServiceTemplateVersion)

-- | A description of a service template version to update.
updateServiceTemplateVersion_description :: Lens.Lens' UpdateServiceTemplateVersion (Prelude.Maybe Prelude.Text)
updateServiceTemplateVersion_description = Lens.lens (\UpdateServiceTemplateVersion' {description} -> description) (\s@UpdateServiceTemplateVersion' {} a -> s {description = a} :: UpdateServiceTemplateVersion) Prelude.. Lens.mapping Core._Sensitive

-- | An array of environment template objects that are compatible with this
-- service template version. A service instance based on this service
-- template version can run in environments based on compatible templates.
updateServiceTemplateVersion_compatibleEnvironmentTemplates :: Lens.Lens' UpdateServiceTemplateVersion (Prelude.Maybe (Prelude.NonEmpty CompatibleEnvironmentTemplateInput))
updateServiceTemplateVersion_compatibleEnvironmentTemplates = Lens.lens (\UpdateServiceTemplateVersion' {compatibleEnvironmentTemplates} -> compatibleEnvironmentTemplates) (\s@UpdateServiceTemplateVersion' {} a -> s {compatibleEnvironmentTemplates = a} :: UpdateServiceTemplateVersion) Prelude.. Lens.mapping Lens.coerced

-- | To update a major version of a service template, include
-- @major Version@.
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
  request overrides =
    Request.postJSON (overrides defaultService)
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
  hashWithSalt _salt UpdateServiceTemplateVersion' {..} =
    _salt
      `Prelude.hashWithSalt` supportedComponentSources
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` compatibleEnvironmentTemplates
      `Prelude.hashWithSalt` majorVersion
      `Prelude.hashWithSalt` minorVersion
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData UpdateServiceTemplateVersion where
  rnf UpdateServiceTemplateVersion' {..} =
    Prelude.rnf supportedComponentSources
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf compatibleEnvironmentTemplates
      `Prelude.seq` Prelude.rnf majorVersion
      `Prelude.seq` Prelude.rnf minorVersion
      `Prelude.seq` Prelude.rnf templateName

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
          [ ("supportedComponentSources" Core..=)
              Prelude.<$> supportedComponentSources,
            ("status" Core..=) Prelude.<$> status,
            ("description" Core..=) Prelude.<$> description,
            ("compatibleEnvironmentTemplates" Core..=)
              Prelude.<$> compatibleEnvironmentTemplates,
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
    -- | The service template version detail data that\'s returned by Proton.
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
-- 'serviceTemplateVersion', 'updateServiceTemplateVersionResponse_serviceTemplateVersion' - The service template version detail data that\'s returned by Proton.
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

-- | The service template version detail data that\'s returned by Proton.
updateServiceTemplateVersionResponse_serviceTemplateVersion :: Lens.Lens' UpdateServiceTemplateVersionResponse ServiceTemplateVersion
updateServiceTemplateVersionResponse_serviceTemplateVersion = Lens.lens (\UpdateServiceTemplateVersionResponse' {serviceTemplateVersion} -> serviceTemplateVersion) (\s@UpdateServiceTemplateVersionResponse' {} a -> s {serviceTemplateVersion = a} :: UpdateServiceTemplateVersionResponse)

instance
  Prelude.NFData
    UpdateServiceTemplateVersionResponse
  where
  rnf UpdateServiceTemplateVersionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serviceTemplateVersion
