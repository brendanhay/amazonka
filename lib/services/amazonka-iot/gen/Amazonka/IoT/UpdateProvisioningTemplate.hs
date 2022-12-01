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
-- Module      : Amazonka.IoT.UpdateProvisioningTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a provisioning template.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdateProvisioningTemplate>
-- action.
module Amazonka.IoT.UpdateProvisioningTemplate
  ( -- * Creating a Request
    UpdateProvisioningTemplate (..),
    newUpdateProvisioningTemplate,

    -- * Request Lenses
    updateProvisioningTemplate_removePreProvisioningHook,
    updateProvisioningTemplate_defaultVersionId,
    updateProvisioningTemplate_preProvisioningHook,
    updateProvisioningTemplate_description,
    updateProvisioningTemplate_enabled,
    updateProvisioningTemplate_provisioningRoleArn,
    updateProvisioningTemplate_templateName,

    -- * Destructuring the Response
    UpdateProvisioningTemplateResponse (..),
    newUpdateProvisioningTemplateResponse,

    -- * Response Lenses
    updateProvisioningTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateProvisioningTemplate' smart constructor.
data UpdateProvisioningTemplate = UpdateProvisioningTemplate'
  { -- | Removes pre-provisioning hook template.
    removePreProvisioningHook :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the default provisioning template version.
    defaultVersionId :: Prelude.Maybe Prelude.Int,
    -- | Updates the pre-provisioning hook template. Only supports template of
    -- type @FLEET_PROVISIONING@. For more information about provisioning
    -- template types, see
    -- <https://docs.aws.amazon.com/iot/latest/apireference/API_CreateProvisioningTemplate.html#iot-CreateProvisioningTemplate-request-type type>.
    preProvisioningHook :: Prelude.Maybe ProvisioningHook,
    -- | The description of the provisioning template.
    description :: Prelude.Maybe Prelude.Text,
    -- | True to enable the provisioning template, otherwise false.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the role associated with the provisioning template. This IoT
    -- role grants permission to provision a device.
    provisioningRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the provisioning template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProvisioningTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'removePreProvisioningHook', 'updateProvisioningTemplate_removePreProvisioningHook' - Removes pre-provisioning hook template.
--
-- 'defaultVersionId', 'updateProvisioningTemplate_defaultVersionId' - The ID of the default provisioning template version.
--
-- 'preProvisioningHook', 'updateProvisioningTemplate_preProvisioningHook' - Updates the pre-provisioning hook template. Only supports template of
-- type @FLEET_PROVISIONING@. For more information about provisioning
-- template types, see
-- <https://docs.aws.amazon.com/iot/latest/apireference/API_CreateProvisioningTemplate.html#iot-CreateProvisioningTemplate-request-type type>.
--
-- 'description', 'updateProvisioningTemplate_description' - The description of the provisioning template.
--
-- 'enabled', 'updateProvisioningTemplate_enabled' - True to enable the provisioning template, otherwise false.
--
-- 'provisioningRoleArn', 'updateProvisioningTemplate_provisioningRoleArn' - The ARN of the role associated with the provisioning template. This IoT
-- role grants permission to provision a device.
--
-- 'templateName', 'updateProvisioningTemplate_templateName' - The name of the provisioning template.
newUpdateProvisioningTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  UpdateProvisioningTemplate
newUpdateProvisioningTemplate pTemplateName_ =
  UpdateProvisioningTemplate'
    { removePreProvisioningHook =
        Prelude.Nothing,
      defaultVersionId = Prelude.Nothing,
      preProvisioningHook = Prelude.Nothing,
      description = Prelude.Nothing,
      enabled = Prelude.Nothing,
      provisioningRoleArn = Prelude.Nothing,
      templateName = pTemplateName_
    }

-- | Removes pre-provisioning hook template.
updateProvisioningTemplate_removePreProvisioningHook :: Lens.Lens' UpdateProvisioningTemplate (Prelude.Maybe Prelude.Bool)
updateProvisioningTemplate_removePreProvisioningHook = Lens.lens (\UpdateProvisioningTemplate' {removePreProvisioningHook} -> removePreProvisioningHook) (\s@UpdateProvisioningTemplate' {} a -> s {removePreProvisioningHook = a} :: UpdateProvisioningTemplate)

-- | The ID of the default provisioning template version.
updateProvisioningTemplate_defaultVersionId :: Lens.Lens' UpdateProvisioningTemplate (Prelude.Maybe Prelude.Int)
updateProvisioningTemplate_defaultVersionId = Lens.lens (\UpdateProvisioningTemplate' {defaultVersionId} -> defaultVersionId) (\s@UpdateProvisioningTemplate' {} a -> s {defaultVersionId = a} :: UpdateProvisioningTemplate)

-- | Updates the pre-provisioning hook template. Only supports template of
-- type @FLEET_PROVISIONING@. For more information about provisioning
-- template types, see
-- <https://docs.aws.amazon.com/iot/latest/apireference/API_CreateProvisioningTemplate.html#iot-CreateProvisioningTemplate-request-type type>.
updateProvisioningTemplate_preProvisioningHook :: Lens.Lens' UpdateProvisioningTemplate (Prelude.Maybe ProvisioningHook)
updateProvisioningTemplate_preProvisioningHook = Lens.lens (\UpdateProvisioningTemplate' {preProvisioningHook} -> preProvisioningHook) (\s@UpdateProvisioningTemplate' {} a -> s {preProvisioningHook = a} :: UpdateProvisioningTemplate)

-- | The description of the provisioning template.
updateProvisioningTemplate_description :: Lens.Lens' UpdateProvisioningTemplate (Prelude.Maybe Prelude.Text)
updateProvisioningTemplate_description = Lens.lens (\UpdateProvisioningTemplate' {description} -> description) (\s@UpdateProvisioningTemplate' {} a -> s {description = a} :: UpdateProvisioningTemplate)

-- | True to enable the provisioning template, otherwise false.
updateProvisioningTemplate_enabled :: Lens.Lens' UpdateProvisioningTemplate (Prelude.Maybe Prelude.Bool)
updateProvisioningTemplate_enabled = Lens.lens (\UpdateProvisioningTemplate' {enabled} -> enabled) (\s@UpdateProvisioningTemplate' {} a -> s {enabled = a} :: UpdateProvisioningTemplate)

-- | The ARN of the role associated with the provisioning template. This IoT
-- role grants permission to provision a device.
updateProvisioningTemplate_provisioningRoleArn :: Lens.Lens' UpdateProvisioningTemplate (Prelude.Maybe Prelude.Text)
updateProvisioningTemplate_provisioningRoleArn = Lens.lens (\UpdateProvisioningTemplate' {provisioningRoleArn} -> provisioningRoleArn) (\s@UpdateProvisioningTemplate' {} a -> s {provisioningRoleArn = a} :: UpdateProvisioningTemplate)

-- | The name of the provisioning template.
updateProvisioningTemplate_templateName :: Lens.Lens' UpdateProvisioningTemplate Prelude.Text
updateProvisioningTemplate_templateName = Lens.lens (\UpdateProvisioningTemplate' {templateName} -> templateName) (\s@UpdateProvisioningTemplate' {} a -> s {templateName = a} :: UpdateProvisioningTemplate)

instance Core.AWSRequest UpdateProvisioningTemplate where
  type
    AWSResponse UpdateProvisioningTemplate =
      UpdateProvisioningTemplateResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateProvisioningTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateProvisioningTemplate where
  hashWithSalt _salt UpdateProvisioningTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` removePreProvisioningHook
      `Prelude.hashWithSalt` defaultVersionId
      `Prelude.hashWithSalt` preProvisioningHook
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` provisioningRoleArn
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData UpdateProvisioningTemplate where
  rnf UpdateProvisioningTemplate' {..} =
    Prelude.rnf removePreProvisioningHook
      `Prelude.seq` Prelude.rnf defaultVersionId
      `Prelude.seq` Prelude.rnf preProvisioningHook
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf provisioningRoleArn
      `Prelude.seq` Prelude.rnf templateName

instance Core.ToHeaders UpdateProvisioningTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateProvisioningTemplate where
  toJSON UpdateProvisioningTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("removePreProvisioningHook" Core..=)
              Prelude.<$> removePreProvisioningHook,
            ("defaultVersionId" Core..=)
              Prelude.<$> defaultVersionId,
            ("preProvisioningHook" Core..=)
              Prelude.<$> preProvisioningHook,
            ("description" Core..=) Prelude.<$> description,
            ("enabled" Core..=) Prelude.<$> enabled,
            ("provisioningRoleArn" Core..=)
              Prelude.<$> provisioningRoleArn
          ]
      )

instance Core.ToPath UpdateProvisioningTemplate where
  toPath UpdateProvisioningTemplate' {..} =
    Prelude.mconcat
      ["/provisioning-templates/", Core.toBS templateName]

instance Core.ToQuery UpdateProvisioningTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProvisioningTemplateResponse' smart constructor.
data UpdateProvisioningTemplateResponse = UpdateProvisioningTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateProvisioningTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateProvisioningTemplateResponse_httpStatus' - The response's http status code.
newUpdateProvisioningTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateProvisioningTemplateResponse
newUpdateProvisioningTemplateResponse pHttpStatus_ =
  UpdateProvisioningTemplateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateProvisioningTemplateResponse_httpStatus :: Lens.Lens' UpdateProvisioningTemplateResponse Prelude.Int
updateProvisioningTemplateResponse_httpStatus = Lens.lens (\UpdateProvisioningTemplateResponse' {httpStatus} -> httpStatus) (\s@UpdateProvisioningTemplateResponse' {} a -> s {httpStatus = a} :: UpdateProvisioningTemplateResponse)

instance
  Prelude.NFData
    UpdateProvisioningTemplateResponse
  where
  rnf UpdateProvisioningTemplateResponse' {..} =
    Prelude.rnf httpStatus
