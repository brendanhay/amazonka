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
-- Module      : Network.AWS.IoT.UpdateProvisioningTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a fleet provisioning template.
module Network.AWS.IoT.UpdateProvisioningTemplate
  ( -- * Creating a Request
    UpdateProvisioningTemplate (..),
    newUpdateProvisioningTemplate,

    -- * Request Lenses
    updateProvisioningTemplate_removePreProvisioningHook,
    updateProvisioningTemplate_enabled,
    updateProvisioningTemplate_preProvisioningHook,
    updateProvisioningTemplate_defaultVersionId,
    updateProvisioningTemplate_description,
    updateProvisioningTemplate_provisioningRoleArn,
    updateProvisioningTemplate_templateName,

    -- * Destructuring the Response
    UpdateProvisioningTemplateResponse (..),
    newUpdateProvisioningTemplateResponse,

    -- * Response Lenses
    updateProvisioningTemplateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateProvisioningTemplate' smart constructor.
data UpdateProvisioningTemplate = UpdateProvisioningTemplate'
  { -- | Removes pre-provisioning hook template.
    removePreProvisioningHook :: Core.Maybe Core.Bool,
    -- | True to enable the fleet provisioning template, otherwise false.
    enabled :: Core.Maybe Core.Bool,
    -- | Updates the pre-provisioning hook template.
    preProvisioningHook :: Core.Maybe ProvisioningHook,
    -- | The ID of the default provisioning template version.
    defaultVersionId :: Core.Maybe Core.Int,
    -- | The description of the fleet provisioning template.
    description :: Core.Maybe Core.Text,
    -- | The ARN of the role associated with the provisioning template. This IoT
    -- role grants permission to provision a device.
    provisioningRoleArn :: Core.Maybe Core.Text,
    -- | The name of the fleet provisioning template.
    templateName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'enabled', 'updateProvisioningTemplate_enabled' - True to enable the fleet provisioning template, otherwise false.
--
-- 'preProvisioningHook', 'updateProvisioningTemplate_preProvisioningHook' - Updates the pre-provisioning hook template.
--
-- 'defaultVersionId', 'updateProvisioningTemplate_defaultVersionId' - The ID of the default provisioning template version.
--
-- 'description', 'updateProvisioningTemplate_description' - The description of the fleet provisioning template.
--
-- 'provisioningRoleArn', 'updateProvisioningTemplate_provisioningRoleArn' - The ARN of the role associated with the provisioning template. This IoT
-- role grants permission to provision a device.
--
-- 'templateName', 'updateProvisioningTemplate_templateName' - The name of the fleet provisioning template.
newUpdateProvisioningTemplate ::
  -- | 'templateName'
  Core.Text ->
  UpdateProvisioningTemplate
newUpdateProvisioningTemplate pTemplateName_ =
  UpdateProvisioningTemplate'
    { removePreProvisioningHook =
        Core.Nothing,
      enabled = Core.Nothing,
      preProvisioningHook = Core.Nothing,
      defaultVersionId = Core.Nothing,
      description = Core.Nothing,
      provisioningRoleArn = Core.Nothing,
      templateName = pTemplateName_
    }

-- | Removes pre-provisioning hook template.
updateProvisioningTemplate_removePreProvisioningHook :: Lens.Lens' UpdateProvisioningTemplate (Core.Maybe Core.Bool)
updateProvisioningTemplate_removePreProvisioningHook = Lens.lens (\UpdateProvisioningTemplate' {removePreProvisioningHook} -> removePreProvisioningHook) (\s@UpdateProvisioningTemplate' {} a -> s {removePreProvisioningHook = a} :: UpdateProvisioningTemplate)

-- | True to enable the fleet provisioning template, otherwise false.
updateProvisioningTemplate_enabled :: Lens.Lens' UpdateProvisioningTemplate (Core.Maybe Core.Bool)
updateProvisioningTemplate_enabled = Lens.lens (\UpdateProvisioningTemplate' {enabled} -> enabled) (\s@UpdateProvisioningTemplate' {} a -> s {enabled = a} :: UpdateProvisioningTemplate)

-- | Updates the pre-provisioning hook template.
updateProvisioningTemplate_preProvisioningHook :: Lens.Lens' UpdateProvisioningTemplate (Core.Maybe ProvisioningHook)
updateProvisioningTemplate_preProvisioningHook = Lens.lens (\UpdateProvisioningTemplate' {preProvisioningHook} -> preProvisioningHook) (\s@UpdateProvisioningTemplate' {} a -> s {preProvisioningHook = a} :: UpdateProvisioningTemplate)

-- | The ID of the default provisioning template version.
updateProvisioningTemplate_defaultVersionId :: Lens.Lens' UpdateProvisioningTemplate (Core.Maybe Core.Int)
updateProvisioningTemplate_defaultVersionId = Lens.lens (\UpdateProvisioningTemplate' {defaultVersionId} -> defaultVersionId) (\s@UpdateProvisioningTemplate' {} a -> s {defaultVersionId = a} :: UpdateProvisioningTemplate)

-- | The description of the fleet provisioning template.
updateProvisioningTemplate_description :: Lens.Lens' UpdateProvisioningTemplate (Core.Maybe Core.Text)
updateProvisioningTemplate_description = Lens.lens (\UpdateProvisioningTemplate' {description} -> description) (\s@UpdateProvisioningTemplate' {} a -> s {description = a} :: UpdateProvisioningTemplate)

-- | The ARN of the role associated with the provisioning template. This IoT
-- role grants permission to provision a device.
updateProvisioningTemplate_provisioningRoleArn :: Lens.Lens' UpdateProvisioningTemplate (Core.Maybe Core.Text)
updateProvisioningTemplate_provisioningRoleArn = Lens.lens (\UpdateProvisioningTemplate' {provisioningRoleArn} -> provisioningRoleArn) (\s@UpdateProvisioningTemplate' {} a -> s {provisioningRoleArn = a} :: UpdateProvisioningTemplate)

-- | The name of the fleet provisioning template.
updateProvisioningTemplate_templateName :: Lens.Lens' UpdateProvisioningTemplate Core.Text
updateProvisioningTemplate_templateName = Lens.lens (\UpdateProvisioningTemplate' {templateName} -> templateName) (\s@UpdateProvisioningTemplate' {} a -> s {templateName = a} :: UpdateProvisioningTemplate)

instance Core.AWSRequest UpdateProvisioningTemplate where
  type
    AWSResponse UpdateProvisioningTemplate =
      UpdateProvisioningTemplateResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateProvisioningTemplateResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateProvisioningTemplate

instance Core.NFData UpdateProvisioningTemplate

instance Core.ToHeaders UpdateProvisioningTemplate where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON UpdateProvisioningTemplate where
  toJSON UpdateProvisioningTemplate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("removePreProvisioningHook" Core..=)
              Core.<$> removePreProvisioningHook,
            ("enabled" Core..=) Core.<$> enabled,
            ("preProvisioningHook" Core..=)
              Core.<$> preProvisioningHook,
            ("defaultVersionId" Core..=)
              Core.<$> defaultVersionId,
            ("description" Core..=) Core.<$> description,
            ("provisioningRoleArn" Core..=)
              Core.<$> provisioningRoleArn
          ]
      )

instance Core.ToPath UpdateProvisioningTemplate where
  toPath UpdateProvisioningTemplate' {..} =
    Core.mconcat
      ["/provisioning-templates/", Core.toBS templateName]

instance Core.ToQuery UpdateProvisioningTemplate where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateProvisioningTemplateResponse' smart constructor.
data UpdateProvisioningTemplateResponse = UpdateProvisioningTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateProvisioningTemplateResponse
newUpdateProvisioningTemplateResponse pHttpStatus_ =
  UpdateProvisioningTemplateResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateProvisioningTemplateResponse_httpStatus :: Lens.Lens' UpdateProvisioningTemplateResponse Core.Int
updateProvisioningTemplateResponse_httpStatus = Lens.lens (\UpdateProvisioningTemplateResponse' {httpStatus} -> httpStatus) (\s@UpdateProvisioningTemplateResponse' {} a -> s {httpStatus = a} :: UpdateProvisioningTemplateResponse)

instance
  Core.NFData
    UpdateProvisioningTemplateResponse
