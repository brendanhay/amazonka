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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a fleet provisioning template.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions UpdateProvisioningTemplate>
-- action.
module Amazonka.IoT.UpdateProvisioningTemplate
  ( -- * Creating a Request
    UpdateProvisioningTemplate (..),
    newUpdateProvisioningTemplate,

    -- * Request Lenses
    updateProvisioningTemplate_preProvisioningHook,
    updateProvisioningTemplate_enabled,
    updateProvisioningTemplate_provisioningRoleArn,
    updateProvisioningTemplate_defaultVersionId,
    updateProvisioningTemplate_removePreProvisioningHook,
    updateProvisioningTemplate_description,
    updateProvisioningTemplate_templateName,

    -- * Destructuring the Response
    UpdateProvisioningTemplateResponse (..),
    newUpdateProvisioningTemplateResponse,

    -- * Response Lenses
    updateProvisioningTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateProvisioningTemplate' smart constructor.
data UpdateProvisioningTemplate = UpdateProvisioningTemplate'
  { -- | Updates the pre-provisioning hook template.
    preProvisioningHook :: Prelude.Maybe ProvisioningHook,
    -- | True to enable the fleet provisioning template, otherwise false.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the role associated with the provisioning template. This IoT
    -- role grants permission to provision a device.
    provisioningRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the default provisioning template version.
    defaultVersionId :: Prelude.Maybe Prelude.Int,
    -- | Removes pre-provisioning hook template.
    removePreProvisioningHook :: Prelude.Maybe Prelude.Bool,
    -- | The description of the fleet provisioning template.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the fleet provisioning template.
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
-- 'preProvisioningHook', 'updateProvisioningTemplate_preProvisioningHook' - Updates the pre-provisioning hook template.
--
-- 'enabled', 'updateProvisioningTemplate_enabled' - True to enable the fleet provisioning template, otherwise false.
--
-- 'provisioningRoleArn', 'updateProvisioningTemplate_provisioningRoleArn' - The ARN of the role associated with the provisioning template. This IoT
-- role grants permission to provision a device.
--
-- 'defaultVersionId', 'updateProvisioningTemplate_defaultVersionId' - The ID of the default provisioning template version.
--
-- 'removePreProvisioningHook', 'updateProvisioningTemplate_removePreProvisioningHook' - Removes pre-provisioning hook template.
--
-- 'description', 'updateProvisioningTemplate_description' - The description of the fleet provisioning template.
--
-- 'templateName', 'updateProvisioningTemplate_templateName' - The name of the fleet provisioning template.
newUpdateProvisioningTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  UpdateProvisioningTemplate
newUpdateProvisioningTemplate pTemplateName_ =
  UpdateProvisioningTemplate'
    { preProvisioningHook =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      provisioningRoleArn = Prelude.Nothing,
      defaultVersionId = Prelude.Nothing,
      removePreProvisioningHook = Prelude.Nothing,
      description = Prelude.Nothing,
      templateName = pTemplateName_
    }

-- | Updates the pre-provisioning hook template.
updateProvisioningTemplate_preProvisioningHook :: Lens.Lens' UpdateProvisioningTemplate (Prelude.Maybe ProvisioningHook)
updateProvisioningTemplate_preProvisioningHook = Lens.lens (\UpdateProvisioningTemplate' {preProvisioningHook} -> preProvisioningHook) (\s@UpdateProvisioningTemplate' {} a -> s {preProvisioningHook = a} :: UpdateProvisioningTemplate)

-- | True to enable the fleet provisioning template, otherwise false.
updateProvisioningTemplate_enabled :: Lens.Lens' UpdateProvisioningTemplate (Prelude.Maybe Prelude.Bool)
updateProvisioningTemplate_enabled = Lens.lens (\UpdateProvisioningTemplate' {enabled} -> enabled) (\s@UpdateProvisioningTemplate' {} a -> s {enabled = a} :: UpdateProvisioningTemplate)

-- | The ARN of the role associated with the provisioning template. This IoT
-- role grants permission to provision a device.
updateProvisioningTemplate_provisioningRoleArn :: Lens.Lens' UpdateProvisioningTemplate (Prelude.Maybe Prelude.Text)
updateProvisioningTemplate_provisioningRoleArn = Lens.lens (\UpdateProvisioningTemplate' {provisioningRoleArn} -> provisioningRoleArn) (\s@UpdateProvisioningTemplate' {} a -> s {provisioningRoleArn = a} :: UpdateProvisioningTemplate)

-- | The ID of the default provisioning template version.
updateProvisioningTemplate_defaultVersionId :: Lens.Lens' UpdateProvisioningTemplate (Prelude.Maybe Prelude.Int)
updateProvisioningTemplate_defaultVersionId = Lens.lens (\UpdateProvisioningTemplate' {defaultVersionId} -> defaultVersionId) (\s@UpdateProvisioningTemplate' {} a -> s {defaultVersionId = a} :: UpdateProvisioningTemplate)

-- | Removes pre-provisioning hook template.
updateProvisioningTemplate_removePreProvisioningHook :: Lens.Lens' UpdateProvisioningTemplate (Prelude.Maybe Prelude.Bool)
updateProvisioningTemplate_removePreProvisioningHook = Lens.lens (\UpdateProvisioningTemplate' {removePreProvisioningHook} -> removePreProvisioningHook) (\s@UpdateProvisioningTemplate' {} a -> s {removePreProvisioningHook = a} :: UpdateProvisioningTemplate)

-- | The description of the fleet provisioning template.
updateProvisioningTemplate_description :: Lens.Lens' UpdateProvisioningTemplate (Prelude.Maybe Prelude.Text)
updateProvisioningTemplate_description = Lens.lens (\UpdateProvisioningTemplate' {description} -> description) (\s@UpdateProvisioningTemplate' {} a -> s {description = a} :: UpdateProvisioningTemplate)

-- | The name of the fleet provisioning template.
updateProvisioningTemplate_templateName :: Lens.Lens' UpdateProvisioningTemplate Prelude.Text
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
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateProvisioningTemplate where
  hashWithSalt salt' UpdateProvisioningTemplate' {..} =
    salt' `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` removePreProvisioningHook
      `Prelude.hashWithSalt` defaultVersionId
      `Prelude.hashWithSalt` provisioningRoleArn
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` preProvisioningHook

instance Prelude.NFData UpdateProvisioningTemplate where
  rnf UpdateProvisioningTemplate' {..} =
    Prelude.rnf preProvisioningHook
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf removePreProvisioningHook
      `Prelude.seq` Prelude.rnf defaultVersionId
      `Prelude.seq` Prelude.rnf provisioningRoleArn
      `Prelude.seq` Prelude.rnf enabled

instance Core.ToHeaders UpdateProvisioningTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateProvisioningTemplate where
  toJSON UpdateProvisioningTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("preProvisioningHook" Core..=)
              Prelude.<$> preProvisioningHook,
            ("enabled" Core..=) Prelude.<$> enabled,
            ("provisioningRoleArn" Core..=)
              Prelude.<$> provisioningRoleArn,
            ("defaultVersionId" Core..=)
              Prelude.<$> defaultVersionId,
            ("removePreProvisioningHook" Core..=)
              Prelude.<$> removePreProvisioningHook,
            ("description" Core..=) Prelude.<$> description
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
