{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateProvisioningTemplate' smart constructor.
data UpdateProvisioningTemplate = UpdateProvisioningTemplate'
  { -- | Removes pre-provisioning hook template.
    removePreProvisioningHook :: Prelude.Maybe Prelude.Bool,
    -- | True to enable the fleet provisioning template, otherwise false.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Updates the pre-provisioning hook template.
    preProvisioningHook :: Prelude.Maybe ProvisioningHook,
    -- | The ID of the default provisioning template version.
    defaultVersionId :: Prelude.Maybe Prelude.Int,
    -- | The description of the fleet provisioning template.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the role associated with the provisioning template. This IoT
    -- role grants permission to provision a device.
    provisioningRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the fleet provisioning template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateProvisioningTemplate
newUpdateProvisioningTemplate pTemplateName_ =
  UpdateProvisioningTemplate'
    { removePreProvisioningHook =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      preProvisioningHook = Prelude.Nothing,
      defaultVersionId = Prelude.Nothing,
      description = Prelude.Nothing,
      provisioningRoleArn = Prelude.Nothing,
      templateName = pTemplateName_
    }

-- | Removes pre-provisioning hook template.
updateProvisioningTemplate_removePreProvisioningHook :: Lens.Lens' UpdateProvisioningTemplate (Prelude.Maybe Prelude.Bool)
updateProvisioningTemplate_removePreProvisioningHook = Lens.lens (\UpdateProvisioningTemplate' {removePreProvisioningHook} -> removePreProvisioningHook) (\s@UpdateProvisioningTemplate' {} a -> s {removePreProvisioningHook = a} :: UpdateProvisioningTemplate)

-- | True to enable the fleet provisioning template, otherwise false.
updateProvisioningTemplate_enabled :: Lens.Lens' UpdateProvisioningTemplate (Prelude.Maybe Prelude.Bool)
updateProvisioningTemplate_enabled = Lens.lens (\UpdateProvisioningTemplate' {enabled} -> enabled) (\s@UpdateProvisioningTemplate' {} a -> s {enabled = a} :: UpdateProvisioningTemplate)

-- | Updates the pre-provisioning hook template.
updateProvisioningTemplate_preProvisioningHook :: Lens.Lens' UpdateProvisioningTemplate (Prelude.Maybe ProvisioningHook)
updateProvisioningTemplate_preProvisioningHook = Lens.lens (\UpdateProvisioningTemplate' {preProvisioningHook} -> preProvisioningHook) (\s@UpdateProvisioningTemplate' {} a -> s {preProvisioningHook = a} :: UpdateProvisioningTemplate)

-- | The ID of the default provisioning template version.
updateProvisioningTemplate_defaultVersionId :: Lens.Lens' UpdateProvisioningTemplate (Prelude.Maybe Prelude.Int)
updateProvisioningTemplate_defaultVersionId = Lens.lens (\UpdateProvisioningTemplate' {defaultVersionId} -> defaultVersionId) (\s@UpdateProvisioningTemplate' {} a -> s {defaultVersionId = a} :: UpdateProvisioningTemplate)

-- | The description of the fleet provisioning template.
updateProvisioningTemplate_description :: Lens.Lens' UpdateProvisioningTemplate (Prelude.Maybe Prelude.Text)
updateProvisioningTemplate_description = Lens.lens (\UpdateProvisioningTemplate' {description} -> description) (\s@UpdateProvisioningTemplate' {} a -> s {description = a} :: UpdateProvisioningTemplate)

-- | The ARN of the role associated with the provisioning template. This IoT
-- role grants permission to provision a device.
updateProvisioningTemplate_provisioningRoleArn :: Lens.Lens' UpdateProvisioningTemplate (Prelude.Maybe Prelude.Text)
updateProvisioningTemplate_provisioningRoleArn = Lens.lens (\UpdateProvisioningTemplate' {provisioningRoleArn} -> provisioningRoleArn) (\s@UpdateProvisioningTemplate' {} a -> s {provisioningRoleArn = a} :: UpdateProvisioningTemplate)

-- | The name of the fleet provisioning template.
updateProvisioningTemplate_templateName :: Lens.Lens' UpdateProvisioningTemplate Prelude.Text
updateProvisioningTemplate_templateName = Lens.lens (\UpdateProvisioningTemplate' {templateName} -> templateName) (\s@UpdateProvisioningTemplate' {} a -> s {templateName = a} :: UpdateProvisioningTemplate)

instance
  Prelude.AWSRequest
    UpdateProvisioningTemplate
  where
  type
    Rs UpdateProvisioningTemplate =
      UpdateProvisioningTemplateResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateProvisioningTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateProvisioningTemplate

instance Prelude.NFData UpdateProvisioningTemplate

instance Prelude.ToHeaders UpdateProvisioningTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON UpdateProvisioningTemplate where
  toJSON UpdateProvisioningTemplate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("removePreProvisioningHook" Prelude..=)
              Prelude.<$> removePreProvisioningHook,
            ("enabled" Prelude..=) Prelude.<$> enabled,
            ("preProvisioningHook" Prelude..=)
              Prelude.<$> preProvisioningHook,
            ("defaultVersionId" Prelude..=)
              Prelude.<$> defaultVersionId,
            ("description" Prelude..=) Prelude.<$> description,
            ("provisioningRoleArn" Prelude..=)
              Prelude.<$> provisioningRoleArn
          ]
      )

instance Prelude.ToPath UpdateProvisioningTemplate where
  toPath UpdateProvisioningTemplate' {..} =
    Prelude.mconcat
      [ "/provisioning-templates/",
        Prelude.toBS templateName
      ]

instance Prelude.ToQuery UpdateProvisioningTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateProvisioningTemplateResponse' smart constructor.
data UpdateProvisioningTemplateResponse = UpdateProvisioningTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
