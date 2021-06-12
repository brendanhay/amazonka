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
-- Module      : Network.AWS.IoT.CreateProvisioningTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a fleet provisioning template.
module Network.AWS.IoT.CreateProvisioningTemplate
  ( -- * Creating a Request
    CreateProvisioningTemplate (..),
    newCreateProvisioningTemplate,

    -- * Request Lenses
    createProvisioningTemplate_enabled,
    createProvisioningTemplate_preProvisioningHook,
    createProvisioningTemplate_tags,
    createProvisioningTemplate_description,
    createProvisioningTemplate_templateName,
    createProvisioningTemplate_templateBody,
    createProvisioningTemplate_provisioningRoleArn,

    -- * Destructuring the Response
    CreateProvisioningTemplateResponse (..),
    newCreateProvisioningTemplateResponse,

    -- * Response Lenses
    createProvisioningTemplateResponse_templateName,
    createProvisioningTemplateResponse_defaultVersionId,
    createProvisioningTemplateResponse_templateArn,
    createProvisioningTemplateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateProvisioningTemplate' smart constructor.
data CreateProvisioningTemplate = CreateProvisioningTemplate'
  { -- | True to enable the fleet provisioning template, otherwise false.
    enabled :: Core.Maybe Core.Bool,
    -- | Creates a pre-provisioning hook template.
    preProvisioningHook :: Core.Maybe ProvisioningHook,
    -- | Metadata which can be used to manage the fleet provisioning template.
    --
    -- For URI Request parameters use format: ...key1=value1&key2=value2...
    --
    -- For the CLI command-line parameter use format: &&tags
    -- \"key1=value1&key2=value2...\"
    --
    -- For the cli-input-json file use format: \"tags\":
    -- \"key1=value1&key2=value2...\"
    tags :: Core.Maybe [Tag],
    -- | The description of the fleet provisioning template.
    description :: Core.Maybe Core.Text,
    -- | The name of the fleet provisioning template.
    templateName :: Core.Text,
    -- | The JSON formatted contents of the fleet provisioning template.
    templateBody :: Core.Text,
    -- | The role ARN for the role associated with the fleet provisioning
    -- template. This IoT role grants permission to provision a device.
    provisioningRoleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateProvisioningTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'createProvisioningTemplate_enabled' - True to enable the fleet provisioning template, otherwise false.
--
-- 'preProvisioningHook', 'createProvisioningTemplate_preProvisioningHook' - Creates a pre-provisioning hook template.
--
-- 'tags', 'createProvisioningTemplate_tags' - Metadata which can be used to manage the fleet provisioning template.
--
-- For URI Request parameters use format: ...key1=value1&key2=value2...
--
-- For the CLI command-line parameter use format: &&tags
-- \"key1=value1&key2=value2...\"
--
-- For the cli-input-json file use format: \"tags\":
-- \"key1=value1&key2=value2...\"
--
-- 'description', 'createProvisioningTemplate_description' - The description of the fleet provisioning template.
--
-- 'templateName', 'createProvisioningTemplate_templateName' - The name of the fleet provisioning template.
--
-- 'templateBody', 'createProvisioningTemplate_templateBody' - The JSON formatted contents of the fleet provisioning template.
--
-- 'provisioningRoleArn', 'createProvisioningTemplate_provisioningRoleArn' - The role ARN for the role associated with the fleet provisioning
-- template. This IoT role grants permission to provision a device.
newCreateProvisioningTemplate ::
  -- | 'templateName'
  Core.Text ->
  -- | 'templateBody'
  Core.Text ->
  -- | 'provisioningRoleArn'
  Core.Text ->
  CreateProvisioningTemplate
newCreateProvisioningTemplate
  pTemplateName_
  pTemplateBody_
  pProvisioningRoleArn_ =
    CreateProvisioningTemplate'
      { enabled = Core.Nothing,
        preProvisioningHook = Core.Nothing,
        tags = Core.Nothing,
        description = Core.Nothing,
        templateName = pTemplateName_,
        templateBody = pTemplateBody_,
        provisioningRoleArn = pProvisioningRoleArn_
      }

-- | True to enable the fleet provisioning template, otherwise false.
createProvisioningTemplate_enabled :: Lens.Lens' CreateProvisioningTemplate (Core.Maybe Core.Bool)
createProvisioningTemplate_enabled = Lens.lens (\CreateProvisioningTemplate' {enabled} -> enabled) (\s@CreateProvisioningTemplate' {} a -> s {enabled = a} :: CreateProvisioningTemplate)

-- | Creates a pre-provisioning hook template.
createProvisioningTemplate_preProvisioningHook :: Lens.Lens' CreateProvisioningTemplate (Core.Maybe ProvisioningHook)
createProvisioningTemplate_preProvisioningHook = Lens.lens (\CreateProvisioningTemplate' {preProvisioningHook} -> preProvisioningHook) (\s@CreateProvisioningTemplate' {} a -> s {preProvisioningHook = a} :: CreateProvisioningTemplate)

-- | Metadata which can be used to manage the fleet provisioning template.
--
-- For URI Request parameters use format: ...key1=value1&key2=value2...
--
-- For the CLI command-line parameter use format: &&tags
-- \"key1=value1&key2=value2...\"
--
-- For the cli-input-json file use format: \"tags\":
-- \"key1=value1&key2=value2...\"
createProvisioningTemplate_tags :: Lens.Lens' CreateProvisioningTemplate (Core.Maybe [Tag])
createProvisioningTemplate_tags = Lens.lens (\CreateProvisioningTemplate' {tags} -> tags) (\s@CreateProvisioningTemplate' {} a -> s {tags = a} :: CreateProvisioningTemplate) Core.. Lens.mapping Lens._Coerce

-- | The description of the fleet provisioning template.
createProvisioningTemplate_description :: Lens.Lens' CreateProvisioningTemplate (Core.Maybe Core.Text)
createProvisioningTemplate_description = Lens.lens (\CreateProvisioningTemplate' {description} -> description) (\s@CreateProvisioningTemplate' {} a -> s {description = a} :: CreateProvisioningTemplate)

-- | The name of the fleet provisioning template.
createProvisioningTemplate_templateName :: Lens.Lens' CreateProvisioningTemplate Core.Text
createProvisioningTemplate_templateName = Lens.lens (\CreateProvisioningTemplate' {templateName} -> templateName) (\s@CreateProvisioningTemplate' {} a -> s {templateName = a} :: CreateProvisioningTemplate)

-- | The JSON formatted contents of the fleet provisioning template.
createProvisioningTemplate_templateBody :: Lens.Lens' CreateProvisioningTemplate Core.Text
createProvisioningTemplate_templateBody = Lens.lens (\CreateProvisioningTemplate' {templateBody} -> templateBody) (\s@CreateProvisioningTemplate' {} a -> s {templateBody = a} :: CreateProvisioningTemplate)

-- | The role ARN for the role associated with the fleet provisioning
-- template. This IoT role grants permission to provision a device.
createProvisioningTemplate_provisioningRoleArn :: Lens.Lens' CreateProvisioningTemplate Core.Text
createProvisioningTemplate_provisioningRoleArn = Lens.lens (\CreateProvisioningTemplate' {provisioningRoleArn} -> provisioningRoleArn) (\s@CreateProvisioningTemplate' {} a -> s {provisioningRoleArn = a} :: CreateProvisioningTemplate)

instance Core.AWSRequest CreateProvisioningTemplate where
  type
    AWSResponse CreateProvisioningTemplate =
      CreateProvisioningTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProvisioningTemplateResponse'
            Core.<$> (x Core..?> "templateName")
            Core.<*> (x Core..?> "defaultVersionId")
            Core.<*> (x Core..?> "templateArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateProvisioningTemplate

instance Core.NFData CreateProvisioningTemplate

instance Core.ToHeaders CreateProvisioningTemplate where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON CreateProvisioningTemplate where
  toJSON CreateProvisioningTemplate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("enabled" Core..=) Core.<$> enabled,
            ("preProvisioningHook" Core..=)
              Core.<$> preProvisioningHook,
            ("tags" Core..=) Core.<$> tags,
            ("description" Core..=) Core.<$> description,
            Core.Just ("templateName" Core..= templateName),
            Core.Just ("templateBody" Core..= templateBody),
            Core.Just
              ("provisioningRoleArn" Core..= provisioningRoleArn)
          ]
      )

instance Core.ToPath CreateProvisioningTemplate where
  toPath = Core.const "/provisioning-templates"

instance Core.ToQuery CreateProvisioningTemplate where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateProvisioningTemplateResponse' smart constructor.
data CreateProvisioningTemplateResponse = CreateProvisioningTemplateResponse'
  { -- | The name of the fleet provisioning template.
    templateName :: Core.Maybe Core.Text,
    -- | The default version of the fleet provisioning template.
    defaultVersionId :: Core.Maybe Core.Int,
    -- | The ARN that identifies the provisioning template.
    templateArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateProvisioningTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'createProvisioningTemplateResponse_templateName' - The name of the fleet provisioning template.
--
-- 'defaultVersionId', 'createProvisioningTemplateResponse_defaultVersionId' - The default version of the fleet provisioning template.
--
-- 'templateArn', 'createProvisioningTemplateResponse_templateArn' - The ARN that identifies the provisioning template.
--
-- 'httpStatus', 'createProvisioningTemplateResponse_httpStatus' - The response's http status code.
newCreateProvisioningTemplateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateProvisioningTemplateResponse
newCreateProvisioningTemplateResponse pHttpStatus_ =
  CreateProvisioningTemplateResponse'
    { templateName =
        Core.Nothing,
      defaultVersionId = Core.Nothing,
      templateArn = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the fleet provisioning template.
createProvisioningTemplateResponse_templateName :: Lens.Lens' CreateProvisioningTemplateResponse (Core.Maybe Core.Text)
createProvisioningTemplateResponse_templateName = Lens.lens (\CreateProvisioningTemplateResponse' {templateName} -> templateName) (\s@CreateProvisioningTemplateResponse' {} a -> s {templateName = a} :: CreateProvisioningTemplateResponse)

-- | The default version of the fleet provisioning template.
createProvisioningTemplateResponse_defaultVersionId :: Lens.Lens' CreateProvisioningTemplateResponse (Core.Maybe Core.Int)
createProvisioningTemplateResponse_defaultVersionId = Lens.lens (\CreateProvisioningTemplateResponse' {defaultVersionId} -> defaultVersionId) (\s@CreateProvisioningTemplateResponse' {} a -> s {defaultVersionId = a} :: CreateProvisioningTemplateResponse)

-- | The ARN that identifies the provisioning template.
createProvisioningTemplateResponse_templateArn :: Lens.Lens' CreateProvisioningTemplateResponse (Core.Maybe Core.Text)
createProvisioningTemplateResponse_templateArn = Lens.lens (\CreateProvisioningTemplateResponse' {templateArn} -> templateArn) (\s@CreateProvisioningTemplateResponse' {} a -> s {templateArn = a} :: CreateProvisioningTemplateResponse)

-- | The response's http status code.
createProvisioningTemplateResponse_httpStatus :: Lens.Lens' CreateProvisioningTemplateResponse Core.Int
createProvisioningTemplateResponse_httpStatus = Lens.lens (\CreateProvisioningTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateProvisioningTemplateResponse' {} a -> s {httpStatus = a} :: CreateProvisioningTemplateResponse)

instance
  Core.NFData
    CreateProvisioningTemplateResponse
