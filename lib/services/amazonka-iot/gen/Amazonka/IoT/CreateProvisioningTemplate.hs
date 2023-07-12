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
-- Module      : Amazonka.IoT.CreateProvisioningTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a provisioning template.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateProvisioningTemplate>
-- action.
module Amazonka.IoT.CreateProvisioningTemplate
  ( -- * Creating a Request
    CreateProvisioningTemplate (..),
    newCreateProvisioningTemplate,

    -- * Request Lenses
    createProvisioningTemplate_description,
    createProvisioningTemplate_enabled,
    createProvisioningTemplate_preProvisioningHook,
    createProvisioningTemplate_tags,
    createProvisioningTemplate_type,
    createProvisioningTemplate_templateName,
    createProvisioningTemplate_templateBody,
    createProvisioningTemplate_provisioningRoleArn,

    -- * Destructuring the Response
    CreateProvisioningTemplateResponse (..),
    newCreateProvisioningTemplateResponse,

    -- * Response Lenses
    createProvisioningTemplateResponse_defaultVersionId,
    createProvisioningTemplateResponse_templateArn,
    createProvisioningTemplateResponse_templateName,
    createProvisioningTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateProvisioningTemplate' smart constructor.
data CreateProvisioningTemplate = CreateProvisioningTemplate'
  { -- | The description of the provisioning template.
    description :: Prelude.Maybe Prelude.Text,
    -- | True to enable the provisioning template, otherwise false.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | Creates a pre-provisioning hook template. Only supports template of type
    -- @FLEET_PROVISIONING@. For more information about provisioning template
    -- types, see
    -- <https://docs.aws.amazon.com/iot/latest/apireference/API_CreateProvisioningTemplate.html#iot-CreateProvisioningTemplate-request-type type>.
    preProvisioningHook :: Prelude.Maybe ProvisioningHook,
    -- | Metadata which can be used to manage the provisioning template.
    --
    -- For URI Request parameters use format: ...key1=value1&key2=value2...
    --
    -- For the CLI command-line parameter use format: &&tags
    -- \"key1=value1&key2=value2...\"
    --
    -- For the cli-input-json file use format: \"tags\":
    -- \"key1=value1&key2=value2...\"
    tags :: Prelude.Maybe [Tag],
    -- | The type you define in a provisioning template. You can create a
    -- template with only one type. You can\'t change the template type after
    -- its creation. The default value is @FLEET_PROVISIONING@. For more
    -- information about provisioning template, see:
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/provision-template.html Provisioning template>.
    type' :: Prelude.Maybe TemplateType,
    -- | The name of the provisioning template.
    templateName :: Prelude.Text,
    -- | The JSON formatted contents of the provisioning template.
    templateBody :: Prelude.Text,
    -- | The role ARN for the role associated with the provisioning template.
    -- This IoT role grants permission to provision a device.
    provisioningRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProvisioningTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createProvisioningTemplate_description' - The description of the provisioning template.
--
-- 'enabled', 'createProvisioningTemplate_enabled' - True to enable the provisioning template, otherwise false.
--
-- 'preProvisioningHook', 'createProvisioningTemplate_preProvisioningHook' - Creates a pre-provisioning hook template. Only supports template of type
-- @FLEET_PROVISIONING@. For more information about provisioning template
-- types, see
-- <https://docs.aws.amazon.com/iot/latest/apireference/API_CreateProvisioningTemplate.html#iot-CreateProvisioningTemplate-request-type type>.
--
-- 'tags', 'createProvisioningTemplate_tags' - Metadata which can be used to manage the provisioning template.
--
-- For URI Request parameters use format: ...key1=value1&key2=value2...
--
-- For the CLI command-line parameter use format: &&tags
-- \"key1=value1&key2=value2...\"
--
-- For the cli-input-json file use format: \"tags\":
-- \"key1=value1&key2=value2...\"
--
-- 'type'', 'createProvisioningTemplate_type' - The type you define in a provisioning template. You can create a
-- template with only one type. You can\'t change the template type after
-- its creation. The default value is @FLEET_PROVISIONING@. For more
-- information about provisioning template, see:
-- <https://docs.aws.amazon.com/iot/latest/developerguide/provision-template.html Provisioning template>.
--
-- 'templateName', 'createProvisioningTemplate_templateName' - The name of the provisioning template.
--
-- 'templateBody', 'createProvisioningTemplate_templateBody' - The JSON formatted contents of the provisioning template.
--
-- 'provisioningRoleArn', 'createProvisioningTemplate_provisioningRoleArn' - The role ARN for the role associated with the provisioning template.
-- This IoT role grants permission to provision a device.
newCreateProvisioningTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'templateBody'
  Prelude.Text ->
  -- | 'provisioningRoleArn'
  Prelude.Text ->
  CreateProvisioningTemplate
newCreateProvisioningTemplate
  pTemplateName_
  pTemplateBody_
  pProvisioningRoleArn_ =
    CreateProvisioningTemplate'
      { description =
          Prelude.Nothing,
        enabled = Prelude.Nothing,
        preProvisioningHook = Prelude.Nothing,
        tags = Prelude.Nothing,
        type' = Prelude.Nothing,
        templateName = pTemplateName_,
        templateBody = pTemplateBody_,
        provisioningRoleArn = pProvisioningRoleArn_
      }

-- | The description of the provisioning template.
createProvisioningTemplate_description :: Lens.Lens' CreateProvisioningTemplate (Prelude.Maybe Prelude.Text)
createProvisioningTemplate_description = Lens.lens (\CreateProvisioningTemplate' {description} -> description) (\s@CreateProvisioningTemplate' {} a -> s {description = a} :: CreateProvisioningTemplate)

-- | True to enable the provisioning template, otherwise false.
createProvisioningTemplate_enabled :: Lens.Lens' CreateProvisioningTemplate (Prelude.Maybe Prelude.Bool)
createProvisioningTemplate_enabled = Lens.lens (\CreateProvisioningTemplate' {enabled} -> enabled) (\s@CreateProvisioningTemplate' {} a -> s {enabled = a} :: CreateProvisioningTemplate)

-- | Creates a pre-provisioning hook template. Only supports template of type
-- @FLEET_PROVISIONING@. For more information about provisioning template
-- types, see
-- <https://docs.aws.amazon.com/iot/latest/apireference/API_CreateProvisioningTemplate.html#iot-CreateProvisioningTemplate-request-type type>.
createProvisioningTemplate_preProvisioningHook :: Lens.Lens' CreateProvisioningTemplate (Prelude.Maybe ProvisioningHook)
createProvisioningTemplate_preProvisioningHook = Lens.lens (\CreateProvisioningTemplate' {preProvisioningHook} -> preProvisioningHook) (\s@CreateProvisioningTemplate' {} a -> s {preProvisioningHook = a} :: CreateProvisioningTemplate)

-- | Metadata which can be used to manage the provisioning template.
--
-- For URI Request parameters use format: ...key1=value1&key2=value2...
--
-- For the CLI command-line parameter use format: &&tags
-- \"key1=value1&key2=value2...\"
--
-- For the cli-input-json file use format: \"tags\":
-- \"key1=value1&key2=value2...\"
createProvisioningTemplate_tags :: Lens.Lens' CreateProvisioningTemplate (Prelude.Maybe [Tag])
createProvisioningTemplate_tags = Lens.lens (\CreateProvisioningTemplate' {tags} -> tags) (\s@CreateProvisioningTemplate' {} a -> s {tags = a} :: CreateProvisioningTemplate) Prelude.. Lens.mapping Lens.coerced

-- | The type you define in a provisioning template. You can create a
-- template with only one type. You can\'t change the template type after
-- its creation. The default value is @FLEET_PROVISIONING@. For more
-- information about provisioning template, see:
-- <https://docs.aws.amazon.com/iot/latest/developerguide/provision-template.html Provisioning template>.
createProvisioningTemplate_type :: Lens.Lens' CreateProvisioningTemplate (Prelude.Maybe TemplateType)
createProvisioningTemplate_type = Lens.lens (\CreateProvisioningTemplate' {type'} -> type') (\s@CreateProvisioningTemplate' {} a -> s {type' = a} :: CreateProvisioningTemplate)

-- | The name of the provisioning template.
createProvisioningTemplate_templateName :: Lens.Lens' CreateProvisioningTemplate Prelude.Text
createProvisioningTemplate_templateName = Lens.lens (\CreateProvisioningTemplate' {templateName} -> templateName) (\s@CreateProvisioningTemplate' {} a -> s {templateName = a} :: CreateProvisioningTemplate)

-- | The JSON formatted contents of the provisioning template.
createProvisioningTemplate_templateBody :: Lens.Lens' CreateProvisioningTemplate Prelude.Text
createProvisioningTemplate_templateBody = Lens.lens (\CreateProvisioningTemplate' {templateBody} -> templateBody) (\s@CreateProvisioningTemplate' {} a -> s {templateBody = a} :: CreateProvisioningTemplate)

-- | The role ARN for the role associated with the provisioning template.
-- This IoT role grants permission to provision a device.
createProvisioningTemplate_provisioningRoleArn :: Lens.Lens' CreateProvisioningTemplate Prelude.Text
createProvisioningTemplate_provisioningRoleArn = Lens.lens (\CreateProvisioningTemplate' {provisioningRoleArn} -> provisioningRoleArn) (\s@CreateProvisioningTemplate' {} a -> s {provisioningRoleArn = a} :: CreateProvisioningTemplate)

instance Core.AWSRequest CreateProvisioningTemplate where
  type
    AWSResponse CreateProvisioningTemplate =
      CreateProvisioningTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProvisioningTemplateResponse'
            Prelude.<$> (x Data..?> "defaultVersionId")
            Prelude.<*> (x Data..?> "templateArn")
            Prelude.<*> (x Data..?> "templateName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateProvisioningTemplate where
  hashWithSalt _salt CreateProvisioningTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` preProvisioningHook
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` templateBody
      `Prelude.hashWithSalt` provisioningRoleArn

instance Prelude.NFData CreateProvisioningTemplate where
  rnf CreateProvisioningTemplate' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf preProvisioningHook
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateBody
      `Prelude.seq` Prelude.rnf provisioningRoleArn

instance Data.ToHeaders CreateProvisioningTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateProvisioningTemplate where
  toJSON CreateProvisioningTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("enabled" Data..=) Prelude.<$> enabled,
            ("preProvisioningHook" Data..=)
              Prelude.<$> preProvisioningHook,
            ("tags" Data..=) Prelude.<$> tags,
            ("type" Data..=) Prelude.<$> type',
            Prelude.Just ("templateName" Data..= templateName),
            Prelude.Just ("templateBody" Data..= templateBody),
            Prelude.Just
              ("provisioningRoleArn" Data..= provisioningRoleArn)
          ]
      )

instance Data.ToPath CreateProvisioningTemplate where
  toPath = Prelude.const "/provisioning-templates"

instance Data.ToQuery CreateProvisioningTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateProvisioningTemplateResponse' smart constructor.
data CreateProvisioningTemplateResponse = CreateProvisioningTemplateResponse'
  { -- | The default version of the provisioning template.
    defaultVersionId :: Prelude.Maybe Prelude.Int,
    -- | The ARN that identifies the provisioning template.
    templateArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the provisioning template.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProvisioningTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultVersionId', 'createProvisioningTemplateResponse_defaultVersionId' - The default version of the provisioning template.
--
-- 'templateArn', 'createProvisioningTemplateResponse_templateArn' - The ARN that identifies the provisioning template.
--
-- 'templateName', 'createProvisioningTemplateResponse_templateName' - The name of the provisioning template.
--
-- 'httpStatus', 'createProvisioningTemplateResponse_httpStatus' - The response's http status code.
newCreateProvisioningTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProvisioningTemplateResponse
newCreateProvisioningTemplateResponse pHttpStatus_ =
  CreateProvisioningTemplateResponse'
    { defaultVersionId =
        Prelude.Nothing,
      templateArn = Prelude.Nothing,
      templateName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The default version of the provisioning template.
createProvisioningTemplateResponse_defaultVersionId :: Lens.Lens' CreateProvisioningTemplateResponse (Prelude.Maybe Prelude.Int)
createProvisioningTemplateResponse_defaultVersionId = Lens.lens (\CreateProvisioningTemplateResponse' {defaultVersionId} -> defaultVersionId) (\s@CreateProvisioningTemplateResponse' {} a -> s {defaultVersionId = a} :: CreateProvisioningTemplateResponse)

-- | The ARN that identifies the provisioning template.
createProvisioningTemplateResponse_templateArn :: Lens.Lens' CreateProvisioningTemplateResponse (Prelude.Maybe Prelude.Text)
createProvisioningTemplateResponse_templateArn = Lens.lens (\CreateProvisioningTemplateResponse' {templateArn} -> templateArn) (\s@CreateProvisioningTemplateResponse' {} a -> s {templateArn = a} :: CreateProvisioningTemplateResponse)

-- | The name of the provisioning template.
createProvisioningTemplateResponse_templateName :: Lens.Lens' CreateProvisioningTemplateResponse (Prelude.Maybe Prelude.Text)
createProvisioningTemplateResponse_templateName = Lens.lens (\CreateProvisioningTemplateResponse' {templateName} -> templateName) (\s@CreateProvisioningTemplateResponse' {} a -> s {templateName = a} :: CreateProvisioningTemplateResponse)

-- | The response's http status code.
createProvisioningTemplateResponse_httpStatus :: Lens.Lens' CreateProvisioningTemplateResponse Prelude.Int
createProvisioningTemplateResponse_httpStatus = Lens.lens (\CreateProvisioningTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateProvisioningTemplateResponse' {} a -> s {httpStatus = a} :: CreateProvisioningTemplateResponse)

instance
  Prelude.NFData
    CreateProvisioningTemplateResponse
  where
  rnf CreateProvisioningTemplateResponse' {..} =
    Prelude.rnf defaultVersionId
      `Prelude.seq` Prelude.rnf templateArn
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf httpStatus
