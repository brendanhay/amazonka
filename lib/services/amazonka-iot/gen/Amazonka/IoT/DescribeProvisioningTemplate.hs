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
-- Module      : Amazonka.IoT.DescribeProvisioningTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a provisioning template.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DescribeProvisioningTemplate>
-- action.
module Amazonka.IoT.DescribeProvisioningTemplate
  ( -- * Creating a Request
    DescribeProvisioningTemplate (..),
    newDescribeProvisioningTemplate,

    -- * Request Lenses
    describeProvisioningTemplate_templateName,

    -- * Destructuring the Response
    DescribeProvisioningTemplateResponse (..),
    newDescribeProvisioningTemplateResponse,

    -- * Response Lenses
    describeProvisioningTemplateResponse_creationDate,
    describeProvisioningTemplateResponse_defaultVersionId,
    describeProvisioningTemplateResponse_description,
    describeProvisioningTemplateResponse_enabled,
    describeProvisioningTemplateResponse_lastModifiedDate,
    describeProvisioningTemplateResponse_preProvisioningHook,
    describeProvisioningTemplateResponse_provisioningRoleArn,
    describeProvisioningTemplateResponse_templateArn,
    describeProvisioningTemplateResponse_templateBody,
    describeProvisioningTemplateResponse_templateName,
    describeProvisioningTemplateResponse_type,
    describeProvisioningTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeProvisioningTemplate' smart constructor.
data DescribeProvisioningTemplate = DescribeProvisioningTemplate'
  { -- | The name of the provisioning template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProvisioningTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'describeProvisioningTemplate_templateName' - The name of the provisioning template.
newDescribeProvisioningTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  DescribeProvisioningTemplate
newDescribeProvisioningTemplate pTemplateName_ =
  DescribeProvisioningTemplate'
    { templateName =
        pTemplateName_
    }

-- | The name of the provisioning template.
describeProvisioningTemplate_templateName :: Lens.Lens' DescribeProvisioningTemplate Prelude.Text
describeProvisioningTemplate_templateName = Lens.lens (\DescribeProvisioningTemplate' {templateName} -> templateName) (\s@DescribeProvisioningTemplate' {} a -> s {templateName = a} :: DescribeProvisioningTemplate)

instance Core.AWSRequest DescribeProvisioningTemplate where
  type
    AWSResponse DescribeProvisioningTemplate =
      DescribeProvisioningTemplateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProvisioningTemplateResponse'
            Prelude.<$> (x Data..?> "creationDate")
            Prelude.<*> (x Data..?> "defaultVersionId")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "enabled")
            Prelude.<*> (x Data..?> "lastModifiedDate")
            Prelude.<*> (x Data..?> "preProvisioningHook")
            Prelude.<*> (x Data..?> "provisioningRoleArn")
            Prelude.<*> (x Data..?> "templateArn")
            Prelude.<*> (x Data..?> "templateBody")
            Prelude.<*> (x Data..?> "templateName")
            Prelude.<*> (x Data..?> "type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeProvisioningTemplate
  where
  hashWithSalt _salt DescribeProvisioningTemplate' {..} =
    _salt `Prelude.hashWithSalt` templateName

instance Prelude.NFData DescribeProvisioningTemplate where
  rnf DescribeProvisioningTemplate' {..} =
    Prelude.rnf templateName

instance Data.ToHeaders DescribeProvisioningTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeProvisioningTemplate where
  toPath DescribeProvisioningTemplate' {..} =
    Prelude.mconcat
      ["/provisioning-templates/", Data.toBS templateName]

instance Data.ToQuery DescribeProvisioningTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProvisioningTemplateResponse' smart constructor.
data DescribeProvisioningTemplateResponse = DescribeProvisioningTemplateResponse'
  { -- | The date when the provisioning template was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The default fleet template version ID.
    defaultVersionId :: Prelude.Maybe Prelude.Int,
    -- | The description of the provisioning template.
    description :: Prelude.Maybe Prelude.Text,
    -- | True if the provisioning template is enabled, otherwise false.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The date when the provisioning template was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | Gets information about a pre-provisioned hook.
    preProvisioningHook :: Prelude.Maybe ProvisioningHook,
    -- | The ARN of the role associated with the provisioning template. This IoT
    -- role grants permission to provision a device.
    provisioningRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the provisioning template.
    templateArn :: Prelude.Maybe Prelude.Text,
    -- | The JSON formatted contents of the provisioning template.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | The name of the provisioning template.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | The type you define in a provisioning template. You can create a
    -- template with only one type. You can\'t change the template type after
    -- its creation. The default value is @FLEET_PROVISIONING@. For more
    -- information about provisioning template, see:
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/provision-template.html Provisioning template>.
    type' :: Prelude.Maybe TemplateType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProvisioningTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'describeProvisioningTemplateResponse_creationDate' - The date when the provisioning template was created.
--
-- 'defaultVersionId', 'describeProvisioningTemplateResponse_defaultVersionId' - The default fleet template version ID.
--
-- 'description', 'describeProvisioningTemplateResponse_description' - The description of the provisioning template.
--
-- 'enabled', 'describeProvisioningTemplateResponse_enabled' - True if the provisioning template is enabled, otherwise false.
--
-- 'lastModifiedDate', 'describeProvisioningTemplateResponse_lastModifiedDate' - The date when the provisioning template was last modified.
--
-- 'preProvisioningHook', 'describeProvisioningTemplateResponse_preProvisioningHook' - Gets information about a pre-provisioned hook.
--
-- 'provisioningRoleArn', 'describeProvisioningTemplateResponse_provisioningRoleArn' - The ARN of the role associated with the provisioning template. This IoT
-- role grants permission to provision a device.
--
-- 'templateArn', 'describeProvisioningTemplateResponse_templateArn' - The ARN of the provisioning template.
--
-- 'templateBody', 'describeProvisioningTemplateResponse_templateBody' - The JSON formatted contents of the provisioning template.
--
-- 'templateName', 'describeProvisioningTemplateResponse_templateName' - The name of the provisioning template.
--
-- 'type'', 'describeProvisioningTemplateResponse_type' - The type you define in a provisioning template. You can create a
-- template with only one type. You can\'t change the template type after
-- its creation. The default value is @FLEET_PROVISIONING@. For more
-- information about provisioning template, see:
-- <https://docs.aws.amazon.com/iot/latest/developerguide/provision-template.html Provisioning template>.
--
-- 'httpStatus', 'describeProvisioningTemplateResponse_httpStatus' - The response's http status code.
newDescribeProvisioningTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeProvisioningTemplateResponse
newDescribeProvisioningTemplateResponse pHttpStatus_ =
  DescribeProvisioningTemplateResponse'
    { creationDate =
        Prelude.Nothing,
      defaultVersionId = Prelude.Nothing,
      description = Prelude.Nothing,
      enabled = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      preProvisioningHook = Prelude.Nothing,
      provisioningRoleArn = Prelude.Nothing,
      templateArn = Prelude.Nothing,
      templateBody = Prelude.Nothing,
      templateName = Prelude.Nothing,
      type' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date when the provisioning template was created.
describeProvisioningTemplateResponse_creationDate :: Lens.Lens' DescribeProvisioningTemplateResponse (Prelude.Maybe Prelude.UTCTime)
describeProvisioningTemplateResponse_creationDate = Lens.lens (\DescribeProvisioningTemplateResponse' {creationDate} -> creationDate) (\s@DescribeProvisioningTemplateResponse' {} a -> s {creationDate = a} :: DescribeProvisioningTemplateResponse) Prelude.. Lens.mapping Data._Time

-- | The default fleet template version ID.
describeProvisioningTemplateResponse_defaultVersionId :: Lens.Lens' DescribeProvisioningTemplateResponse (Prelude.Maybe Prelude.Int)
describeProvisioningTemplateResponse_defaultVersionId = Lens.lens (\DescribeProvisioningTemplateResponse' {defaultVersionId} -> defaultVersionId) (\s@DescribeProvisioningTemplateResponse' {} a -> s {defaultVersionId = a} :: DescribeProvisioningTemplateResponse)

-- | The description of the provisioning template.
describeProvisioningTemplateResponse_description :: Lens.Lens' DescribeProvisioningTemplateResponse (Prelude.Maybe Prelude.Text)
describeProvisioningTemplateResponse_description = Lens.lens (\DescribeProvisioningTemplateResponse' {description} -> description) (\s@DescribeProvisioningTemplateResponse' {} a -> s {description = a} :: DescribeProvisioningTemplateResponse)

-- | True if the provisioning template is enabled, otherwise false.
describeProvisioningTemplateResponse_enabled :: Lens.Lens' DescribeProvisioningTemplateResponse (Prelude.Maybe Prelude.Bool)
describeProvisioningTemplateResponse_enabled = Lens.lens (\DescribeProvisioningTemplateResponse' {enabled} -> enabled) (\s@DescribeProvisioningTemplateResponse' {} a -> s {enabled = a} :: DescribeProvisioningTemplateResponse)

-- | The date when the provisioning template was last modified.
describeProvisioningTemplateResponse_lastModifiedDate :: Lens.Lens' DescribeProvisioningTemplateResponse (Prelude.Maybe Prelude.UTCTime)
describeProvisioningTemplateResponse_lastModifiedDate = Lens.lens (\DescribeProvisioningTemplateResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeProvisioningTemplateResponse' {} a -> s {lastModifiedDate = a} :: DescribeProvisioningTemplateResponse) Prelude.. Lens.mapping Data._Time

-- | Gets information about a pre-provisioned hook.
describeProvisioningTemplateResponse_preProvisioningHook :: Lens.Lens' DescribeProvisioningTemplateResponse (Prelude.Maybe ProvisioningHook)
describeProvisioningTemplateResponse_preProvisioningHook = Lens.lens (\DescribeProvisioningTemplateResponse' {preProvisioningHook} -> preProvisioningHook) (\s@DescribeProvisioningTemplateResponse' {} a -> s {preProvisioningHook = a} :: DescribeProvisioningTemplateResponse)

-- | The ARN of the role associated with the provisioning template. This IoT
-- role grants permission to provision a device.
describeProvisioningTemplateResponse_provisioningRoleArn :: Lens.Lens' DescribeProvisioningTemplateResponse (Prelude.Maybe Prelude.Text)
describeProvisioningTemplateResponse_provisioningRoleArn = Lens.lens (\DescribeProvisioningTemplateResponse' {provisioningRoleArn} -> provisioningRoleArn) (\s@DescribeProvisioningTemplateResponse' {} a -> s {provisioningRoleArn = a} :: DescribeProvisioningTemplateResponse)

-- | The ARN of the provisioning template.
describeProvisioningTemplateResponse_templateArn :: Lens.Lens' DescribeProvisioningTemplateResponse (Prelude.Maybe Prelude.Text)
describeProvisioningTemplateResponse_templateArn = Lens.lens (\DescribeProvisioningTemplateResponse' {templateArn} -> templateArn) (\s@DescribeProvisioningTemplateResponse' {} a -> s {templateArn = a} :: DescribeProvisioningTemplateResponse)

-- | The JSON formatted contents of the provisioning template.
describeProvisioningTemplateResponse_templateBody :: Lens.Lens' DescribeProvisioningTemplateResponse (Prelude.Maybe Prelude.Text)
describeProvisioningTemplateResponse_templateBody = Lens.lens (\DescribeProvisioningTemplateResponse' {templateBody} -> templateBody) (\s@DescribeProvisioningTemplateResponse' {} a -> s {templateBody = a} :: DescribeProvisioningTemplateResponse)

-- | The name of the provisioning template.
describeProvisioningTemplateResponse_templateName :: Lens.Lens' DescribeProvisioningTemplateResponse (Prelude.Maybe Prelude.Text)
describeProvisioningTemplateResponse_templateName = Lens.lens (\DescribeProvisioningTemplateResponse' {templateName} -> templateName) (\s@DescribeProvisioningTemplateResponse' {} a -> s {templateName = a} :: DescribeProvisioningTemplateResponse)

-- | The type you define in a provisioning template. You can create a
-- template with only one type. You can\'t change the template type after
-- its creation. The default value is @FLEET_PROVISIONING@. For more
-- information about provisioning template, see:
-- <https://docs.aws.amazon.com/iot/latest/developerguide/provision-template.html Provisioning template>.
describeProvisioningTemplateResponse_type :: Lens.Lens' DescribeProvisioningTemplateResponse (Prelude.Maybe TemplateType)
describeProvisioningTemplateResponse_type = Lens.lens (\DescribeProvisioningTemplateResponse' {type'} -> type') (\s@DescribeProvisioningTemplateResponse' {} a -> s {type' = a} :: DescribeProvisioningTemplateResponse)

-- | The response's http status code.
describeProvisioningTemplateResponse_httpStatus :: Lens.Lens' DescribeProvisioningTemplateResponse Prelude.Int
describeProvisioningTemplateResponse_httpStatus = Lens.lens (\DescribeProvisioningTemplateResponse' {httpStatus} -> httpStatus) (\s@DescribeProvisioningTemplateResponse' {} a -> s {httpStatus = a} :: DescribeProvisioningTemplateResponse)

instance
  Prelude.NFData
    DescribeProvisioningTemplateResponse
  where
  rnf DescribeProvisioningTemplateResponse' {..} =
    Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf defaultVersionId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf preProvisioningHook
      `Prelude.seq` Prelude.rnf provisioningRoleArn
      `Prelude.seq` Prelude.rnf templateArn
      `Prelude.seq` Prelude.rnf templateBody
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf httpStatus
