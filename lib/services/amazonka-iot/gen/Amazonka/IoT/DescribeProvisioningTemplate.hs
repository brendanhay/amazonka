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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a fleet provisioning template.
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
    describeProvisioningTemplateResponse_lastModifiedDate,
    describeProvisioningTemplateResponse_templateName,
    describeProvisioningTemplateResponse_preProvisioningHook,
    describeProvisioningTemplateResponse_enabled,
    describeProvisioningTemplateResponse_provisioningRoleArn,
    describeProvisioningTemplateResponse_defaultVersionId,
    describeProvisioningTemplateResponse_creationDate,
    describeProvisioningTemplateResponse_templateArn,
    describeProvisioningTemplateResponse_templateBody,
    describeProvisioningTemplateResponse_description,
    describeProvisioningTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeProvisioningTemplate' smart constructor.
data DescribeProvisioningTemplate = DescribeProvisioningTemplate'
  { -- | The name of the fleet provisioning template.
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
-- 'templateName', 'describeProvisioningTemplate_templateName' - The name of the fleet provisioning template.
newDescribeProvisioningTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  DescribeProvisioningTemplate
newDescribeProvisioningTemplate pTemplateName_ =
  DescribeProvisioningTemplate'
    { templateName =
        pTemplateName_
    }

-- | The name of the fleet provisioning template.
describeProvisioningTemplate_templateName :: Lens.Lens' DescribeProvisioningTemplate Prelude.Text
describeProvisioningTemplate_templateName = Lens.lens (\DescribeProvisioningTemplate' {templateName} -> templateName) (\s@DescribeProvisioningTemplate' {} a -> s {templateName = a} :: DescribeProvisioningTemplate)

instance Core.AWSRequest DescribeProvisioningTemplate where
  type
    AWSResponse DescribeProvisioningTemplate =
      DescribeProvisioningTemplateResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProvisioningTemplateResponse'
            Prelude.<$> (x Core..?> "lastModifiedDate")
            Prelude.<*> (x Core..?> "templateName")
            Prelude.<*> (x Core..?> "preProvisioningHook")
            Prelude.<*> (x Core..?> "enabled")
            Prelude.<*> (x Core..?> "provisioningRoleArn")
            Prelude.<*> (x Core..?> "defaultVersionId")
            Prelude.<*> (x Core..?> "creationDate")
            Prelude.<*> (x Core..?> "templateArn")
            Prelude.<*> (x Core..?> "templateBody")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeProvisioningTemplate
  where
  hashWithSalt salt' DescribeProvisioningTemplate' {..} =
    salt' `Prelude.hashWithSalt` templateName

instance Prelude.NFData DescribeProvisioningTemplate where
  rnf DescribeProvisioningTemplate' {..} =
    Prelude.rnf templateName

instance Core.ToHeaders DescribeProvisioningTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeProvisioningTemplate where
  toPath DescribeProvisioningTemplate' {..} =
    Prelude.mconcat
      ["/provisioning-templates/", Core.toBS templateName]

instance Core.ToQuery DescribeProvisioningTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProvisioningTemplateResponse' smart constructor.
data DescribeProvisioningTemplateResponse = DescribeProvisioningTemplateResponse'
  { -- | The date when the fleet provisioning template was last modified.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The name of the fleet provisioning template.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | Gets information about a pre-provisioned hook.
    preProvisioningHook :: Prelude.Maybe ProvisioningHook,
    -- | True if the fleet provisioning template is enabled, otherwise false.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the role associated with the provisioning template. This IoT
    -- role grants permission to provision a device.
    provisioningRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The default fleet template version ID.
    defaultVersionId :: Prelude.Maybe Prelude.Int,
    -- | The date when the fleet provisioning template was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the fleet provisioning template.
    templateArn :: Prelude.Maybe Prelude.Text,
    -- | The JSON formatted contents of the fleet provisioning template.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | The description of the fleet provisioning template.
    description :: Prelude.Maybe Prelude.Text,
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
-- 'lastModifiedDate', 'describeProvisioningTemplateResponse_lastModifiedDate' - The date when the fleet provisioning template was last modified.
--
-- 'templateName', 'describeProvisioningTemplateResponse_templateName' - The name of the fleet provisioning template.
--
-- 'preProvisioningHook', 'describeProvisioningTemplateResponse_preProvisioningHook' - Gets information about a pre-provisioned hook.
--
-- 'enabled', 'describeProvisioningTemplateResponse_enabled' - True if the fleet provisioning template is enabled, otherwise false.
--
-- 'provisioningRoleArn', 'describeProvisioningTemplateResponse_provisioningRoleArn' - The ARN of the role associated with the provisioning template. This IoT
-- role grants permission to provision a device.
--
-- 'defaultVersionId', 'describeProvisioningTemplateResponse_defaultVersionId' - The default fleet template version ID.
--
-- 'creationDate', 'describeProvisioningTemplateResponse_creationDate' - The date when the fleet provisioning template was created.
--
-- 'templateArn', 'describeProvisioningTemplateResponse_templateArn' - The ARN of the fleet provisioning template.
--
-- 'templateBody', 'describeProvisioningTemplateResponse_templateBody' - The JSON formatted contents of the fleet provisioning template.
--
-- 'description', 'describeProvisioningTemplateResponse_description' - The description of the fleet provisioning template.
--
-- 'httpStatus', 'describeProvisioningTemplateResponse_httpStatus' - The response's http status code.
newDescribeProvisioningTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeProvisioningTemplateResponse
newDescribeProvisioningTemplateResponse pHttpStatus_ =
  DescribeProvisioningTemplateResponse'
    { lastModifiedDate =
        Prelude.Nothing,
      templateName = Prelude.Nothing,
      preProvisioningHook = Prelude.Nothing,
      enabled = Prelude.Nothing,
      provisioningRoleArn = Prelude.Nothing,
      defaultVersionId = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      templateArn = Prelude.Nothing,
      templateBody = Prelude.Nothing,
      description = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date when the fleet provisioning template was last modified.
describeProvisioningTemplateResponse_lastModifiedDate :: Lens.Lens' DescribeProvisioningTemplateResponse (Prelude.Maybe Prelude.UTCTime)
describeProvisioningTemplateResponse_lastModifiedDate = Lens.lens (\DescribeProvisioningTemplateResponse' {lastModifiedDate} -> lastModifiedDate) (\s@DescribeProvisioningTemplateResponse' {} a -> s {lastModifiedDate = a} :: DescribeProvisioningTemplateResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the fleet provisioning template.
describeProvisioningTemplateResponse_templateName :: Lens.Lens' DescribeProvisioningTemplateResponse (Prelude.Maybe Prelude.Text)
describeProvisioningTemplateResponse_templateName = Lens.lens (\DescribeProvisioningTemplateResponse' {templateName} -> templateName) (\s@DescribeProvisioningTemplateResponse' {} a -> s {templateName = a} :: DescribeProvisioningTemplateResponse)

-- | Gets information about a pre-provisioned hook.
describeProvisioningTemplateResponse_preProvisioningHook :: Lens.Lens' DescribeProvisioningTemplateResponse (Prelude.Maybe ProvisioningHook)
describeProvisioningTemplateResponse_preProvisioningHook = Lens.lens (\DescribeProvisioningTemplateResponse' {preProvisioningHook} -> preProvisioningHook) (\s@DescribeProvisioningTemplateResponse' {} a -> s {preProvisioningHook = a} :: DescribeProvisioningTemplateResponse)

-- | True if the fleet provisioning template is enabled, otherwise false.
describeProvisioningTemplateResponse_enabled :: Lens.Lens' DescribeProvisioningTemplateResponse (Prelude.Maybe Prelude.Bool)
describeProvisioningTemplateResponse_enabled = Lens.lens (\DescribeProvisioningTemplateResponse' {enabled} -> enabled) (\s@DescribeProvisioningTemplateResponse' {} a -> s {enabled = a} :: DescribeProvisioningTemplateResponse)

-- | The ARN of the role associated with the provisioning template. This IoT
-- role grants permission to provision a device.
describeProvisioningTemplateResponse_provisioningRoleArn :: Lens.Lens' DescribeProvisioningTemplateResponse (Prelude.Maybe Prelude.Text)
describeProvisioningTemplateResponse_provisioningRoleArn = Lens.lens (\DescribeProvisioningTemplateResponse' {provisioningRoleArn} -> provisioningRoleArn) (\s@DescribeProvisioningTemplateResponse' {} a -> s {provisioningRoleArn = a} :: DescribeProvisioningTemplateResponse)

-- | The default fleet template version ID.
describeProvisioningTemplateResponse_defaultVersionId :: Lens.Lens' DescribeProvisioningTemplateResponse (Prelude.Maybe Prelude.Int)
describeProvisioningTemplateResponse_defaultVersionId = Lens.lens (\DescribeProvisioningTemplateResponse' {defaultVersionId} -> defaultVersionId) (\s@DescribeProvisioningTemplateResponse' {} a -> s {defaultVersionId = a} :: DescribeProvisioningTemplateResponse)

-- | The date when the fleet provisioning template was created.
describeProvisioningTemplateResponse_creationDate :: Lens.Lens' DescribeProvisioningTemplateResponse (Prelude.Maybe Prelude.UTCTime)
describeProvisioningTemplateResponse_creationDate = Lens.lens (\DescribeProvisioningTemplateResponse' {creationDate} -> creationDate) (\s@DescribeProvisioningTemplateResponse' {} a -> s {creationDate = a} :: DescribeProvisioningTemplateResponse) Prelude.. Lens.mapping Core._Time

-- | The ARN of the fleet provisioning template.
describeProvisioningTemplateResponse_templateArn :: Lens.Lens' DescribeProvisioningTemplateResponse (Prelude.Maybe Prelude.Text)
describeProvisioningTemplateResponse_templateArn = Lens.lens (\DescribeProvisioningTemplateResponse' {templateArn} -> templateArn) (\s@DescribeProvisioningTemplateResponse' {} a -> s {templateArn = a} :: DescribeProvisioningTemplateResponse)

-- | The JSON formatted contents of the fleet provisioning template.
describeProvisioningTemplateResponse_templateBody :: Lens.Lens' DescribeProvisioningTemplateResponse (Prelude.Maybe Prelude.Text)
describeProvisioningTemplateResponse_templateBody = Lens.lens (\DescribeProvisioningTemplateResponse' {templateBody} -> templateBody) (\s@DescribeProvisioningTemplateResponse' {} a -> s {templateBody = a} :: DescribeProvisioningTemplateResponse)

-- | The description of the fleet provisioning template.
describeProvisioningTemplateResponse_description :: Lens.Lens' DescribeProvisioningTemplateResponse (Prelude.Maybe Prelude.Text)
describeProvisioningTemplateResponse_description = Lens.lens (\DescribeProvisioningTemplateResponse' {description} -> description) (\s@DescribeProvisioningTemplateResponse' {} a -> s {description = a} :: DescribeProvisioningTemplateResponse)

-- | The response's http status code.
describeProvisioningTemplateResponse_httpStatus :: Lens.Lens' DescribeProvisioningTemplateResponse Prelude.Int
describeProvisioningTemplateResponse_httpStatus = Lens.lens (\DescribeProvisioningTemplateResponse' {httpStatus} -> httpStatus) (\s@DescribeProvisioningTemplateResponse' {} a -> s {httpStatus = a} :: DescribeProvisioningTemplateResponse)

instance
  Prelude.NFData
    DescribeProvisioningTemplateResponse
  where
  rnf DescribeProvisioningTemplateResponse' {..} =
    Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf templateBody
      `Prelude.seq` Prelude.rnf templateArn
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf defaultVersionId
      `Prelude.seq` Prelude.rnf provisioningRoleArn
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf preProvisioningHook
      `Prelude.seq` Prelude.rnf templateName
