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
-- Module      : Amazonka.IoT.CreateProvisioningTemplateVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of a provisioning template.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions CreateProvisioningTemplateVersion>
-- action.
module Amazonka.IoT.CreateProvisioningTemplateVersion
  ( -- * Creating a Request
    CreateProvisioningTemplateVersion (..),
    newCreateProvisioningTemplateVersion,

    -- * Request Lenses
    createProvisioningTemplateVersion_setAsDefault,
    createProvisioningTemplateVersion_templateName,
    createProvisioningTemplateVersion_templateBody,

    -- * Destructuring the Response
    CreateProvisioningTemplateVersionResponse (..),
    newCreateProvisioningTemplateVersionResponse,

    -- * Response Lenses
    createProvisioningTemplateVersionResponse_isDefaultVersion,
    createProvisioningTemplateVersionResponse_templateArn,
    createProvisioningTemplateVersionResponse_templateName,
    createProvisioningTemplateVersionResponse_versionId,
    createProvisioningTemplateVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateProvisioningTemplateVersion' smart constructor.
data CreateProvisioningTemplateVersion = CreateProvisioningTemplateVersion'
  { -- | Sets a fleet provision template version as the default version.
    setAsDefault :: Prelude.Maybe Prelude.Bool,
    -- | The name of the provisioning template.
    templateName :: Prelude.Text,
    -- | The JSON formatted contents of the provisioning template.
    templateBody :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProvisioningTemplateVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'setAsDefault', 'createProvisioningTemplateVersion_setAsDefault' - Sets a fleet provision template version as the default version.
--
-- 'templateName', 'createProvisioningTemplateVersion_templateName' - The name of the provisioning template.
--
-- 'templateBody', 'createProvisioningTemplateVersion_templateBody' - The JSON formatted contents of the provisioning template.
newCreateProvisioningTemplateVersion ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'templateBody'
  Prelude.Text ->
  CreateProvisioningTemplateVersion
newCreateProvisioningTemplateVersion
  pTemplateName_
  pTemplateBody_ =
    CreateProvisioningTemplateVersion'
      { setAsDefault =
          Prelude.Nothing,
        templateName = pTemplateName_,
        templateBody = pTemplateBody_
      }

-- | Sets a fleet provision template version as the default version.
createProvisioningTemplateVersion_setAsDefault :: Lens.Lens' CreateProvisioningTemplateVersion (Prelude.Maybe Prelude.Bool)
createProvisioningTemplateVersion_setAsDefault = Lens.lens (\CreateProvisioningTemplateVersion' {setAsDefault} -> setAsDefault) (\s@CreateProvisioningTemplateVersion' {} a -> s {setAsDefault = a} :: CreateProvisioningTemplateVersion)

-- | The name of the provisioning template.
createProvisioningTemplateVersion_templateName :: Lens.Lens' CreateProvisioningTemplateVersion Prelude.Text
createProvisioningTemplateVersion_templateName = Lens.lens (\CreateProvisioningTemplateVersion' {templateName} -> templateName) (\s@CreateProvisioningTemplateVersion' {} a -> s {templateName = a} :: CreateProvisioningTemplateVersion)

-- | The JSON formatted contents of the provisioning template.
createProvisioningTemplateVersion_templateBody :: Lens.Lens' CreateProvisioningTemplateVersion Prelude.Text
createProvisioningTemplateVersion_templateBody = Lens.lens (\CreateProvisioningTemplateVersion' {templateBody} -> templateBody) (\s@CreateProvisioningTemplateVersion' {} a -> s {templateBody = a} :: CreateProvisioningTemplateVersion)

instance
  Core.AWSRequest
    CreateProvisioningTemplateVersion
  where
  type
    AWSResponse CreateProvisioningTemplateVersion =
      CreateProvisioningTemplateVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProvisioningTemplateVersionResponse'
            Prelude.<$> (x Data..?> "isDefaultVersion")
              Prelude.<*> (x Data..?> "templateArn")
              Prelude.<*> (x Data..?> "templateName")
              Prelude.<*> (x Data..?> "versionId")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateProvisioningTemplateVersion
  where
  hashWithSalt
    _salt
    CreateProvisioningTemplateVersion' {..} =
      _salt `Prelude.hashWithSalt` setAsDefault
        `Prelude.hashWithSalt` templateName
        `Prelude.hashWithSalt` templateBody

instance
  Prelude.NFData
    CreateProvisioningTemplateVersion
  where
  rnf CreateProvisioningTemplateVersion' {..} =
    Prelude.rnf setAsDefault
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateBody

instance
  Data.ToHeaders
    CreateProvisioningTemplateVersion
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    CreateProvisioningTemplateVersion
  where
  toJSON CreateProvisioningTemplateVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("templateBody" Data..= templateBody)]
      )

instance
  Data.ToPath
    CreateProvisioningTemplateVersion
  where
  toPath CreateProvisioningTemplateVersion' {..} =
    Prelude.mconcat
      [ "/provisioning-templates/",
        Data.toBS templateName,
        "/versions"
      ]

instance
  Data.ToQuery
    CreateProvisioningTemplateVersion
  where
  toQuery CreateProvisioningTemplateVersion' {..} =
    Prelude.mconcat
      ["setAsDefault" Data.=: setAsDefault]

-- | /See:/ 'newCreateProvisioningTemplateVersionResponse' smart constructor.
data CreateProvisioningTemplateVersionResponse = CreateProvisioningTemplateVersionResponse'
  { -- | True if the provisioning template version is the default version,
    -- otherwise false.
    isDefaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | The ARN that identifies the provisioning template.
    templateArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the provisioning template.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | The version of the provisioning template.
    versionId :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateProvisioningTemplateVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isDefaultVersion', 'createProvisioningTemplateVersionResponse_isDefaultVersion' - True if the provisioning template version is the default version,
-- otherwise false.
--
-- 'templateArn', 'createProvisioningTemplateVersionResponse_templateArn' - The ARN that identifies the provisioning template.
--
-- 'templateName', 'createProvisioningTemplateVersionResponse_templateName' - The name of the provisioning template.
--
-- 'versionId', 'createProvisioningTemplateVersionResponse_versionId' - The version of the provisioning template.
--
-- 'httpStatus', 'createProvisioningTemplateVersionResponse_httpStatus' - The response's http status code.
newCreateProvisioningTemplateVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProvisioningTemplateVersionResponse
newCreateProvisioningTemplateVersionResponse
  pHttpStatus_ =
    CreateProvisioningTemplateVersionResponse'
      { isDefaultVersion =
          Prelude.Nothing,
        templateArn = Prelude.Nothing,
        templateName = Prelude.Nothing,
        versionId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | True if the provisioning template version is the default version,
-- otherwise false.
createProvisioningTemplateVersionResponse_isDefaultVersion :: Lens.Lens' CreateProvisioningTemplateVersionResponse (Prelude.Maybe Prelude.Bool)
createProvisioningTemplateVersionResponse_isDefaultVersion = Lens.lens (\CreateProvisioningTemplateVersionResponse' {isDefaultVersion} -> isDefaultVersion) (\s@CreateProvisioningTemplateVersionResponse' {} a -> s {isDefaultVersion = a} :: CreateProvisioningTemplateVersionResponse)

-- | The ARN that identifies the provisioning template.
createProvisioningTemplateVersionResponse_templateArn :: Lens.Lens' CreateProvisioningTemplateVersionResponse (Prelude.Maybe Prelude.Text)
createProvisioningTemplateVersionResponse_templateArn = Lens.lens (\CreateProvisioningTemplateVersionResponse' {templateArn} -> templateArn) (\s@CreateProvisioningTemplateVersionResponse' {} a -> s {templateArn = a} :: CreateProvisioningTemplateVersionResponse)

-- | The name of the provisioning template.
createProvisioningTemplateVersionResponse_templateName :: Lens.Lens' CreateProvisioningTemplateVersionResponse (Prelude.Maybe Prelude.Text)
createProvisioningTemplateVersionResponse_templateName = Lens.lens (\CreateProvisioningTemplateVersionResponse' {templateName} -> templateName) (\s@CreateProvisioningTemplateVersionResponse' {} a -> s {templateName = a} :: CreateProvisioningTemplateVersionResponse)

-- | The version of the provisioning template.
createProvisioningTemplateVersionResponse_versionId :: Lens.Lens' CreateProvisioningTemplateVersionResponse (Prelude.Maybe Prelude.Int)
createProvisioningTemplateVersionResponse_versionId = Lens.lens (\CreateProvisioningTemplateVersionResponse' {versionId} -> versionId) (\s@CreateProvisioningTemplateVersionResponse' {} a -> s {versionId = a} :: CreateProvisioningTemplateVersionResponse)

-- | The response's http status code.
createProvisioningTemplateVersionResponse_httpStatus :: Lens.Lens' CreateProvisioningTemplateVersionResponse Prelude.Int
createProvisioningTemplateVersionResponse_httpStatus = Lens.lens (\CreateProvisioningTemplateVersionResponse' {httpStatus} -> httpStatus) (\s@CreateProvisioningTemplateVersionResponse' {} a -> s {httpStatus = a} :: CreateProvisioningTemplateVersionResponse)

instance
  Prelude.NFData
    CreateProvisioningTemplateVersionResponse
  where
  rnf CreateProvisioningTemplateVersionResponse' {..} =
    Prelude.rnf isDefaultVersion
      `Prelude.seq` Prelude.rnf templateArn
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf httpStatus
