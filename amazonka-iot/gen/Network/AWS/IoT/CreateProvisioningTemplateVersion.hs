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
-- Module      : Network.AWS.IoT.CreateProvisioningTemplateVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of a fleet provisioning template.
module Network.AWS.IoT.CreateProvisioningTemplateVersion
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
    createProvisioningTemplateVersionResponse_templateName,
    createProvisioningTemplateVersionResponse_versionId,
    createProvisioningTemplateVersionResponse_isDefaultVersion,
    createProvisioningTemplateVersionResponse_templateArn,
    createProvisioningTemplateVersionResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateProvisioningTemplateVersion' smart constructor.
data CreateProvisioningTemplateVersion = CreateProvisioningTemplateVersion'
  { -- | Sets a fleet provision template version as the default version.
    setAsDefault :: Prelude.Maybe Prelude.Bool,
    -- | The name of the fleet provisioning template.
    templateName :: Prelude.Text,
    -- | The JSON formatted contents of the fleet provisioning template.
    templateBody :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'templateName', 'createProvisioningTemplateVersion_templateName' - The name of the fleet provisioning template.
--
-- 'templateBody', 'createProvisioningTemplateVersion_templateBody' - The JSON formatted contents of the fleet provisioning template.
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

-- | The name of the fleet provisioning template.
createProvisioningTemplateVersion_templateName :: Lens.Lens' CreateProvisioningTemplateVersion Prelude.Text
createProvisioningTemplateVersion_templateName = Lens.lens (\CreateProvisioningTemplateVersion' {templateName} -> templateName) (\s@CreateProvisioningTemplateVersion' {} a -> s {templateName = a} :: CreateProvisioningTemplateVersion)

-- | The JSON formatted contents of the fleet provisioning template.
createProvisioningTemplateVersion_templateBody :: Lens.Lens' CreateProvisioningTemplateVersion Prelude.Text
createProvisioningTemplateVersion_templateBody = Lens.lens (\CreateProvisioningTemplateVersion' {templateBody} -> templateBody) (\s@CreateProvisioningTemplateVersion' {} a -> s {templateBody = a} :: CreateProvisioningTemplateVersion)

instance
  Prelude.AWSRequest
    CreateProvisioningTemplateVersion
  where
  type
    Rs CreateProvisioningTemplateVersion =
      CreateProvisioningTemplateVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateProvisioningTemplateVersionResponse'
            Prelude.<$> (x Prelude..?> "templateName")
              Prelude.<*> (x Prelude..?> "versionId")
              Prelude.<*> (x Prelude..?> "isDefaultVersion")
              Prelude.<*> (x Prelude..?> "templateArn")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateProvisioningTemplateVersion

instance
  Prelude.NFData
    CreateProvisioningTemplateVersion

instance
  Prelude.ToHeaders
    CreateProvisioningTemplateVersion
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToJSON
    CreateProvisioningTemplateVersion
  where
  toJSON CreateProvisioningTemplateVersion' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("templateBody" Prelude..= templateBody)
          ]
      )

instance
  Prelude.ToPath
    CreateProvisioningTemplateVersion
  where
  toPath CreateProvisioningTemplateVersion' {..} =
    Prelude.mconcat
      [ "/provisioning-templates/",
        Prelude.toBS templateName,
        "/versions"
      ]

instance
  Prelude.ToQuery
    CreateProvisioningTemplateVersion
  where
  toQuery CreateProvisioningTemplateVersion' {..} =
    Prelude.mconcat
      ["setAsDefault" Prelude.=: setAsDefault]

-- | /See:/ 'newCreateProvisioningTemplateVersionResponse' smart constructor.
data CreateProvisioningTemplateVersionResponse = CreateProvisioningTemplateVersionResponse'
  { -- | The name of the fleet provisioning template.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | The version of the fleet provisioning template.
    versionId :: Prelude.Maybe Prelude.Int,
    -- | True if the fleet provisioning template version is the default version,
    -- otherwise false.
    isDefaultVersion :: Prelude.Maybe Prelude.Bool,
    -- | The ARN that identifies the provisioning template.
    templateArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateProvisioningTemplateVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'createProvisioningTemplateVersionResponse_templateName' - The name of the fleet provisioning template.
--
-- 'versionId', 'createProvisioningTemplateVersionResponse_versionId' - The version of the fleet provisioning template.
--
-- 'isDefaultVersion', 'createProvisioningTemplateVersionResponse_isDefaultVersion' - True if the fleet provisioning template version is the default version,
-- otherwise false.
--
-- 'templateArn', 'createProvisioningTemplateVersionResponse_templateArn' - The ARN that identifies the provisioning template.
--
-- 'httpStatus', 'createProvisioningTemplateVersionResponse_httpStatus' - The response's http status code.
newCreateProvisioningTemplateVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateProvisioningTemplateVersionResponse
newCreateProvisioningTemplateVersionResponse
  pHttpStatus_ =
    CreateProvisioningTemplateVersionResponse'
      { templateName =
          Prelude.Nothing,
        versionId = Prelude.Nothing,
        isDefaultVersion =
          Prelude.Nothing,
        templateArn = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The name of the fleet provisioning template.
createProvisioningTemplateVersionResponse_templateName :: Lens.Lens' CreateProvisioningTemplateVersionResponse (Prelude.Maybe Prelude.Text)
createProvisioningTemplateVersionResponse_templateName = Lens.lens (\CreateProvisioningTemplateVersionResponse' {templateName} -> templateName) (\s@CreateProvisioningTemplateVersionResponse' {} a -> s {templateName = a} :: CreateProvisioningTemplateVersionResponse)

-- | The version of the fleet provisioning template.
createProvisioningTemplateVersionResponse_versionId :: Lens.Lens' CreateProvisioningTemplateVersionResponse (Prelude.Maybe Prelude.Int)
createProvisioningTemplateVersionResponse_versionId = Lens.lens (\CreateProvisioningTemplateVersionResponse' {versionId} -> versionId) (\s@CreateProvisioningTemplateVersionResponse' {} a -> s {versionId = a} :: CreateProvisioningTemplateVersionResponse)

-- | True if the fleet provisioning template version is the default version,
-- otherwise false.
createProvisioningTemplateVersionResponse_isDefaultVersion :: Lens.Lens' CreateProvisioningTemplateVersionResponse (Prelude.Maybe Prelude.Bool)
createProvisioningTemplateVersionResponse_isDefaultVersion = Lens.lens (\CreateProvisioningTemplateVersionResponse' {isDefaultVersion} -> isDefaultVersion) (\s@CreateProvisioningTemplateVersionResponse' {} a -> s {isDefaultVersion = a} :: CreateProvisioningTemplateVersionResponse)

-- | The ARN that identifies the provisioning template.
createProvisioningTemplateVersionResponse_templateArn :: Lens.Lens' CreateProvisioningTemplateVersionResponse (Prelude.Maybe Prelude.Text)
createProvisioningTemplateVersionResponse_templateArn = Lens.lens (\CreateProvisioningTemplateVersionResponse' {templateArn} -> templateArn) (\s@CreateProvisioningTemplateVersionResponse' {} a -> s {templateArn = a} :: CreateProvisioningTemplateVersionResponse)

-- | The response's http status code.
createProvisioningTemplateVersionResponse_httpStatus :: Lens.Lens' CreateProvisioningTemplateVersionResponse Prelude.Int
createProvisioningTemplateVersionResponse_httpStatus = Lens.lens (\CreateProvisioningTemplateVersionResponse' {httpStatus} -> httpStatus) (\s@CreateProvisioningTemplateVersionResponse' {} a -> s {httpStatus = a} :: CreateProvisioningTemplateVersionResponse)

instance
  Prelude.NFData
    CreateProvisioningTemplateVersionResponse
