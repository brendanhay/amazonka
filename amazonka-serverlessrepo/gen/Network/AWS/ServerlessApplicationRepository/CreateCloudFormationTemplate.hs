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
-- Module      : Network.AWS.ServerlessApplicationRepository.CreateCloudFormationTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS CloudFormation template.
module Network.AWS.ServerlessApplicationRepository.CreateCloudFormationTemplate
  ( -- * Creating a Request
    CreateCloudFormationTemplate (..),
    newCreateCloudFormationTemplate,

    -- * Request Lenses
    createCloudFormationTemplate_semanticVersion,
    createCloudFormationTemplate_applicationId,

    -- * Destructuring the Response
    CreateCloudFormationTemplateResponse (..),
    newCreateCloudFormationTemplateResponse,

    -- * Response Lenses
    createCloudFormationTemplateResponse_applicationId,
    createCloudFormationTemplateResponse_status,
    createCloudFormationTemplateResponse_creationTime,
    createCloudFormationTemplateResponse_expirationTime,
    createCloudFormationTemplateResponse_templateUrl,
    createCloudFormationTemplateResponse_semanticVersion,
    createCloudFormationTemplateResponse_templateId,
    createCloudFormationTemplateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'newCreateCloudFormationTemplate' smart constructor.
data CreateCloudFormationTemplate = CreateCloudFormationTemplate'
  { -- | The semantic version of the application:
    --
    -- <https://semver.org/>
    semanticVersion :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCloudFormationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'semanticVersion', 'createCloudFormationTemplate_semanticVersion' - The semantic version of the application:
--
-- <https://semver.org/>
--
-- 'applicationId', 'createCloudFormationTemplate_applicationId' - The Amazon Resource Name (ARN) of the application.
newCreateCloudFormationTemplate ::
  -- | 'applicationId'
  Core.Text ->
  CreateCloudFormationTemplate
newCreateCloudFormationTemplate pApplicationId_ =
  CreateCloudFormationTemplate'
    { semanticVersion =
        Core.Nothing,
      applicationId = pApplicationId_
    }

-- | The semantic version of the application:
--
-- <https://semver.org/>
createCloudFormationTemplate_semanticVersion :: Lens.Lens' CreateCloudFormationTemplate (Core.Maybe Core.Text)
createCloudFormationTemplate_semanticVersion = Lens.lens (\CreateCloudFormationTemplate' {semanticVersion} -> semanticVersion) (\s@CreateCloudFormationTemplate' {} a -> s {semanticVersion = a} :: CreateCloudFormationTemplate)

-- | The Amazon Resource Name (ARN) of the application.
createCloudFormationTemplate_applicationId :: Lens.Lens' CreateCloudFormationTemplate Core.Text
createCloudFormationTemplate_applicationId = Lens.lens (\CreateCloudFormationTemplate' {applicationId} -> applicationId) (\s@CreateCloudFormationTemplate' {} a -> s {applicationId = a} :: CreateCloudFormationTemplate)

instance Core.AWSRequest CreateCloudFormationTemplate where
  type
    AWSResponse CreateCloudFormationTemplate =
      CreateCloudFormationTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCloudFormationTemplateResponse'
            Core.<$> (x Core..?> "applicationId")
            Core.<*> (x Core..?> "status")
            Core.<*> (x Core..?> "creationTime")
            Core.<*> (x Core..?> "expirationTime")
            Core.<*> (x Core..?> "templateUrl")
            Core.<*> (x Core..?> "semanticVersion")
            Core.<*> (x Core..?> "templateId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateCloudFormationTemplate

instance Core.NFData CreateCloudFormationTemplate

instance Core.ToHeaders CreateCloudFormationTemplate where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateCloudFormationTemplate where
  toJSON CreateCloudFormationTemplate' {..} =
    Core.object
      ( Core.catMaybes
          [ ("semanticVersion" Core..=)
              Core.<$> semanticVersion
          ]
      )

instance Core.ToPath CreateCloudFormationTemplate where
  toPath CreateCloudFormationTemplate' {..} =
    Core.mconcat
      [ "/applications/",
        Core.toBS applicationId,
        "/templates"
      ]

instance Core.ToQuery CreateCloudFormationTemplate where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateCloudFormationTemplateResponse' smart constructor.
data CreateCloudFormationTemplateResponse = CreateCloudFormationTemplateResponse'
  { -- | The application Amazon Resource Name (ARN).
    applicationId :: Core.Maybe Core.Text,
    -- | Status of the template creation workflow.
    --
    -- Possible values: PREPARING | ACTIVE | EXPIRED
    status :: Core.Maybe Status,
    -- | The date and time this resource was created.
    creationTime :: Core.Maybe Core.Text,
    -- | The date and time this template expires. Templates expire 1 hour after
    -- creation.
    expirationTime :: Core.Maybe Core.Text,
    -- | A link to the template that can be used to deploy the application using
    -- AWS CloudFormation.
    templateUrl :: Core.Maybe Core.Text,
    -- | The semantic version of the application:
    --
    -- <https://semver.org/>
    semanticVersion :: Core.Maybe Core.Text,
    -- | The UUID returned by CreateCloudFormationTemplate.
    --
    -- Pattern:
    -- [0-9a-fA-F]{8}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{12}
    templateId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCloudFormationTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'createCloudFormationTemplateResponse_applicationId' - The application Amazon Resource Name (ARN).
--
-- 'status', 'createCloudFormationTemplateResponse_status' - Status of the template creation workflow.
--
-- Possible values: PREPARING | ACTIVE | EXPIRED
--
-- 'creationTime', 'createCloudFormationTemplateResponse_creationTime' - The date and time this resource was created.
--
-- 'expirationTime', 'createCloudFormationTemplateResponse_expirationTime' - The date and time this template expires. Templates expire 1 hour after
-- creation.
--
-- 'templateUrl', 'createCloudFormationTemplateResponse_templateUrl' - A link to the template that can be used to deploy the application using
-- AWS CloudFormation.
--
-- 'semanticVersion', 'createCloudFormationTemplateResponse_semanticVersion' - The semantic version of the application:
--
-- <https://semver.org/>
--
-- 'templateId', 'createCloudFormationTemplateResponse_templateId' - The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern:
-- [0-9a-fA-F]{8}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{12}
--
-- 'httpStatus', 'createCloudFormationTemplateResponse_httpStatus' - The response's http status code.
newCreateCloudFormationTemplateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateCloudFormationTemplateResponse
newCreateCloudFormationTemplateResponse pHttpStatus_ =
  CreateCloudFormationTemplateResponse'
    { applicationId =
        Core.Nothing,
      status = Core.Nothing,
      creationTime = Core.Nothing,
      expirationTime = Core.Nothing,
      templateUrl = Core.Nothing,
      semanticVersion = Core.Nothing,
      templateId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The application Amazon Resource Name (ARN).
createCloudFormationTemplateResponse_applicationId :: Lens.Lens' CreateCloudFormationTemplateResponse (Core.Maybe Core.Text)
createCloudFormationTemplateResponse_applicationId = Lens.lens (\CreateCloudFormationTemplateResponse' {applicationId} -> applicationId) (\s@CreateCloudFormationTemplateResponse' {} a -> s {applicationId = a} :: CreateCloudFormationTemplateResponse)

-- | Status of the template creation workflow.
--
-- Possible values: PREPARING | ACTIVE | EXPIRED
createCloudFormationTemplateResponse_status :: Lens.Lens' CreateCloudFormationTemplateResponse (Core.Maybe Status)
createCloudFormationTemplateResponse_status = Lens.lens (\CreateCloudFormationTemplateResponse' {status} -> status) (\s@CreateCloudFormationTemplateResponse' {} a -> s {status = a} :: CreateCloudFormationTemplateResponse)

-- | The date and time this resource was created.
createCloudFormationTemplateResponse_creationTime :: Lens.Lens' CreateCloudFormationTemplateResponse (Core.Maybe Core.Text)
createCloudFormationTemplateResponse_creationTime = Lens.lens (\CreateCloudFormationTemplateResponse' {creationTime} -> creationTime) (\s@CreateCloudFormationTemplateResponse' {} a -> s {creationTime = a} :: CreateCloudFormationTemplateResponse)

-- | The date and time this template expires. Templates expire 1 hour after
-- creation.
createCloudFormationTemplateResponse_expirationTime :: Lens.Lens' CreateCloudFormationTemplateResponse (Core.Maybe Core.Text)
createCloudFormationTemplateResponse_expirationTime = Lens.lens (\CreateCloudFormationTemplateResponse' {expirationTime} -> expirationTime) (\s@CreateCloudFormationTemplateResponse' {} a -> s {expirationTime = a} :: CreateCloudFormationTemplateResponse)

-- | A link to the template that can be used to deploy the application using
-- AWS CloudFormation.
createCloudFormationTemplateResponse_templateUrl :: Lens.Lens' CreateCloudFormationTemplateResponse (Core.Maybe Core.Text)
createCloudFormationTemplateResponse_templateUrl = Lens.lens (\CreateCloudFormationTemplateResponse' {templateUrl} -> templateUrl) (\s@CreateCloudFormationTemplateResponse' {} a -> s {templateUrl = a} :: CreateCloudFormationTemplateResponse)

-- | The semantic version of the application:
--
-- <https://semver.org/>
createCloudFormationTemplateResponse_semanticVersion :: Lens.Lens' CreateCloudFormationTemplateResponse (Core.Maybe Core.Text)
createCloudFormationTemplateResponse_semanticVersion = Lens.lens (\CreateCloudFormationTemplateResponse' {semanticVersion} -> semanticVersion) (\s@CreateCloudFormationTemplateResponse' {} a -> s {semanticVersion = a} :: CreateCloudFormationTemplateResponse)

-- | The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern:
-- [0-9a-fA-F]{8}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{12}
createCloudFormationTemplateResponse_templateId :: Lens.Lens' CreateCloudFormationTemplateResponse (Core.Maybe Core.Text)
createCloudFormationTemplateResponse_templateId = Lens.lens (\CreateCloudFormationTemplateResponse' {templateId} -> templateId) (\s@CreateCloudFormationTemplateResponse' {} a -> s {templateId = a} :: CreateCloudFormationTemplateResponse)

-- | The response's http status code.
createCloudFormationTemplateResponse_httpStatus :: Lens.Lens' CreateCloudFormationTemplateResponse Core.Int
createCloudFormationTemplateResponse_httpStatus = Lens.lens (\CreateCloudFormationTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateCloudFormationTemplateResponse' {} a -> s {httpStatus = a} :: CreateCloudFormationTemplateResponse)

instance
  Core.NFData
    CreateCloudFormationTemplateResponse
