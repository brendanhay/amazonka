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
-- Module      : Network.AWS.ServerlessApplicationRepository.GetCloudFormationTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified AWS CloudFormation template.
module Network.AWS.ServerlessApplicationRepository.GetCloudFormationTemplate
  ( -- * Creating a Request
    GetCloudFormationTemplate (..),
    newGetCloudFormationTemplate,

    -- * Request Lenses
    getCloudFormationTemplate_applicationId,
    getCloudFormationTemplate_templateId,

    -- * Destructuring the Response
    GetCloudFormationTemplateResponse (..),
    newGetCloudFormationTemplateResponse,

    -- * Response Lenses
    getCloudFormationTemplateResponse_applicationId,
    getCloudFormationTemplateResponse_status,
    getCloudFormationTemplateResponse_creationTime,
    getCloudFormationTemplateResponse_expirationTime,
    getCloudFormationTemplateResponse_templateUrl,
    getCloudFormationTemplateResponse_semanticVersion,
    getCloudFormationTemplateResponse_templateId,
    getCloudFormationTemplateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'newGetCloudFormationTemplate' smart constructor.
data GetCloudFormationTemplate = GetCloudFormationTemplate'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Core.Text,
    -- | The UUID returned by CreateCloudFormationTemplate.
    --
    -- Pattern:
    -- [0-9a-fA-F]{8}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{12}
    templateId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCloudFormationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getCloudFormationTemplate_applicationId' - The Amazon Resource Name (ARN) of the application.
--
-- 'templateId', 'getCloudFormationTemplate_templateId' - The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern:
-- [0-9a-fA-F]{8}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{12}
newGetCloudFormationTemplate ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'templateId'
  Core.Text ->
  GetCloudFormationTemplate
newGetCloudFormationTemplate
  pApplicationId_
  pTemplateId_ =
    GetCloudFormationTemplate'
      { applicationId =
          pApplicationId_,
        templateId = pTemplateId_
      }

-- | The Amazon Resource Name (ARN) of the application.
getCloudFormationTemplate_applicationId :: Lens.Lens' GetCloudFormationTemplate Core.Text
getCloudFormationTemplate_applicationId = Lens.lens (\GetCloudFormationTemplate' {applicationId} -> applicationId) (\s@GetCloudFormationTemplate' {} a -> s {applicationId = a} :: GetCloudFormationTemplate)

-- | The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern:
-- [0-9a-fA-F]{8}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{12}
getCloudFormationTemplate_templateId :: Lens.Lens' GetCloudFormationTemplate Core.Text
getCloudFormationTemplate_templateId = Lens.lens (\GetCloudFormationTemplate' {templateId} -> templateId) (\s@GetCloudFormationTemplate' {} a -> s {templateId = a} :: GetCloudFormationTemplate)

instance Core.AWSRequest GetCloudFormationTemplate where
  type
    AWSResponse GetCloudFormationTemplate =
      GetCloudFormationTemplateResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCloudFormationTemplateResponse'
            Core.<$> (x Core..?> "applicationId")
            Core.<*> (x Core..?> "status")
            Core.<*> (x Core..?> "creationTime")
            Core.<*> (x Core..?> "expirationTime")
            Core.<*> (x Core..?> "templateUrl")
            Core.<*> (x Core..?> "semanticVersion")
            Core.<*> (x Core..?> "templateId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetCloudFormationTemplate

instance Core.NFData GetCloudFormationTemplate

instance Core.ToHeaders GetCloudFormationTemplate where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetCloudFormationTemplate where
  toPath GetCloudFormationTemplate' {..} =
    Core.mconcat
      [ "/applications/",
        Core.toBS applicationId,
        "/templates/",
        Core.toBS templateId
      ]

instance Core.ToQuery GetCloudFormationTemplate where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetCloudFormationTemplateResponse' smart constructor.
data GetCloudFormationTemplateResponse = GetCloudFormationTemplateResponse'
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
-- Create a value of 'GetCloudFormationTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getCloudFormationTemplateResponse_applicationId' - The application Amazon Resource Name (ARN).
--
-- 'status', 'getCloudFormationTemplateResponse_status' - Status of the template creation workflow.
--
-- Possible values: PREPARING | ACTIVE | EXPIRED
--
-- 'creationTime', 'getCloudFormationTemplateResponse_creationTime' - The date and time this resource was created.
--
-- 'expirationTime', 'getCloudFormationTemplateResponse_expirationTime' - The date and time this template expires. Templates expire 1 hour after
-- creation.
--
-- 'templateUrl', 'getCloudFormationTemplateResponse_templateUrl' - A link to the template that can be used to deploy the application using
-- AWS CloudFormation.
--
-- 'semanticVersion', 'getCloudFormationTemplateResponse_semanticVersion' - The semantic version of the application:
--
-- <https://semver.org/>
--
-- 'templateId', 'getCloudFormationTemplateResponse_templateId' - The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern:
-- [0-9a-fA-F]{8}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{12}
--
-- 'httpStatus', 'getCloudFormationTemplateResponse_httpStatus' - The response's http status code.
newGetCloudFormationTemplateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetCloudFormationTemplateResponse
newGetCloudFormationTemplateResponse pHttpStatus_ =
  GetCloudFormationTemplateResponse'
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
getCloudFormationTemplateResponse_applicationId :: Lens.Lens' GetCloudFormationTemplateResponse (Core.Maybe Core.Text)
getCloudFormationTemplateResponse_applicationId = Lens.lens (\GetCloudFormationTemplateResponse' {applicationId} -> applicationId) (\s@GetCloudFormationTemplateResponse' {} a -> s {applicationId = a} :: GetCloudFormationTemplateResponse)

-- | Status of the template creation workflow.
--
-- Possible values: PREPARING | ACTIVE | EXPIRED
getCloudFormationTemplateResponse_status :: Lens.Lens' GetCloudFormationTemplateResponse (Core.Maybe Status)
getCloudFormationTemplateResponse_status = Lens.lens (\GetCloudFormationTemplateResponse' {status} -> status) (\s@GetCloudFormationTemplateResponse' {} a -> s {status = a} :: GetCloudFormationTemplateResponse)

-- | The date and time this resource was created.
getCloudFormationTemplateResponse_creationTime :: Lens.Lens' GetCloudFormationTemplateResponse (Core.Maybe Core.Text)
getCloudFormationTemplateResponse_creationTime = Lens.lens (\GetCloudFormationTemplateResponse' {creationTime} -> creationTime) (\s@GetCloudFormationTemplateResponse' {} a -> s {creationTime = a} :: GetCloudFormationTemplateResponse)

-- | The date and time this template expires. Templates expire 1 hour after
-- creation.
getCloudFormationTemplateResponse_expirationTime :: Lens.Lens' GetCloudFormationTemplateResponse (Core.Maybe Core.Text)
getCloudFormationTemplateResponse_expirationTime = Lens.lens (\GetCloudFormationTemplateResponse' {expirationTime} -> expirationTime) (\s@GetCloudFormationTemplateResponse' {} a -> s {expirationTime = a} :: GetCloudFormationTemplateResponse)

-- | A link to the template that can be used to deploy the application using
-- AWS CloudFormation.
getCloudFormationTemplateResponse_templateUrl :: Lens.Lens' GetCloudFormationTemplateResponse (Core.Maybe Core.Text)
getCloudFormationTemplateResponse_templateUrl = Lens.lens (\GetCloudFormationTemplateResponse' {templateUrl} -> templateUrl) (\s@GetCloudFormationTemplateResponse' {} a -> s {templateUrl = a} :: GetCloudFormationTemplateResponse)

-- | The semantic version of the application:
--
-- <https://semver.org/>
getCloudFormationTemplateResponse_semanticVersion :: Lens.Lens' GetCloudFormationTemplateResponse (Core.Maybe Core.Text)
getCloudFormationTemplateResponse_semanticVersion = Lens.lens (\GetCloudFormationTemplateResponse' {semanticVersion} -> semanticVersion) (\s@GetCloudFormationTemplateResponse' {} a -> s {semanticVersion = a} :: GetCloudFormationTemplateResponse)

-- | The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern:
-- [0-9a-fA-F]{8}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{12}
getCloudFormationTemplateResponse_templateId :: Lens.Lens' GetCloudFormationTemplateResponse (Core.Maybe Core.Text)
getCloudFormationTemplateResponse_templateId = Lens.lens (\GetCloudFormationTemplateResponse' {templateId} -> templateId) (\s@GetCloudFormationTemplateResponse' {} a -> s {templateId = a} :: GetCloudFormationTemplateResponse)

-- | The response's http status code.
getCloudFormationTemplateResponse_httpStatus :: Lens.Lens' GetCloudFormationTemplateResponse Core.Int
getCloudFormationTemplateResponse_httpStatus = Lens.lens (\GetCloudFormationTemplateResponse' {httpStatus} -> httpStatus) (\s@GetCloudFormationTemplateResponse' {} a -> s {httpStatus = a} :: GetCloudFormationTemplateResponse)

instance
  Core.NFData
    GetCloudFormationTemplateResponse
