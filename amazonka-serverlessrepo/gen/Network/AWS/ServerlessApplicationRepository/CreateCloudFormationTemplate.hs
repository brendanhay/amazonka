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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'newCreateCloudFormationTemplate' smart constructor.
data CreateCloudFormationTemplate = CreateCloudFormationTemplate'
  { -- | The semantic version of the application:
    --
    -- <https://semver.org/>
    semanticVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  CreateCloudFormationTemplate
newCreateCloudFormationTemplate pApplicationId_ =
  CreateCloudFormationTemplate'
    { semanticVersion =
        Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | The semantic version of the application:
--
-- <https://semver.org/>
createCloudFormationTemplate_semanticVersion :: Lens.Lens' CreateCloudFormationTemplate (Prelude.Maybe Prelude.Text)
createCloudFormationTemplate_semanticVersion = Lens.lens (\CreateCloudFormationTemplate' {semanticVersion} -> semanticVersion) (\s@CreateCloudFormationTemplate' {} a -> s {semanticVersion = a} :: CreateCloudFormationTemplate)

-- | The Amazon Resource Name (ARN) of the application.
createCloudFormationTemplate_applicationId :: Lens.Lens' CreateCloudFormationTemplate Prelude.Text
createCloudFormationTemplate_applicationId = Lens.lens (\CreateCloudFormationTemplate' {applicationId} -> applicationId) (\s@CreateCloudFormationTemplate' {} a -> s {applicationId = a} :: CreateCloudFormationTemplate)

instance
  Prelude.AWSRequest
    CreateCloudFormationTemplate
  where
  type
    Rs CreateCloudFormationTemplate =
      CreateCloudFormationTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCloudFormationTemplateResponse'
            Prelude.<$> (x Prelude..?> "applicationId")
            Prelude.<*> (x Prelude..?> "status")
            Prelude.<*> (x Prelude..?> "creationTime")
            Prelude.<*> (x Prelude..?> "expirationTime")
            Prelude.<*> (x Prelude..?> "templateUrl")
            Prelude.<*> (x Prelude..?> "semanticVersion")
            Prelude.<*> (x Prelude..?> "templateId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateCloudFormationTemplate

instance Prelude.NFData CreateCloudFormationTemplate

instance
  Prelude.ToHeaders
    CreateCloudFormationTemplate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreateCloudFormationTemplate where
  toJSON CreateCloudFormationTemplate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("semanticVersion" Prelude..=)
              Prelude.<$> semanticVersion
          ]
      )

instance Prelude.ToPath CreateCloudFormationTemplate where
  toPath CreateCloudFormationTemplate' {..} =
    Prelude.mconcat
      [ "/applications/",
        Prelude.toBS applicationId,
        "/templates"
      ]

instance Prelude.ToQuery CreateCloudFormationTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCloudFormationTemplateResponse' smart constructor.
data CreateCloudFormationTemplateResponse = CreateCloudFormationTemplateResponse'
  { -- | The application Amazon Resource Name (ARN).
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | Status of the template creation workflow.
    --
    -- Possible values: PREPARING | ACTIVE | EXPIRED
    status :: Prelude.Maybe Status,
    -- | The date and time this resource was created.
    creationTime :: Prelude.Maybe Prelude.Text,
    -- | The date and time this template expires. Templates expire 1 hour after
    -- creation.
    expirationTime :: Prelude.Maybe Prelude.Text,
    -- | A link to the template that can be used to deploy the application using
    -- AWS CloudFormation.
    templateUrl :: Prelude.Maybe Prelude.Text,
    -- | The semantic version of the application:
    --
    -- <https://semver.org/>
    semanticVersion :: Prelude.Maybe Prelude.Text,
    -- | The UUID returned by CreateCloudFormationTemplate.
    --
    -- Pattern:
    -- [0-9a-fA-F]{8}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{12}
    templateId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreateCloudFormationTemplateResponse
newCreateCloudFormationTemplateResponse pHttpStatus_ =
  CreateCloudFormationTemplateResponse'
    { applicationId =
        Prelude.Nothing,
      status = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      expirationTime = Prelude.Nothing,
      templateUrl = Prelude.Nothing,
      semanticVersion = Prelude.Nothing,
      templateId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The application Amazon Resource Name (ARN).
createCloudFormationTemplateResponse_applicationId :: Lens.Lens' CreateCloudFormationTemplateResponse (Prelude.Maybe Prelude.Text)
createCloudFormationTemplateResponse_applicationId = Lens.lens (\CreateCloudFormationTemplateResponse' {applicationId} -> applicationId) (\s@CreateCloudFormationTemplateResponse' {} a -> s {applicationId = a} :: CreateCloudFormationTemplateResponse)

-- | Status of the template creation workflow.
--
-- Possible values: PREPARING | ACTIVE | EXPIRED
createCloudFormationTemplateResponse_status :: Lens.Lens' CreateCloudFormationTemplateResponse (Prelude.Maybe Status)
createCloudFormationTemplateResponse_status = Lens.lens (\CreateCloudFormationTemplateResponse' {status} -> status) (\s@CreateCloudFormationTemplateResponse' {} a -> s {status = a} :: CreateCloudFormationTemplateResponse)

-- | The date and time this resource was created.
createCloudFormationTemplateResponse_creationTime :: Lens.Lens' CreateCloudFormationTemplateResponse (Prelude.Maybe Prelude.Text)
createCloudFormationTemplateResponse_creationTime = Lens.lens (\CreateCloudFormationTemplateResponse' {creationTime} -> creationTime) (\s@CreateCloudFormationTemplateResponse' {} a -> s {creationTime = a} :: CreateCloudFormationTemplateResponse)

-- | The date and time this template expires. Templates expire 1 hour after
-- creation.
createCloudFormationTemplateResponse_expirationTime :: Lens.Lens' CreateCloudFormationTemplateResponse (Prelude.Maybe Prelude.Text)
createCloudFormationTemplateResponse_expirationTime = Lens.lens (\CreateCloudFormationTemplateResponse' {expirationTime} -> expirationTime) (\s@CreateCloudFormationTemplateResponse' {} a -> s {expirationTime = a} :: CreateCloudFormationTemplateResponse)

-- | A link to the template that can be used to deploy the application using
-- AWS CloudFormation.
createCloudFormationTemplateResponse_templateUrl :: Lens.Lens' CreateCloudFormationTemplateResponse (Prelude.Maybe Prelude.Text)
createCloudFormationTemplateResponse_templateUrl = Lens.lens (\CreateCloudFormationTemplateResponse' {templateUrl} -> templateUrl) (\s@CreateCloudFormationTemplateResponse' {} a -> s {templateUrl = a} :: CreateCloudFormationTemplateResponse)

-- | The semantic version of the application:
--
-- <https://semver.org/>
createCloudFormationTemplateResponse_semanticVersion :: Lens.Lens' CreateCloudFormationTemplateResponse (Prelude.Maybe Prelude.Text)
createCloudFormationTemplateResponse_semanticVersion = Lens.lens (\CreateCloudFormationTemplateResponse' {semanticVersion} -> semanticVersion) (\s@CreateCloudFormationTemplateResponse' {} a -> s {semanticVersion = a} :: CreateCloudFormationTemplateResponse)

-- | The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern:
-- [0-9a-fA-F]{8}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{12}
createCloudFormationTemplateResponse_templateId :: Lens.Lens' CreateCloudFormationTemplateResponse (Prelude.Maybe Prelude.Text)
createCloudFormationTemplateResponse_templateId = Lens.lens (\CreateCloudFormationTemplateResponse' {templateId} -> templateId) (\s@CreateCloudFormationTemplateResponse' {} a -> s {templateId = a} :: CreateCloudFormationTemplateResponse)

-- | The response's http status code.
createCloudFormationTemplateResponse_httpStatus :: Lens.Lens' CreateCloudFormationTemplateResponse Prelude.Int
createCloudFormationTemplateResponse_httpStatus = Lens.lens (\CreateCloudFormationTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateCloudFormationTemplateResponse' {} a -> s {httpStatus = a} :: CreateCloudFormationTemplateResponse)

instance
  Prelude.NFData
    CreateCloudFormationTemplateResponse
