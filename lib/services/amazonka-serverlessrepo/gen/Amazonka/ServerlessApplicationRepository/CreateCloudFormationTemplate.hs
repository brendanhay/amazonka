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
-- Module      : Amazonka.ServerlessApplicationRepository.CreateCloudFormationTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS CloudFormation template.
module Amazonka.ServerlessApplicationRepository.CreateCloudFormationTemplate
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
    createCloudFormationTemplateResponse_creationTime,
    createCloudFormationTemplateResponse_status,
    createCloudFormationTemplateResponse_templateId,
    createCloudFormationTemplateResponse_semanticVersion,
    createCloudFormationTemplateResponse_applicationId,
    createCloudFormationTemplateResponse_templateUrl,
    createCloudFormationTemplateResponse_expirationTime,
    createCloudFormationTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServerlessApplicationRepository.Types

-- | /See:/ 'newCreateCloudFormationTemplate' smart constructor.
data CreateCloudFormationTemplate = CreateCloudFormationTemplate'
  { -- | The semantic version of the application:
    --
    -- <https://semver.org/>
    semanticVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest CreateCloudFormationTemplate where
  type
    AWSResponse CreateCloudFormationTemplate =
      CreateCloudFormationTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCloudFormationTemplateResponse'
            Prelude.<$> (x Core..?> "creationTime")
            Prelude.<*> (x Core..?> "status")
            Prelude.<*> (x Core..?> "templateId")
            Prelude.<*> (x Core..?> "semanticVersion")
            Prelude.<*> (x Core..?> "applicationId")
            Prelude.<*> (x Core..?> "templateUrl")
            Prelude.<*> (x Core..?> "expirationTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateCloudFormationTemplate
  where
  hashWithSalt salt' CreateCloudFormationTemplate' {..} =
    salt' `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` semanticVersion

instance Prelude.NFData CreateCloudFormationTemplate where
  rnf CreateCloudFormationTemplate' {..} =
    Prelude.rnf semanticVersion
      `Prelude.seq` Prelude.rnf applicationId

instance Core.ToHeaders CreateCloudFormationTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateCloudFormationTemplate where
  toJSON CreateCloudFormationTemplate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("semanticVersion" Core..=)
              Prelude.<$> semanticVersion
          ]
      )

instance Core.ToPath CreateCloudFormationTemplate where
  toPath CreateCloudFormationTemplate' {..} =
    Prelude.mconcat
      [ "/applications/",
        Core.toBS applicationId,
        "/templates"
      ]

instance Core.ToQuery CreateCloudFormationTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCloudFormationTemplateResponse' smart constructor.
data CreateCloudFormationTemplateResponse = CreateCloudFormationTemplateResponse'
  { -- | The date and time this resource was created.
    creationTime :: Prelude.Maybe Prelude.Text,
    -- | Status of the template creation workflow.
    --
    -- Possible values: PREPARING | ACTIVE | EXPIRED
    status :: Prelude.Maybe Status,
    -- | The UUID returned by CreateCloudFormationTemplate.
    --
    -- Pattern:
    -- [0-9a-fA-F]{8}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{12}
    templateId :: Prelude.Maybe Prelude.Text,
    -- | The semantic version of the application:
    --
    -- <https://semver.org/>
    semanticVersion :: Prelude.Maybe Prelude.Text,
    -- | The application Amazon Resource Name (ARN).
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | A link to the template that can be used to deploy the application using
    -- AWS CloudFormation.
    templateUrl :: Prelude.Maybe Prelude.Text,
    -- | The date and time this template expires. Templates expire 1 hour after
    -- creation.
    expirationTime :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCloudFormationTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'createCloudFormationTemplateResponse_creationTime' - The date and time this resource was created.
--
-- 'status', 'createCloudFormationTemplateResponse_status' - Status of the template creation workflow.
--
-- Possible values: PREPARING | ACTIVE | EXPIRED
--
-- 'templateId', 'createCloudFormationTemplateResponse_templateId' - The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern:
-- [0-9a-fA-F]{8}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{12}
--
-- 'semanticVersion', 'createCloudFormationTemplateResponse_semanticVersion' - The semantic version of the application:
--
-- <https://semver.org/>
--
-- 'applicationId', 'createCloudFormationTemplateResponse_applicationId' - The application Amazon Resource Name (ARN).
--
-- 'templateUrl', 'createCloudFormationTemplateResponse_templateUrl' - A link to the template that can be used to deploy the application using
-- AWS CloudFormation.
--
-- 'expirationTime', 'createCloudFormationTemplateResponse_expirationTime' - The date and time this template expires. Templates expire 1 hour after
-- creation.
--
-- 'httpStatus', 'createCloudFormationTemplateResponse_httpStatus' - The response's http status code.
newCreateCloudFormationTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCloudFormationTemplateResponse
newCreateCloudFormationTemplateResponse pHttpStatus_ =
  CreateCloudFormationTemplateResponse'
    { creationTime =
        Prelude.Nothing,
      status = Prelude.Nothing,
      templateId = Prelude.Nothing,
      semanticVersion = Prelude.Nothing,
      applicationId = Prelude.Nothing,
      templateUrl = Prelude.Nothing,
      expirationTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time this resource was created.
createCloudFormationTemplateResponse_creationTime :: Lens.Lens' CreateCloudFormationTemplateResponse (Prelude.Maybe Prelude.Text)
createCloudFormationTemplateResponse_creationTime = Lens.lens (\CreateCloudFormationTemplateResponse' {creationTime} -> creationTime) (\s@CreateCloudFormationTemplateResponse' {} a -> s {creationTime = a} :: CreateCloudFormationTemplateResponse)

-- | Status of the template creation workflow.
--
-- Possible values: PREPARING | ACTIVE | EXPIRED
createCloudFormationTemplateResponse_status :: Lens.Lens' CreateCloudFormationTemplateResponse (Prelude.Maybe Status)
createCloudFormationTemplateResponse_status = Lens.lens (\CreateCloudFormationTemplateResponse' {status} -> status) (\s@CreateCloudFormationTemplateResponse' {} a -> s {status = a} :: CreateCloudFormationTemplateResponse)

-- | The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern:
-- [0-9a-fA-F]{8}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{12}
createCloudFormationTemplateResponse_templateId :: Lens.Lens' CreateCloudFormationTemplateResponse (Prelude.Maybe Prelude.Text)
createCloudFormationTemplateResponse_templateId = Lens.lens (\CreateCloudFormationTemplateResponse' {templateId} -> templateId) (\s@CreateCloudFormationTemplateResponse' {} a -> s {templateId = a} :: CreateCloudFormationTemplateResponse)

-- | The semantic version of the application:
--
-- <https://semver.org/>
createCloudFormationTemplateResponse_semanticVersion :: Lens.Lens' CreateCloudFormationTemplateResponse (Prelude.Maybe Prelude.Text)
createCloudFormationTemplateResponse_semanticVersion = Lens.lens (\CreateCloudFormationTemplateResponse' {semanticVersion} -> semanticVersion) (\s@CreateCloudFormationTemplateResponse' {} a -> s {semanticVersion = a} :: CreateCloudFormationTemplateResponse)

-- | The application Amazon Resource Name (ARN).
createCloudFormationTemplateResponse_applicationId :: Lens.Lens' CreateCloudFormationTemplateResponse (Prelude.Maybe Prelude.Text)
createCloudFormationTemplateResponse_applicationId = Lens.lens (\CreateCloudFormationTemplateResponse' {applicationId} -> applicationId) (\s@CreateCloudFormationTemplateResponse' {} a -> s {applicationId = a} :: CreateCloudFormationTemplateResponse)

-- | A link to the template that can be used to deploy the application using
-- AWS CloudFormation.
createCloudFormationTemplateResponse_templateUrl :: Lens.Lens' CreateCloudFormationTemplateResponse (Prelude.Maybe Prelude.Text)
createCloudFormationTemplateResponse_templateUrl = Lens.lens (\CreateCloudFormationTemplateResponse' {templateUrl} -> templateUrl) (\s@CreateCloudFormationTemplateResponse' {} a -> s {templateUrl = a} :: CreateCloudFormationTemplateResponse)

-- | The date and time this template expires. Templates expire 1 hour after
-- creation.
createCloudFormationTemplateResponse_expirationTime :: Lens.Lens' CreateCloudFormationTemplateResponse (Prelude.Maybe Prelude.Text)
createCloudFormationTemplateResponse_expirationTime = Lens.lens (\CreateCloudFormationTemplateResponse' {expirationTime} -> expirationTime) (\s@CreateCloudFormationTemplateResponse' {} a -> s {expirationTime = a} :: CreateCloudFormationTemplateResponse)

-- | The response's http status code.
createCloudFormationTemplateResponse_httpStatus :: Lens.Lens' CreateCloudFormationTemplateResponse Prelude.Int
createCloudFormationTemplateResponse_httpStatus = Lens.lens (\CreateCloudFormationTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateCloudFormationTemplateResponse' {} a -> s {httpStatus = a} :: CreateCloudFormationTemplateResponse)

instance
  Prelude.NFData
    CreateCloudFormationTemplateResponse
  where
  rnf CreateCloudFormationTemplateResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf expirationTime
      `Prelude.seq` Prelude.rnf templateUrl
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf semanticVersion
      `Prelude.seq` Prelude.rnf templateId
      `Prelude.seq` Prelude.rnf status
