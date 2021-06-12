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
-- Module      : Network.AWS.CodePipeline.CreateCustomActionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom action that can be used in all pipelines associated
-- with the AWS account. Only used for custom actions.
module Network.AWS.CodePipeline.CreateCustomActionType
  ( -- * Creating a Request
    CreateCustomActionType (..),
    newCreateCustomActionType,

    -- * Request Lenses
    createCustomActionType_configurationProperties,
    createCustomActionType_tags,
    createCustomActionType_settings,
    createCustomActionType_category,
    createCustomActionType_provider,
    createCustomActionType_version,
    createCustomActionType_inputArtifactDetails,
    createCustomActionType_outputArtifactDetails,

    -- * Destructuring the Response
    CreateCustomActionTypeResponse (..),
    newCreateCustomActionTypeResponse,

    -- * Response Lenses
    createCustomActionTypeResponse_tags,
    createCustomActionTypeResponse_httpStatus,
    createCustomActionTypeResponse_actionType,
  )
where

import Network.AWS.CodePipeline.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a CreateCustomActionType operation.
--
-- /See:/ 'newCreateCustomActionType' smart constructor.
data CreateCustomActionType = CreateCustomActionType'
  { -- | The configuration properties for the custom action.
    --
    -- You can refer to a name in the configuration properties of the custom
    -- action within the URL templates by following the format of
    -- {Config:name}, as long as the configuration property is both required
    -- and not secret. For more information, see
    -- <https://docs.aws.amazon.com/codepipeline/latest/userguide/how-to-create-custom-action.html Create a Custom Action for a Pipeline>.
    configurationProperties :: Core.Maybe [ActionConfigurationProperty],
    -- | The tags for the custom action.
    tags :: Core.Maybe [Tag],
    -- | URLs that provide users information about this custom action.
    settings :: Core.Maybe ActionTypeSettings,
    -- | The category of the custom action, such as a build action or a test
    -- action.
    category :: ActionCategory,
    -- | The provider of the service used in the custom action, such as AWS
    -- CodeDeploy.
    provider :: Core.Text,
    -- | The version identifier of the custom action.
    version :: Core.Text,
    -- | The details of the input artifact for the action, such as its commit ID.
    inputArtifactDetails :: ArtifactDetails,
    -- | The details of the output artifact of the action, such as its commit ID.
    outputArtifactDetails :: ArtifactDetails
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCustomActionType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationProperties', 'createCustomActionType_configurationProperties' - The configuration properties for the custom action.
--
-- You can refer to a name in the configuration properties of the custom
-- action within the URL templates by following the format of
-- {Config:name}, as long as the configuration property is both required
-- and not secret. For more information, see
-- <https://docs.aws.amazon.com/codepipeline/latest/userguide/how-to-create-custom-action.html Create a Custom Action for a Pipeline>.
--
-- 'tags', 'createCustomActionType_tags' - The tags for the custom action.
--
-- 'settings', 'createCustomActionType_settings' - URLs that provide users information about this custom action.
--
-- 'category', 'createCustomActionType_category' - The category of the custom action, such as a build action or a test
-- action.
--
-- 'provider', 'createCustomActionType_provider' - The provider of the service used in the custom action, such as AWS
-- CodeDeploy.
--
-- 'version', 'createCustomActionType_version' - The version identifier of the custom action.
--
-- 'inputArtifactDetails', 'createCustomActionType_inputArtifactDetails' - The details of the input artifact for the action, such as its commit ID.
--
-- 'outputArtifactDetails', 'createCustomActionType_outputArtifactDetails' - The details of the output artifact of the action, such as its commit ID.
newCreateCustomActionType ::
  -- | 'category'
  ActionCategory ->
  -- | 'provider'
  Core.Text ->
  -- | 'version'
  Core.Text ->
  -- | 'inputArtifactDetails'
  ArtifactDetails ->
  -- | 'outputArtifactDetails'
  ArtifactDetails ->
  CreateCustomActionType
newCreateCustomActionType
  pCategory_
  pProvider_
  pVersion_
  pInputArtifactDetails_
  pOutputArtifactDetails_ =
    CreateCustomActionType'
      { configurationProperties =
          Core.Nothing,
        tags = Core.Nothing,
        settings = Core.Nothing,
        category = pCategory_,
        provider = pProvider_,
        version = pVersion_,
        inputArtifactDetails = pInputArtifactDetails_,
        outputArtifactDetails = pOutputArtifactDetails_
      }

-- | The configuration properties for the custom action.
--
-- You can refer to a name in the configuration properties of the custom
-- action within the URL templates by following the format of
-- {Config:name}, as long as the configuration property is both required
-- and not secret. For more information, see
-- <https://docs.aws.amazon.com/codepipeline/latest/userguide/how-to-create-custom-action.html Create a Custom Action for a Pipeline>.
createCustomActionType_configurationProperties :: Lens.Lens' CreateCustomActionType (Core.Maybe [ActionConfigurationProperty])
createCustomActionType_configurationProperties = Lens.lens (\CreateCustomActionType' {configurationProperties} -> configurationProperties) (\s@CreateCustomActionType' {} a -> s {configurationProperties = a} :: CreateCustomActionType) Core.. Lens.mapping Lens._Coerce

-- | The tags for the custom action.
createCustomActionType_tags :: Lens.Lens' CreateCustomActionType (Core.Maybe [Tag])
createCustomActionType_tags = Lens.lens (\CreateCustomActionType' {tags} -> tags) (\s@CreateCustomActionType' {} a -> s {tags = a} :: CreateCustomActionType) Core.. Lens.mapping Lens._Coerce

-- | URLs that provide users information about this custom action.
createCustomActionType_settings :: Lens.Lens' CreateCustomActionType (Core.Maybe ActionTypeSettings)
createCustomActionType_settings = Lens.lens (\CreateCustomActionType' {settings} -> settings) (\s@CreateCustomActionType' {} a -> s {settings = a} :: CreateCustomActionType)

-- | The category of the custom action, such as a build action or a test
-- action.
createCustomActionType_category :: Lens.Lens' CreateCustomActionType ActionCategory
createCustomActionType_category = Lens.lens (\CreateCustomActionType' {category} -> category) (\s@CreateCustomActionType' {} a -> s {category = a} :: CreateCustomActionType)

-- | The provider of the service used in the custom action, such as AWS
-- CodeDeploy.
createCustomActionType_provider :: Lens.Lens' CreateCustomActionType Core.Text
createCustomActionType_provider = Lens.lens (\CreateCustomActionType' {provider} -> provider) (\s@CreateCustomActionType' {} a -> s {provider = a} :: CreateCustomActionType)

-- | The version identifier of the custom action.
createCustomActionType_version :: Lens.Lens' CreateCustomActionType Core.Text
createCustomActionType_version = Lens.lens (\CreateCustomActionType' {version} -> version) (\s@CreateCustomActionType' {} a -> s {version = a} :: CreateCustomActionType)

-- | The details of the input artifact for the action, such as its commit ID.
createCustomActionType_inputArtifactDetails :: Lens.Lens' CreateCustomActionType ArtifactDetails
createCustomActionType_inputArtifactDetails = Lens.lens (\CreateCustomActionType' {inputArtifactDetails} -> inputArtifactDetails) (\s@CreateCustomActionType' {} a -> s {inputArtifactDetails = a} :: CreateCustomActionType)

-- | The details of the output artifact of the action, such as its commit ID.
createCustomActionType_outputArtifactDetails :: Lens.Lens' CreateCustomActionType ArtifactDetails
createCustomActionType_outputArtifactDetails = Lens.lens (\CreateCustomActionType' {outputArtifactDetails} -> outputArtifactDetails) (\s@CreateCustomActionType' {} a -> s {outputArtifactDetails = a} :: CreateCustomActionType)

instance Core.AWSRequest CreateCustomActionType where
  type
    AWSResponse CreateCustomActionType =
      CreateCustomActionTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCustomActionTypeResponse'
            Core.<$> (x Core..?> "tags" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "actionType")
      )

instance Core.Hashable CreateCustomActionType

instance Core.NFData CreateCustomActionType

instance Core.ToHeaders CreateCustomActionType where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodePipeline_20150709.CreateCustomActionType" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateCustomActionType where
  toJSON CreateCustomActionType' {..} =
    Core.object
      ( Core.catMaybes
          [ ("configurationProperties" Core..=)
              Core.<$> configurationProperties,
            ("tags" Core..=) Core.<$> tags,
            ("settings" Core..=) Core.<$> settings,
            Core.Just ("category" Core..= category),
            Core.Just ("provider" Core..= provider),
            Core.Just ("version" Core..= version),
            Core.Just
              ( "inputArtifactDetails"
                  Core..= inputArtifactDetails
              ),
            Core.Just
              ( "outputArtifactDetails"
                  Core..= outputArtifactDetails
              )
          ]
      )

instance Core.ToPath CreateCustomActionType where
  toPath = Core.const "/"

instance Core.ToQuery CreateCustomActionType where
  toQuery = Core.const Core.mempty

-- | Represents the output of a @CreateCustomActionType@ operation.
--
-- /See:/ 'newCreateCustomActionTypeResponse' smart constructor.
data CreateCustomActionTypeResponse = CreateCustomActionTypeResponse'
  { -- | Specifies the tags applied to the custom action.
    tags :: Core.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Returns information about the details of an action type.
    actionType :: ActionType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCustomActionTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createCustomActionTypeResponse_tags' - Specifies the tags applied to the custom action.
--
-- 'httpStatus', 'createCustomActionTypeResponse_httpStatus' - The response's http status code.
--
-- 'actionType', 'createCustomActionTypeResponse_actionType' - Returns information about the details of an action type.
newCreateCustomActionTypeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'actionType'
  ActionType ->
  CreateCustomActionTypeResponse
newCreateCustomActionTypeResponse
  pHttpStatus_
  pActionType_ =
    CreateCustomActionTypeResponse'
      { tags =
          Core.Nothing,
        httpStatus = pHttpStatus_,
        actionType = pActionType_
      }

-- | Specifies the tags applied to the custom action.
createCustomActionTypeResponse_tags :: Lens.Lens' CreateCustomActionTypeResponse (Core.Maybe [Tag])
createCustomActionTypeResponse_tags = Lens.lens (\CreateCustomActionTypeResponse' {tags} -> tags) (\s@CreateCustomActionTypeResponse' {} a -> s {tags = a} :: CreateCustomActionTypeResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createCustomActionTypeResponse_httpStatus :: Lens.Lens' CreateCustomActionTypeResponse Core.Int
createCustomActionTypeResponse_httpStatus = Lens.lens (\CreateCustomActionTypeResponse' {httpStatus} -> httpStatus) (\s@CreateCustomActionTypeResponse' {} a -> s {httpStatus = a} :: CreateCustomActionTypeResponse)

-- | Returns information about the details of an action type.
createCustomActionTypeResponse_actionType :: Lens.Lens' CreateCustomActionTypeResponse ActionType
createCustomActionTypeResponse_actionType = Lens.lens (\CreateCustomActionTypeResponse' {actionType} -> actionType) (\s@CreateCustomActionTypeResponse' {} a -> s {actionType = a} :: CreateCustomActionTypeResponse)

instance Core.NFData CreateCustomActionTypeResponse
