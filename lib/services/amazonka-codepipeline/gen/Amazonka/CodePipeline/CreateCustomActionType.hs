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
-- Module      : Amazonka.CodePipeline.CreateCustomActionType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom action that can be used in all pipelines associated
-- with the AWS account. Only used for custom actions.
module Amazonka.CodePipeline.CreateCustomActionType
  ( -- * Creating a Request
    CreateCustomActionType (..),
    newCreateCustomActionType,

    -- * Request Lenses
    createCustomActionType_configurationProperties,
    createCustomActionType_settings,
    createCustomActionType_tags,
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

import Amazonka.CodePipeline.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    configurationProperties :: Prelude.Maybe [ActionConfigurationProperty],
    -- | URLs that provide users information about this custom action.
    settings :: Prelude.Maybe ActionTypeSettings,
    -- | The tags for the custom action.
    tags :: Prelude.Maybe [Tag],
    -- | The category of the custom action, such as a build action or a test
    -- action.
    category :: ActionCategory,
    -- | The provider of the service used in the custom action, such as AWS
    -- CodeDeploy.
    provider :: Prelude.Text,
    -- | The version identifier of the custom action.
    version :: Prelude.Text,
    -- | The details of the input artifact for the action, such as its commit ID.
    inputArtifactDetails :: ArtifactDetails,
    -- | The details of the output artifact of the action, such as its commit ID.
    outputArtifactDetails :: ArtifactDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'settings', 'createCustomActionType_settings' - URLs that provide users information about this custom action.
--
-- 'tags', 'createCustomActionType_tags' - The tags for the custom action.
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
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
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
          Prelude.Nothing,
        settings = Prelude.Nothing,
        tags = Prelude.Nothing,
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
createCustomActionType_configurationProperties :: Lens.Lens' CreateCustomActionType (Prelude.Maybe [ActionConfigurationProperty])
createCustomActionType_configurationProperties = Lens.lens (\CreateCustomActionType' {configurationProperties} -> configurationProperties) (\s@CreateCustomActionType' {} a -> s {configurationProperties = a} :: CreateCustomActionType) Prelude.. Lens.mapping Lens.coerced

-- | URLs that provide users information about this custom action.
createCustomActionType_settings :: Lens.Lens' CreateCustomActionType (Prelude.Maybe ActionTypeSettings)
createCustomActionType_settings = Lens.lens (\CreateCustomActionType' {settings} -> settings) (\s@CreateCustomActionType' {} a -> s {settings = a} :: CreateCustomActionType)

-- | The tags for the custom action.
createCustomActionType_tags :: Lens.Lens' CreateCustomActionType (Prelude.Maybe [Tag])
createCustomActionType_tags = Lens.lens (\CreateCustomActionType' {tags} -> tags) (\s@CreateCustomActionType' {} a -> s {tags = a} :: CreateCustomActionType) Prelude.. Lens.mapping Lens.coerced

-- | The category of the custom action, such as a build action or a test
-- action.
createCustomActionType_category :: Lens.Lens' CreateCustomActionType ActionCategory
createCustomActionType_category = Lens.lens (\CreateCustomActionType' {category} -> category) (\s@CreateCustomActionType' {} a -> s {category = a} :: CreateCustomActionType)

-- | The provider of the service used in the custom action, such as AWS
-- CodeDeploy.
createCustomActionType_provider :: Lens.Lens' CreateCustomActionType Prelude.Text
createCustomActionType_provider = Lens.lens (\CreateCustomActionType' {provider} -> provider) (\s@CreateCustomActionType' {} a -> s {provider = a} :: CreateCustomActionType)

-- | The version identifier of the custom action.
createCustomActionType_version :: Lens.Lens' CreateCustomActionType Prelude.Text
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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCustomActionTypeResponse'
            Prelude.<$> (x Data..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "actionType")
      )

instance Prelude.Hashable CreateCustomActionType where
  hashWithSalt _salt CreateCustomActionType' {..} =
    _salt
      `Prelude.hashWithSalt` configurationProperties
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` provider
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` inputArtifactDetails
      `Prelude.hashWithSalt` outputArtifactDetails

instance Prelude.NFData CreateCustomActionType where
  rnf CreateCustomActionType' {..} =
    Prelude.rnf configurationProperties
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf provider
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf inputArtifactDetails
      `Prelude.seq` Prelude.rnf outputArtifactDetails

instance Data.ToHeaders CreateCustomActionType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodePipeline_20150709.CreateCustomActionType" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCustomActionType where
  toJSON CreateCustomActionType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("configurationProperties" Data..=)
              Prelude.<$> configurationProperties,
            ("settings" Data..=) Prelude.<$> settings,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("category" Data..= category),
            Prelude.Just ("provider" Data..= provider),
            Prelude.Just ("version" Data..= version),
            Prelude.Just
              ( "inputArtifactDetails"
                  Data..= inputArtifactDetails
              ),
            Prelude.Just
              ( "outputArtifactDetails"
                  Data..= outputArtifactDetails
              )
          ]
      )

instance Data.ToPath CreateCustomActionType where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCustomActionType where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @CreateCustomActionType@ operation.
--
-- /See:/ 'newCreateCustomActionTypeResponse' smart constructor.
data CreateCustomActionTypeResponse = CreateCustomActionTypeResponse'
  { -- | Specifies the tags applied to the custom action.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Returns information about the details of an action type.
    actionType :: ActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'actionType'
  ActionType ->
  CreateCustomActionTypeResponse
newCreateCustomActionTypeResponse
  pHttpStatus_
  pActionType_ =
    CreateCustomActionTypeResponse'
      { tags =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        actionType = pActionType_
      }

-- | Specifies the tags applied to the custom action.
createCustomActionTypeResponse_tags :: Lens.Lens' CreateCustomActionTypeResponse (Prelude.Maybe [Tag])
createCustomActionTypeResponse_tags = Lens.lens (\CreateCustomActionTypeResponse' {tags} -> tags) (\s@CreateCustomActionTypeResponse' {} a -> s {tags = a} :: CreateCustomActionTypeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createCustomActionTypeResponse_httpStatus :: Lens.Lens' CreateCustomActionTypeResponse Prelude.Int
createCustomActionTypeResponse_httpStatus = Lens.lens (\CreateCustomActionTypeResponse' {httpStatus} -> httpStatus) (\s@CreateCustomActionTypeResponse' {} a -> s {httpStatus = a} :: CreateCustomActionTypeResponse)

-- | Returns information about the details of an action type.
createCustomActionTypeResponse_actionType :: Lens.Lens' CreateCustomActionTypeResponse ActionType
createCustomActionTypeResponse_actionType = Lens.lens (\CreateCustomActionTypeResponse' {actionType} -> actionType) (\s@CreateCustomActionTypeResponse' {} a -> s {actionType = a} :: CreateCustomActionTypeResponse)

instance
  Prelude.NFData
    CreateCustomActionTypeResponse
  where
  rnf CreateCustomActionTypeResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf actionType
