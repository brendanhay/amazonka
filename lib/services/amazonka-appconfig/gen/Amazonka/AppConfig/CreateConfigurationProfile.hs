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
-- Module      : Amazonka.AppConfig.CreateConfigurationProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a configuration profile, which is information that enables
-- AppConfig to access the configuration source. Valid configuration
-- sources include the AppConfig hosted configuration store, Amazon Web
-- Services Systems Manager (SSM) documents, SSM Parameter Store
-- parameters, Amazon S3 objects, or any
-- <http://docs.aws.amazon.com/codepipeline/latest/userguide/integrations-action-type.html#integrations-source integration source action>
-- supported by CodePipeline. A configuration profile includes the
-- following information:
--
-- -   The URI location of the configuration data.
--
-- -   The Identity and Access Management (IAM) role that provides access
--     to the configuration data.
--
-- -   A validator for the configuration data. Available validators include
--     either a JSON Schema or an Amazon Web Services Lambda function.
--
-- For more information, see
-- <http://docs.aws.amazon.com/appconfig/latest/userguide/appconfig-creating-configuration-and-profile.html Create a Configuration and a Configuration Profile>
-- in the /AppConfig User Guide/.
module Amazonka.AppConfig.CreateConfigurationProfile
  ( -- * Creating a Request
    CreateConfigurationProfile (..),
    newCreateConfigurationProfile,

    -- * Request Lenses
    createConfigurationProfile_tags,
    createConfigurationProfile_type,
    createConfigurationProfile_retrievalRoleArn,
    createConfigurationProfile_description,
    createConfigurationProfile_validators,
    createConfigurationProfile_applicationId,
    createConfigurationProfile_name,
    createConfigurationProfile_locationUri,

    -- * Destructuring the Response
    ConfigurationProfile (..),
    newConfigurationProfile,

    -- * Response Lenses
    configurationProfile_name,
    configurationProfile_type,
    configurationProfile_retrievalRoleArn,
    configurationProfile_id,
    configurationProfile_description,
    configurationProfile_locationUri,
    configurationProfile_applicationId,
    configurationProfile_validators,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateConfigurationProfile' smart constructor.
data CreateConfigurationProfile = CreateConfigurationProfile'
  { -- | Metadata to assign to the configuration profile. Tags help organize and
    -- categorize your AppConfig resources. Each tag consists of a key and an
    -- optional value, both of which you define.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The type of configurations contained in the profile. AppConfig supports
    -- @feature flags@ and @freeform@ configurations. We recommend you create
    -- feature flag configurations to enable or disable new features and
    -- freeform configurations to distribute configurations to an application.
    -- When calling this API, enter one of the following values for @Type@:
    --
    -- @AWS.AppConfig.FeatureFlags@
    --
    -- @AWS.Freeform@
    type' :: Prelude.Maybe Prelude.Text,
    -- | The ARN of an IAM role with permission to access the configuration at
    -- the specified @LocationUri@.
    --
    -- A retrieval role ARN is not required for configurations stored in the
    -- AppConfig hosted configuration store. It is required for all other
    -- sources that store your configuration.
    retrievalRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A description of the configuration profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of methods for validating the configuration.
    validators :: Prelude.Maybe [Validator],
    -- | The application ID.
    applicationId :: Prelude.Text,
    -- | A name for the configuration profile.
    name :: Prelude.Text,
    -- | A URI to locate the configuration. You can specify the AppConfig hosted
    -- configuration store, Systems Manager (SSM) document, an SSM Parameter
    -- Store parameter, or an Amazon S3 object. For the hosted configuration
    -- store and for feature flags, specify @hosted@. For an SSM document,
    -- specify either the document name in the format
    -- @ssm-document:\/\/\<Document_name>@ or the Amazon Resource Name (ARN).
    -- For a parameter, specify either the parameter name in the format
    -- @ssm-parameter:\/\/\<Parameter_name>@ or the ARN. For an Amazon S3
    -- object, specify the URI in the following format:
    -- @s3:\/\/\<bucket>\/\<objectKey> @. Here is an example:
    -- @s3:\/\/my-bucket\/my-app\/us-east-1\/my-config.json@
    locationUri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConfigurationProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createConfigurationProfile_tags' - Metadata to assign to the configuration profile. Tags help organize and
-- categorize your AppConfig resources. Each tag consists of a key and an
-- optional value, both of which you define.
--
-- 'type'', 'createConfigurationProfile_type' - The type of configurations contained in the profile. AppConfig supports
-- @feature flags@ and @freeform@ configurations. We recommend you create
-- feature flag configurations to enable or disable new features and
-- freeform configurations to distribute configurations to an application.
-- When calling this API, enter one of the following values for @Type@:
--
-- @AWS.AppConfig.FeatureFlags@
--
-- @AWS.Freeform@
--
-- 'retrievalRoleArn', 'createConfigurationProfile_retrievalRoleArn' - The ARN of an IAM role with permission to access the configuration at
-- the specified @LocationUri@.
--
-- A retrieval role ARN is not required for configurations stored in the
-- AppConfig hosted configuration store. It is required for all other
-- sources that store your configuration.
--
-- 'description', 'createConfigurationProfile_description' - A description of the configuration profile.
--
-- 'validators', 'createConfigurationProfile_validators' - A list of methods for validating the configuration.
--
-- 'applicationId', 'createConfigurationProfile_applicationId' - The application ID.
--
-- 'name', 'createConfigurationProfile_name' - A name for the configuration profile.
--
-- 'locationUri', 'createConfigurationProfile_locationUri' - A URI to locate the configuration. You can specify the AppConfig hosted
-- configuration store, Systems Manager (SSM) document, an SSM Parameter
-- Store parameter, or an Amazon S3 object. For the hosted configuration
-- store and for feature flags, specify @hosted@. For an SSM document,
-- specify either the document name in the format
-- @ssm-document:\/\/\<Document_name>@ or the Amazon Resource Name (ARN).
-- For a parameter, specify either the parameter name in the format
-- @ssm-parameter:\/\/\<Parameter_name>@ or the ARN. For an Amazon S3
-- object, specify the URI in the following format:
-- @s3:\/\/\<bucket>\/\<objectKey> @. Here is an example:
-- @s3:\/\/my-bucket\/my-app\/us-east-1\/my-config.json@
newCreateConfigurationProfile ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'locationUri'
  Prelude.Text ->
  CreateConfigurationProfile
newCreateConfigurationProfile
  pApplicationId_
  pName_
  pLocationUri_ =
    CreateConfigurationProfile'
      { tags = Prelude.Nothing,
        type' = Prelude.Nothing,
        retrievalRoleArn = Prelude.Nothing,
        description = Prelude.Nothing,
        validators = Prelude.Nothing,
        applicationId = pApplicationId_,
        name = pName_,
        locationUri = pLocationUri_
      }

-- | Metadata to assign to the configuration profile. Tags help organize and
-- categorize your AppConfig resources. Each tag consists of a key and an
-- optional value, both of which you define.
createConfigurationProfile_tags :: Lens.Lens' CreateConfigurationProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createConfigurationProfile_tags = Lens.lens (\CreateConfigurationProfile' {tags} -> tags) (\s@CreateConfigurationProfile' {} a -> s {tags = a} :: CreateConfigurationProfile) Prelude.. Lens.mapping Lens.coerced

-- | The type of configurations contained in the profile. AppConfig supports
-- @feature flags@ and @freeform@ configurations. We recommend you create
-- feature flag configurations to enable or disable new features and
-- freeform configurations to distribute configurations to an application.
-- When calling this API, enter one of the following values for @Type@:
--
-- @AWS.AppConfig.FeatureFlags@
--
-- @AWS.Freeform@
createConfigurationProfile_type :: Lens.Lens' CreateConfigurationProfile (Prelude.Maybe Prelude.Text)
createConfigurationProfile_type = Lens.lens (\CreateConfigurationProfile' {type'} -> type') (\s@CreateConfigurationProfile' {} a -> s {type' = a} :: CreateConfigurationProfile)

-- | The ARN of an IAM role with permission to access the configuration at
-- the specified @LocationUri@.
--
-- A retrieval role ARN is not required for configurations stored in the
-- AppConfig hosted configuration store. It is required for all other
-- sources that store your configuration.
createConfigurationProfile_retrievalRoleArn :: Lens.Lens' CreateConfigurationProfile (Prelude.Maybe Prelude.Text)
createConfigurationProfile_retrievalRoleArn = Lens.lens (\CreateConfigurationProfile' {retrievalRoleArn} -> retrievalRoleArn) (\s@CreateConfigurationProfile' {} a -> s {retrievalRoleArn = a} :: CreateConfigurationProfile)

-- | A description of the configuration profile.
createConfigurationProfile_description :: Lens.Lens' CreateConfigurationProfile (Prelude.Maybe Prelude.Text)
createConfigurationProfile_description = Lens.lens (\CreateConfigurationProfile' {description} -> description) (\s@CreateConfigurationProfile' {} a -> s {description = a} :: CreateConfigurationProfile)

-- | A list of methods for validating the configuration.
createConfigurationProfile_validators :: Lens.Lens' CreateConfigurationProfile (Prelude.Maybe [Validator])
createConfigurationProfile_validators = Lens.lens (\CreateConfigurationProfile' {validators} -> validators) (\s@CreateConfigurationProfile' {} a -> s {validators = a} :: CreateConfigurationProfile) Prelude.. Lens.mapping Lens.coerced

-- | The application ID.
createConfigurationProfile_applicationId :: Lens.Lens' CreateConfigurationProfile Prelude.Text
createConfigurationProfile_applicationId = Lens.lens (\CreateConfigurationProfile' {applicationId} -> applicationId) (\s@CreateConfigurationProfile' {} a -> s {applicationId = a} :: CreateConfigurationProfile)

-- | A name for the configuration profile.
createConfigurationProfile_name :: Lens.Lens' CreateConfigurationProfile Prelude.Text
createConfigurationProfile_name = Lens.lens (\CreateConfigurationProfile' {name} -> name) (\s@CreateConfigurationProfile' {} a -> s {name = a} :: CreateConfigurationProfile)

-- | A URI to locate the configuration. You can specify the AppConfig hosted
-- configuration store, Systems Manager (SSM) document, an SSM Parameter
-- Store parameter, or an Amazon S3 object. For the hosted configuration
-- store and for feature flags, specify @hosted@. For an SSM document,
-- specify either the document name in the format
-- @ssm-document:\/\/\<Document_name>@ or the Amazon Resource Name (ARN).
-- For a parameter, specify either the parameter name in the format
-- @ssm-parameter:\/\/\<Parameter_name>@ or the ARN. For an Amazon S3
-- object, specify the URI in the following format:
-- @s3:\/\/\<bucket>\/\<objectKey> @. Here is an example:
-- @s3:\/\/my-bucket\/my-app\/us-east-1\/my-config.json@
createConfigurationProfile_locationUri :: Lens.Lens' CreateConfigurationProfile Prelude.Text
createConfigurationProfile_locationUri = Lens.lens (\CreateConfigurationProfile' {locationUri} -> locationUri) (\s@CreateConfigurationProfile' {} a -> s {locationUri = a} :: CreateConfigurationProfile)

instance Core.AWSRequest CreateConfigurationProfile where
  type
    AWSResponse CreateConfigurationProfile =
      ConfigurationProfile
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable CreateConfigurationProfile where
  hashWithSalt _salt CreateConfigurationProfile' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` retrievalRoleArn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` validators
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` locationUri

instance Prelude.NFData CreateConfigurationProfile where
  rnf CreateConfigurationProfile' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf retrievalRoleArn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf validators
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf locationUri

instance Core.ToHeaders CreateConfigurationProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateConfigurationProfile where
  toJSON CreateConfigurationProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("Type" Core..=) Prelude.<$> type',
            ("RetrievalRoleArn" Core..=)
              Prelude.<$> retrievalRoleArn,
            ("Description" Core..=) Prelude.<$> description,
            ("Validators" Core..=) Prelude.<$> validators,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("LocationUri" Core..= locationUri)
          ]
      )

instance Core.ToPath CreateConfigurationProfile where
  toPath CreateConfigurationProfile' {..} =
    Prelude.mconcat
      [ "/applications/",
        Core.toBS applicationId,
        "/configurationprofiles"
      ]

instance Core.ToQuery CreateConfigurationProfile where
  toQuery = Prelude.const Prelude.mempty
