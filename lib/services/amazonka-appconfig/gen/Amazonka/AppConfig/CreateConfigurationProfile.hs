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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information that enables AppConfig to access the configuration source.
-- Valid configuration sources include Systems Manager (SSM) documents, SSM
-- Parameter Store parameters, and Amazon S3 objects. A configuration
-- profile includes the following information.
--
-- -   The Uri location of the configuration data.
--
-- -   The AWS Identity and Access Management (IAM) role that provides
--     access to the configuration data.
--
-- -   A validator for the configuration data. Available validators include
--     either a JSON Schema or an AWS Lambda function.
--
-- For more information, see
-- <http://docs.aws.amazon.com/systems-manager/latest/userguide/appconfig-creating-configuration-and-profile.html Create a Configuration and a Configuration Profile>
-- in the /AWS AppConfig User Guide/.
module Amazonka.AppConfig.CreateConfigurationProfile
  ( -- * Creating a Request
    CreateConfigurationProfile (..),
    newCreateConfigurationProfile,

    -- * Request Lenses
    createConfigurationProfile_retrievalRoleArn,
    createConfigurationProfile_validators,
    createConfigurationProfile_description,
    createConfigurationProfile_tags,
    createConfigurationProfile_applicationId,
    createConfigurationProfile_name,
    createConfigurationProfile_locationUri,

    -- * Destructuring the Response
    ConfigurationProfile (..),
    newConfigurationProfile,

    -- * Response Lenses
    configurationProfile_retrievalRoleArn,
    configurationProfile_validators,
    configurationProfile_locationUri,
    configurationProfile_applicationId,
    configurationProfile_name,
    configurationProfile_id,
    configurationProfile_description,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateConfigurationProfile' smart constructor.
data CreateConfigurationProfile = CreateConfigurationProfile'
  { -- | The ARN of an IAM role with permission to access the configuration at
    -- the specified LocationUri.
    retrievalRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A list of methods for validating the configuration.
    validators :: Prelude.Maybe [Validator],
    -- | A description of the configuration profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | Metadata to assign to the configuration profile. Tags help organize and
    -- categorize your AppConfig resources. Each tag consists of a key and an
    -- optional value, both of which you define.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The application ID.
    applicationId :: Prelude.Text,
    -- | A name for the configuration profile.
    name :: Prelude.Text,
    -- | A URI to locate the configuration. You can specify a Systems Manager
    -- (SSM) document, an SSM Parameter Store parameter, or an Amazon S3
    -- object. For an SSM document, specify either the document name in the
    -- format @ssm-document:\/\/\<Document_name>@ or the Amazon Resource Name
    -- (ARN). For a parameter, specify either the parameter name in the format
    -- @ssm-parameter:\/\/\<Parameter_name>@ or the ARN. For an Amazon S3
    -- object, specify the URI in the following format:
    -- @s3:\/\/\<bucket>\/\<objectKey> @. Here is an example:
    -- s3:\/\/my-bucket\/my-app\/us-east-1\/my-config.json
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
-- 'retrievalRoleArn', 'createConfigurationProfile_retrievalRoleArn' - The ARN of an IAM role with permission to access the configuration at
-- the specified LocationUri.
--
-- 'validators', 'createConfigurationProfile_validators' - A list of methods for validating the configuration.
--
-- 'description', 'createConfigurationProfile_description' - A description of the configuration profile.
--
-- 'tags', 'createConfigurationProfile_tags' - Metadata to assign to the configuration profile. Tags help organize and
-- categorize your AppConfig resources. Each tag consists of a key and an
-- optional value, both of which you define.
--
-- 'applicationId', 'createConfigurationProfile_applicationId' - The application ID.
--
-- 'name', 'createConfigurationProfile_name' - A name for the configuration profile.
--
-- 'locationUri', 'createConfigurationProfile_locationUri' - A URI to locate the configuration. You can specify a Systems Manager
-- (SSM) document, an SSM Parameter Store parameter, or an Amazon S3
-- object. For an SSM document, specify either the document name in the
-- format @ssm-document:\/\/\<Document_name>@ or the Amazon Resource Name
-- (ARN). For a parameter, specify either the parameter name in the format
-- @ssm-parameter:\/\/\<Parameter_name>@ or the ARN. For an Amazon S3
-- object, specify the URI in the following format:
-- @s3:\/\/\<bucket>\/\<objectKey> @. Here is an example:
-- s3:\/\/my-bucket\/my-app\/us-east-1\/my-config.json
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
      { retrievalRoleArn =
          Prelude.Nothing,
        validators = Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        applicationId = pApplicationId_,
        name = pName_,
        locationUri = pLocationUri_
      }

-- | The ARN of an IAM role with permission to access the configuration at
-- the specified LocationUri.
createConfigurationProfile_retrievalRoleArn :: Lens.Lens' CreateConfigurationProfile (Prelude.Maybe Prelude.Text)
createConfigurationProfile_retrievalRoleArn = Lens.lens (\CreateConfigurationProfile' {retrievalRoleArn} -> retrievalRoleArn) (\s@CreateConfigurationProfile' {} a -> s {retrievalRoleArn = a} :: CreateConfigurationProfile)

-- | A list of methods for validating the configuration.
createConfigurationProfile_validators :: Lens.Lens' CreateConfigurationProfile (Prelude.Maybe [Validator])
createConfigurationProfile_validators = Lens.lens (\CreateConfigurationProfile' {validators} -> validators) (\s@CreateConfigurationProfile' {} a -> s {validators = a} :: CreateConfigurationProfile) Prelude.. Lens.mapping Lens.coerced

-- | A description of the configuration profile.
createConfigurationProfile_description :: Lens.Lens' CreateConfigurationProfile (Prelude.Maybe Prelude.Text)
createConfigurationProfile_description = Lens.lens (\CreateConfigurationProfile' {description} -> description) (\s@CreateConfigurationProfile' {} a -> s {description = a} :: CreateConfigurationProfile)

-- | Metadata to assign to the configuration profile. Tags help organize and
-- categorize your AppConfig resources. Each tag consists of a key and an
-- optional value, both of which you define.
createConfigurationProfile_tags :: Lens.Lens' CreateConfigurationProfile (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createConfigurationProfile_tags = Lens.lens (\CreateConfigurationProfile' {tags} -> tags) (\s@CreateConfigurationProfile' {} a -> s {tags = a} :: CreateConfigurationProfile) Prelude.. Lens.mapping Lens.coerced

-- | The application ID.
createConfigurationProfile_applicationId :: Lens.Lens' CreateConfigurationProfile Prelude.Text
createConfigurationProfile_applicationId = Lens.lens (\CreateConfigurationProfile' {applicationId} -> applicationId) (\s@CreateConfigurationProfile' {} a -> s {applicationId = a} :: CreateConfigurationProfile)

-- | A name for the configuration profile.
createConfigurationProfile_name :: Lens.Lens' CreateConfigurationProfile Prelude.Text
createConfigurationProfile_name = Lens.lens (\CreateConfigurationProfile' {name} -> name) (\s@CreateConfigurationProfile' {} a -> s {name = a} :: CreateConfigurationProfile)

-- | A URI to locate the configuration. You can specify a Systems Manager
-- (SSM) document, an SSM Parameter Store parameter, or an Amazon S3
-- object. For an SSM document, specify either the document name in the
-- format @ssm-document:\/\/\<Document_name>@ or the Amazon Resource Name
-- (ARN). For a parameter, specify either the parameter name in the format
-- @ssm-parameter:\/\/\<Parameter_name>@ or the ARN. For an Amazon S3
-- object, specify the URI in the following format:
-- @s3:\/\/\<bucket>\/\<objectKey> @. Here is an example:
-- s3:\/\/my-bucket\/my-app\/us-east-1\/my-config.json
createConfigurationProfile_locationUri :: Lens.Lens' CreateConfigurationProfile Prelude.Text
createConfigurationProfile_locationUri = Lens.lens (\CreateConfigurationProfile' {locationUri} -> locationUri) (\s@CreateConfigurationProfile' {} a -> s {locationUri = a} :: CreateConfigurationProfile)

instance Core.AWSRequest CreateConfigurationProfile where
  type
    AWSResponse CreateConfigurationProfile =
      ConfigurationProfile
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable CreateConfigurationProfile where
  hashWithSalt _salt CreateConfigurationProfile' {..} =
    _salt `Prelude.hashWithSalt` retrievalRoleArn
      `Prelude.hashWithSalt` validators
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` locationUri

instance Prelude.NFData CreateConfigurationProfile where
  rnf CreateConfigurationProfile' {..} =
    Prelude.rnf retrievalRoleArn
      `Prelude.seq` Prelude.rnf validators
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
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
          [ ("RetrievalRoleArn" Core..=)
              Prelude.<$> retrievalRoleArn,
            ("Validators" Core..=) Prelude.<$> validators,
            ("Description" Core..=) Prelude.<$> description,
            ("Tags" Core..=) Prelude.<$> tags,
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
