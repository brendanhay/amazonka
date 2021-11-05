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
-- Module      : Amazonka.ImageBuilder.CreateInfrastructureConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new infrastructure configuration. An infrastructure
-- configuration defines the environment in which your image will be built
-- and tested.
module Amazonka.ImageBuilder.CreateInfrastructureConfiguration
  ( -- * Creating a Request
    CreateInfrastructureConfiguration (..),
    newCreateInfrastructureConfiguration,

    -- * Request Lenses
    createInfrastructureConfiguration_securityGroupIds,
    createInfrastructureConfiguration_snsTopicArn,
    createInfrastructureConfiguration_instanceTypes,
    createInfrastructureConfiguration_keyPair,
    createInfrastructureConfiguration_resourceTags,
    createInfrastructureConfiguration_subnetId,
    createInfrastructureConfiguration_instanceMetadataOptions,
    createInfrastructureConfiguration_logging,
    createInfrastructureConfiguration_description,
    createInfrastructureConfiguration_tags,
    createInfrastructureConfiguration_terminateInstanceOnFailure,
    createInfrastructureConfiguration_name,
    createInfrastructureConfiguration_instanceProfileName,
    createInfrastructureConfiguration_clientToken,

    -- * Destructuring the Response
    CreateInfrastructureConfigurationResponse (..),
    newCreateInfrastructureConfigurationResponse,

    -- * Response Lenses
    createInfrastructureConfigurationResponse_requestId,
    createInfrastructureConfigurationResponse_infrastructureConfigurationArn,
    createInfrastructureConfigurationResponse_clientToken,
    createInfrastructureConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateInfrastructureConfiguration' smart constructor.
data CreateInfrastructureConfiguration = CreateInfrastructureConfiguration'
  { -- | The security group IDs to associate with the instance used to customize
    -- your Amazon EC2 AMI.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The SNS topic on which to send image build events.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | The instance types of the infrastructure configuration. You can specify
    -- one or more instance types to use for this build. The service will pick
    -- one of these instance types based on availability.
    instanceTypes :: Prelude.Maybe [Prelude.Text],
    -- | The key pair of the infrastructure configuration. You can use this to
    -- log on to and debug the instance used to create your image.
    keyPair :: Prelude.Maybe Prelude.Text,
    -- | The tags attached to the resource created by Image Builder.
    resourceTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The subnet ID in which to place the instance used to customize your
    -- Amazon EC2 AMI.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The instance metadata options that you can set for the HTTP requests
    -- that pipeline builds use to launch EC2 build and test instances.
    instanceMetadataOptions :: Prelude.Maybe InstanceMetadataOptions,
    -- | The logging configuration of the infrastructure configuration.
    logging :: Prelude.Maybe Logging,
    -- | The description of the infrastructure configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags of the infrastructure configuration.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The terminate instance on failure setting of the infrastructure
    -- configuration. Set to false if you want Image Builder to retain the
    -- instance used to configure your AMI if the build or test phase of your
    -- workflow fails.
    terminateInstanceOnFailure :: Prelude.Maybe Prelude.Bool,
    -- | The name of the infrastructure configuration.
    name :: Prelude.Text,
    -- | The instance profile to associate with the instance used to customize
    -- your Amazon EC2 AMI.
    instanceProfileName :: Prelude.Text,
    -- | The idempotency token used to make this request idempotent.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInfrastructureConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityGroupIds', 'createInfrastructureConfiguration_securityGroupIds' - The security group IDs to associate with the instance used to customize
-- your Amazon EC2 AMI.
--
-- 'snsTopicArn', 'createInfrastructureConfiguration_snsTopicArn' - The SNS topic on which to send image build events.
--
-- 'instanceTypes', 'createInfrastructureConfiguration_instanceTypes' - The instance types of the infrastructure configuration. You can specify
-- one or more instance types to use for this build. The service will pick
-- one of these instance types based on availability.
--
-- 'keyPair', 'createInfrastructureConfiguration_keyPair' - The key pair of the infrastructure configuration. You can use this to
-- log on to and debug the instance used to create your image.
--
-- 'resourceTags', 'createInfrastructureConfiguration_resourceTags' - The tags attached to the resource created by Image Builder.
--
-- 'subnetId', 'createInfrastructureConfiguration_subnetId' - The subnet ID in which to place the instance used to customize your
-- Amazon EC2 AMI.
--
-- 'instanceMetadataOptions', 'createInfrastructureConfiguration_instanceMetadataOptions' - The instance metadata options that you can set for the HTTP requests
-- that pipeline builds use to launch EC2 build and test instances.
--
-- 'logging', 'createInfrastructureConfiguration_logging' - The logging configuration of the infrastructure configuration.
--
-- 'description', 'createInfrastructureConfiguration_description' - The description of the infrastructure configuration.
--
-- 'tags', 'createInfrastructureConfiguration_tags' - The tags of the infrastructure configuration.
--
-- 'terminateInstanceOnFailure', 'createInfrastructureConfiguration_terminateInstanceOnFailure' - The terminate instance on failure setting of the infrastructure
-- configuration. Set to false if you want Image Builder to retain the
-- instance used to configure your AMI if the build or test phase of your
-- workflow fails.
--
-- 'name', 'createInfrastructureConfiguration_name' - The name of the infrastructure configuration.
--
-- 'instanceProfileName', 'createInfrastructureConfiguration_instanceProfileName' - The instance profile to associate with the instance used to customize
-- your Amazon EC2 AMI.
--
-- 'clientToken', 'createInfrastructureConfiguration_clientToken' - The idempotency token used to make this request idempotent.
newCreateInfrastructureConfiguration ::
  -- | 'name'
  Prelude.Text ->
  -- | 'instanceProfileName'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  CreateInfrastructureConfiguration
newCreateInfrastructureConfiguration
  pName_
  pInstanceProfileName_
  pClientToken_ =
    CreateInfrastructureConfiguration'
      { securityGroupIds =
          Prelude.Nothing,
        snsTopicArn = Prelude.Nothing,
        instanceTypes = Prelude.Nothing,
        keyPair = Prelude.Nothing,
        resourceTags = Prelude.Nothing,
        subnetId = Prelude.Nothing,
        instanceMetadataOptions =
          Prelude.Nothing,
        logging = Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        terminateInstanceOnFailure =
          Prelude.Nothing,
        name = pName_,
        instanceProfileName =
          pInstanceProfileName_,
        clientToken = pClientToken_
      }

-- | The security group IDs to associate with the instance used to customize
-- your Amazon EC2 AMI.
createInfrastructureConfiguration_securityGroupIds :: Lens.Lens' CreateInfrastructureConfiguration (Prelude.Maybe [Prelude.Text])
createInfrastructureConfiguration_securityGroupIds = Lens.lens (\CreateInfrastructureConfiguration' {securityGroupIds} -> securityGroupIds) (\s@CreateInfrastructureConfiguration' {} a -> s {securityGroupIds = a} :: CreateInfrastructureConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The SNS topic on which to send image build events.
createInfrastructureConfiguration_snsTopicArn :: Lens.Lens' CreateInfrastructureConfiguration (Prelude.Maybe Prelude.Text)
createInfrastructureConfiguration_snsTopicArn = Lens.lens (\CreateInfrastructureConfiguration' {snsTopicArn} -> snsTopicArn) (\s@CreateInfrastructureConfiguration' {} a -> s {snsTopicArn = a} :: CreateInfrastructureConfiguration)

-- | The instance types of the infrastructure configuration. You can specify
-- one or more instance types to use for this build. The service will pick
-- one of these instance types based on availability.
createInfrastructureConfiguration_instanceTypes :: Lens.Lens' CreateInfrastructureConfiguration (Prelude.Maybe [Prelude.Text])
createInfrastructureConfiguration_instanceTypes = Lens.lens (\CreateInfrastructureConfiguration' {instanceTypes} -> instanceTypes) (\s@CreateInfrastructureConfiguration' {} a -> s {instanceTypes = a} :: CreateInfrastructureConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The key pair of the infrastructure configuration. You can use this to
-- log on to and debug the instance used to create your image.
createInfrastructureConfiguration_keyPair :: Lens.Lens' CreateInfrastructureConfiguration (Prelude.Maybe Prelude.Text)
createInfrastructureConfiguration_keyPair = Lens.lens (\CreateInfrastructureConfiguration' {keyPair} -> keyPair) (\s@CreateInfrastructureConfiguration' {} a -> s {keyPair = a} :: CreateInfrastructureConfiguration)

-- | The tags attached to the resource created by Image Builder.
createInfrastructureConfiguration_resourceTags :: Lens.Lens' CreateInfrastructureConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createInfrastructureConfiguration_resourceTags = Lens.lens (\CreateInfrastructureConfiguration' {resourceTags} -> resourceTags) (\s@CreateInfrastructureConfiguration' {} a -> s {resourceTags = a} :: CreateInfrastructureConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The subnet ID in which to place the instance used to customize your
-- Amazon EC2 AMI.
createInfrastructureConfiguration_subnetId :: Lens.Lens' CreateInfrastructureConfiguration (Prelude.Maybe Prelude.Text)
createInfrastructureConfiguration_subnetId = Lens.lens (\CreateInfrastructureConfiguration' {subnetId} -> subnetId) (\s@CreateInfrastructureConfiguration' {} a -> s {subnetId = a} :: CreateInfrastructureConfiguration)

-- | The instance metadata options that you can set for the HTTP requests
-- that pipeline builds use to launch EC2 build and test instances.
createInfrastructureConfiguration_instanceMetadataOptions :: Lens.Lens' CreateInfrastructureConfiguration (Prelude.Maybe InstanceMetadataOptions)
createInfrastructureConfiguration_instanceMetadataOptions = Lens.lens (\CreateInfrastructureConfiguration' {instanceMetadataOptions} -> instanceMetadataOptions) (\s@CreateInfrastructureConfiguration' {} a -> s {instanceMetadataOptions = a} :: CreateInfrastructureConfiguration)

-- | The logging configuration of the infrastructure configuration.
createInfrastructureConfiguration_logging :: Lens.Lens' CreateInfrastructureConfiguration (Prelude.Maybe Logging)
createInfrastructureConfiguration_logging = Lens.lens (\CreateInfrastructureConfiguration' {logging} -> logging) (\s@CreateInfrastructureConfiguration' {} a -> s {logging = a} :: CreateInfrastructureConfiguration)

-- | The description of the infrastructure configuration.
createInfrastructureConfiguration_description :: Lens.Lens' CreateInfrastructureConfiguration (Prelude.Maybe Prelude.Text)
createInfrastructureConfiguration_description = Lens.lens (\CreateInfrastructureConfiguration' {description} -> description) (\s@CreateInfrastructureConfiguration' {} a -> s {description = a} :: CreateInfrastructureConfiguration)

-- | The tags of the infrastructure configuration.
createInfrastructureConfiguration_tags :: Lens.Lens' CreateInfrastructureConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createInfrastructureConfiguration_tags = Lens.lens (\CreateInfrastructureConfiguration' {tags} -> tags) (\s@CreateInfrastructureConfiguration' {} a -> s {tags = a} :: CreateInfrastructureConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The terminate instance on failure setting of the infrastructure
-- configuration. Set to false if you want Image Builder to retain the
-- instance used to configure your AMI if the build or test phase of your
-- workflow fails.
createInfrastructureConfiguration_terminateInstanceOnFailure :: Lens.Lens' CreateInfrastructureConfiguration (Prelude.Maybe Prelude.Bool)
createInfrastructureConfiguration_terminateInstanceOnFailure = Lens.lens (\CreateInfrastructureConfiguration' {terminateInstanceOnFailure} -> terminateInstanceOnFailure) (\s@CreateInfrastructureConfiguration' {} a -> s {terminateInstanceOnFailure = a} :: CreateInfrastructureConfiguration)

-- | The name of the infrastructure configuration.
createInfrastructureConfiguration_name :: Lens.Lens' CreateInfrastructureConfiguration Prelude.Text
createInfrastructureConfiguration_name = Lens.lens (\CreateInfrastructureConfiguration' {name} -> name) (\s@CreateInfrastructureConfiguration' {} a -> s {name = a} :: CreateInfrastructureConfiguration)

-- | The instance profile to associate with the instance used to customize
-- your Amazon EC2 AMI.
createInfrastructureConfiguration_instanceProfileName :: Lens.Lens' CreateInfrastructureConfiguration Prelude.Text
createInfrastructureConfiguration_instanceProfileName = Lens.lens (\CreateInfrastructureConfiguration' {instanceProfileName} -> instanceProfileName) (\s@CreateInfrastructureConfiguration' {} a -> s {instanceProfileName = a} :: CreateInfrastructureConfiguration)

-- | The idempotency token used to make this request idempotent.
createInfrastructureConfiguration_clientToken :: Lens.Lens' CreateInfrastructureConfiguration Prelude.Text
createInfrastructureConfiguration_clientToken = Lens.lens (\CreateInfrastructureConfiguration' {clientToken} -> clientToken) (\s@CreateInfrastructureConfiguration' {} a -> s {clientToken = a} :: CreateInfrastructureConfiguration)

instance
  Core.AWSRequest
    CreateInfrastructureConfiguration
  where
  type
    AWSResponse CreateInfrastructureConfiguration =
      CreateInfrastructureConfigurationResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateInfrastructureConfigurationResponse'
            Prelude.<$> (x Core..?> "requestId")
              Prelude.<*> (x Core..?> "infrastructureConfigurationArn")
              Prelude.<*> (x Core..?> "clientToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateInfrastructureConfiguration

instance
  Prelude.NFData
    CreateInfrastructureConfiguration

instance
  Core.ToHeaders
    CreateInfrastructureConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    CreateInfrastructureConfiguration
  where
  toJSON CreateInfrastructureConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("securityGroupIds" Core..=)
              Prelude.<$> securityGroupIds,
            ("snsTopicArn" Core..=) Prelude.<$> snsTopicArn,
            ("instanceTypes" Core..=) Prelude.<$> instanceTypes,
            ("keyPair" Core..=) Prelude.<$> keyPair,
            ("resourceTags" Core..=) Prelude.<$> resourceTags,
            ("subnetId" Core..=) Prelude.<$> subnetId,
            ("instanceMetadataOptions" Core..=)
              Prelude.<$> instanceMetadataOptions,
            ("logging" Core..=) Prelude.<$> logging,
            ("description" Core..=) Prelude.<$> description,
            ("tags" Core..=) Prelude.<$> tags,
            ("terminateInstanceOnFailure" Core..=)
              Prelude.<$> terminateInstanceOnFailure,
            Prelude.Just ("name" Core..= name),
            Prelude.Just
              ("instanceProfileName" Core..= instanceProfileName),
            Prelude.Just ("clientToken" Core..= clientToken)
          ]
      )

instance
  Core.ToPath
    CreateInfrastructureConfiguration
  where
  toPath =
    Prelude.const "/CreateInfrastructureConfiguration"

instance
  Core.ToQuery
    CreateInfrastructureConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateInfrastructureConfigurationResponse' smart constructor.
data CreateInfrastructureConfigurationResponse = CreateInfrastructureConfigurationResponse'
  { -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the infrastructure configuration that
    -- was created by this request.
    infrastructureConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The idempotency token used to make this request idempotent.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInfrastructureConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'createInfrastructureConfigurationResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'infrastructureConfigurationArn', 'createInfrastructureConfigurationResponse_infrastructureConfigurationArn' - The Amazon Resource Name (ARN) of the infrastructure configuration that
-- was created by this request.
--
-- 'clientToken', 'createInfrastructureConfigurationResponse_clientToken' - The idempotency token used to make this request idempotent.
--
-- 'httpStatus', 'createInfrastructureConfigurationResponse_httpStatus' - The response's http status code.
newCreateInfrastructureConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateInfrastructureConfigurationResponse
newCreateInfrastructureConfigurationResponse
  pHttpStatus_ =
    CreateInfrastructureConfigurationResponse'
      { requestId =
          Prelude.Nothing,
        infrastructureConfigurationArn =
          Prelude.Nothing,
        clientToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The request ID that uniquely identifies this request.
createInfrastructureConfigurationResponse_requestId :: Lens.Lens' CreateInfrastructureConfigurationResponse (Prelude.Maybe Prelude.Text)
createInfrastructureConfigurationResponse_requestId = Lens.lens (\CreateInfrastructureConfigurationResponse' {requestId} -> requestId) (\s@CreateInfrastructureConfigurationResponse' {} a -> s {requestId = a} :: CreateInfrastructureConfigurationResponse)

-- | The Amazon Resource Name (ARN) of the infrastructure configuration that
-- was created by this request.
createInfrastructureConfigurationResponse_infrastructureConfigurationArn :: Lens.Lens' CreateInfrastructureConfigurationResponse (Prelude.Maybe Prelude.Text)
createInfrastructureConfigurationResponse_infrastructureConfigurationArn = Lens.lens (\CreateInfrastructureConfigurationResponse' {infrastructureConfigurationArn} -> infrastructureConfigurationArn) (\s@CreateInfrastructureConfigurationResponse' {} a -> s {infrastructureConfigurationArn = a} :: CreateInfrastructureConfigurationResponse)

-- | The idempotency token used to make this request idempotent.
createInfrastructureConfigurationResponse_clientToken :: Lens.Lens' CreateInfrastructureConfigurationResponse (Prelude.Maybe Prelude.Text)
createInfrastructureConfigurationResponse_clientToken = Lens.lens (\CreateInfrastructureConfigurationResponse' {clientToken} -> clientToken) (\s@CreateInfrastructureConfigurationResponse' {} a -> s {clientToken = a} :: CreateInfrastructureConfigurationResponse)

-- | The response's http status code.
createInfrastructureConfigurationResponse_httpStatus :: Lens.Lens' CreateInfrastructureConfigurationResponse Prelude.Int
createInfrastructureConfigurationResponse_httpStatus = Lens.lens (\CreateInfrastructureConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateInfrastructureConfigurationResponse' {} a -> s {httpStatus = a} :: CreateInfrastructureConfigurationResponse)

instance
  Prelude.NFData
    CreateInfrastructureConfigurationResponse
