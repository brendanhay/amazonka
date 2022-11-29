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
-- Module      : Amazonka.ImageBuilder.UpdateInfrastructureConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a new infrastructure configuration. An infrastructure
-- configuration defines the environment in which your image will be built
-- and tested.
module Amazonka.ImageBuilder.UpdateInfrastructureConfiguration
  ( -- * Creating a Request
    UpdateInfrastructureConfiguration (..),
    newUpdateInfrastructureConfiguration,

    -- * Request Lenses
    updateInfrastructureConfiguration_instanceTypes,
    updateInfrastructureConfiguration_securityGroupIds,
    updateInfrastructureConfiguration_subnetId,
    updateInfrastructureConfiguration_description,
    updateInfrastructureConfiguration_resourceTags,
    updateInfrastructureConfiguration_keyPair,
    updateInfrastructureConfiguration_logging,
    updateInfrastructureConfiguration_snsTopicArn,
    updateInfrastructureConfiguration_instanceMetadataOptions,
    updateInfrastructureConfiguration_terminateInstanceOnFailure,
    updateInfrastructureConfiguration_infrastructureConfigurationArn,
    updateInfrastructureConfiguration_instanceProfileName,
    updateInfrastructureConfiguration_clientToken,

    -- * Destructuring the Response
    UpdateInfrastructureConfigurationResponse (..),
    newUpdateInfrastructureConfigurationResponse,

    -- * Response Lenses
    updateInfrastructureConfigurationResponse_clientToken,
    updateInfrastructureConfigurationResponse_requestId,
    updateInfrastructureConfigurationResponse_infrastructureConfigurationArn,
    updateInfrastructureConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateInfrastructureConfiguration' smart constructor.
data UpdateInfrastructureConfiguration = UpdateInfrastructureConfiguration'
  { -- | The instance types of the infrastructure configuration. You can specify
    -- one or more instance types to use for this build. The service will pick
    -- one of these instance types based on availability.
    instanceTypes :: Prelude.Maybe [Prelude.Text],
    -- | The security group IDs to associate with the instance used to customize
    -- your Amazon EC2 AMI.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The subnet ID to place the instance used to customize your Amazon EC2
    -- AMI in.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The description of the infrastructure configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags attached to the resource created by Image Builder.
    resourceTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The key pair of the infrastructure configuration. You can use this to
    -- log on to and debug the instance used to create your image.
    keyPair :: Prelude.Maybe Prelude.Text,
    -- | The logging configuration of the infrastructure configuration.
    logging :: Prelude.Maybe Logging,
    -- | The Amazon Resource Name (ARN) for the SNS topic to which we send image
    -- build event notifications.
    --
    -- EC2 Image Builder is unable to send notifications to SNS topics that are
    -- encrypted using keys from other accounts. The key that is used to
    -- encrypt the SNS topic must reside in the account that the Image Builder
    -- service runs under.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | The instance metadata options that you can set for the HTTP requests
    -- that pipeline builds use to launch EC2 build and test instances. For
    -- more information about instance metadata options, see one of the
    -- following links:
    --
    -- -   <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/configuring-instance-metadata-options.html Configure the instance metadata options>
    --     in the //Amazon EC2 User Guide// for Linux instances.
    --
    -- -   <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/configuring-instance-metadata-options.html Configure the instance metadata options>
    --     in the //Amazon EC2 Windows Guide// for Windows instances.
    instanceMetadataOptions :: Prelude.Maybe InstanceMetadataOptions,
    -- | The terminate instance on failure setting of the infrastructure
    -- configuration. Set to false if you want Image Builder to retain the
    -- instance used to configure your AMI if the build or test phase of your
    -- workflow fails.
    terminateInstanceOnFailure :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the infrastructure configuration that
    -- you want to update.
    infrastructureConfigurationArn :: Prelude.Text,
    -- | The instance profile to associate with the instance used to customize
    -- your Amazon EC2 AMI.
    instanceProfileName :: Prelude.Text,
    -- | The idempotency token used to make this request idempotent.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInfrastructureConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceTypes', 'updateInfrastructureConfiguration_instanceTypes' - The instance types of the infrastructure configuration. You can specify
-- one or more instance types to use for this build. The service will pick
-- one of these instance types based on availability.
--
-- 'securityGroupIds', 'updateInfrastructureConfiguration_securityGroupIds' - The security group IDs to associate with the instance used to customize
-- your Amazon EC2 AMI.
--
-- 'subnetId', 'updateInfrastructureConfiguration_subnetId' - The subnet ID to place the instance used to customize your Amazon EC2
-- AMI in.
--
-- 'description', 'updateInfrastructureConfiguration_description' - The description of the infrastructure configuration.
--
-- 'resourceTags', 'updateInfrastructureConfiguration_resourceTags' - The tags attached to the resource created by Image Builder.
--
-- 'keyPair', 'updateInfrastructureConfiguration_keyPair' - The key pair of the infrastructure configuration. You can use this to
-- log on to and debug the instance used to create your image.
--
-- 'logging', 'updateInfrastructureConfiguration_logging' - The logging configuration of the infrastructure configuration.
--
-- 'snsTopicArn', 'updateInfrastructureConfiguration_snsTopicArn' - The Amazon Resource Name (ARN) for the SNS topic to which we send image
-- build event notifications.
--
-- EC2 Image Builder is unable to send notifications to SNS topics that are
-- encrypted using keys from other accounts. The key that is used to
-- encrypt the SNS topic must reside in the account that the Image Builder
-- service runs under.
--
-- 'instanceMetadataOptions', 'updateInfrastructureConfiguration_instanceMetadataOptions' - The instance metadata options that you can set for the HTTP requests
-- that pipeline builds use to launch EC2 build and test instances. For
-- more information about instance metadata options, see one of the
-- following links:
--
-- -   <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/configuring-instance-metadata-options.html Configure the instance metadata options>
--     in the //Amazon EC2 User Guide// for Linux instances.
--
-- -   <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/configuring-instance-metadata-options.html Configure the instance metadata options>
--     in the //Amazon EC2 Windows Guide// for Windows instances.
--
-- 'terminateInstanceOnFailure', 'updateInfrastructureConfiguration_terminateInstanceOnFailure' - The terminate instance on failure setting of the infrastructure
-- configuration. Set to false if you want Image Builder to retain the
-- instance used to configure your AMI if the build or test phase of your
-- workflow fails.
--
-- 'infrastructureConfigurationArn', 'updateInfrastructureConfiguration_infrastructureConfigurationArn' - The Amazon Resource Name (ARN) of the infrastructure configuration that
-- you want to update.
--
-- 'instanceProfileName', 'updateInfrastructureConfiguration_instanceProfileName' - The instance profile to associate with the instance used to customize
-- your Amazon EC2 AMI.
--
-- 'clientToken', 'updateInfrastructureConfiguration_clientToken' - The idempotency token used to make this request idempotent.
newUpdateInfrastructureConfiguration ::
  -- | 'infrastructureConfigurationArn'
  Prelude.Text ->
  -- | 'instanceProfileName'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  UpdateInfrastructureConfiguration
newUpdateInfrastructureConfiguration
  pInfrastructureConfigurationArn_
  pInstanceProfileName_
  pClientToken_ =
    UpdateInfrastructureConfiguration'
      { instanceTypes =
          Prelude.Nothing,
        securityGroupIds = Prelude.Nothing,
        subnetId = Prelude.Nothing,
        description = Prelude.Nothing,
        resourceTags = Prelude.Nothing,
        keyPair = Prelude.Nothing,
        logging = Prelude.Nothing,
        snsTopicArn = Prelude.Nothing,
        instanceMetadataOptions =
          Prelude.Nothing,
        terminateInstanceOnFailure =
          Prelude.Nothing,
        infrastructureConfigurationArn =
          pInfrastructureConfigurationArn_,
        instanceProfileName =
          pInstanceProfileName_,
        clientToken = pClientToken_
      }

-- | The instance types of the infrastructure configuration. You can specify
-- one or more instance types to use for this build. The service will pick
-- one of these instance types based on availability.
updateInfrastructureConfiguration_instanceTypes :: Lens.Lens' UpdateInfrastructureConfiguration (Prelude.Maybe [Prelude.Text])
updateInfrastructureConfiguration_instanceTypes = Lens.lens (\UpdateInfrastructureConfiguration' {instanceTypes} -> instanceTypes) (\s@UpdateInfrastructureConfiguration' {} a -> s {instanceTypes = a} :: UpdateInfrastructureConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The security group IDs to associate with the instance used to customize
-- your Amazon EC2 AMI.
updateInfrastructureConfiguration_securityGroupIds :: Lens.Lens' UpdateInfrastructureConfiguration (Prelude.Maybe [Prelude.Text])
updateInfrastructureConfiguration_securityGroupIds = Lens.lens (\UpdateInfrastructureConfiguration' {securityGroupIds} -> securityGroupIds) (\s@UpdateInfrastructureConfiguration' {} a -> s {securityGroupIds = a} :: UpdateInfrastructureConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The subnet ID to place the instance used to customize your Amazon EC2
-- AMI in.
updateInfrastructureConfiguration_subnetId :: Lens.Lens' UpdateInfrastructureConfiguration (Prelude.Maybe Prelude.Text)
updateInfrastructureConfiguration_subnetId = Lens.lens (\UpdateInfrastructureConfiguration' {subnetId} -> subnetId) (\s@UpdateInfrastructureConfiguration' {} a -> s {subnetId = a} :: UpdateInfrastructureConfiguration)

-- | The description of the infrastructure configuration.
updateInfrastructureConfiguration_description :: Lens.Lens' UpdateInfrastructureConfiguration (Prelude.Maybe Prelude.Text)
updateInfrastructureConfiguration_description = Lens.lens (\UpdateInfrastructureConfiguration' {description} -> description) (\s@UpdateInfrastructureConfiguration' {} a -> s {description = a} :: UpdateInfrastructureConfiguration)

-- | The tags attached to the resource created by Image Builder.
updateInfrastructureConfiguration_resourceTags :: Lens.Lens' UpdateInfrastructureConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateInfrastructureConfiguration_resourceTags = Lens.lens (\UpdateInfrastructureConfiguration' {resourceTags} -> resourceTags) (\s@UpdateInfrastructureConfiguration' {} a -> s {resourceTags = a} :: UpdateInfrastructureConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The key pair of the infrastructure configuration. You can use this to
-- log on to and debug the instance used to create your image.
updateInfrastructureConfiguration_keyPair :: Lens.Lens' UpdateInfrastructureConfiguration (Prelude.Maybe Prelude.Text)
updateInfrastructureConfiguration_keyPair = Lens.lens (\UpdateInfrastructureConfiguration' {keyPair} -> keyPair) (\s@UpdateInfrastructureConfiguration' {} a -> s {keyPair = a} :: UpdateInfrastructureConfiguration)

-- | The logging configuration of the infrastructure configuration.
updateInfrastructureConfiguration_logging :: Lens.Lens' UpdateInfrastructureConfiguration (Prelude.Maybe Logging)
updateInfrastructureConfiguration_logging = Lens.lens (\UpdateInfrastructureConfiguration' {logging} -> logging) (\s@UpdateInfrastructureConfiguration' {} a -> s {logging = a} :: UpdateInfrastructureConfiguration)

-- | The Amazon Resource Name (ARN) for the SNS topic to which we send image
-- build event notifications.
--
-- EC2 Image Builder is unable to send notifications to SNS topics that are
-- encrypted using keys from other accounts. The key that is used to
-- encrypt the SNS topic must reside in the account that the Image Builder
-- service runs under.
updateInfrastructureConfiguration_snsTopicArn :: Lens.Lens' UpdateInfrastructureConfiguration (Prelude.Maybe Prelude.Text)
updateInfrastructureConfiguration_snsTopicArn = Lens.lens (\UpdateInfrastructureConfiguration' {snsTopicArn} -> snsTopicArn) (\s@UpdateInfrastructureConfiguration' {} a -> s {snsTopicArn = a} :: UpdateInfrastructureConfiguration)

-- | The instance metadata options that you can set for the HTTP requests
-- that pipeline builds use to launch EC2 build and test instances. For
-- more information about instance metadata options, see one of the
-- following links:
--
-- -   <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/configuring-instance-metadata-options.html Configure the instance metadata options>
--     in the //Amazon EC2 User Guide// for Linux instances.
--
-- -   <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/configuring-instance-metadata-options.html Configure the instance metadata options>
--     in the //Amazon EC2 Windows Guide// for Windows instances.
updateInfrastructureConfiguration_instanceMetadataOptions :: Lens.Lens' UpdateInfrastructureConfiguration (Prelude.Maybe InstanceMetadataOptions)
updateInfrastructureConfiguration_instanceMetadataOptions = Lens.lens (\UpdateInfrastructureConfiguration' {instanceMetadataOptions} -> instanceMetadataOptions) (\s@UpdateInfrastructureConfiguration' {} a -> s {instanceMetadataOptions = a} :: UpdateInfrastructureConfiguration)

-- | The terminate instance on failure setting of the infrastructure
-- configuration. Set to false if you want Image Builder to retain the
-- instance used to configure your AMI if the build or test phase of your
-- workflow fails.
updateInfrastructureConfiguration_terminateInstanceOnFailure :: Lens.Lens' UpdateInfrastructureConfiguration (Prelude.Maybe Prelude.Bool)
updateInfrastructureConfiguration_terminateInstanceOnFailure = Lens.lens (\UpdateInfrastructureConfiguration' {terminateInstanceOnFailure} -> terminateInstanceOnFailure) (\s@UpdateInfrastructureConfiguration' {} a -> s {terminateInstanceOnFailure = a} :: UpdateInfrastructureConfiguration)

-- | The Amazon Resource Name (ARN) of the infrastructure configuration that
-- you want to update.
updateInfrastructureConfiguration_infrastructureConfigurationArn :: Lens.Lens' UpdateInfrastructureConfiguration Prelude.Text
updateInfrastructureConfiguration_infrastructureConfigurationArn = Lens.lens (\UpdateInfrastructureConfiguration' {infrastructureConfigurationArn} -> infrastructureConfigurationArn) (\s@UpdateInfrastructureConfiguration' {} a -> s {infrastructureConfigurationArn = a} :: UpdateInfrastructureConfiguration)

-- | The instance profile to associate with the instance used to customize
-- your Amazon EC2 AMI.
updateInfrastructureConfiguration_instanceProfileName :: Lens.Lens' UpdateInfrastructureConfiguration Prelude.Text
updateInfrastructureConfiguration_instanceProfileName = Lens.lens (\UpdateInfrastructureConfiguration' {instanceProfileName} -> instanceProfileName) (\s@UpdateInfrastructureConfiguration' {} a -> s {instanceProfileName = a} :: UpdateInfrastructureConfiguration)

-- | The idempotency token used to make this request idempotent.
updateInfrastructureConfiguration_clientToken :: Lens.Lens' UpdateInfrastructureConfiguration Prelude.Text
updateInfrastructureConfiguration_clientToken = Lens.lens (\UpdateInfrastructureConfiguration' {clientToken} -> clientToken) (\s@UpdateInfrastructureConfiguration' {} a -> s {clientToken = a} :: UpdateInfrastructureConfiguration)

instance
  Core.AWSRequest
    UpdateInfrastructureConfiguration
  where
  type
    AWSResponse UpdateInfrastructureConfiguration =
      UpdateInfrastructureConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateInfrastructureConfigurationResponse'
            Prelude.<$> (x Core..?> "clientToken")
              Prelude.<*> (x Core..?> "requestId")
              Prelude.<*> (x Core..?> "infrastructureConfigurationArn")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateInfrastructureConfiguration
  where
  hashWithSalt
    _salt
    UpdateInfrastructureConfiguration' {..} =
      _salt `Prelude.hashWithSalt` instanceTypes
        `Prelude.hashWithSalt` securityGroupIds
        `Prelude.hashWithSalt` subnetId
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` resourceTags
        `Prelude.hashWithSalt` keyPair
        `Prelude.hashWithSalt` logging
        `Prelude.hashWithSalt` snsTopicArn
        `Prelude.hashWithSalt` instanceMetadataOptions
        `Prelude.hashWithSalt` terminateInstanceOnFailure
        `Prelude.hashWithSalt` infrastructureConfigurationArn
        `Prelude.hashWithSalt` instanceProfileName
        `Prelude.hashWithSalt` clientToken

instance
  Prelude.NFData
    UpdateInfrastructureConfiguration
  where
  rnf UpdateInfrastructureConfiguration' {..} =
    Prelude.rnf instanceTypes
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf resourceTags
      `Prelude.seq` Prelude.rnf keyPair
      `Prelude.seq` Prelude.rnf logging
      `Prelude.seq` Prelude.rnf snsTopicArn
      `Prelude.seq` Prelude.rnf instanceMetadataOptions
      `Prelude.seq` Prelude.rnf terminateInstanceOnFailure
      `Prelude.seq` Prelude.rnf infrastructureConfigurationArn
      `Prelude.seq` Prelude.rnf instanceProfileName
      `Prelude.seq` Prelude.rnf clientToken

instance
  Core.ToHeaders
    UpdateInfrastructureConfiguration
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
    UpdateInfrastructureConfiguration
  where
  toJSON UpdateInfrastructureConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("instanceTypes" Core..=) Prelude.<$> instanceTypes,
            ("securityGroupIds" Core..=)
              Prelude.<$> securityGroupIds,
            ("subnetId" Core..=) Prelude.<$> subnetId,
            ("description" Core..=) Prelude.<$> description,
            ("resourceTags" Core..=) Prelude.<$> resourceTags,
            ("keyPair" Core..=) Prelude.<$> keyPair,
            ("logging" Core..=) Prelude.<$> logging,
            ("snsTopicArn" Core..=) Prelude.<$> snsTopicArn,
            ("instanceMetadataOptions" Core..=)
              Prelude.<$> instanceMetadataOptions,
            ("terminateInstanceOnFailure" Core..=)
              Prelude.<$> terminateInstanceOnFailure,
            Prelude.Just
              ( "infrastructureConfigurationArn"
                  Core..= infrastructureConfigurationArn
              ),
            Prelude.Just
              ("instanceProfileName" Core..= instanceProfileName),
            Prelude.Just ("clientToken" Core..= clientToken)
          ]
      )

instance
  Core.ToPath
    UpdateInfrastructureConfiguration
  where
  toPath =
    Prelude.const "/UpdateInfrastructureConfiguration"

instance
  Core.ToQuery
    UpdateInfrastructureConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateInfrastructureConfigurationResponse' smart constructor.
data UpdateInfrastructureConfigurationResponse = UpdateInfrastructureConfigurationResponse'
  { -- | The idempotency token used to make this request idempotent.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the infrastructure configuration that
    -- was updated by this request.
    infrastructureConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInfrastructureConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateInfrastructureConfigurationResponse_clientToken' - The idempotency token used to make this request idempotent.
--
-- 'requestId', 'updateInfrastructureConfigurationResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'infrastructureConfigurationArn', 'updateInfrastructureConfigurationResponse_infrastructureConfigurationArn' - The Amazon Resource Name (ARN) of the infrastructure configuration that
-- was updated by this request.
--
-- 'httpStatus', 'updateInfrastructureConfigurationResponse_httpStatus' - The response's http status code.
newUpdateInfrastructureConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateInfrastructureConfigurationResponse
newUpdateInfrastructureConfigurationResponse
  pHttpStatus_ =
    UpdateInfrastructureConfigurationResponse'
      { clientToken =
          Prelude.Nothing,
        requestId = Prelude.Nothing,
        infrastructureConfigurationArn =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The idempotency token used to make this request idempotent.
updateInfrastructureConfigurationResponse_clientToken :: Lens.Lens' UpdateInfrastructureConfigurationResponse (Prelude.Maybe Prelude.Text)
updateInfrastructureConfigurationResponse_clientToken = Lens.lens (\UpdateInfrastructureConfigurationResponse' {clientToken} -> clientToken) (\s@UpdateInfrastructureConfigurationResponse' {} a -> s {clientToken = a} :: UpdateInfrastructureConfigurationResponse)

-- | The request ID that uniquely identifies this request.
updateInfrastructureConfigurationResponse_requestId :: Lens.Lens' UpdateInfrastructureConfigurationResponse (Prelude.Maybe Prelude.Text)
updateInfrastructureConfigurationResponse_requestId = Lens.lens (\UpdateInfrastructureConfigurationResponse' {requestId} -> requestId) (\s@UpdateInfrastructureConfigurationResponse' {} a -> s {requestId = a} :: UpdateInfrastructureConfigurationResponse)

-- | The Amazon Resource Name (ARN) of the infrastructure configuration that
-- was updated by this request.
updateInfrastructureConfigurationResponse_infrastructureConfigurationArn :: Lens.Lens' UpdateInfrastructureConfigurationResponse (Prelude.Maybe Prelude.Text)
updateInfrastructureConfigurationResponse_infrastructureConfigurationArn = Lens.lens (\UpdateInfrastructureConfigurationResponse' {infrastructureConfigurationArn} -> infrastructureConfigurationArn) (\s@UpdateInfrastructureConfigurationResponse' {} a -> s {infrastructureConfigurationArn = a} :: UpdateInfrastructureConfigurationResponse)

-- | The response's http status code.
updateInfrastructureConfigurationResponse_httpStatus :: Lens.Lens' UpdateInfrastructureConfigurationResponse Prelude.Int
updateInfrastructureConfigurationResponse_httpStatus = Lens.lens (\UpdateInfrastructureConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateInfrastructureConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateInfrastructureConfigurationResponse)

instance
  Prelude.NFData
    UpdateInfrastructureConfigurationResponse
  where
  rnf UpdateInfrastructureConfigurationResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf infrastructureConfigurationArn
      `Prelude.seq` Prelude.rnf httpStatus
