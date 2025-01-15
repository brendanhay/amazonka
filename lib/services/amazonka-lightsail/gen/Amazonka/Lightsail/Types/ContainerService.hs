{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Lightsail.Types.ContainerService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ContainerService where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.ContainerServiceDeployment
import Amazonka.Lightsail.Types.ContainerServicePowerName
import Amazonka.Lightsail.Types.ContainerServiceState
import Amazonka.Lightsail.Types.ContainerServiceStateDetail
import Amazonka.Lightsail.Types.PrivateRegistryAccess
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceType
import Amazonka.Lightsail.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon Lightsail container service.
--
-- /See:/ 'newContainerService' smart constructor.
data ContainerService = ContainerService'
  { -- | The Amazon Resource Name (ARN) of the container service.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the container service.
    containerServiceName :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the container service was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | An object that describes the current container deployment of the
    -- container service.
    currentDeployment :: Prelude.Maybe ContainerServiceDeployment,
    -- | A Boolean value indicating whether the container service is disabled.
    isDisabled :: Prelude.Maybe Prelude.Bool,
    -- | An object that describes the location of the container service, such as
    -- the Amazon Web Services Region and Availability Zone.
    location :: Prelude.Maybe ResourceLocation,
    -- | An object that describes the next deployment of the container service.
    --
    -- This value is @null@ when there is no deployment in a @pending@ state.
    nextDeployment :: Prelude.Maybe ContainerServiceDeployment,
    -- | The power specification of the container service.
    --
    -- The power specifies the amount of RAM, the number of vCPUs, and the base
    -- price of the container service.
    power :: Prelude.Maybe ContainerServicePowerName,
    -- | The ID of the power of the container service.
    powerId :: Prelude.Maybe Prelude.Text,
    -- | The principal ARN of the container service.
    --
    -- The principal ARN can be used to create a trust relationship between
    -- your standard Amazon Web Services account and your Lightsail container
    -- service. This allows you to give your service permission to access
    -- resources in your standard Amazon Web Services account.
    principalArn :: Prelude.Maybe Prelude.Text,
    -- | The private domain name of the container service.
    --
    -- The private domain name is accessible only by other resources within the
    -- default virtual private cloud (VPC) of your Lightsail account.
    privateDomainName :: Prelude.Maybe Prelude.Text,
    -- | An object that describes the configuration for the container service to
    -- access private container image repositories, such as Amazon Elastic
    -- Container Registry (Amazon ECR) private repositories.
    --
    -- For more information, see
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-container-service-ecr-private-repo-access Configuring access to an Amazon ECR private repository for an Amazon Lightsail container service>
    -- in the /Amazon Lightsail Developer Guide/.
    privateRegistryAccess :: Prelude.Maybe PrivateRegistryAccess,
    -- | The public domain name of the container service, such as @example.com@
    -- and @www.example.com@.
    --
    -- You can specify up to four public domain names for a container service.
    -- The domain names that you specify are used when you create a deployment
    -- with a container configured as the public endpoint of your container
    -- service.
    --
    -- If you don\'t specify public domain names, then you can use the default
    -- domain of the container service.
    --
    -- You must create and validate an SSL\/TLS certificate before you can use
    -- public domain names with your container service. Use the
    -- @CreateCertificate@ action to create a certificate for the public domain
    -- names you want to use with your container service.
    --
    -- See @CreateContainerService@ or @UpdateContainerService@ for information
    -- about how to specify public domain names for your Lightsail container
    -- service.
    publicDomainNames :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The Lightsail resource type of the container service (i.e.,
    -- @ContainerService@).
    resourceType :: Prelude.Maybe ResourceType,
    -- | The scale specification of the container service.
    --
    -- The scale specifies the allocated compute nodes of the container
    -- service.
    scale :: Prelude.Maybe Prelude.Natural,
    -- | The current state of the container service.
    --
    -- The following container service states are possible:
    --
    -- -   @PENDING@ - The container service is being created.
    --
    -- -   @READY@ - The container service is running but it does not have an
    --     active container deployment.
    --
    -- -   @DEPLOYING@ - The container service is launching a container
    --     deployment.
    --
    -- -   @RUNNING@ - The container service is running and it has an active
    --     container deployment.
    --
    -- -   @UPDATING@ - The container service capacity or its custom domains
    --     are being updated.
    --
    -- -   @DELETING@ - The container service is being deleted.
    --
    -- -   @DISABLED@ - The container service is disabled, and its active
    --     deployment and containers, if any, are shut down.
    state :: Prelude.Maybe ContainerServiceState,
    -- | An object that describes the current state of the container service.
    --
    -- The state detail is populated only when a container service is in a
    -- @PENDING@, @DEPLOYING@, or @UPDATING@ state.
    stateDetail :: Prelude.Maybe ContainerServiceStateDetail,
    -- | The tag keys and optional values for the resource. For more information
    -- about tags in Lightsail, see the
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
    tags :: Prelude.Maybe [Tag],
    -- | The publicly accessible URL of the container service.
    --
    -- If no public endpoint is specified in the @currentDeployment@, this URL
    -- returns a 404 response.
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'containerService_arn' - The Amazon Resource Name (ARN) of the container service.
--
-- 'containerServiceName', 'containerService_containerServiceName' - The name of the container service.
--
-- 'createdAt', 'containerService_createdAt' - The timestamp when the container service was created.
--
-- 'currentDeployment', 'containerService_currentDeployment' - An object that describes the current container deployment of the
-- container service.
--
-- 'isDisabled', 'containerService_isDisabled' - A Boolean value indicating whether the container service is disabled.
--
-- 'location', 'containerService_location' - An object that describes the location of the container service, such as
-- the Amazon Web Services Region and Availability Zone.
--
-- 'nextDeployment', 'containerService_nextDeployment' - An object that describes the next deployment of the container service.
--
-- This value is @null@ when there is no deployment in a @pending@ state.
--
-- 'power', 'containerService_power' - The power specification of the container service.
--
-- The power specifies the amount of RAM, the number of vCPUs, and the base
-- price of the container service.
--
-- 'powerId', 'containerService_powerId' - The ID of the power of the container service.
--
-- 'principalArn', 'containerService_principalArn' - The principal ARN of the container service.
--
-- The principal ARN can be used to create a trust relationship between
-- your standard Amazon Web Services account and your Lightsail container
-- service. This allows you to give your service permission to access
-- resources in your standard Amazon Web Services account.
--
-- 'privateDomainName', 'containerService_privateDomainName' - The private domain name of the container service.
--
-- The private domain name is accessible only by other resources within the
-- default virtual private cloud (VPC) of your Lightsail account.
--
-- 'privateRegistryAccess', 'containerService_privateRegistryAccess' - An object that describes the configuration for the container service to
-- access private container image repositories, such as Amazon Elastic
-- Container Registry (Amazon ECR) private repositories.
--
-- For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-container-service-ecr-private-repo-access Configuring access to an Amazon ECR private repository for an Amazon Lightsail container service>
-- in the /Amazon Lightsail Developer Guide/.
--
-- 'publicDomainNames', 'containerService_publicDomainNames' - The public domain name of the container service, such as @example.com@
-- and @www.example.com@.
--
-- You can specify up to four public domain names for a container service.
-- The domain names that you specify are used when you create a deployment
-- with a container configured as the public endpoint of your container
-- service.
--
-- If you don\'t specify public domain names, then you can use the default
-- domain of the container service.
--
-- You must create and validate an SSL\/TLS certificate before you can use
-- public domain names with your container service. Use the
-- @CreateCertificate@ action to create a certificate for the public domain
-- names you want to use with your container service.
--
-- See @CreateContainerService@ or @UpdateContainerService@ for information
-- about how to specify public domain names for your Lightsail container
-- service.
--
-- 'resourceType', 'containerService_resourceType' - The Lightsail resource type of the container service (i.e.,
-- @ContainerService@).
--
-- 'scale', 'containerService_scale' - The scale specification of the container service.
--
-- The scale specifies the allocated compute nodes of the container
-- service.
--
-- 'state', 'containerService_state' - The current state of the container service.
--
-- The following container service states are possible:
--
-- -   @PENDING@ - The container service is being created.
--
-- -   @READY@ - The container service is running but it does not have an
--     active container deployment.
--
-- -   @DEPLOYING@ - The container service is launching a container
--     deployment.
--
-- -   @RUNNING@ - The container service is running and it has an active
--     container deployment.
--
-- -   @UPDATING@ - The container service capacity or its custom domains
--     are being updated.
--
-- -   @DELETING@ - The container service is being deleted.
--
-- -   @DISABLED@ - The container service is disabled, and its active
--     deployment and containers, if any, are shut down.
--
-- 'stateDetail', 'containerService_stateDetail' - An object that describes the current state of the container service.
--
-- The state detail is populated only when a container service is in a
-- @PENDING@, @DEPLOYING@, or @UPDATING@ state.
--
-- 'tags', 'containerService_tags' - The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
--
-- 'url', 'containerService_url' - The publicly accessible URL of the container service.
--
-- If no public endpoint is specified in the @currentDeployment@, this URL
-- returns a 404 response.
newContainerService ::
  ContainerService
newContainerService =
  ContainerService'
    { arn = Prelude.Nothing,
      containerServiceName = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      currentDeployment = Prelude.Nothing,
      isDisabled = Prelude.Nothing,
      location = Prelude.Nothing,
      nextDeployment = Prelude.Nothing,
      power = Prelude.Nothing,
      powerId = Prelude.Nothing,
      principalArn = Prelude.Nothing,
      privateDomainName = Prelude.Nothing,
      privateRegistryAccess = Prelude.Nothing,
      publicDomainNames = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      scale = Prelude.Nothing,
      state = Prelude.Nothing,
      stateDetail = Prelude.Nothing,
      tags = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the container service.
containerService_arn :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Text)
containerService_arn = Lens.lens (\ContainerService' {arn} -> arn) (\s@ContainerService' {} a -> s {arn = a} :: ContainerService)

-- | The name of the container service.
containerService_containerServiceName :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Text)
containerService_containerServiceName = Lens.lens (\ContainerService' {containerServiceName} -> containerServiceName) (\s@ContainerService' {} a -> s {containerServiceName = a} :: ContainerService)

-- | The timestamp when the container service was created.
containerService_createdAt :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.UTCTime)
containerService_createdAt = Lens.lens (\ContainerService' {createdAt} -> createdAt) (\s@ContainerService' {} a -> s {createdAt = a} :: ContainerService) Prelude.. Lens.mapping Data._Time

-- | An object that describes the current container deployment of the
-- container service.
containerService_currentDeployment :: Lens.Lens' ContainerService (Prelude.Maybe ContainerServiceDeployment)
containerService_currentDeployment = Lens.lens (\ContainerService' {currentDeployment} -> currentDeployment) (\s@ContainerService' {} a -> s {currentDeployment = a} :: ContainerService)

-- | A Boolean value indicating whether the container service is disabled.
containerService_isDisabled :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Bool)
containerService_isDisabled = Lens.lens (\ContainerService' {isDisabled} -> isDisabled) (\s@ContainerService' {} a -> s {isDisabled = a} :: ContainerService)

-- | An object that describes the location of the container service, such as
-- the Amazon Web Services Region and Availability Zone.
containerService_location :: Lens.Lens' ContainerService (Prelude.Maybe ResourceLocation)
containerService_location = Lens.lens (\ContainerService' {location} -> location) (\s@ContainerService' {} a -> s {location = a} :: ContainerService)

-- | An object that describes the next deployment of the container service.
--
-- This value is @null@ when there is no deployment in a @pending@ state.
containerService_nextDeployment :: Lens.Lens' ContainerService (Prelude.Maybe ContainerServiceDeployment)
containerService_nextDeployment = Lens.lens (\ContainerService' {nextDeployment} -> nextDeployment) (\s@ContainerService' {} a -> s {nextDeployment = a} :: ContainerService)

-- | The power specification of the container service.
--
-- The power specifies the amount of RAM, the number of vCPUs, and the base
-- price of the container service.
containerService_power :: Lens.Lens' ContainerService (Prelude.Maybe ContainerServicePowerName)
containerService_power = Lens.lens (\ContainerService' {power} -> power) (\s@ContainerService' {} a -> s {power = a} :: ContainerService)

-- | The ID of the power of the container service.
containerService_powerId :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Text)
containerService_powerId = Lens.lens (\ContainerService' {powerId} -> powerId) (\s@ContainerService' {} a -> s {powerId = a} :: ContainerService)

-- | The principal ARN of the container service.
--
-- The principal ARN can be used to create a trust relationship between
-- your standard Amazon Web Services account and your Lightsail container
-- service. This allows you to give your service permission to access
-- resources in your standard Amazon Web Services account.
containerService_principalArn :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Text)
containerService_principalArn = Lens.lens (\ContainerService' {principalArn} -> principalArn) (\s@ContainerService' {} a -> s {principalArn = a} :: ContainerService)

-- | The private domain name of the container service.
--
-- The private domain name is accessible only by other resources within the
-- default virtual private cloud (VPC) of your Lightsail account.
containerService_privateDomainName :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Text)
containerService_privateDomainName = Lens.lens (\ContainerService' {privateDomainName} -> privateDomainName) (\s@ContainerService' {} a -> s {privateDomainName = a} :: ContainerService)

-- | An object that describes the configuration for the container service to
-- access private container image repositories, such as Amazon Elastic
-- Container Registry (Amazon ECR) private repositories.
--
-- For more information, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-container-service-ecr-private-repo-access Configuring access to an Amazon ECR private repository for an Amazon Lightsail container service>
-- in the /Amazon Lightsail Developer Guide/.
containerService_privateRegistryAccess :: Lens.Lens' ContainerService (Prelude.Maybe PrivateRegistryAccess)
containerService_privateRegistryAccess = Lens.lens (\ContainerService' {privateRegistryAccess} -> privateRegistryAccess) (\s@ContainerService' {} a -> s {privateRegistryAccess = a} :: ContainerService)

-- | The public domain name of the container service, such as @example.com@
-- and @www.example.com@.
--
-- You can specify up to four public domain names for a container service.
-- The domain names that you specify are used when you create a deployment
-- with a container configured as the public endpoint of your container
-- service.
--
-- If you don\'t specify public domain names, then you can use the default
-- domain of the container service.
--
-- You must create and validate an SSL\/TLS certificate before you can use
-- public domain names with your container service. Use the
-- @CreateCertificate@ action to create a certificate for the public domain
-- names you want to use with your container service.
--
-- See @CreateContainerService@ or @UpdateContainerService@ for information
-- about how to specify public domain names for your Lightsail container
-- service.
containerService_publicDomainNames :: Lens.Lens' ContainerService (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
containerService_publicDomainNames = Lens.lens (\ContainerService' {publicDomainNames} -> publicDomainNames) (\s@ContainerService' {} a -> s {publicDomainNames = a} :: ContainerService) Prelude.. Lens.mapping Lens.coerced

-- | The Lightsail resource type of the container service (i.e.,
-- @ContainerService@).
containerService_resourceType :: Lens.Lens' ContainerService (Prelude.Maybe ResourceType)
containerService_resourceType = Lens.lens (\ContainerService' {resourceType} -> resourceType) (\s@ContainerService' {} a -> s {resourceType = a} :: ContainerService)

-- | The scale specification of the container service.
--
-- The scale specifies the allocated compute nodes of the container
-- service.
containerService_scale :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Natural)
containerService_scale = Lens.lens (\ContainerService' {scale} -> scale) (\s@ContainerService' {} a -> s {scale = a} :: ContainerService)

-- | The current state of the container service.
--
-- The following container service states are possible:
--
-- -   @PENDING@ - The container service is being created.
--
-- -   @READY@ - The container service is running but it does not have an
--     active container deployment.
--
-- -   @DEPLOYING@ - The container service is launching a container
--     deployment.
--
-- -   @RUNNING@ - The container service is running and it has an active
--     container deployment.
--
-- -   @UPDATING@ - The container service capacity or its custom domains
--     are being updated.
--
-- -   @DELETING@ - The container service is being deleted.
--
-- -   @DISABLED@ - The container service is disabled, and its active
--     deployment and containers, if any, are shut down.
containerService_state :: Lens.Lens' ContainerService (Prelude.Maybe ContainerServiceState)
containerService_state = Lens.lens (\ContainerService' {state} -> state) (\s@ContainerService' {} a -> s {state = a} :: ContainerService)

-- | An object that describes the current state of the container service.
--
-- The state detail is populated only when a container service is in a
-- @PENDING@, @DEPLOYING@, or @UPDATING@ state.
containerService_stateDetail :: Lens.Lens' ContainerService (Prelude.Maybe ContainerServiceStateDetail)
containerService_stateDetail = Lens.lens (\ContainerService' {stateDetail} -> stateDetail) (\s@ContainerService' {} a -> s {stateDetail = a} :: ContainerService)

-- | The tag keys and optional values for the resource. For more information
-- about tags in Lightsail, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-tags Amazon Lightsail Developer Guide>.
containerService_tags :: Lens.Lens' ContainerService (Prelude.Maybe [Tag])
containerService_tags = Lens.lens (\ContainerService' {tags} -> tags) (\s@ContainerService' {} a -> s {tags = a} :: ContainerService) Prelude.. Lens.mapping Lens.coerced

-- | The publicly accessible URL of the container service.
--
-- If no public endpoint is specified in the @currentDeployment@, this URL
-- returns a 404 response.
containerService_url :: Lens.Lens' ContainerService (Prelude.Maybe Prelude.Text)
containerService_url = Lens.lens (\ContainerService' {url} -> url) (\s@ContainerService' {} a -> s {url = a} :: ContainerService)

instance Data.FromJSON ContainerService where
  parseJSON =
    Data.withObject
      "ContainerService"
      ( \x ->
          ContainerService'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "containerServiceName")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "currentDeployment")
            Prelude.<*> (x Data..:? "isDisabled")
            Prelude.<*> (x Data..:? "location")
            Prelude.<*> (x Data..:? "nextDeployment")
            Prelude.<*> (x Data..:? "power")
            Prelude.<*> (x Data..:? "powerId")
            Prelude.<*> (x Data..:? "principalArn")
            Prelude.<*> (x Data..:? "privateDomainName")
            Prelude.<*> (x Data..:? "privateRegistryAccess")
            Prelude.<*> ( x
                            Data..:? "publicDomainNames"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> (x Data..:? "scale")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "stateDetail")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "url")
      )

instance Prelude.Hashable ContainerService where
  hashWithSalt _salt ContainerService' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` containerServiceName
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` currentDeployment
      `Prelude.hashWithSalt` isDisabled
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` nextDeployment
      `Prelude.hashWithSalt` power
      `Prelude.hashWithSalt` powerId
      `Prelude.hashWithSalt` principalArn
      `Prelude.hashWithSalt` privateDomainName
      `Prelude.hashWithSalt` privateRegistryAccess
      `Prelude.hashWithSalt` publicDomainNames
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` scale
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` stateDetail
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` url

instance Prelude.NFData ContainerService where
  rnf ContainerService' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf containerServiceName `Prelude.seq`
        Prelude.rnf createdAt `Prelude.seq`
          Prelude.rnf currentDeployment `Prelude.seq`
            Prelude.rnf isDisabled `Prelude.seq`
              Prelude.rnf location `Prelude.seq`
                Prelude.rnf nextDeployment `Prelude.seq`
                  Prelude.rnf power `Prelude.seq`
                    Prelude.rnf powerId `Prelude.seq`
                      Prelude.rnf principalArn `Prelude.seq`
                        Prelude.rnf privateDomainName `Prelude.seq`
                          Prelude.rnf privateRegistryAccess `Prelude.seq`
                            Prelude.rnf publicDomainNames `Prelude.seq`
                              Prelude.rnf resourceType `Prelude.seq`
                                Prelude.rnf scale `Prelude.seq`
                                  Prelude.rnf state `Prelude.seq`
                                    Prelude.rnf stateDetail `Prelude.seq`
                                      Prelude.rnf tags `Prelude.seq`
                                        Prelude.rnf url
