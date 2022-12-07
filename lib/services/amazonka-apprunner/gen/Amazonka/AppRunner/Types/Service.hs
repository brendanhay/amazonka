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
-- Module      : Amazonka.AppRunner.Types.Service
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.Service where

import Amazonka.AppRunner.Types.AutoScalingConfigurationSummary
import Amazonka.AppRunner.Types.EncryptionConfiguration
import Amazonka.AppRunner.Types.HealthCheckConfiguration
import Amazonka.AppRunner.Types.InstanceConfiguration
import Amazonka.AppRunner.Types.NetworkConfiguration
import Amazonka.AppRunner.Types.ServiceObservabilityConfiguration
import Amazonka.AppRunner.Types.ServiceStatus
import Amazonka.AppRunner.Types.SourceConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes an App Runner service. It can describe a service in any state,
-- including deleted services.
--
-- This type contains the full information about a service, including
-- configuration details. It\'s returned by the
-- <https://docs.aws.amazon.com/apprunner/latest/api/API_CreateService.html CreateService>,
-- <https://docs.aws.amazon.com/apprunner/latest/api/API_DescribeService.html DescribeService>,
-- and
-- <https://docs.aws.amazon.com/apprunner/latest/api/API_DeleteService.html DeleteService>
-- actions. A subset of this information is returned by the
-- <https://docs.aws.amazon.com/apprunner/latest/api/API_ListServices.html ListServices>
-- action using the
-- <https://docs.aws.amazon.com/apprunner/latest/api/API_ServiceSummary.html ServiceSummary>
-- type.
--
-- /See:/ 'newService' smart constructor.
data Service = Service'
  { -- | The observability configuration of this service.
    observabilityConfiguration :: Prelude.Maybe ServiceObservabilityConfiguration,
    -- | The time when the App Runner service was deleted. It\'s in the Unix time
    -- stamp format.
    deletedAt :: Prelude.Maybe Data.POSIX,
    -- | The encryption key that App Runner uses to encrypt the service logs and
    -- the copy of the source repository that App Runner maintains for the
    -- service. It can be either a customer-provided encryption key or an
    -- Amazon Web Services managed key.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | A subdomain URL that App Runner generated for this service. You can use
    -- this URL to access your service web application.
    serviceUrl :: Prelude.Maybe Prelude.Text,
    -- | The settings for the health check that App Runner performs to monitor
    -- the health of this service.
    healthCheckConfiguration :: Prelude.Maybe HealthCheckConfiguration,
    -- | The customer-provided service name.
    serviceName :: Prelude.Text,
    -- | An ID that App Runner generated for this service. It\'s unique within
    -- the Amazon Web Services Region.
    serviceId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of this service.
    serviceArn :: Prelude.Text,
    -- | The time when the App Runner service was created. It\'s in the Unix time
    -- stamp format.
    createdAt :: Data.POSIX,
    -- | The time when the App Runner service was last updated at. It\'s in the
    -- Unix time stamp format.
    updatedAt :: Data.POSIX,
    -- | The current state of the App Runner service. These particular values
    -- mean the following.
    --
    -- -   @CREATE_FAILED@ – The service failed to create. To troubleshoot this
    --     failure, read the failure events and logs, change any parameters
    --     that need to be fixed, and retry the call to create the service.
    --
    --     The failed service isn\'t usable, and still counts towards your
    --     service quota. When you\'re done analyzing the failure, delete the
    --     service.
    --
    -- -   @DELETE_FAILED@ – The service failed to delete and can\'t be
    --     successfully recovered. Retry the service deletion call to ensure
    --     that all related resources are removed.
    status :: ServiceStatus,
    -- | The source deployed to the App Runner service. It can be a code or an
    -- image repository.
    sourceConfiguration :: SourceConfiguration,
    -- | The runtime configuration of instances (scaling units) of this service.
    instanceConfiguration :: InstanceConfiguration,
    -- | Summary information for the App Runner automatic scaling configuration
    -- resource that\'s associated with this service.
    autoScalingConfigurationSummary :: AutoScalingConfigurationSummary,
    -- | Configuration settings related to network traffic of the web application
    -- that this service runs.
    networkConfiguration :: NetworkConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Service' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'observabilityConfiguration', 'service_observabilityConfiguration' - The observability configuration of this service.
--
-- 'deletedAt', 'service_deletedAt' - The time when the App Runner service was deleted. It\'s in the Unix time
-- stamp format.
--
-- 'encryptionConfiguration', 'service_encryptionConfiguration' - The encryption key that App Runner uses to encrypt the service logs and
-- the copy of the source repository that App Runner maintains for the
-- service. It can be either a customer-provided encryption key or an
-- Amazon Web Services managed key.
--
-- 'serviceUrl', 'service_serviceUrl' - A subdomain URL that App Runner generated for this service. You can use
-- this URL to access your service web application.
--
-- 'healthCheckConfiguration', 'service_healthCheckConfiguration' - The settings for the health check that App Runner performs to monitor
-- the health of this service.
--
-- 'serviceName', 'service_serviceName' - The customer-provided service name.
--
-- 'serviceId', 'service_serviceId' - An ID that App Runner generated for this service. It\'s unique within
-- the Amazon Web Services Region.
--
-- 'serviceArn', 'service_serviceArn' - The Amazon Resource Name (ARN) of this service.
--
-- 'createdAt', 'service_createdAt' - The time when the App Runner service was created. It\'s in the Unix time
-- stamp format.
--
-- 'updatedAt', 'service_updatedAt' - The time when the App Runner service was last updated at. It\'s in the
-- Unix time stamp format.
--
-- 'status', 'service_status' - The current state of the App Runner service. These particular values
-- mean the following.
--
-- -   @CREATE_FAILED@ – The service failed to create. To troubleshoot this
--     failure, read the failure events and logs, change any parameters
--     that need to be fixed, and retry the call to create the service.
--
--     The failed service isn\'t usable, and still counts towards your
--     service quota. When you\'re done analyzing the failure, delete the
--     service.
--
-- -   @DELETE_FAILED@ – The service failed to delete and can\'t be
--     successfully recovered. Retry the service deletion call to ensure
--     that all related resources are removed.
--
-- 'sourceConfiguration', 'service_sourceConfiguration' - The source deployed to the App Runner service. It can be a code or an
-- image repository.
--
-- 'instanceConfiguration', 'service_instanceConfiguration' - The runtime configuration of instances (scaling units) of this service.
--
-- 'autoScalingConfigurationSummary', 'service_autoScalingConfigurationSummary' - Summary information for the App Runner automatic scaling configuration
-- resource that\'s associated with this service.
--
-- 'networkConfiguration', 'service_networkConfiguration' - Configuration settings related to network traffic of the web application
-- that this service runs.
newService ::
  -- | 'serviceName'
  Prelude.Text ->
  -- | 'serviceId'
  Prelude.Text ->
  -- | 'serviceArn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  -- | 'status'
  ServiceStatus ->
  -- | 'sourceConfiguration'
  SourceConfiguration ->
  -- | 'instanceConfiguration'
  InstanceConfiguration ->
  -- | 'autoScalingConfigurationSummary'
  AutoScalingConfigurationSummary ->
  -- | 'networkConfiguration'
  NetworkConfiguration ->
  Service
newService
  pServiceName_
  pServiceId_
  pServiceArn_
  pCreatedAt_
  pUpdatedAt_
  pStatus_
  pSourceConfiguration_
  pInstanceConfiguration_
  pAutoScalingConfigurationSummary_
  pNetworkConfiguration_ =
    Service'
      { observabilityConfiguration =
          Prelude.Nothing,
        deletedAt = Prelude.Nothing,
        encryptionConfiguration = Prelude.Nothing,
        serviceUrl = Prelude.Nothing,
        healthCheckConfiguration = Prelude.Nothing,
        serviceName = pServiceName_,
        serviceId = pServiceId_,
        serviceArn = pServiceArn_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        updatedAt = Data._Time Lens.# pUpdatedAt_,
        status = pStatus_,
        sourceConfiguration = pSourceConfiguration_,
        instanceConfiguration = pInstanceConfiguration_,
        autoScalingConfigurationSummary =
          pAutoScalingConfigurationSummary_,
        networkConfiguration = pNetworkConfiguration_
      }

-- | The observability configuration of this service.
service_observabilityConfiguration :: Lens.Lens' Service (Prelude.Maybe ServiceObservabilityConfiguration)
service_observabilityConfiguration = Lens.lens (\Service' {observabilityConfiguration} -> observabilityConfiguration) (\s@Service' {} a -> s {observabilityConfiguration = a} :: Service)

-- | The time when the App Runner service was deleted. It\'s in the Unix time
-- stamp format.
service_deletedAt :: Lens.Lens' Service (Prelude.Maybe Prelude.UTCTime)
service_deletedAt = Lens.lens (\Service' {deletedAt} -> deletedAt) (\s@Service' {} a -> s {deletedAt = a} :: Service) Prelude.. Lens.mapping Data._Time

-- | The encryption key that App Runner uses to encrypt the service logs and
-- the copy of the source repository that App Runner maintains for the
-- service. It can be either a customer-provided encryption key or an
-- Amazon Web Services managed key.
service_encryptionConfiguration :: Lens.Lens' Service (Prelude.Maybe EncryptionConfiguration)
service_encryptionConfiguration = Lens.lens (\Service' {encryptionConfiguration} -> encryptionConfiguration) (\s@Service' {} a -> s {encryptionConfiguration = a} :: Service)

-- | A subdomain URL that App Runner generated for this service. You can use
-- this URL to access your service web application.
service_serviceUrl :: Lens.Lens' Service (Prelude.Maybe Prelude.Text)
service_serviceUrl = Lens.lens (\Service' {serviceUrl} -> serviceUrl) (\s@Service' {} a -> s {serviceUrl = a} :: Service)

-- | The settings for the health check that App Runner performs to monitor
-- the health of this service.
service_healthCheckConfiguration :: Lens.Lens' Service (Prelude.Maybe HealthCheckConfiguration)
service_healthCheckConfiguration = Lens.lens (\Service' {healthCheckConfiguration} -> healthCheckConfiguration) (\s@Service' {} a -> s {healthCheckConfiguration = a} :: Service)

-- | The customer-provided service name.
service_serviceName :: Lens.Lens' Service Prelude.Text
service_serviceName = Lens.lens (\Service' {serviceName} -> serviceName) (\s@Service' {} a -> s {serviceName = a} :: Service)

-- | An ID that App Runner generated for this service. It\'s unique within
-- the Amazon Web Services Region.
service_serviceId :: Lens.Lens' Service Prelude.Text
service_serviceId = Lens.lens (\Service' {serviceId} -> serviceId) (\s@Service' {} a -> s {serviceId = a} :: Service)

-- | The Amazon Resource Name (ARN) of this service.
service_serviceArn :: Lens.Lens' Service Prelude.Text
service_serviceArn = Lens.lens (\Service' {serviceArn} -> serviceArn) (\s@Service' {} a -> s {serviceArn = a} :: Service)

-- | The time when the App Runner service was created. It\'s in the Unix time
-- stamp format.
service_createdAt :: Lens.Lens' Service Prelude.UTCTime
service_createdAt = Lens.lens (\Service' {createdAt} -> createdAt) (\s@Service' {} a -> s {createdAt = a} :: Service) Prelude.. Data._Time

-- | The time when the App Runner service was last updated at. It\'s in the
-- Unix time stamp format.
service_updatedAt :: Lens.Lens' Service Prelude.UTCTime
service_updatedAt = Lens.lens (\Service' {updatedAt} -> updatedAt) (\s@Service' {} a -> s {updatedAt = a} :: Service) Prelude.. Data._Time

-- | The current state of the App Runner service. These particular values
-- mean the following.
--
-- -   @CREATE_FAILED@ – The service failed to create. To troubleshoot this
--     failure, read the failure events and logs, change any parameters
--     that need to be fixed, and retry the call to create the service.
--
--     The failed service isn\'t usable, and still counts towards your
--     service quota. When you\'re done analyzing the failure, delete the
--     service.
--
-- -   @DELETE_FAILED@ – The service failed to delete and can\'t be
--     successfully recovered. Retry the service deletion call to ensure
--     that all related resources are removed.
service_status :: Lens.Lens' Service ServiceStatus
service_status = Lens.lens (\Service' {status} -> status) (\s@Service' {} a -> s {status = a} :: Service)

-- | The source deployed to the App Runner service. It can be a code or an
-- image repository.
service_sourceConfiguration :: Lens.Lens' Service SourceConfiguration
service_sourceConfiguration = Lens.lens (\Service' {sourceConfiguration} -> sourceConfiguration) (\s@Service' {} a -> s {sourceConfiguration = a} :: Service)

-- | The runtime configuration of instances (scaling units) of this service.
service_instanceConfiguration :: Lens.Lens' Service InstanceConfiguration
service_instanceConfiguration = Lens.lens (\Service' {instanceConfiguration} -> instanceConfiguration) (\s@Service' {} a -> s {instanceConfiguration = a} :: Service)

-- | Summary information for the App Runner automatic scaling configuration
-- resource that\'s associated with this service.
service_autoScalingConfigurationSummary :: Lens.Lens' Service AutoScalingConfigurationSummary
service_autoScalingConfigurationSummary = Lens.lens (\Service' {autoScalingConfigurationSummary} -> autoScalingConfigurationSummary) (\s@Service' {} a -> s {autoScalingConfigurationSummary = a} :: Service)

-- | Configuration settings related to network traffic of the web application
-- that this service runs.
service_networkConfiguration :: Lens.Lens' Service NetworkConfiguration
service_networkConfiguration = Lens.lens (\Service' {networkConfiguration} -> networkConfiguration) (\s@Service' {} a -> s {networkConfiguration = a} :: Service)

instance Data.FromJSON Service where
  parseJSON =
    Data.withObject
      "Service"
      ( \x ->
          Service'
            Prelude.<$> (x Data..:? "ObservabilityConfiguration")
            Prelude.<*> (x Data..:? "DeletedAt")
            Prelude.<*> (x Data..:? "EncryptionConfiguration")
            Prelude.<*> (x Data..:? "ServiceUrl")
            Prelude.<*> (x Data..:? "HealthCheckConfiguration")
            Prelude.<*> (x Data..: "ServiceName")
            Prelude.<*> (x Data..: "ServiceId")
            Prelude.<*> (x Data..: "ServiceArn")
            Prelude.<*> (x Data..: "CreatedAt")
            Prelude.<*> (x Data..: "UpdatedAt")
            Prelude.<*> (x Data..: "Status")
            Prelude.<*> (x Data..: "SourceConfiguration")
            Prelude.<*> (x Data..: "InstanceConfiguration")
            Prelude.<*> (x Data..: "AutoScalingConfigurationSummary")
            Prelude.<*> (x Data..: "NetworkConfiguration")
      )

instance Prelude.Hashable Service where
  hashWithSalt _salt Service' {..} =
    _salt
      `Prelude.hashWithSalt` observabilityConfiguration
      `Prelude.hashWithSalt` deletedAt
      `Prelude.hashWithSalt` encryptionConfiguration
      `Prelude.hashWithSalt` serviceUrl
      `Prelude.hashWithSalt` healthCheckConfiguration
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` serviceId
      `Prelude.hashWithSalt` serviceArn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` sourceConfiguration
      `Prelude.hashWithSalt` instanceConfiguration
      `Prelude.hashWithSalt` autoScalingConfigurationSummary
      `Prelude.hashWithSalt` networkConfiguration

instance Prelude.NFData Service where
  rnf Service' {..} =
    Prelude.rnf observabilityConfiguration
      `Prelude.seq` Prelude.rnf deletedAt
      `Prelude.seq` Prelude.rnf encryptionConfiguration
      `Prelude.seq` Prelude.rnf serviceUrl
      `Prelude.seq` Prelude.rnf healthCheckConfiguration
      `Prelude.seq` Prelude.rnf serviceName
      `Prelude.seq` Prelude.rnf serviceId
      `Prelude.seq` Prelude.rnf serviceArn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf sourceConfiguration
      `Prelude.seq` Prelude.rnf instanceConfiguration
      `Prelude.seq` Prelude.rnf
        autoScalingConfigurationSummary
      `Prelude.seq` Prelude.rnf networkConfiguration
