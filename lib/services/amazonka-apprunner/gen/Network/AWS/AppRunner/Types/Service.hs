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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.Service where

import Amazonka.AppRunner.Types.AutoScalingConfigurationSummary
import Amazonka.AppRunner.Types.EncryptionConfiguration
import Amazonka.AppRunner.Types.HealthCheckConfiguration
import Amazonka.AppRunner.Types.InstanceConfiguration
import Amazonka.AppRunner.Types.ServiceStatus
import Amazonka.AppRunner.Types.SourceConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
  { -- | The encryption key that App Runner uses to encrypt the service logs and
    -- the copy of the source repository that App Runner maintains for the
    -- service. It can be either a customer-provided encryption key or an
    -- Amazon Web Services managed CMK.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | The settings for the health check that App Runner performs to monitor
    -- the health of this service.
    healthCheckConfiguration :: Prelude.Maybe HealthCheckConfiguration,
    -- | The time when the App Runner service was deleted. It\'s in the Unix time
    -- stamp format.
    deletedAt :: Prelude.Maybe Core.POSIX,
    -- | The customer-provided service name.
    serviceName :: Prelude.Text,
    -- | An ID that App Runner generated for this service. It\'s unique within
    -- the Amazon Web Services Region.
    serviceId :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of this service.
    serviceArn :: Prelude.Text,
    -- | A subdomain URL that App Runner generated for this service. You can use
    -- this URL to access your service web application.
    serviceUrl :: Prelude.Text,
    -- | The time when the App Runner service was created. It\'s in the Unix time
    -- stamp format.
    createdAt :: Core.POSIX,
    -- | The time when the App Runner service was last updated at. It\'s in the
    -- Unix time stamp format.
    updatedAt :: Core.POSIX,
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
    autoScalingConfigurationSummary :: AutoScalingConfigurationSummary
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
-- 'encryptionConfiguration', 'service_encryptionConfiguration' - The encryption key that App Runner uses to encrypt the service logs and
-- the copy of the source repository that App Runner maintains for the
-- service. It can be either a customer-provided encryption key or an
-- Amazon Web Services managed CMK.
--
-- 'healthCheckConfiguration', 'service_healthCheckConfiguration' - The settings for the health check that App Runner performs to monitor
-- the health of this service.
--
-- 'deletedAt', 'service_deletedAt' - The time when the App Runner service was deleted. It\'s in the Unix time
-- stamp format.
--
-- 'serviceName', 'service_serviceName' - The customer-provided service name.
--
-- 'serviceId', 'service_serviceId' - An ID that App Runner generated for this service. It\'s unique within
-- the Amazon Web Services Region.
--
-- 'serviceArn', 'service_serviceArn' - The Amazon Resource Name (ARN) of this service.
--
-- 'serviceUrl', 'service_serviceUrl' - A subdomain URL that App Runner generated for this service. You can use
-- this URL to access your service web application.
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
newService ::
  -- | 'serviceName'
  Prelude.Text ->
  -- | 'serviceId'
  Prelude.Text ->
  -- | 'serviceArn'
  Prelude.Text ->
  -- | 'serviceUrl'
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
  Service
newService
  pServiceName_
  pServiceId_
  pServiceArn_
  pServiceUrl_
  pCreatedAt_
  pUpdatedAt_
  pStatus_
  pSourceConfiguration_
  pInstanceConfiguration_
  pAutoScalingConfigurationSummary_ =
    Service'
      { encryptionConfiguration = Prelude.Nothing,
        healthCheckConfiguration = Prelude.Nothing,
        deletedAt = Prelude.Nothing,
        serviceName = pServiceName_,
        serviceId = pServiceId_,
        serviceArn = pServiceArn_,
        serviceUrl = pServiceUrl_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        updatedAt = Core._Time Lens.# pUpdatedAt_,
        status = pStatus_,
        sourceConfiguration = pSourceConfiguration_,
        instanceConfiguration = pInstanceConfiguration_,
        autoScalingConfigurationSummary =
          pAutoScalingConfigurationSummary_
      }

-- | The encryption key that App Runner uses to encrypt the service logs and
-- the copy of the source repository that App Runner maintains for the
-- service. It can be either a customer-provided encryption key or an
-- Amazon Web Services managed CMK.
service_encryptionConfiguration :: Lens.Lens' Service (Prelude.Maybe EncryptionConfiguration)
service_encryptionConfiguration = Lens.lens (\Service' {encryptionConfiguration} -> encryptionConfiguration) (\s@Service' {} a -> s {encryptionConfiguration = a} :: Service)

-- | The settings for the health check that App Runner performs to monitor
-- the health of this service.
service_healthCheckConfiguration :: Lens.Lens' Service (Prelude.Maybe HealthCheckConfiguration)
service_healthCheckConfiguration = Lens.lens (\Service' {healthCheckConfiguration} -> healthCheckConfiguration) (\s@Service' {} a -> s {healthCheckConfiguration = a} :: Service)

-- | The time when the App Runner service was deleted. It\'s in the Unix time
-- stamp format.
service_deletedAt :: Lens.Lens' Service (Prelude.Maybe Prelude.UTCTime)
service_deletedAt = Lens.lens (\Service' {deletedAt} -> deletedAt) (\s@Service' {} a -> s {deletedAt = a} :: Service) Prelude.. Lens.mapping Core._Time

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

-- | A subdomain URL that App Runner generated for this service. You can use
-- this URL to access your service web application.
service_serviceUrl :: Lens.Lens' Service Prelude.Text
service_serviceUrl = Lens.lens (\Service' {serviceUrl} -> serviceUrl) (\s@Service' {} a -> s {serviceUrl = a} :: Service)

-- | The time when the App Runner service was created. It\'s in the Unix time
-- stamp format.
service_createdAt :: Lens.Lens' Service Prelude.UTCTime
service_createdAt = Lens.lens (\Service' {createdAt} -> createdAt) (\s@Service' {} a -> s {createdAt = a} :: Service) Prelude.. Core._Time

-- | The time when the App Runner service was last updated at. It\'s in the
-- Unix time stamp format.
service_updatedAt :: Lens.Lens' Service Prelude.UTCTime
service_updatedAt = Lens.lens (\Service' {updatedAt} -> updatedAt) (\s@Service' {} a -> s {updatedAt = a} :: Service) Prelude.. Core._Time

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

instance Core.FromJSON Service where
  parseJSON =
    Core.withObject
      "Service"
      ( \x ->
          Service'
            Prelude.<$> (x Core..:? "EncryptionConfiguration")
            Prelude.<*> (x Core..:? "HealthCheckConfiguration")
            Prelude.<*> (x Core..:? "DeletedAt")
            Prelude.<*> (x Core..: "ServiceName")
            Prelude.<*> (x Core..: "ServiceId")
            Prelude.<*> (x Core..: "ServiceArn")
            Prelude.<*> (x Core..: "ServiceUrl")
            Prelude.<*> (x Core..: "CreatedAt")
            Prelude.<*> (x Core..: "UpdatedAt")
            Prelude.<*> (x Core..: "Status")
            Prelude.<*> (x Core..: "SourceConfiguration")
            Prelude.<*> (x Core..: "InstanceConfiguration")
            Prelude.<*> (x Core..: "AutoScalingConfigurationSummary")
      )

instance Prelude.Hashable Service

instance Prelude.NFData Service
