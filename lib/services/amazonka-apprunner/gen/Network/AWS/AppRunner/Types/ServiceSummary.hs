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
-- Module      : Network.AWS.AppRunner.Types.ServiceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppRunner.Types.ServiceSummary where

import Network.AWS.AppRunner.Types.ServiceStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides summary information for an App Runner service.
--
-- This type contains limited information about a service. It doesn\'t
-- include configuration details. It\'s returned by the
-- <https://docs.aws.amazon.com/apprunner/latest/api/API_ListServices.html ListServices>
-- action. Complete service information is returned by the
-- <https://docs.aws.amazon.com/apprunner/latest/api/API_CreateService.html CreateService>,
-- <https://docs.aws.amazon.com/apprunner/latest/api/API_DescribeService.html DescribeService>,
-- and
-- <https://docs.aws.amazon.com/apprunner/latest/api/API_DeleteService.html DeleteService>
-- actions using the
-- <https://docs.aws.amazon.com/apprunner/latest/api/API_Service.html Service>
-- type.
--
-- /See:/ 'newServiceSummary' smart constructor.
data ServiceSummary = ServiceSummary'
  { -- | The current state of the App Runner service. These particular values
    -- mean the following.
    --
    -- -   @CREATE_FAILED@ – The service failed to create. Read the failure
    --     events and logs, change any parameters that need to be fixed, and
    --     retry the call to create the service.
    --
    --     The failed service isn\'t usable, and still counts towards your
    --     service quota. When you\'re done analyzing the failure, delete the
    --     service.
    --
    -- -   @DELETE_FAILED@ – The service failed to delete and can\'t be
    --     successfully recovered. Retry the service deletion call to ensure
    --     that all related resources are removed.
    status :: Prelude.Maybe ServiceStatus,
    -- | The time when the App Runner service was created. It\'s in the Unix time
    -- stamp format.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | A subdomain URL that App Runner generated for this service. You can use
    -- this URL to access your service web application.
    serviceUrl :: Prelude.Maybe Prelude.Text,
    -- | The customer-provided service name.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The time when the App Runner service was last updated. It\'s in theUnix
    -- time stamp format.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of this service.
    serviceArn :: Prelude.Maybe Prelude.Text,
    -- | An ID that App Runner generated for this service. It\'s unique within
    -- the Amazon Web Services Region.
    serviceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'serviceSummary_status' - The current state of the App Runner service. These particular values
-- mean the following.
--
-- -   @CREATE_FAILED@ – The service failed to create. Read the failure
--     events and logs, change any parameters that need to be fixed, and
--     retry the call to create the service.
--
--     The failed service isn\'t usable, and still counts towards your
--     service quota. When you\'re done analyzing the failure, delete the
--     service.
--
-- -   @DELETE_FAILED@ – The service failed to delete and can\'t be
--     successfully recovered. Retry the service deletion call to ensure
--     that all related resources are removed.
--
-- 'createdAt', 'serviceSummary_createdAt' - The time when the App Runner service was created. It\'s in the Unix time
-- stamp format.
--
-- 'serviceUrl', 'serviceSummary_serviceUrl' - A subdomain URL that App Runner generated for this service. You can use
-- this URL to access your service web application.
--
-- 'serviceName', 'serviceSummary_serviceName' - The customer-provided service name.
--
-- 'updatedAt', 'serviceSummary_updatedAt' - The time when the App Runner service was last updated. It\'s in theUnix
-- time stamp format.
--
-- 'serviceArn', 'serviceSummary_serviceArn' - The Amazon Resource Name (ARN) of this service.
--
-- 'serviceId', 'serviceSummary_serviceId' - An ID that App Runner generated for this service. It\'s unique within
-- the Amazon Web Services Region.
newServiceSummary ::
  ServiceSummary
newServiceSummary =
  ServiceSummary'
    { status = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      serviceUrl = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      serviceArn = Prelude.Nothing,
      serviceId = Prelude.Nothing
    }

-- | The current state of the App Runner service. These particular values
-- mean the following.
--
-- -   @CREATE_FAILED@ – The service failed to create. Read the failure
--     events and logs, change any parameters that need to be fixed, and
--     retry the call to create the service.
--
--     The failed service isn\'t usable, and still counts towards your
--     service quota. When you\'re done analyzing the failure, delete the
--     service.
--
-- -   @DELETE_FAILED@ – The service failed to delete and can\'t be
--     successfully recovered. Retry the service deletion call to ensure
--     that all related resources are removed.
serviceSummary_status :: Lens.Lens' ServiceSummary (Prelude.Maybe ServiceStatus)
serviceSummary_status = Lens.lens (\ServiceSummary' {status} -> status) (\s@ServiceSummary' {} a -> s {status = a} :: ServiceSummary)

-- | The time when the App Runner service was created. It\'s in the Unix time
-- stamp format.
serviceSummary_createdAt :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.UTCTime)
serviceSummary_createdAt = Lens.lens (\ServiceSummary' {createdAt} -> createdAt) (\s@ServiceSummary' {} a -> s {createdAt = a} :: ServiceSummary) Prelude.. Lens.mapping Core._Time

-- | A subdomain URL that App Runner generated for this service. You can use
-- this URL to access your service web application.
serviceSummary_serviceUrl :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_serviceUrl = Lens.lens (\ServiceSummary' {serviceUrl} -> serviceUrl) (\s@ServiceSummary' {} a -> s {serviceUrl = a} :: ServiceSummary)

-- | The customer-provided service name.
serviceSummary_serviceName :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_serviceName = Lens.lens (\ServiceSummary' {serviceName} -> serviceName) (\s@ServiceSummary' {} a -> s {serviceName = a} :: ServiceSummary)

-- | The time when the App Runner service was last updated. It\'s in theUnix
-- time stamp format.
serviceSummary_updatedAt :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.UTCTime)
serviceSummary_updatedAt = Lens.lens (\ServiceSummary' {updatedAt} -> updatedAt) (\s@ServiceSummary' {} a -> s {updatedAt = a} :: ServiceSummary) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of this service.
serviceSummary_serviceArn :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_serviceArn = Lens.lens (\ServiceSummary' {serviceArn} -> serviceArn) (\s@ServiceSummary' {} a -> s {serviceArn = a} :: ServiceSummary)

-- | An ID that App Runner generated for this service. It\'s unique within
-- the Amazon Web Services Region.
serviceSummary_serviceId :: Lens.Lens' ServiceSummary (Prelude.Maybe Prelude.Text)
serviceSummary_serviceId = Lens.lens (\ServiceSummary' {serviceId} -> serviceId) (\s@ServiceSummary' {} a -> s {serviceId = a} :: ServiceSummary)

instance Core.FromJSON ServiceSummary where
  parseJSON =
    Core.withObject
      "ServiceSummary"
      ( \x ->
          ServiceSummary'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "ServiceUrl")
            Prelude.<*> (x Core..:? "ServiceName")
            Prelude.<*> (x Core..:? "UpdatedAt")
            Prelude.<*> (x Core..:? "ServiceArn")
            Prelude.<*> (x Core..:? "ServiceId")
      )

instance Prelude.Hashable ServiceSummary

instance Prelude.NFData ServiceSummary
