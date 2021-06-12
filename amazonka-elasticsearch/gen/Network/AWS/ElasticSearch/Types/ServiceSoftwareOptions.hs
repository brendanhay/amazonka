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
-- Module      : Network.AWS.ElasticSearch.Types.ServiceSoftwareOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ServiceSoftwareOptions where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types.DeploymentStatus
import qualified Network.AWS.Lens as Lens

-- | The current options of an Elasticsearch domain service software options.
--
-- /See:/ 'newServiceSoftwareOptions' smart constructor.
data ServiceSoftwareOptions = ServiceSoftwareOptions'
  { -- | The new service software version if one is available.
    newVersion' :: Core.Maybe Core.Text,
    -- | The current service software version that is present on the domain.
    currentVersion :: Core.Maybe Core.Text,
    -- | @True@ if you are able to update you service software version. @False@
    -- if you are not able to update your service software version.
    updateAvailable :: Core.Maybe Core.Bool,
    -- | @True@ if you are able to cancel your service software version update.
    -- @False@ if you are not able to cancel your service software version.
    cancellable :: Core.Maybe Core.Bool,
    -- | The status of your service software update. This field can take the
    -- following values: @ELIGIBLE@, @PENDING_UPDATE@, @IN_PROGRESS@,
    -- @COMPLETED@, and @NOT_ELIGIBLE@.
    updateStatus :: Core.Maybe DeploymentStatus,
    -- | @True@ if a service software is never automatically updated. @False@ if
    -- a service software is automatically updated after @AutomatedUpdateDate@.
    optionalDeployment :: Core.Maybe Core.Bool,
    -- | The description of the @UpdateStatus@.
    description :: Core.Maybe Core.Text,
    -- | Timestamp, in Epoch time, until which you can manually request a service
    -- software update. After this date, we automatically update your service
    -- software.
    automatedUpdateDate :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ServiceSoftwareOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'newVersion'', 'serviceSoftwareOptions_newVersion' - The new service software version if one is available.
--
-- 'currentVersion', 'serviceSoftwareOptions_currentVersion' - The current service software version that is present on the domain.
--
-- 'updateAvailable', 'serviceSoftwareOptions_updateAvailable' - @True@ if you are able to update you service software version. @False@
-- if you are not able to update your service software version.
--
-- 'cancellable', 'serviceSoftwareOptions_cancellable' - @True@ if you are able to cancel your service software version update.
-- @False@ if you are not able to cancel your service software version.
--
-- 'updateStatus', 'serviceSoftwareOptions_updateStatus' - The status of your service software update. This field can take the
-- following values: @ELIGIBLE@, @PENDING_UPDATE@, @IN_PROGRESS@,
-- @COMPLETED@, and @NOT_ELIGIBLE@.
--
-- 'optionalDeployment', 'serviceSoftwareOptions_optionalDeployment' - @True@ if a service software is never automatically updated. @False@ if
-- a service software is automatically updated after @AutomatedUpdateDate@.
--
-- 'description', 'serviceSoftwareOptions_description' - The description of the @UpdateStatus@.
--
-- 'automatedUpdateDate', 'serviceSoftwareOptions_automatedUpdateDate' - Timestamp, in Epoch time, until which you can manually request a service
-- software update. After this date, we automatically update your service
-- software.
newServiceSoftwareOptions ::
  ServiceSoftwareOptions
newServiceSoftwareOptions =
  ServiceSoftwareOptions'
    { newVersion' = Core.Nothing,
      currentVersion = Core.Nothing,
      updateAvailable = Core.Nothing,
      cancellable = Core.Nothing,
      updateStatus = Core.Nothing,
      optionalDeployment = Core.Nothing,
      description = Core.Nothing,
      automatedUpdateDate = Core.Nothing
    }

-- | The new service software version if one is available.
serviceSoftwareOptions_newVersion :: Lens.Lens' ServiceSoftwareOptions (Core.Maybe Core.Text)
serviceSoftwareOptions_newVersion = Lens.lens (\ServiceSoftwareOptions' {newVersion'} -> newVersion') (\s@ServiceSoftwareOptions' {} a -> s {newVersion' = a} :: ServiceSoftwareOptions)

-- | The current service software version that is present on the domain.
serviceSoftwareOptions_currentVersion :: Lens.Lens' ServiceSoftwareOptions (Core.Maybe Core.Text)
serviceSoftwareOptions_currentVersion = Lens.lens (\ServiceSoftwareOptions' {currentVersion} -> currentVersion) (\s@ServiceSoftwareOptions' {} a -> s {currentVersion = a} :: ServiceSoftwareOptions)

-- | @True@ if you are able to update you service software version. @False@
-- if you are not able to update your service software version.
serviceSoftwareOptions_updateAvailable :: Lens.Lens' ServiceSoftwareOptions (Core.Maybe Core.Bool)
serviceSoftwareOptions_updateAvailable = Lens.lens (\ServiceSoftwareOptions' {updateAvailable} -> updateAvailable) (\s@ServiceSoftwareOptions' {} a -> s {updateAvailable = a} :: ServiceSoftwareOptions)

-- | @True@ if you are able to cancel your service software version update.
-- @False@ if you are not able to cancel your service software version.
serviceSoftwareOptions_cancellable :: Lens.Lens' ServiceSoftwareOptions (Core.Maybe Core.Bool)
serviceSoftwareOptions_cancellable = Lens.lens (\ServiceSoftwareOptions' {cancellable} -> cancellable) (\s@ServiceSoftwareOptions' {} a -> s {cancellable = a} :: ServiceSoftwareOptions)

-- | The status of your service software update. This field can take the
-- following values: @ELIGIBLE@, @PENDING_UPDATE@, @IN_PROGRESS@,
-- @COMPLETED@, and @NOT_ELIGIBLE@.
serviceSoftwareOptions_updateStatus :: Lens.Lens' ServiceSoftwareOptions (Core.Maybe DeploymentStatus)
serviceSoftwareOptions_updateStatus = Lens.lens (\ServiceSoftwareOptions' {updateStatus} -> updateStatus) (\s@ServiceSoftwareOptions' {} a -> s {updateStatus = a} :: ServiceSoftwareOptions)

-- | @True@ if a service software is never automatically updated. @False@ if
-- a service software is automatically updated after @AutomatedUpdateDate@.
serviceSoftwareOptions_optionalDeployment :: Lens.Lens' ServiceSoftwareOptions (Core.Maybe Core.Bool)
serviceSoftwareOptions_optionalDeployment = Lens.lens (\ServiceSoftwareOptions' {optionalDeployment} -> optionalDeployment) (\s@ServiceSoftwareOptions' {} a -> s {optionalDeployment = a} :: ServiceSoftwareOptions)

-- | The description of the @UpdateStatus@.
serviceSoftwareOptions_description :: Lens.Lens' ServiceSoftwareOptions (Core.Maybe Core.Text)
serviceSoftwareOptions_description = Lens.lens (\ServiceSoftwareOptions' {description} -> description) (\s@ServiceSoftwareOptions' {} a -> s {description = a} :: ServiceSoftwareOptions)

-- | Timestamp, in Epoch time, until which you can manually request a service
-- software update. After this date, we automatically update your service
-- software.
serviceSoftwareOptions_automatedUpdateDate :: Lens.Lens' ServiceSoftwareOptions (Core.Maybe Core.UTCTime)
serviceSoftwareOptions_automatedUpdateDate = Lens.lens (\ServiceSoftwareOptions' {automatedUpdateDate} -> automatedUpdateDate) (\s@ServiceSoftwareOptions' {} a -> s {automatedUpdateDate = a} :: ServiceSoftwareOptions) Core.. Lens.mapping Core._Time

instance Core.FromJSON ServiceSoftwareOptions where
  parseJSON =
    Core.withObject
      "ServiceSoftwareOptions"
      ( \x ->
          ServiceSoftwareOptions'
            Core.<$> (x Core..:? "NewVersion")
            Core.<*> (x Core..:? "CurrentVersion")
            Core.<*> (x Core..:? "UpdateAvailable")
            Core.<*> (x Core..:? "Cancellable")
            Core.<*> (x Core..:? "UpdateStatus")
            Core.<*> (x Core..:? "OptionalDeployment")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "AutomatedUpdateDate")
      )

instance Core.Hashable ServiceSoftwareOptions

instance Core.NFData ServiceSoftwareOptions
