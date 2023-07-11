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
-- Module      : Amazonka.ElasticSearch.Types.ServiceSoftwareOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.ServiceSoftwareOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.DeploymentStatus
import qualified Amazonka.Prelude as Prelude

-- | The current options of an Elasticsearch domain service software options.
--
-- /See:/ 'newServiceSoftwareOptions' smart constructor.
data ServiceSoftwareOptions = ServiceSoftwareOptions'
  { -- | Timestamp, in Epoch time, until which you can manually request a service
    -- software update. After this date, we automatically update your service
    -- software.
    automatedUpdateDate :: Prelude.Maybe Data.POSIX,
    -- | @True@ if you are able to cancel your service software version update.
    -- @False@ if you are not able to cancel your service software version.
    cancellable :: Prelude.Maybe Prelude.Bool,
    -- | The current service software version that is present on the domain.
    currentVersion :: Prelude.Maybe Prelude.Text,
    -- | The description of the @UpdateStatus@.
    description :: Prelude.Maybe Prelude.Text,
    -- | The new service software version if one is available.
    newVersion' :: Prelude.Maybe Prelude.Text,
    -- | @True@ if a service software is never automatically updated. @False@ if
    -- a service software is automatically updated after @AutomatedUpdateDate@.
    optionalDeployment :: Prelude.Maybe Prelude.Bool,
    -- | @True@ if you are able to update you service software version. @False@
    -- if you are not able to update your service software version.
    updateAvailable :: Prelude.Maybe Prelude.Bool,
    -- | The status of your service software update. This field can take the
    -- following values: @ELIGIBLE@, @PENDING_UPDATE@, @IN_PROGRESS@,
    -- @COMPLETED@, and @NOT_ELIGIBLE@.
    updateStatus :: Prelude.Maybe DeploymentStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServiceSoftwareOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automatedUpdateDate', 'serviceSoftwareOptions_automatedUpdateDate' - Timestamp, in Epoch time, until which you can manually request a service
-- software update. After this date, we automatically update your service
-- software.
--
-- 'cancellable', 'serviceSoftwareOptions_cancellable' - @True@ if you are able to cancel your service software version update.
-- @False@ if you are not able to cancel your service software version.
--
-- 'currentVersion', 'serviceSoftwareOptions_currentVersion' - The current service software version that is present on the domain.
--
-- 'description', 'serviceSoftwareOptions_description' - The description of the @UpdateStatus@.
--
-- 'newVersion'', 'serviceSoftwareOptions_newVersion' - The new service software version if one is available.
--
-- 'optionalDeployment', 'serviceSoftwareOptions_optionalDeployment' - @True@ if a service software is never automatically updated. @False@ if
-- a service software is automatically updated after @AutomatedUpdateDate@.
--
-- 'updateAvailable', 'serviceSoftwareOptions_updateAvailable' - @True@ if you are able to update you service software version. @False@
-- if you are not able to update your service software version.
--
-- 'updateStatus', 'serviceSoftwareOptions_updateStatus' - The status of your service software update. This field can take the
-- following values: @ELIGIBLE@, @PENDING_UPDATE@, @IN_PROGRESS@,
-- @COMPLETED@, and @NOT_ELIGIBLE@.
newServiceSoftwareOptions ::
  ServiceSoftwareOptions
newServiceSoftwareOptions =
  ServiceSoftwareOptions'
    { automatedUpdateDate =
        Prelude.Nothing,
      cancellable = Prelude.Nothing,
      currentVersion = Prelude.Nothing,
      description = Prelude.Nothing,
      newVersion' = Prelude.Nothing,
      optionalDeployment = Prelude.Nothing,
      updateAvailable = Prelude.Nothing,
      updateStatus = Prelude.Nothing
    }

-- | Timestamp, in Epoch time, until which you can manually request a service
-- software update. After this date, we automatically update your service
-- software.
serviceSoftwareOptions_automatedUpdateDate :: Lens.Lens' ServiceSoftwareOptions (Prelude.Maybe Prelude.UTCTime)
serviceSoftwareOptions_automatedUpdateDate = Lens.lens (\ServiceSoftwareOptions' {automatedUpdateDate} -> automatedUpdateDate) (\s@ServiceSoftwareOptions' {} a -> s {automatedUpdateDate = a} :: ServiceSoftwareOptions) Prelude.. Lens.mapping Data._Time

-- | @True@ if you are able to cancel your service software version update.
-- @False@ if you are not able to cancel your service software version.
serviceSoftwareOptions_cancellable :: Lens.Lens' ServiceSoftwareOptions (Prelude.Maybe Prelude.Bool)
serviceSoftwareOptions_cancellable = Lens.lens (\ServiceSoftwareOptions' {cancellable} -> cancellable) (\s@ServiceSoftwareOptions' {} a -> s {cancellable = a} :: ServiceSoftwareOptions)

-- | The current service software version that is present on the domain.
serviceSoftwareOptions_currentVersion :: Lens.Lens' ServiceSoftwareOptions (Prelude.Maybe Prelude.Text)
serviceSoftwareOptions_currentVersion = Lens.lens (\ServiceSoftwareOptions' {currentVersion} -> currentVersion) (\s@ServiceSoftwareOptions' {} a -> s {currentVersion = a} :: ServiceSoftwareOptions)

-- | The description of the @UpdateStatus@.
serviceSoftwareOptions_description :: Lens.Lens' ServiceSoftwareOptions (Prelude.Maybe Prelude.Text)
serviceSoftwareOptions_description = Lens.lens (\ServiceSoftwareOptions' {description} -> description) (\s@ServiceSoftwareOptions' {} a -> s {description = a} :: ServiceSoftwareOptions)

-- | The new service software version if one is available.
serviceSoftwareOptions_newVersion :: Lens.Lens' ServiceSoftwareOptions (Prelude.Maybe Prelude.Text)
serviceSoftwareOptions_newVersion = Lens.lens (\ServiceSoftwareOptions' {newVersion'} -> newVersion') (\s@ServiceSoftwareOptions' {} a -> s {newVersion' = a} :: ServiceSoftwareOptions)

-- | @True@ if a service software is never automatically updated. @False@ if
-- a service software is automatically updated after @AutomatedUpdateDate@.
serviceSoftwareOptions_optionalDeployment :: Lens.Lens' ServiceSoftwareOptions (Prelude.Maybe Prelude.Bool)
serviceSoftwareOptions_optionalDeployment = Lens.lens (\ServiceSoftwareOptions' {optionalDeployment} -> optionalDeployment) (\s@ServiceSoftwareOptions' {} a -> s {optionalDeployment = a} :: ServiceSoftwareOptions)

-- | @True@ if you are able to update you service software version. @False@
-- if you are not able to update your service software version.
serviceSoftwareOptions_updateAvailable :: Lens.Lens' ServiceSoftwareOptions (Prelude.Maybe Prelude.Bool)
serviceSoftwareOptions_updateAvailable = Lens.lens (\ServiceSoftwareOptions' {updateAvailable} -> updateAvailable) (\s@ServiceSoftwareOptions' {} a -> s {updateAvailable = a} :: ServiceSoftwareOptions)

-- | The status of your service software update. This field can take the
-- following values: @ELIGIBLE@, @PENDING_UPDATE@, @IN_PROGRESS@,
-- @COMPLETED@, and @NOT_ELIGIBLE@.
serviceSoftwareOptions_updateStatus :: Lens.Lens' ServiceSoftwareOptions (Prelude.Maybe DeploymentStatus)
serviceSoftwareOptions_updateStatus = Lens.lens (\ServiceSoftwareOptions' {updateStatus} -> updateStatus) (\s@ServiceSoftwareOptions' {} a -> s {updateStatus = a} :: ServiceSoftwareOptions)

instance Data.FromJSON ServiceSoftwareOptions where
  parseJSON =
    Data.withObject
      "ServiceSoftwareOptions"
      ( \x ->
          ServiceSoftwareOptions'
            Prelude.<$> (x Data..:? "AutomatedUpdateDate")
            Prelude.<*> (x Data..:? "Cancellable")
            Prelude.<*> (x Data..:? "CurrentVersion")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "NewVersion")
            Prelude.<*> (x Data..:? "OptionalDeployment")
            Prelude.<*> (x Data..:? "UpdateAvailable")
            Prelude.<*> (x Data..:? "UpdateStatus")
      )

instance Prelude.Hashable ServiceSoftwareOptions where
  hashWithSalt _salt ServiceSoftwareOptions' {..} =
    _salt
      `Prelude.hashWithSalt` automatedUpdateDate
      `Prelude.hashWithSalt` cancellable
      `Prelude.hashWithSalt` currentVersion
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` newVersion'
      `Prelude.hashWithSalt` optionalDeployment
      `Prelude.hashWithSalt` updateAvailable
      `Prelude.hashWithSalt` updateStatus

instance Prelude.NFData ServiceSoftwareOptions where
  rnf ServiceSoftwareOptions' {..} =
    Prelude.rnf automatedUpdateDate
      `Prelude.seq` Prelude.rnf cancellable
      `Prelude.seq` Prelude.rnf currentVersion
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf newVersion'
      `Prelude.seq` Prelude.rnf optionalDeployment
      `Prelude.seq` Prelude.rnf updateAvailable
      `Prelude.seq` Prelude.rnf updateStatus
