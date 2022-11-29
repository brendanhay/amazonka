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
-- Module      : Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the state of the domain relative to the
-- latest service software.
--
-- /See:/ 'newAwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' smart constructor.
data AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails = AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails'
  { -- | Whether the service software update is optional.
    optionalDeployment :: Prelude.Maybe Prelude.Bool,
    -- | The most recent version of the service software.
    newVersion' :: Prelude.Maybe Prelude.Text,
    -- | Whether a service software update is available for the domain.
    updateAvailable :: Prelude.Maybe Prelude.Bool,
    -- | Whether a request to update the domain can be canceled.
    cancellable :: Prelude.Maybe Prelude.Bool,
    -- | The status of the service software update. Valid values are as follows:
    --
    -- -   @COMPLETED@
    --
    -- -   @ELIGIBLE@
    --
    -- -   @IN_PROGRESS@
    --
    -- -   @NOT_ELIGIBLE@
    --
    -- -   @PENDING_UPDATE@
    updateStatus :: Prelude.Maybe Prelude.Text,
    -- | The epoch time when the deployment window closes for required updates.
    -- After this time, OpenSearch Service schedules the software upgrade
    -- automatically.
    automatedUpdateDate :: Prelude.Maybe Prelude.Text,
    -- | A more detailed description of the service software status.
    description :: Prelude.Maybe Prelude.Text,
    -- | The version of the service software that is currently installed on the
    -- domain.
    currentVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optionalDeployment', 'awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_optionalDeployment' - Whether the service software update is optional.
--
-- 'newVersion'', 'awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_newVersion' - The most recent version of the service software.
--
-- 'updateAvailable', 'awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_updateAvailable' - Whether a service software update is available for the domain.
--
-- 'cancellable', 'awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_cancellable' - Whether a request to update the domain can be canceled.
--
-- 'updateStatus', 'awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_updateStatus' - The status of the service software update. Valid values are as follows:
--
-- -   @COMPLETED@
--
-- -   @ELIGIBLE@
--
-- -   @IN_PROGRESS@
--
-- -   @NOT_ELIGIBLE@
--
-- -   @PENDING_UPDATE@
--
-- 'automatedUpdateDate', 'awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_automatedUpdateDate' - The epoch time when the deployment window closes for required updates.
-- After this time, OpenSearch Service schedules the software upgrade
-- automatically.
--
-- 'description', 'awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_description' - A more detailed description of the service software status.
--
-- 'currentVersion', 'awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_currentVersion' - The version of the service software that is currently installed on the
-- domain.
newAwsOpenSearchServiceDomainServiceSoftwareOptionsDetails ::
  AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails
newAwsOpenSearchServiceDomainServiceSoftwareOptionsDetails =
  AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails'
    { optionalDeployment =
        Prelude.Nothing,
      newVersion' =
        Prelude.Nothing,
      updateAvailable =
        Prelude.Nothing,
      cancellable =
        Prelude.Nothing,
      updateStatus =
        Prelude.Nothing,
      automatedUpdateDate =
        Prelude.Nothing,
      description =
        Prelude.Nothing,
      currentVersion =
        Prelude.Nothing
    }

-- | Whether the service software update is optional.
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_optionalDeployment :: Lens.Lens' AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails (Prelude.Maybe Prelude.Bool)
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_optionalDeployment = Lens.lens (\AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {optionalDeployment} -> optionalDeployment) (\s@AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {} a -> s {optionalDeployment = a} :: AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails)

-- | The most recent version of the service software.
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_newVersion :: Lens.Lens' AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_newVersion = Lens.lens (\AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {newVersion'} -> newVersion') (\s@AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {} a -> s {newVersion' = a} :: AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails)

-- | Whether a service software update is available for the domain.
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_updateAvailable :: Lens.Lens' AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails (Prelude.Maybe Prelude.Bool)
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_updateAvailable = Lens.lens (\AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {updateAvailable} -> updateAvailable) (\s@AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {} a -> s {updateAvailable = a} :: AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails)

-- | Whether a request to update the domain can be canceled.
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_cancellable :: Lens.Lens' AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails (Prelude.Maybe Prelude.Bool)
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_cancellable = Lens.lens (\AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {cancellable} -> cancellable) (\s@AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {} a -> s {cancellable = a} :: AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails)

-- | The status of the service software update. Valid values are as follows:
--
-- -   @COMPLETED@
--
-- -   @ELIGIBLE@
--
-- -   @IN_PROGRESS@
--
-- -   @NOT_ELIGIBLE@
--
-- -   @PENDING_UPDATE@
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_updateStatus :: Lens.Lens' AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_updateStatus = Lens.lens (\AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {updateStatus} -> updateStatus) (\s@AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {} a -> s {updateStatus = a} :: AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails)

-- | The epoch time when the deployment window closes for required updates.
-- After this time, OpenSearch Service schedules the software upgrade
-- automatically.
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_automatedUpdateDate :: Lens.Lens' AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_automatedUpdateDate = Lens.lens (\AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {automatedUpdateDate} -> automatedUpdateDate) (\s@AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {} a -> s {automatedUpdateDate = a} :: AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails)

-- | A more detailed description of the service software status.
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_description :: Lens.Lens' AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_description = Lens.lens (\AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {description} -> description) (\s@AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {} a -> s {description = a} :: AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails)

-- | The version of the service software that is currently installed on the
-- domain.
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_currentVersion :: Lens.Lens' AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_currentVersion = Lens.lens (\AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {currentVersion} -> currentVersion) (\s@AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {} a -> s {currentVersion = a} :: AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails)

instance
  Core.FromJSON
    AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails
  where
  parseJSON =
    Core.withObject
      "AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails"
      ( \x ->
          AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails'
            Prelude.<$> (x Core..:? "OptionalDeployment")
              Prelude.<*> (x Core..:? "NewVersion")
              Prelude.<*> (x Core..:? "UpdateAvailable")
              Prelude.<*> (x Core..:? "Cancellable")
              Prelude.<*> (x Core..:? "UpdateStatus")
              Prelude.<*> (x Core..:? "AutomatedUpdateDate")
              Prelude.<*> (x Core..:? "Description")
              Prelude.<*> (x Core..:? "CurrentVersion")
      )

instance
  Prelude.Hashable
    AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails
  where
  hashWithSalt
    _salt
    AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {..} =
      _salt `Prelude.hashWithSalt` optionalDeployment
        `Prelude.hashWithSalt` newVersion'
        `Prelude.hashWithSalt` updateAvailable
        `Prelude.hashWithSalt` cancellable
        `Prelude.hashWithSalt` updateStatus
        `Prelude.hashWithSalt` automatedUpdateDate
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` currentVersion

instance
  Prelude.NFData
    AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails
  where
  rnf
    AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {..} =
      Prelude.rnf optionalDeployment
        `Prelude.seq` Prelude.rnf newVersion'
        `Prelude.seq` Prelude.rnf updateAvailable
        `Prelude.seq` Prelude.rnf cancellable
        `Prelude.seq` Prelude.rnf updateStatus
        `Prelude.seq` Prelude.rnf automatedUpdateDate
        `Prelude.seq` Prelude.rnf description
        `Prelude.seq` Prelude.rnf currentVersion

instance
  Core.ToJSON
    AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails
  where
  toJSON
    AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("OptionalDeployment" Core..=)
                Prelude.<$> optionalDeployment,
              ("NewVersion" Core..=) Prelude.<$> newVersion',
              ("UpdateAvailable" Core..=)
                Prelude.<$> updateAvailable,
              ("Cancellable" Core..=) Prelude.<$> cancellable,
              ("UpdateStatus" Core..=) Prelude.<$> updateStatus,
              ("AutomatedUpdateDate" Core..=)
                Prelude.<$> automatedUpdateDate,
              ("Description" Core..=) Prelude.<$> description,
              ("CurrentVersion" Core..=)
                Prelude.<$> currentVersion
            ]
        )
