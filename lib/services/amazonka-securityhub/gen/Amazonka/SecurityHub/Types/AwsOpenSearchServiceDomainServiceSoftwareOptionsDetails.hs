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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the state of the domain relative to the
-- latest service software.
--
-- /See:/ 'newAwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' smart constructor.
data AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails = AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails'
  { -- | The epoch time when the deployment window closes for required updates.
    -- After this time, OpenSearch Service schedules the software upgrade
    -- automatically.
    automatedUpdateDate :: Prelude.Maybe Prelude.Text,
    -- | Whether a request to update the domain can be canceled.
    cancellable :: Prelude.Maybe Prelude.Bool,
    -- | The version of the service software that is currently installed on the
    -- domain.
    currentVersion :: Prelude.Maybe Prelude.Text,
    -- | A more detailed description of the service software status.
    description :: Prelude.Maybe Prelude.Text,
    -- | The most recent version of the service software.
    newVersion' :: Prelude.Maybe Prelude.Text,
    -- | Whether the service software update is optional.
    optionalDeployment :: Prelude.Maybe Prelude.Bool,
    -- | Whether a service software update is available for the domain.
    updateAvailable :: Prelude.Maybe Prelude.Bool,
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
    updateStatus :: Prelude.Maybe Prelude.Text
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
-- 'automatedUpdateDate', 'awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_automatedUpdateDate' - The epoch time when the deployment window closes for required updates.
-- After this time, OpenSearch Service schedules the software upgrade
-- automatically.
--
-- 'cancellable', 'awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_cancellable' - Whether a request to update the domain can be canceled.
--
-- 'currentVersion', 'awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_currentVersion' - The version of the service software that is currently installed on the
-- domain.
--
-- 'description', 'awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_description' - A more detailed description of the service software status.
--
-- 'newVersion'', 'awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_newVersion' - The most recent version of the service software.
--
-- 'optionalDeployment', 'awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_optionalDeployment' - Whether the service software update is optional.
--
-- 'updateAvailable', 'awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_updateAvailable' - Whether a service software update is available for the domain.
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
newAwsOpenSearchServiceDomainServiceSoftwareOptionsDetails ::
  AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails
newAwsOpenSearchServiceDomainServiceSoftwareOptionsDetails =
  AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails'
    { automatedUpdateDate =
        Prelude.Nothing,
      cancellable =
        Prelude.Nothing,
      currentVersion =
        Prelude.Nothing,
      description =
        Prelude.Nothing,
      newVersion' =
        Prelude.Nothing,
      optionalDeployment =
        Prelude.Nothing,
      updateAvailable =
        Prelude.Nothing,
      updateStatus =
        Prelude.Nothing
    }

-- | The epoch time when the deployment window closes for required updates.
-- After this time, OpenSearch Service schedules the software upgrade
-- automatically.
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_automatedUpdateDate :: Lens.Lens' AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_automatedUpdateDate = Lens.lens (\AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {automatedUpdateDate} -> automatedUpdateDate) (\s@AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {} a -> s {automatedUpdateDate = a} :: AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails)

-- | Whether a request to update the domain can be canceled.
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_cancellable :: Lens.Lens' AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails (Prelude.Maybe Prelude.Bool)
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_cancellable = Lens.lens (\AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {cancellable} -> cancellable) (\s@AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {} a -> s {cancellable = a} :: AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails)

-- | The version of the service software that is currently installed on the
-- domain.
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_currentVersion :: Lens.Lens' AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_currentVersion = Lens.lens (\AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {currentVersion} -> currentVersion) (\s@AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {} a -> s {currentVersion = a} :: AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails)

-- | A more detailed description of the service software status.
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_description :: Lens.Lens' AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_description = Lens.lens (\AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {description} -> description) (\s@AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {} a -> s {description = a} :: AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails)

-- | The most recent version of the service software.
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_newVersion :: Lens.Lens' AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails (Prelude.Maybe Prelude.Text)
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_newVersion = Lens.lens (\AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {newVersion'} -> newVersion') (\s@AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {} a -> s {newVersion' = a} :: AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails)

-- | Whether the service software update is optional.
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_optionalDeployment :: Lens.Lens' AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails (Prelude.Maybe Prelude.Bool)
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_optionalDeployment = Lens.lens (\AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {optionalDeployment} -> optionalDeployment) (\s@AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {} a -> s {optionalDeployment = a} :: AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails)

-- | Whether a service software update is available for the domain.
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_updateAvailable :: Lens.Lens' AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails (Prelude.Maybe Prelude.Bool)
awsOpenSearchServiceDomainServiceSoftwareOptionsDetails_updateAvailable = Lens.lens (\AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {updateAvailable} -> updateAvailable) (\s@AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {} a -> s {updateAvailable = a} :: AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails)

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

instance
  Data.FromJSON
    AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails
  where
  parseJSON =
    Data.withObject
      "AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails"
      ( \x ->
          AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails'
            Prelude.<$> (x Data..:? "AutomatedUpdateDate")
            Prelude.<*> (x Data..:? "Cancellable")
            Prelude.<*> (x Data..:? "CurrentVersion")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "NewVersion")
            Prelude.<*> (x Data..:? "OptionalDeployment")
            Prelude.<*> (x Data..:? "UpdateAvailable")
            Prelude.<*> (x Data..:? "UpdateStatus")
      )

instance
  Prelude.Hashable
    AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails
  where
  hashWithSalt
    _salt
    AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {..} =
      _salt
        `Prelude.hashWithSalt` automatedUpdateDate
        `Prelude.hashWithSalt` cancellable
        `Prelude.hashWithSalt` currentVersion
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` newVersion'
        `Prelude.hashWithSalt` optionalDeployment
        `Prelude.hashWithSalt` updateAvailable
        `Prelude.hashWithSalt` updateStatus

instance
  Prelude.NFData
    AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails
  where
  rnf
    AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {..} =
      Prelude.rnf automatedUpdateDate `Prelude.seq`
        Prelude.rnf cancellable `Prelude.seq`
          Prelude.rnf currentVersion `Prelude.seq`
            Prelude.rnf description `Prelude.seq`
              Prelude.rnf newVersion' `Prelude.seq`
                Prelude.rnf optionalDeployment `Prelude.seq`
                  Prelude.rnf updateAvailable `Prelude.seq`
                    Prelude.rnf updateStatus

instance
  Data.ToJSON
    AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails
  where
  toJSON
    AwsOpenSearchServiceDomainServiceSoftwareOptionsDetails' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("AutomatedUpdateDate" Data..=)
                Prelude.<$> automatedUpdateDate,
              ("Cancellable" Data..=) Prelude.<$> cancellable,
              ("CurrentVersion" Data..=)
                Prelude.<$> currentVersion,
              ("Description" Data..=) Prelude.<$> description,
              ("NewVersion" Data..=) Prelude.<$> newVersion',
              ("OptionalDeployment" Data..=)
                Prelude.<$> optionalDeployment,
              ("UpdateAvailable" Data..=)
                Prelude.<$> updateAvailable,
              ("UpdateStatus" Data..=) Prelude.<$> updateStatus
            ]
        )
