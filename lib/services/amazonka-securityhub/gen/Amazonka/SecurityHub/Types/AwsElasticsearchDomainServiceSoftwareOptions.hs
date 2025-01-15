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
-- Module      : Amazonka.SecurityHub.Types.AwsElasticsearchDomainServiceSoftwareOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsElasticsearchDomainServiceSoftwareOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the state of the domain relative to the latest service
-- software.
--
-- /See:/ 'newAwsElasticsearchDomainServiceSoftwareOptions' smart constructor.
data AwsElasticsearchDomainServiceSoftwareOptions = AwsElasticsearchDomainServiceSoftwareOptions'
  { -- | The epoch time when the deployment window closes for required updates.
    -- After this time, Amazon OpenSearch Service schedules the software
    -- upgrade automatically.
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
-- Create a value of 'AwsElasticsearchDomainServiceSoftwareOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automatedUpdateDate', 'awsElasticsearchDomainServiceSoftwareOptions_automatedUpdateDate' - The epoch time when the deployment window closes for required updates.
-- After this time, Amazon OpenSearch Service schedules the software
-- upgrade automatically.
--
-- 'cancellable', 'awsElasticsearchDomainServiceSoftwareOptions_cancellable' - Whether a request to update the domain can be canceled.
--
-- 'currentVersion', 'awsElasticsearchDomainServiceSoftwareOptions_currentVersion' - The version of the service software that is currently installed on the
-- domain.
--
-- 'description', 'awsElasticsearchDomainServiceSoftwareOptions_description' - A more detailed description of the service software status.
--
-- 'newVersion'', 'awsElasticsearchDomainServiceSoftwareOptions_newVersion' - The most recent version of the service software.
--
-- 'updateAvailable', 'awsElasticsearchDomainServiceSoftwareOptions_updateAvailable' - Whether a service software update is available for the domain.
--
-- 'updateStatus', 'awsElasticsearchDomainServiceSoftwareOptions_updateStatus' - The status of the service software update. Valid values are as follows:
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
newAwsElasticsearchDomainServiceSoftwareOptions ::
  AwsElasticsearchDomainServiceSoftwareOptions
newAwsElasticsearchDomainServiceSoftwareOptions =
  AwsElasticsearchDomainServiceSoftwareOptions'
    { automatedUpdateDate =
        Prelude.Nothing,
      cancellable = Prelude.Nothing,
      currentVersion =
        Prelude.Nothing,
      description = Prelude.Nothing,
      newVersion' = Prelude.Nothing,
      updateAvailable =
        Prelude.Nothing,
      updateStatus =
        Prelude.Nothing
    }

-- | The epoch time when the deployment window closes for required updates.
-- After this time, Amazon OpenSearch Service schedules the software
-- upgrade automatically.
awsElasticsearchDomainServiceSoftwareOptions_automatedUpdateDate :: Lens.Lens' AwsElasticsearchDomainServiceSoftwareOptions (Prelude.Maybe Prelude.Text)
awsElasticsearchDomainServiceSoftwareOptions_automatedUpdateDate = Lens.lens (\AwsElasticsearchDomainServiceSoftwareOptions' {automatedUpdateDate} -> automatedUpdateDate) (\s@AwsElasticsearchDomainServiceSoftwareOptions' {} a -> s {automatedUpdateDate = a} :: AwsElasticsearchDomainServiceSoftwareOptions)

-- | Whether a request to update the domain can be canceled.
awsElasticsearchDomainServiceSoftwareOptions_cancellable :: Lens.Lens' AwsElasticsearchDomainServiceSoftwareOptions (Prelude.Maybe Prelude.Bool)
awsElasticsearchDomainServiceSoftwareOptions_cancellable = Lens.lens (\AwsElasticsearchDomainServiceSoftwareOptions' {cancellable} -> cancellable) (\s@AwsElasticsearchDomainServiceSoftwareOptions' {} a -> s {cancellable = a} :: AwsElasticsearchDomainServiceSoftwareOptions)

-- | The version of the service software that is currently installed on the
-- domain.
awsElasticsearchDomainServiceSoftwareOptions_currentVersion :: Lens.Lens' AwsElasticsearchDomainServiceSoftwareOptions (Prelude.Maybe Prelude.Text)
awsElasticsearchDomainServiceSoftwareOptions_currentVersion = Lens.lens (\AwsElasticsearchDomainServiceSoftwareOptions' {currentVersion} -> currentVersion) (\s@AwsElasticsearchDomainServiceSoftwareOptions' {} a -> s {currentVersion = a} :: AwsElasticsearchDomainServiceSoftwareOptions)

-- | A more detailed description of the service software status.
awsElasticsearchDomainServiceSoftwareOptions_description :: Lens.Lens' AwsElasticsearchDomainServiceSoftwareOptions (Prelude.Maybe Prelude.Text)
awsElasticsearchDomainServiceSoftwareOptions_description = Lens.lens (\AwsElasticsearchDomainServiceSoftwareOptions' {description} -> description) (\s@AwsElasticsearchDomainServiceSoftwareOptions' {} a -> s {description = a} :: AwsElasticsearchDomainServiceSoftwareOptions)

-- | The most recent version of the service software.
awsElasticsearchDomainServiceSoftwareOptions_newVersion :: Lens.Lens' AwsElasticsearchDomainServiceSoftwareOptions (Prelude.Maybe Prelude.Text)
awsElasticsearchDomainServiceSoftwareOptions_newVersion = Lens.lens (\AwsElasticsearchDomainServiceSoftwareOptions' {newVersion'} -> newVersion') (\s@AwsElasticsearchDomainServiceSoftwareOptions' {} a -> s {newVersion' = a} :: AwsElasticsearchDomainServiceSoftwareOptions)

-- | Whether a service software update is available for the domain.
awsElasticsearchDomainServiceSoftwareOptions_updateAvailable :: Lens.Lens' AwsElasticsearchDomainServiceSoftwareOptions (Prelude.Maybe Prelude.Bool)
awsElasticsearchDomainServiceSoftwareOptions_updateAvailable = Lens.lens (\AwsElasticsearchDomainServiceSoftwareOptions' {updateAvailable} -> updateAvailable) (\s@AwsElasticsearchDomainServiceSoftwareOptions' {} a -> s {updateAvailable = a} :: AwsElasticsearchDomainServiceSoftwareOptions)

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
awsElasticsearchDomainServiceSoftwareOptions_updateStatus :: Lens.Lens' AwsElasticsearchDomainServiceSoftwareOptions (Prelude.Maybe Prelude.Text)
awsElasticsearchDomainServiceSoftwareOptions_updateStatus = Lens.lens (\AwsElasticsearchDomainServiceSoftwareOptions' {updateStatus} -> updateStatus) (\s@AwsElasticsearchDomainServiceSoftwareOptions' {} a -> s {updateStatus = a} :: AwsElasticsearchDomainServiceSoftwareOptions)

instance
  Data.FromJSON
    AwsElasticsearchDomainServiceSoftwareOptions
  where
  parseJSON =
    Data.withObject
      "AwsElasticsearchDomainServiceSoftwareOptions"
      ( \x ->
          AwsElasticsearchDomainServiceSoftwareOptions'
            Prelude.<$> (x Data..:? "AutomatedUpdateDate")
            Prelude.<*> (x Data..:? "Cancellable")
            Prelude.<*> (x Data..:? "CurrentVersion")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "NewVersion")
            Prelude.<*> (x Data..:? "UpdateAvailable")
            Prelude.<*> (x Data..:? "UpdateStatus")
      )

instance
  Prelude.Hashable
    AwsElasticsearchDomainServiceSoftwareOptions
  where
  hashWithSalt
    _salt
    AwsElasticsearchDomainServiceSoftwareOptions' {..} =
      _salt
        `Prelude.hashWithSalt` automatedUpdateDate
        `Prelude.hashWithSalt` cancellable
        `Prelude.hashWithSalt` currentVersion
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` newVersion'
        `Prelude.hashWithSalt` updateAvailable
        `Prelude.hashWithSalt` updateStatus

instance
  Prelude.NFData
    AwsElasticsearchDomainServiceSoftwareOptions
  where
  rnf AwsElasticsearchDomainServiceSoftwareOptions' {..} =
    Prelude.rnf automatedUpdateDate `Prelude.seq`
      Prelude.rnf cancellable `Prelude.seq`
        Prelude.rnf currentVersion `Prelude.seq`
          Prelude.rnf description `Prelude.seq`
            Prelude.rnf newVersion' `Prelude.seq`
              Prelude.rnf updateAvailable `Prelude.seq`
                Prelude.rnf updateStatus

instance
  Data.ToJSON
    AwsElasticsearchDomainServiceSoftwareOptions
  where
  toJSON
    AwsElasticsearchDomainServiceSoftwareOptions' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("AutomatedUpdateDate" Data..=)
                Prelude.<$> automatedUpdateDate,
              ("Cancellable" Data..=) Prelude.<$> cancellable,
              ("CurrentVersion" Data..=)
                Prelude.<$> currentVersion,
              ("Description" Data..=) Prelude.<$> description,
              ("NewVersion" Data..=) Prelude.<$> newVersion',
              ("UpdateAvailable" Data..=)
                Prelude.<$> updateAvailable,
              ("UpdateStatus" Data..=) Prelude.<$> updateStatus
            ]
        )
