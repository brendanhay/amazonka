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
-- Module      : Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorDataSourcesDetails
import Amazonka.SecurityHub.Types.AwsGuardDutyDetectorFeaturesDetails

-- | Provides details about an Amazon GuardDuty detector. A detector is an
-- object that represents the GuardDuty service. A detector is required for
-- GuardDuty to become operational.
--
-- /See:/ 'newAwsGuardDutyDetectorDetails' smart constructor.
data AwsGuardDutyDetectorDetails = AwsGuardDutyDetectorDetails'
  { -- | Describes which data sources are activated for the detector.
    dataSources :: Prelude.Maybe AwsGuardDutyDetectorDataSourcesDetails,
    -- | Describes which features are activated for the detector.
    features :: Prelude.Maybe [AwsGuardDutyDetectorFeaturesDetails],
    -- | The publishing frequency of the finding.
    findingPublishingFrequency :: Prelude.Maybe Prelude.Text,
    -- | The GuardDuty service role.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | The activation status of the detector.
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsGuardDutyDetectorDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSources', 'awsGuardDutyDetectorDetails_dataSources' - Describes which data sources are activated for the detector.
--
-- 'features', 'awsGuardDutyDetectorDetails_features' - Describes which features are activated for the detector.
--
-- 'findingPublishingFrequency', 'awsGuardDutyDetectorDetails_findingPublishingFrequency' - The publishing frequency of the finding.
--
-- 'serviceRole', 'awsGuardDutyDetectorDetails_serviceRole' - The GuardDuty service role.
--
-- 'status', 'awsGuardDutyDetectorDetails_status' - The activation status of the detector.
newAwsGuardDutyDetectorDetails ::
  AwsGuardDutyDetectorDetails
newAwsGuardDutyDetectorDetails =
  AwsGuardDutyDetectorDetails'
    { dataSources =
        Prelude.Nothing,
      features = Prelude.Nothing,
      findingPublishingFrequency = Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Describes which data sources are activated for the detector.
awsGuardDutyDetectorDetails_dataSources :: Lens.Lens' AwsGuardDutyDetectorDetails (Prelude.Maybe AwsGuardDutyDetectorDataSourcesDetails)
awsGuardDutyDetectorDetails_dataSources = Lens.lens (\AwsGuardDutyDetectorDetails' {dataSources} -> dataSources) (\s@AwsGuardDutyDetectorDetails' {} a -> s {dataSources = a} :: AwsGuardDutyDetectorDetails)

-- | Describes which features are activated for the detector.
awsGuardDutyDetectorDetails_features :: Lens.Lens' AwsGuardDutyDetectorDetails (Prelude.Maybe [AwsGuardDutyDetectorFeaturesDetails])
awsGuardDutyDetectorDetails_features = Lens.lens (\AwsGuardDutyDetectorDetails' {features} -> features) (\s@AwsGuardDutyDetectorDetails' {} a -> s {features = a} :: AwsGuardDutyDetectorDetails) Prelude.. Lens.mapping Lens.coerced

-- | The publishing frequency of the finding.
awsGuardDutyDetectorDetails_findingPublishingFrequency :: Lens.Lens' AwsGuardDutyDetectorDetails (Prelude.Maybe Prelude.Text)
awsGuardDutyDetectorDetails_findingPublishingFrequency = Lens.lens (\AwsGuardDutyDetectorDetails' {findingPublishingFrequency} -> findingPublishingFrequency) (\s@AwsGuardDutyDetectorDetails' {} a -> s {findingPublishingFrequency = a} :: AwsGuardDutyDetectorDetails)

-- | The GuardDuty service role.
awsGuardDutyDetectorDetails_serviceRole :: Lens.Lens' AwsGuardDutyDetectorDetails (Prelude.Maybe Prelude.Text)
awsGuardDutyDetectorDetails_serviceRole = Lens.lens (\AwsGuardDutyDetectorDetails' {serviceRole} -> serviceRole) (\s@AwsGuardDutyDetectorDetails' {} a -> s {serviceRole = a} :: AwsGuardDutyDetectorDetails)

-- | The activation status of the detector.
awsGuardDutyDetectorDetails_status :: Lens.Lens' AwsGuardDutyDetectorDetails (Prelude.Maybe Prelude.Text)
awsGuardDutyDetectorDetails_status = Lens.lens (\AwsGuardDutyDetectorDetails' {status} -> status) (\s@AwsGuardDutyDetectorDetails' {} a -> s {status = a} :: AwsGuardDutyDetectorDetails)

instance Data.FromJSON AwsGuardDutyDetectorDetails where
  parseJSON =
    Data.withObject
      "AwsGuardDutyDetectorDetails"
      ( \x ->
          AwsGuardDutyDetectorDetails'
            Prelude.<$> (x Data..:? "DataSources")
            Prelude.<*> (x Data..:? "Features" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "FindingPublishingFrequency")
            Prelude.<*> (x Data..:? "ServiceRole")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable AwsGuardDutyDetectorDetails where
  hashWithSalt _salt AwsGuardDutyDetectorDetails' {..} =
    _salt
      `Prelude.hashWithSalt` dataSources
      `Prelude.hashWithSalt` features
      `Prelude.hashWithSalt` findingPublishingFrequency
      `Prelude.hashWithSalt` serviceRole
      `Prelude.hashWithSalt` status

instance Prelude.NFData AwsGuardDutyDetectorDetails where
  rnf AwsGuardDutyDetectorDetails' {..} =
    Prelude.rnf dataSources
      `Prelude.seq` Prelude.rnf features
      `Prelude.seq` Prelude.rnf findingPublishingFrequency
      `Prelude.seq` Prelude.rnf serviceRole
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON AwsGuardDutyDetectorDetails where
  toJSON AwsGuardDutyDetectorDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataSources" Data..=) Prelude.<$> dataSources,
            ("Features" Data..=) Prelude.<$> features,
            ("FindingPublishingFrequency" Data..=)
              Prelude.<$> findingPublishingFrequency,
            ("ServiceRole" Data..=) Prelude.<$> serviceRole,
            ("Status" Data..=) Prelude.<$> status
          ]
      )
