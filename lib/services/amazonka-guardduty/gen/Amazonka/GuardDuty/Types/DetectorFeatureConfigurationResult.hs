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
-- Module      : Amazonka.GuardDuty.Types.DetectorFeatureConfigurationResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.DetectorFeatureConfigurationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.DetectorAdditionalConfigurationResult
import Amazonka.GuardDuty.Types.DetectorFeatureResult
import Amazonka.GuardDuty.Types.FeatureStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a GuardDuty feature.
--
-- /See:/ 'newDetectorFeatureConfigurationResult' smart constructor.
data DetectorFeatureConfigurationResult = DetectorFeatureConfigurationResult'
  { -- | Additional configuration for a resource.
    additionalConfiguration :: Prelude.Maybe [DetectorAdditionalConfigurationResult],
    -- | Indicates the name of the feature that can be enabled for the detector.
    name :: Prelude.Maybe DetectorFeatureResult,
    -- | Indicates the status of the feature that is enabled for the detector.
    status :: Prelude.Maybe FeatureStatus,
    -- | The timestamp at which the feature object was updated.
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectorFeatureConfigurationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalConfiguration', 'detectorFeatureConfigurationResult_additionalConfiguration' - Additional configuration for a resource.
--
-- 'name', 'detectorFeatureConfigurationResult_name' - Indicates the name of the feature that can be enabled for the detector.
--
-- 'status', 'detectorFeatureConfigurationResult_status' - Indicates the status of the feature that is enabled for the detector.
--
-- 'updatedAt', 'detectorFeatureConfigurationResult_updatedAt' - The timestamp at which the feature object was updated.
newDetectorFeatureConfigurationResult ::
  DetectorFeatureConfigurationResult
newDetectorFeatureConfigurationResult =
  DetectorFeatureConfigurationResult'
    { additionalConfiguration =
        Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | Additional configuration for a resource.
detectorFeatureConfigurationResult_additionalConfiguration :: Lens.Lens' DetectorFeatureConfigurationResult (Prelude.Maybe [DetectorAdditionalConfigurationResult])
detectorFeatureConfigurationResult_additionalConfiguration = Lens.lens (\DetectorFeatureConfigurationResult' {additionalConfiguration} -> additionalConfiguration) (\s@DetectorFeatureConfigurationResult' {} a -> s {additionalConfiguration = a} :: DetectorFeatureConfigurationResult) Prelude.. Lens.mapping Lens.coerced

-- | Indicates the name of the feature that can be enabled for the detector.
detectorFeatureConfigurationResult_name :: Lens.Lens' DetectorFeatureConfigurationResult (Prelude.Maybe DetectorFeatureResult)
detectorFeatureConfigurationResult_name = Lens.lens (\DetectorFeatureConfigurationResult' {name} -> name) (\s@DetectorFeatureConfigurationResult' {} a -> s {name = a} :: DetectorFeatureConfigurationResult)

-- | Indicates the status of the feature that is enabled for the detector.
detectorFeatureConfigurationResult_status :: Lens.Lens' DetectorFeatureConfigurationResult (Prelude.Maybe FeatureStatus)
detectorFeatureConfigurationResult_status = Lens.lens (\DetectorFeatureConfigurationResult' {status} -> status) (\s@DetectorFeatureConfigurationResult' {} a -> s {status = a} :: DetectorFeatureConfigurationResult)

-- | The timestamp at which the feature object was updated.
detectorFeatureConfigurationResult_updatedAt :: Lens.Lens' DetectorFeatureConfigurationResult (Prelude.Maybe Prelude.UTCTime)
detectorFeatureConfigurationResult_updatedAt = Lens.lens (\DetectorFeatureConfigurationResult' {updatedAt} -> updatedAt) (\s@DetectorFeatureConfigurationResult' {} a -> s {updatedAt = a} :: DetectorFeatureConfigurationResult) Prelude.. Lens.mapping Data._Time

instance
  Data.FromJSON
    DetectorFeatureConfigurationResult
  where
  parseJSON =
    Data.withObject
      "DetectorFeatureConfigurationResult"
      ( \x ->
          DetectorFeatureConfigurationResult'
            Prelude.<$> ( x
                            Data..:? "additionalConfiguration"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "updatedAt")
      )

instance
  Prelude.Hashable
    DetectorFeatureConfigurationResult
  where
  hashWithSalt
    _salt
    DetectorFeatureConfigurationResult' {..} =
      _salt
        `Prelude.hashWithSalt` additionalConfiguration
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` updatedAt

instance
  Prelude.NFData
    DetectorFeatureConfigurationResult
  where
  rnf DetectorFeatureConfigurationResult' {..} =
    Prelude.rnf additionalConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updatedAt
