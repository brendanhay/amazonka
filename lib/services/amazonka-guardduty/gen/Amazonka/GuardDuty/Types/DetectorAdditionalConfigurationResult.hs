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
-- Module      : Amazonka.GuardDuty.Types.DetectorAdditionalConfigurationResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.DetectorAdditionalConfigurationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.FeatureAdditionalConfiguration
import Amazonka.GuardDuty.Types.FeatureStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about the additional configuration.
--
-- /See:/ 'newDetectorAdditionalConfigurationResult' smart constructor.
data DetectorAdditionalConfigurationResult = DetectorAdditionalConfigurationResult'
  { -- | Name of the additional configuration.
    name :: Prelude.Maybe FeatureAdditionalConfiguration,
    -- | Status of the additional configuration.
    status :: Prelude.Maybe FeatureStatus,
    -- | The timestamp at which the additional configuration was last updated.
    -- This is in UTC format.
    updatedAt :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectorAdditionalConfigurationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'detectorAdditionalConfigurationResult_name' - Name of the additional configuration.
--
-- 'status', 'detectorAdditionalConfigurationResult_status' - Status of the additional configuration.
--
-- 'updatedAt', 'detectorAdditionalConfigurationResult_updatedAt' - The timestamp at which the additional configuration was last updated.
-- This is in UTC format.
newDetectorAdditionalConfigurationResult ::
  DetectorAdditionalConfigurationResult
newDetectorAdditionalConfigurationResult =
  DetectorAdditionalConfigurationResult'
    { name =
        Prelude.Nothing,
      status = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | Name of the additional configuration.
detectorAdditionalConfigurationResult_name :: Lens.Lens' DetectorAdditionalConfigurationResult (Prelude.Maybe FeatureAdditionalConfiguration)
detectorAdditionalConfigurationResult_name = Lens.lens (\DetectorAdditionalConfigurationResult' {name} -> name) (\s@DetectorAdditionalConfigurationResult' {} a -> s {name = a} :: DetectorAdditionalConfigurationResult)

-- | Status of the additional configuration.
detectorAdditionalConfigurationResult_status :: Lens.Lens' DetectorAdditionalConfigurationResult (Prelude.Maybe FeatureStatus)
detectorAdditionalConfigurationResult_status = Lens.lens (\DetectorAdditionalConfigurationResult' {status} -> status) (\s@DetectorAdditionalConfigurationResult' {} a -> s {status = a} :: DetectorAdditionalConfigurationResult)

-- | The timestamp at which the additional configuration was last updated.
-- This is in UTC format.
detectorAdditionalConfigurationResult_updatedAt :: Lens.Lens' DetectorAdditionalConfigurationResult (Prelude.Maybe Prelude.UTCTime)
detectorAdditionalConfigurationResult_updatedAt = Lens.lens (\DetectorAdditionalConfigurationResult' {updatedAt} -> updatedAt) (\s@DetectorAdditionalConfigurationResult' {} a -> s {updatedAt = a} :: DetectorAdditionalConfigurationResult) Prelude.. Lens.mapping Data._Time

instance
  Data.FromJSON
    DetectorAdditionalConfigurationResult
  where
  parseJSON =
    Data.withObject
      "DetectorAdditionalConfigurationResult"
      ( \x ->
          DetectorAdditionalConfigurationResult'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "updatedAt")
      )

instance
  Prelude.Hashable
    DetectorAdditionalConfigurationResult
  where
  hashWithSalt
    _salt
    DetectorAdditionalConfigurationResult' {..} =
      _salt
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` updatedAt

instance
  Prelude.NFData
    DetectorAdditionalConfigurationResult
  where
  rnf DetectorAdditionalConfigurationResult' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updatedAt
