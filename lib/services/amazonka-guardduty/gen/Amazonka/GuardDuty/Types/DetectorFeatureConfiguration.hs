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
-- Module      : Amazonka.GuardDuty.Types.DetectorFeatureConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.DetectorFeatureConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.DetectorAdditionalConfiguration
import Amazonka.GuardDuty.Types.DetectorFeature
import Amazonka.GuardDuty.Types.FeatureStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a GuardDuty feature.
--
-- /See:/ 'newDetectorFeatureConfiguration' smart constructor.
data DetectorFeatureConfiguration = DetectorFeatureConfiguration'
  { -- | Additional configuration for a resource.
    additionalConfiguration :: Prelude.Maybe [DetectorAdditionalConfiguration],
    -- | The name of the feature.
    name :: Prelude.Maybe DetectorFeature,
    -- | The status of the feature.
    status :: Prelude.Maybe FeatureStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectorFeatureConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalConfiguration', 'detectorFeatureConfiguration_additionalConfiguration' - Additional configuration for a resource.
--
-- 'name', 'detectorFeatureConfiguration_name' - The name of the feature.
--
-- 'status', 'detectorFeatureConfiguration_status' - The status of the feature.
newDetectorFeatureConfiguration ::
  DetectorFeatureConfiguration
newDetectorFeatureConfiguration =
  DetectorFeatureConfiguration'
    { additionalConfiguration =
        Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Additional configuration for a resource.
detectorFeatureConfiguration_additionalConfiguration :: Lens.Lens' DetectorFeatureConfiguration (Prelude.Maybe [DetectorAdditionalConfiguration])
detectorFeatureConfiguration_additionalConfiguration = Lens.lens (\DetectorFeatureConfiguration' {additionalConfiguration} -> additionalConfiguration) (\s@DetectorFeatureConfiguration' {} a -> s {additionalConfiguration = a} :: DetectorFeatureConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the feature.
detectorFeatureConfiguration_name :: Lens.Lens' DetectorFeatureConfiguration (Prelude.Maybe DetectorFeature)
detectorFeatureConfiguration_name = Lens.lens (\DetectorFeatureConfiguration' {name} -> name) (\s@DetectorFeatureConfiguration' {} a -> s {name = a} :: DetectorFeatureConfiguration)

-- | The status of the feature.
detectorFeatureConfiguration_status :: Lens.Lens' DetectorFeatureConfiguration (Prelude.Maybe FeatureStatus)
detectorFeatureConfiguration_status = Lens.lens (\DetectorFeatureConfiguration' {status} -> status) (\s@DetectorFeatureConfiguration' {} a -> s {status = a} :: DetectorFeatureConfiguration)

instance
  Prelude.Hashable
    DetectorFeatureConfiguration
  where
  hashWithSalt _salt DetectorFeatureConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` additionalConfiguration
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData DetectorFeatureConfiguration where
  rnf DetectorFeatureConfiguration' {..} =
    Prelude.rnf additionalConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON DetectorFeatureConfiguration where
  toJSON DetectorFeatureConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("additionalConfiguration" Data..=)
              Prelude.<$> additionalConfiguration,
            ("name" Data..=) Prelude.<$> name,
            ("status" Data..=) Prelude.<$> status
          ]
      )
