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
-- Module      : Amazonka.GuardDuty.Types.DetectorAdditionalConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.DetectorAdditionalConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.FeatureAdditionalConfiguration
import Amazonka.GuardDuty.Types.FeatureStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about the additional configuration for a feature in your
-- GuardDuty account.
--
-- /See:/ 'newDetectorAdditionalConfiguration' smart constructor.
data DetectorAdditionalConfiguration = DetectorAdditionalConfiguration'
  { -- | Name of the additional configuration.
    name :: Prelude.Maybe FeatureAdditionalConfiguration,
    -- | Status of the additional configuration.
    status :: Prelude.Maybe FeatureStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectorAdditionalConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'detectorAdditionalConfiguration_name' - Name of the additional configuration.
--
-- 'status', 'detectorAdditionalConfiguration_status' - Status of the additional configuration.
newDetectorAdditionalConfiguration ::
  DetectorAdditionalConfiguration
newDetectorAdditionalConfiguration =
  DetectorAdditionalConfiguration'
    { name =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Name of the additional configuration.
detectorAdditionalConfiguration_name :: Lens.Lens' DetectorAdditionalConfiguration (Prelude.Maybe FeatureAdditionalConfiguration)
detectorAdditionalConfiguration_name = Lens.lens (\DetectorAdditionalConfiguration' {name} -> name) (\s@DetectorAdditionalConfiguration' {} a -> s {name = a} :: DetectorAdditionalConfiguration)

-- | Status of the additional configuration.
detectorAdditionalConfiguration_status :: Lens.Lens' DetectorAdditionalConfiguration (Prelude.Maybe FeatureStatus)
detectorAdditionalConfiguration_status = Lens.lens (\DetectorAdditionalConfiguration' {status} -> status) (\s@DetectorAdditionalConfiguration' {} a -> s {status = a} :: DetectorAdditionalConfiguration)

instance
  Prelude.Hashable
    DetectorAdditionalConfiguration
  where
  hashWithSalt
    _salt
    DetectorAdditionalConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    DetectorAdditionalConfiguration
  where
  rnf DetectorAdditionalConfiguration' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf status

instance Data.ToJSON DetectorAdditionalConfiguration where
  toJSON DetectorAdditionalConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("name" Data..=) Prelude.<$> name,
            ("status" Data..=) Prelude.<$> status
          ]
      )
