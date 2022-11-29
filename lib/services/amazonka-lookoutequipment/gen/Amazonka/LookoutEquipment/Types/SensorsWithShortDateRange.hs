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
-- Module      : Amazonka.LookoutEquipment.Types.SensorsWithShortDateRange
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.SensorsWithShortDateRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Entity that comprises information on sensors that have shorter date
-- range.
--
-- /See:/ 'newSensorsWithShortDateRange' smart constructor.
data SensorsWithShortDateRange = SensorsWithShortDateRange'
  { -- | Indicates the number of sensors that have less than 90 days of data.
    affectedSensorCount :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SensorsWithShortDateRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'affectedSensorCount', 'sensorsWithShortDateRange_affectedSensorCount' - Indicates the number of sensors that have less than 90 days of data.
newSensorsWithShortDateRange ::
  -- | 'affectedSensorCount'
  Prelude.Int ->
  SensorsWithShortDateRange
newSensorsWithShortDateRange pAffectedSensorCount_ =
  SensorsWithShortDateRange'
    { affectedSensorCount =
        pAffectedSensorCount_
    }

-- | Indicates the number of sensors that have less than 90 days of data.
sensorsWithShortDateRange_affectedSensorCount :: Lens.Lens' SensorsWithShortDateRange Prelude.Int
sensorsWithShortDateRange_affectedSensorCount = Lens.lens (\SensorsWithShortDateRange' {affectedSensorCount} -> affectedSensorCount) (\s@SensorsWithShortDateRange' {} a -> s {affectedSensorCount = a} :: SensorsWithShortDateRange)

instance Core.FromJSON SensorsWithShortDateRange where
  parseJSON =
    Core.withObject
      "SensorsWithShortDateRange"
      ( \x ->
          SensorsWithShortDateRange'
            Prelude.<$> (x Core..: "AffectedSensorCount")
      )

instance Prelude.Hashable SensorsWithShortDateRange where
  hashWithSalt _salt SensorsWithShortDateRange' {..} =
    _salt `Prelude.hashWithSalt` affectedSensorCount

instance Prelude.NFData SensorsWithShortDateRange where
  rnf SensorsWithShortDateRange' {..} =
    Prelude.rnf affectedSensorCount
