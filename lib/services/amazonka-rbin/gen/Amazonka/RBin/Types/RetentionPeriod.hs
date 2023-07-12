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
-- Module      : Amazonka.RBin.Types.RetentionPeriod
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RBin.Types.RetentionPeriod where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RBin.Types.RetentionPeriodUnit

-- | Information about the retention period for which the retention rule is
-- to retain resources.
--
-- /See:/ 'newRetentionPeriod' smart constructor.
data RetentionPeriod = RetentionPeriod'
  { -- | The period value for which the retention rule is to retain resources.
    -- The period is measured using the unit specified for
    -- __RetentionPeriodUnit__.
    retentionPeriodValue :: Prelude.Natural,
    -- | The unit of time in which the retention period is measured. Currently,
    -- only @DAYS@ is supported.
    retentionPeriodUnit :: RetentionPeriodUnit
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetentionPeriod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retentionPeriodValue', 'retentionPeriod_retentionPeriodValue' - The period value for which the retention rule is to retain resources.
-- The period is measured using the unit specified for
-- __RetentionPeriodUnit__.
--
-- 'retentionPeriodUnit', 'retentionPeriod_retentionPeriodUnit' - The unit of time in which the retention period is measured. Currently,
-- only @DAYS@ is supported.
newRetentionPeriod ::
  -- | 'retentionPeriodValue'
  Prelude.Natural ->
  -- | 'retentionPeriodUnit'
  RetentionPeriodUnit ->
  RetentionPeriod
newRetentionPeriod
  pRetentionPeriodValue_
  pRetentionPeriodUnit_ =
    RetentionPeriod'
      { retentionPeriodValue =
          pRetentionPeriodValue_,
        retentionPeriodUnit = pRetentionPeriodUnit_
      }

-- | The period value for which the retention rule is to retain resources.
-- The period is measured using the unit specified for
-- __RetentionPeriodUnit__.
retentionPeriod_retentionPeriodValue :: Lens.Lens' RetentionPeriod Prelude.Natural
retentionPeriod_retentionPeriodValue = Lens.lens (\RetentionPeriod' {retentionPeriodValue} -> retentionPeriodValue) (\s@RetentionPeriod' {} a -> s {retentionPeriodValue = a} :: RetentionPeriod)

-- | The unit of time in which the retention period is measured. Currently,
-- only @DAYS@ is supported.
retentionPeriod_retentionPeriodUnit :: Lens.Lens' RetentionPeriod RetentionPeriodUnit
retentionPeriod_retentionPeriodUnit = Lens.lens (\RetentionPeriod' {retentionPeriodUnit} -> retentionPeriodUnit) (\s@RetentionPeriod' {} a -> s {retentionPeriodUnit = a} :: RetentionPeriod)

instance Data.FromJSON RetentionPeriod where
  parseJSON =
    Data.withObject
      "RetentionPeriod"
      ( \x ->
          RetentionPeriod'
            Prelude.<$> (x Data..: "RetentionPeriodValue")
            Prelude.<*> (x Data..: "RetentionPeriodUnit")
      )

instance Prelude.Hashable RetentionPeriod where
  hashWithSalt _salt RetentionPeriod' {..} =
    _salt
      `Prelude.hashWithSalt` retentionPeriodValue
      `Prelude.hashWithSalt` retentionPeriodUnit

instance Prelude.NFData RetentionPeriod where
  rnf RetentionPeriod' {..} =
    Prelude.rnf retentionPeriodValue
      `Prelude.seq` Prelude.rnf retentionPeriodUnit

instance Data.ToJSON RetentionPeriod where
  toJSON RetentionPeriod' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "RetentionPeriodValue"
                  Data..= retentionPeriodValue
              ),
            Prelude.Just
              ("RetentionPeriodUnit" Data..= retentionPeriodUnit)
          ]
      )
