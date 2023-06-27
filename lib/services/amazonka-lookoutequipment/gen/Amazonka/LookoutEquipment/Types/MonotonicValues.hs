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
-- Module      : Amazonka.LookoutEquipment.Types.MonotonicValues
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.MonotonicValues where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types.Monotonicity
import Amazonka.LookoutEquipment.Types.StatisticalIssueStatus
import qualified Amazonka.Prelude as Prelude

-- | Entity that comprises information on monotonic values in the data.
--
-- /See:/ 'newMonotonicValues' smart constructor.
data MonotonicValues = MonotonicValues'
  { -- | Indicates the monotonicity of values. Can be INCREASING, DECREASING, or
    -- STATIC.
    monotonicity :: Prelude.Maybe Monotonicity,
    -- | Indicates whether there is a potential data issue related to having
    -- monotonic values.
    status :: StatisticalIssueStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonotonicValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monotonicity', 'monotonicValues_monotonicity' - Indicates the monotonicity of values. Can be INCREASING, DECREASING, or
-- STATIC.
--
-- 'status', 'monotonicValues_status' - Indicates whether there is a potential data issue related to having
-- monotonic values.
newMonotonicValues ::
  -- | 'status'
  StatisticalIssueStatus ->
  MonotonicValues
newMonotonicValues pStatus_ =
  MonotonicValues'
    { monotonicity = Prelude.Nothing,
      status = pStatus_
    }

-- | Indicates the monotonicity of values. Can be INCREASING, DECREASING, or
-- STATIC.
monotonicValues_monotonicity :: Lens.Lens' MonotonicValues (Prelude.Maybe Monotonicity)
monotonicValues_monotonicity = Lens.lens (\MonotonicValues' {monotonicity} -> monotonicity) (\s@MonotonicValues' {} a -> s {monotonicity = a} :: MonotonicValues)

-- | Indicates whether there is a potential data issue related to having
-- monotonic values.
monotonicValues_status :: Lens.Lens' MonotonicValues StatisticalIssueStatus
monotonicValues_status = Lens.lens (\MonotonicValues' {status} -> status) (\s@MonotonicValues' {} a -> s {status = a} :: MonotonicValues)

instance Data.FromJSON MonotonicValues where
  parseJSON =
    Data.withObject
      "MonotonicValues"
      ( \x ->
          MonotonicValues'
            Prelude.<$> (x Data..:? "Monotonicity")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable MonotonicValues where
  hashWithSalt _salt MonotonicValues' {..} =
    _salt
      `Prelude.hashWithSalt` monotonicity
      `Prelude.hashWithSalt` status

instance Prelude.NFData MonotonicValues where
  rnf MonotonicValues' {..} =
    Prelude.rnf monotonicity
      `Prelude.seq` Prelude.rnf status
