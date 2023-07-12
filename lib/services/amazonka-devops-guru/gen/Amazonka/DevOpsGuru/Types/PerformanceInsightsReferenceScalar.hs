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
-- Module      : Amazonka.DevOpsGuru.Types.PerformanceInsightsReferenceScalar
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.PerformanceInsightsReferenceScalar where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A reference value to compare Performance Insights metrics against to
-- determine if the metrics demonstrate anomalous behavior.
--
-- /See:/ 'newPerformanceInsightsReferenceScalar' smart constructor.
data PerformanceInsightsReferenceScalar = PerformanceInsightsReferenceScalar'
  { -- | The reference value.
    value :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PerformanceInsightsReferenceScalar' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'performanceInsightsReferenceScalar_value' - The reference value.
newPerformanceInsightsReferenceScalar ::
  PerformanceInsightsReferenceScalar
newPerformanceInsightsReferenceScalar =
  PerformanceInsightsReferenceScalar'
    { value =
        Prelude.Nothing
    }

-- | The reference value.
performanceInsightsReferenceScalar_value :: Lens.Lens' PerformanceInsightsReferenceScalar (Prelude.Maybe Prelude.Double)
performanceInsightsReferenceScalar_value = Lens.lens (\PerformanceInsightsReferenceScalar' {value} -> value) (\s@PerformanceInsightsReferenceScalar' {} a -> s {value = a} :: PerformanceInsightsReferenceScalar)

instance
  Data.FromJSON
    PerformanceInsightsReferenceScalar
  where
  parseJSON =
    Data.withObject
      "PerformanceInsightsReferenceScalar"
      ( \x ->
          PerformanceInsightsReferenceScalar'
            Prelude.<$> (x Data..:? "Value")
      )

instance
  Prelude.Hashable
    PerformanceInsightsReferenceScalar
  where
  hashWithSalt
    _salt
    PerformanceInsightsReferenceScalar' {..} =
      _salt `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    PerformanceInsightsReferenceScalar
  where
  rnf PerformanceInsightsReferenceScalar' {..} =
    Prelude.rnf value
