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
-- Module      : Amazonka.Athena.Types.CalculationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.CalculationSummary where

import Amazonka.Athena.Types.CalculationStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary information for a notebook calculation.
--
-- /See:/ 'newCalculationSummary' smart constructor.
data CalculationSummary = CalculationSummary'
  { -- | The calculation execution UUID.
    calculationExecutionId :: Prelude.Maybe Prelude.Text,
    -- | A description of the calculation.
    description :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the status of the calculation.
    status :: Prelude.Maybe CalculationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CalculationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'calculationExecutionId', 'calculationSummary_calculationExecutionId' - The calculation execution UUID.
--
-- 'description', 'calculationSummary_description' - A description of the calculation.
--
-- 'status', 'calculationSummary_status' - Contains information about the status of the calculation.
newCalculationSummary ::
  CalculationSummary
newCalculationSummary =
  CalculationSummary'
    { calculationExecutionId =
        Prelude.Nothing,
      description = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The calculation execution UUID.
calculationSummary_calculationExecutionId :: Lens.Lens' CalculationSummary (Prelude.Maybe Prelude.Text)
calculationSummary_calculationExecutionId = Lens.lens (\CalculationSummary' {calculationExecutionId} -> calculationExecutionId) (\s@CalculationSummary' {} a -> s {calculationExecutionId = a} :: CalculationSummary)

-- | A description of the calculation.
calculationSummary_description :: Lens.Lens' CalculationSummary (Prelude.Maybe Prelude.Text)
calculationSummary_description = Lens.lens (\CalculationSummary' {description} -> description) (\s@CalculationSummary' {} a -> s {description = a} :: CalculationSummary)

-- | Contains information about the status of the calculation.
calculationSummary_status :: Lens.Lens' CalculationSummary (Prelude.Maybe CalculationStatus)
calculationSummary_status = Lens.lens (\CalculationSummary' {status} -> status) (\s@CalculationSummary' {} a -> s {status = a} :: CalculationSummary)

instance Data.FromJSON CalculationSummary where
  parseJSON =
    Data.withObject
      "CalculationSummary"
      ( \x ->
          CalculationSummary'
            Prelude.<$> (x Data..:? "CalculationExecutionId")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable CalculationSummary where
  hashWithSalt _salt CalculationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` calculationExecutionId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` status

instance Prelude.NFData CalculationSummary where
  rnf CalculationSummary' {..} =
    Prelude.rnf calculationExecutionId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf status
