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
-- Module      : Amazonka.StepFunctions.Types.BillingDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.BillingDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that describes workflow billing details.
--
-- /See:/ 'newBillingDetails' smart constructor.
data BillingDetails = BillingDetails'
  { -- | Billed duration of your workflow, in milliseconds.
    billedDurationInMilliseconds :: Prelude.Maybe Prelude.Natural,
    -- | Billed memory consumption of your workflow, in MB.
    billedMemoryUsedInMB :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BillingDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billedDurationInMilliseconds', 'billingDetails_billedDurationInMilliseconds' - Billed duration of your workflow, in milliseconds.
--
-- 'billedMemoryUsedInMB', 'billingDetails_billedMemoryUsedInMB' - Billed memory consumption of your workflow, in MB.
newBillingDetails ::
  BillingDetails
newBillingDetails =
  BillingDetails'
    { billedDurationInMilliseconds =
        Prelude.Nothing,
      billedMemoryUsedInMB = Prelude.Nothing
    }

-- | Billed duration of your workflow, in milliseconds.
billingDetails_billedDurationInMilliseconds :: Lens.Lens' BillingDetails (Prelude.Maybe Prelude.Natural)
billingDetails_billedDurationInMilliseconds = Lens.lens (\BillingDetails' {billedDurationInMilliseconds} -> billedDurationInMilliseconds) (\s@BillingDetails' {} a -> s {billedDurationInMilliseconds = a} :: BillingDetails)

-- | Billed memory consumption of your workflow, in MB.
billingDetails_billedMemoryUsedInMB :: Lens.Lens' BillingDetails (Prelude.Maybe Prelude.Natural)
billingDetails_billedMemoryUsedInMB = Lens.lens (\BillingDetails' {billedMemoryUsedInMB} -> billedMemoryUsedInMB) (\s@BillingDetails' {} a -> s {billedMemoryUsedInMB = a} :: BillingDetails)

instance Core.FromJSON BillingDetails where
  parseJSON =
    Core.withObject
      "BillingDetails"
      ( \x ->
          BillingDetails'
            Prelude.<$> (x Core..:? "billedDurationInMilliseconds")
            Prelude.<*> (x Core..:? "billedMemoryUsedInMB")
      )

instance Prelude.Hashable BillingDetails where
  hashWithSalt _salt BillingDetails' {..} =
    _salt
      `Prelude.hashWithSalt` billedDurationInMilliseconds
      `Prelude.hashWithSalt` billedMemoryUsedInMB

instance Prelude.NFData BillingDetails where
  rnf BillingDetails' {..} =
    Prelude.rnf billedDurationInMilliseconds
      `Prelude.seq` Prelude.rnf billedMemoryUsedInMB
