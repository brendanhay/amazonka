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
-- Module      : Amazonka.MGN.Types.LifeCycleLastCutover
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.LifeCycleLastCutover where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.LifeCycleLastCutoverFinalized
import Amazonka.MGN.Types.LifeCycleLastCutoverInitiated
import Amazonka.MGN.Types.LifeCycleLastCutoverReverted
import qualified Amazonka.Prelude as Prelude

-- | Lifecycle last Cutover .
--
-- /See:/ 'newLifeCycleLastCutover' smart constructor.
data LifeCycleLastCutover = LifeCycleLastCutover'
  { -- | Lifecycle Cutover finalized date and time.
    finalized :: Prelude.Maybe LifeCycleLastCutoverFinalized,
    -- | Lifecycle last Cutover initiated.
    initiated :: Prelude.Maybe LifeCycleLastCutoverInitiated,
    -- | Lifecycle last Cutover reverted.
    reverted :: Prelude.Maybe LifeCycleLastCutoverReverted
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifeCycleLastCutover' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finalized', 'lifeCycleLastCutover_finalized' - Lifecycle Cutover finalized date and time.
--
-- 'initiated', 'lifeCycleLastCutover_initiated' - Lifecycle last Cutover initiated.
--
-- 'reverted', 'lifeCycleLastCutover_reverted' - Lifecycle last Cutover reverted.
newLifeCycleLastCutover ::
  LifeCycleLastCutover
newLifeCycleLastCutover =
  LifeCycleLastCutover'
    { finalized = Prelude.Nothing,
      initiated = Prelude.Nothing,
      reverted = Prelude.Nothing
    }

-- | Lifecycle Cutover finalized date and time.
lifeCycleLastCutover_finalized :: Lens.Lens' LifeCycleLastCutover (Prelude.Maybe LifeCycleLastCutoverFinalized)
lifeCycleLastCutover_finalized = Lens.lens (\LifeCycleLastCutover' {finalized} -> finalized) (\s@LifeCycleLastCutover' {} a -> s {finalized = a} :: LifeCycleLastCutover)

-- | Lifecycle last Cutover initiated.
lifeCycleLastCutover_initiated :: Lens.Lens' LifeCycleLastCutover (Prelude.Maybe LifeCycleLastCutoverInitiated)
lifeCycleLastCutover_initiated = Lens.lens (\LifeCycleLastCutover' {initiated} -> initiated) (\s@LifeCycleLastCutover' {} a -> s {initiated = a} :: LifeCycleLastCutover)

-- | Lifecycle last Cutover reverted.
lifeCycleLastCutover_reverted :: Lens.Lens' LifeCycleLastCutover (Prelude.Maybe LifeCycleLastCutoverReverted)
lifeCycleLastCutover_reverted = Lens.lens (\LifeCycleLastCutover' {reverted} -> reverted) (\s@LifeCycleLastCutover' {} a -> s {reverted = a} :: LifeCycleLastCutover)

instance Data.FromJSON LifeCycleLastCutover where
  parseJSON =
    Data.withObject
      "LifeCycleLastCutover"
      ( \x ->
          LifeCycleLastCutover'
            Prelude.<$> (x Data..:? "finalized")
            Prelude.<*> (x Data..:? "initiated")
            Prelude.<*> (x Data..:? "reverted")
      )

instance Prelude.Hashable LifeCycleLastCutover where
  hashWithSalt _salt LifeCycleLastCutover' {..} =
    _salt `Prelude.hashWithSalt` finalized
      `Prelude.hashWithSalt` initiated
      `Prelude.hashWithSalt` reverted

instance Prelude.NFData LifeCycleLastCutover where
  rnf LifeCycleLastCutover' {..} =
    Prelude.rnf finalized
      `Prelude.seq` Prelude.rnf initiated
      `Prelude.seq` Prelude.rnf reverted
