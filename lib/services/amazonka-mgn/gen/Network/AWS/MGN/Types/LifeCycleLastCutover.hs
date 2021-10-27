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
-- Module      : Network.AWS.MGN.Types.LifeCycleLastCutover
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MGN.Types.LifeCycleLastCutover where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MGN.Types.LifeCycleLastCutoverFinalized
import Network.AWS.MGN.Types.LifeCycleLastCutoverInitiated
import Network.AWS.MGN.Types.LifeCycleLastCutoverReverted
import qualified Network.AWS.Prelude as Prelude

-- | Lifecycle last Cutover .
--
-- /See:/ 'newLifeCycleLastCutover' smart constructor.
data LifeCycleLastCutover = LifeCycleLastCutover'
  { -- | Lifecycle last Cutover initiated.
    initiated :: Prelude.Maybe LifeCycleLastCutoverInitiated,
    -- | Lifecycle last Cutover reverted.
    reverted :: Prelude.Maybe LifeCycleLastCutoverReverted,
    -- | Lifecycle Cutover finalized date and time.
    finalized :: Prelude.Maybe LifeCycleLastCutoverFinalized
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
-- 'initiated', 'lifeCycleLastCutover_initiated' - Lifecycle last Cutover initiated.
--
-- 'reverted', 'lifeCycleLastCutover_reverted' - Lifecycle last Cutover reverted.
--
-- 'finalized', 'lifeCycleLastCutover_finalized' - Lifecycle Cutover finalized date and time.
newLifeCycleLastCutover ::
  LifeCycleLastCutover
newLifeCycleLastCutover =
  LifeCycleLastCutover'
    { initiated = Prelude.Nothing,
      reverted = Prelude.Nothing,
      finalized = Prelude.Nothing
    }

-- | Lifecycle last Cutover initiated.
lifeCycleLastCutover_initiated :: Lens.Lens' LifeCycleLastCutover (Prelude.Maybe LifeCycleLastCutoverInitiated)
lifeCycleLastCutover_initiated = Lens.lens (\LifeCycleLastCutover' {initiated} -> initiated) (\s@LifeCycleLastCutover' {} a -> s {initiated = a} :: LifeCycleLastCutover)

-- | Lifecycle last Cutover reverted.
lifeCycleLastCutover_reverted :: Lens.Lens' LifeCycleLastCutover (Prelude.Maybe LifeCycleLastCutoverReverted)
lifeCycleLastCutover_reverted = Lens.lens (\LifeCycleLastCutover' {reverted} -> reverted) (\s@LifeCycleLastCutover' {} a -> s {reverted = a} :: LifeCycleLastCutover)

-- | Lifecycle Cutover finalized date and time.
lifeCycleLastCutover_finalized :: Lens.Lens' LifeCycleLastCutover (Prelude.Maybe LifeCycleLastCutoverFinalized)
lifeCycleLastCutover_finalized = Lens.lens (\LifeCycleLastCutover' {finalized} -> finalized) (\s@LifeCycleLastCutover' {} a -> s {finalized = a} :: LifeCycleLastCutover)

instance Core.FromJSON LifeCycleLastCutover where
  parseJSON =
    Core.withObject
      "LifeCycleLastCutover"
      ( \x ->
          LifeCycleLastCutover'
            Prelude.<$> (x Core..:? "initiated")
            Prelude.<*> (x Core..:? "reverted")
            Prelude.<*> (x Core..:? "finalized")
      )

instance Prelude.Hashable LifeCycleLastCutover

instance Prelude.NFData LifeCycleLastCutover
