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
-- Module      : Amazonka.MGN.Types.LifeCycleLastTest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.LifeCycleLastTest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.LifeCycleLastTestFinalized
import Amazonka.MGN.Types.LifeCycleLastTestInitiated
import Amazonka.MGN.Types.LifeCycleLastTestReverted
import qualified Amazonka.Prelude as Prelude

-- | Lifecycle last Test.
--
-- /See:/ 'newLifeCycleLastTest' smart constructor.
data LifeCycleLastTest = LifeCycleLastTest'
  { -- | Lifecycle last Test finalized.
    finalized :: Prelude.Maybe LifeCycleLastTestFinalized,
    -- | Lifecycle last Test initiated.
    initiated :: Prelude.Maybe LifeCycleLastTestInitiated,
    -- | Lifecycle last Test reverted.
    reverted :: Prelude.Maybe LifeCycleLastTestReverted
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifeCycleLastTest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finalized', 'lifeCycleLastTest_finalized' - Lifecycle last Test finalized.
--
-- 'initiated', 'lifeCycleLastTest_initiated' - Lifecycle last Test initiated.
--
-- 'reverted', 'lifeCycleLastTest_reverted' - Lifecycle last Test reverted.
newLifeCycleLastTest ::
  LifeCycleLastTest
newLifeCycleLastTest =
  LifeCycleLastTest'
    { finalized = Prelude.Nothing,
      initiated = Prelude.Nothing,
      reverted = Prelude.Nothing
    }

-- | Lifecycle last Test finalized.
lifeCycleLastTest_finalized :: Lens.Lens' LifeCycleLastTest (Prelude.Maybe LifeCycleLastTestFinalized)
lifeCycleLastTest_finalized = Lens.lens (\LifeCycleLastTest' {finalized} -> finalized) (\s@LifeCycleLastTest' {} a -> s {finalized = a} :: LifeCycleLastTest)

-- | Lifecycle last Test initiated.
lifeCycleLastTest_initiated :: Lens.Lens' LifeCycleLastTest (Prelude.Maybe LifeCycleLastTestInitiated)
lifeCycleLastTest_initiated = Lens.lens (\LifeCycleLastTest' {initiated} -> initiated) (\s@LifeCycleLastTest' {} a -> s {initiated = a} :: LifeCycleLastTest)

-- | Lifecycle last Test reverted.
lifeCycleLastTest_reverted :: Lens.Lens' LifeCycleLastTest (Prelude.Maybe LifeCycleLastTestReverted)
lifeCycleLastTest_reverted = Lens.lens (\LifeCycleLastTest' {reverted} -> reverted) (\s@LifeCycleLastTest' {} a -> s {reverted = a} :: LifeCycleLastTest)

instance Data.FromJSON LifeCycleLastTest where
  parseJSON =
    Data.withObject
      "LifeCycleLastTest"
      ( \x ->
          LifeCycleLastTest'
            Prelude.<$> (x Data..:? "finalized")
            Prelude.<*> (x Data..:? "initiated")
            Prelude.<*> (x Data..:? "reverted")
      )

instance Prelude.Hashable LifeCycleLastTest where
  hashWithSalt _salt LifeCycleLastTest' {..} =
    _salt
      `Prelude.hashWithSalt` finalized
      `Prelude.hashWithSalt` initiated
      `Prelude.hashWithSalt` reverted

instance Prelude.NFData LifeCycleLastTest where
  rnf LifeCycleLastTest' {..} =
    Prelude.rnf finalized
      `Prelude.seq` Prelude.rnf initiated
      `Prelude.seq` Prelude.rnf reverted
