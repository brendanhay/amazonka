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
-- Module      : Amazonka.RBin.Types.UnlockDelay
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RBin.Types.UnlockDelay where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RBin.Types.UnlockDelayUnit

-- | Information about the retention rule unlock delay. The unlock delay is
-- the period after which a retention rule can be modified or edited after
-- it has been unlocked by a user with the required permissions. The
-- retention rule can\'t be modified or deleted during the unlock delay.
--
-- /See:/ 'newUnlockDelay' smart constructor.
data UnlockDelay = UnlockDelay'
  { -- | The unlock delay period, measured in the unit specified for
    -- __UnlockDelayUnit__.
    unlockDelayValue :: Prelude.Natural,
    -- | The unit of time in which to measure the unlock delay. Currently, the
    -- unlock delay can be measure only in days.
    unlockDelayUnit :: UnlockDelayUnit
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnlockDelay' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unlockDelayValue', 'unlockDelay_unlockDelayValue' - The unlock delay period, measured in the unit specified for
-- __UnlockDelayUnit__.
--
-- 'unlockDelayUnit', 'unlockDelay_unlockDelayUnit' - The unit of time in which to measure the unlock delay. Currently, the
-- unlock delay can be measure only in days.
newUnlockDelay ::
  -- | 'unlockDelayValue'
  Prelude.Natural ->
  -- | 'unlockDelayUnit'
  UnlockDelayUnit ->
  UnlockDelay
newUnlockDelay pUnlockDelayValue_ pUnlockDelayUnit_ =
  UnlockDelay'
    { unlockDelayValue = pUnlockDelayValue_,
      unlockDelayUnit = pUnlockDelayUnit_
    }

-- | The unlock delay period, measured in the unit specified for
-- __UnlockDelayUnit__.
unlockDelay_unlockDelayValue :: Lens.Lens' UnlockDelay Prelude.Natural
unlockDelay_unlockDelayValue = Lens.lens (\UnlockDelay' {unlockDelayValue} -> unlockDelayValue) (\s@UnlockDelay' {} a -> s {unlockDelayValue = a} :: UnlockDelay)

-- | The unit of time in which to measure the unlock delay. Currently, the
-- unlock delay can be measure only in days.
unlockDelay_unlockDelayUnit :: Lens.Lens' UnlockDelay UnlockDelayUnit
unlockDelay_unlockDelayUnit = Lens.lens (\UnlockDelay' {unlockDelayUnit} -> unlockDelayUnit) (\s@UnlockDelay' {} a -> s {unlockDelayUnit = a} :: UnlockDelay)

instance Data.FromJSON UnlockDelay where
  parseJSON =
    Data.withObject
      "UnlockDelay"
      ( \x ->
          UnlockDelay'
            Prelude.<$> (x Data..: "UnlockDelayValue")
            Prelude.<*> (x Data..: "UnlockDelayUnit")
      )

instance Prelude.Hashable UnlockDelay where
  hashWithSalt _salt UnlockDelay' {..} =
    _salt `Prelude.hashWithSalt` unlockDelayValue
      `Prelude.hashWithSalt` unlockDelayUnit

instance Prelude.NFData UnlockDelay where
  rnf UnlockDelay' {..} =
    Prelude.rnf unlockDelayValue
      `Prelude.seq` Prelude.rnf unlockDelayUnit

instance Data.ToJSON UnlockDelay where
  toJSON UnlockDelay' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("UnlockDelayValue" Data..= unlockDelayValue),
            Prelude.Just
              ("UnlockDelayUnit" Data..= unlockDelayUnit)
          ]
      )
