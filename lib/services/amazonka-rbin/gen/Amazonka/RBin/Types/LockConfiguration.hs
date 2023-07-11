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
-- Module      : Amazonka.RBin.Types.LockConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RBin.Types.LockConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RBin.Types.UnlockDelay

-- | Information about a retention rule lock configuration.
--
-- /See:/ 'newLockConfiguration' smart constructor.
data LockConfiguration = LockConfiguration'
  { -- | Information about the retention rule unlock delay.
    unlockDelay :: UnlockDelay
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LockConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unlockDelay', 'lockConfiguration_unlockDelay' - Information about the retention rule unlock delay.
newLockConfiguration ::
  -- | 'unlockDelay'
  UnlockDelay ->
  LockConfiguration
newLockConfiguration pUnlockDelay_ =
  LockConfiguration' {unlockDelay = pUnlockDelay_}

-- | Information about the retention rule unlock delay.
lockConfiguration_unlockDelay :: Lens.Lens' LockConfiguration UnlockDelay
lockConfiguration_unlockDelay = Lens.lens (\LockConfiguration' {unlockDelay} -> unlockDelay) (\s@LockConfiguration' {} a -> s {unlockDelay = a} :: LockConfiguration)

instance Data.FromJSON LockConfiguration where
  parseJSON =
    Data.withObject
      "LockConfiguration"
      ( \x ->
          LockConfiguration'
            Prelude.<$> (x Data..: "UnlockDelay")
      )

instance Prelude.Hashable LockConfiguration where
  hashWithSalt _salt LockConfiguration' {..} =
    _salt `Prelude.hashWithSalt` unlockDelay

instance Prelude.NFData LockConfiguration where
  rnf LockConfiguration' {..} = Prelude.rnf unlockDelay

instance Data.ToJSON LockConfiguration where
  toJSON LockConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("UnlockDelay" Data..= unlockDelay)]
      )
