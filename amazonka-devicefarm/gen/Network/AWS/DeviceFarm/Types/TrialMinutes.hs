{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DeviceFarm.Types.TrialMinutes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.TrialMinutes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents information about free trial device minutes for an AWS
-- account.
--
-- /See:/ 'newTrialMinutes' smart constructor.
data TrialMinutes = TrialMinutes'
  { -- | The total number of free trial minutes that the account started with.
    total :: Prelude.Maybe Prelude.Double,
    -- | The number of free trial minutes remaining in the account.
    remaining :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TrialMinutes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'total', 'trialMinutes_total' - The total number of free trial minutes that the account started with.
--
-- 'remaining', 'trialMinutes_remaining' - The number of free trial minutes remaining in the account.
newTrialMinutes ::
  TrialMinutes
newTrialMinutes =
  TrialMinutes'
    { total = Prelude.Nothing,
      remaining = Prelude.Nothing
    }

-- | The total number of free trial minutes that the account started with.
trialMinutes_total :: Lens.Lens' TrialMinutes (Prelude.Maybe Prelude.Double)
trialMinutes_total = Lens.lens (\TrialMinutes' {total} -> total) (\s@TrialMinutes' {} a -> s {total = a} :: TrialMinutes)

-- | The number of free trial minutes remaining in the account.
trialMinutes_remaining :: Lens.Lens' TrialMinutes (Prelude.Maybe Prelude.Double)
trialMinutes_remaining = Lens.lens (\TrialMinutes' {remaining} -> remaining) (\s@TrialMinutes' {} a -> s {remaining = a} :: TrialMinutes)

instance Prelude.FromJSON TrialMinutes where
  parseJSON =
    Prelude.withObject
      "TrialMinutes"
      ( \x ->
          TrialMinutes'
            Prelude.<$> (x Prelude..:? "total")
            Prelude.<*> (x Prelude..:? "remaining")
      )

instance Prelude.Hashable TrialMinutes

instance Prelude.NFData TrialMinutes
