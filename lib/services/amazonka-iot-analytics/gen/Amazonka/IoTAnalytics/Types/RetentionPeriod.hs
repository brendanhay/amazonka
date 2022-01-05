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
-- Module      : Amazonka.IoTAnalytics.Types.RetentionPeriod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.RetentionPeriod where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | How long, in days, message data is kept.
--
-- /See:/ 'newRetentionPeriod' smart constructor.
data RetentionPeriod = RetentionPeriod'
  { -- | If true, message data is kept indefinitely.
    unlimited :: Prelude.Maybe Prelude.Bool,
    -- | The number of days that message data is kept. The @unlimited@ parameter
    -- must be false.
    numberOfDays :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RetentionPeriod' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unlimited', 'retentionPeriod_unlimited' - If true, message data is kept indefinitely.
--
-- 'numberOfDays', 'retentionPeriod_numberOfDays' - The number of days that message data is kept. The @unlimited@ parameter
-- must be false.
newRetentionPeriod ::
  RetentionPeriod
newRetentionPeriod =
  RetentionPeriod'
    { unlimited = Prelude.Nothing,
      numberOfDays = Prelude.Nothing
    }

-- | If true, message data is kept indefinitely.
retentionPeriod_unlimited :: Lens.Lens' RetentionPeriod (Prelude.Maybe Prelude.Bool)
retentionPeriod_unlimited = Lens.lens (\RetentionPeriod' {unlimited} -> unlimited) (\s@RetentionPeriod' {} a -> s {unlimited = a} :: RetentionPeriod)

-- | The number of days that message data is kept. The @unlimited@ parameter
-- must be false.
retentionPeriod_numberOfDays :: Lens.Lens' RetentionPeriod (Prelude.Maybe Prelude.Natural)
retentionPeriod_numberOfDays = Lens.lens (\RetentionPeriod' {numberOfDays} -> numberOfDays) (\s@RetentionPeriod' {} a -> s {numberOfDays = a} :: RetentionPeriod)

instance Core.FromJSON RetentionPeriod where
  parseJSON =
    Core.withObject
      "RetentionPeriod"
      ( \x ->
          RetentionPeriod'
            Prelude.<$> (x Core..:? "unlimited")
            Prelude.<*> (x Core..:? "numberOfDays")
      )

instance Prelude.Hashable RetentionPeriod where
  hashWithSalt _salt RetentionPeriod' {..} =
    _salt `Prelude.hashWithSalt` unlimited
      `Prelude.hashWithSalt` numberOfDays

instance Prelude.NFData RetentionPeriod where
  rnf RetentionPeriod' {..} =
    Prelude.rnf unlimited
      `Prelude.seq` Prelude.rnf numberOfDays

instance Core.ToJSON RetentionPeriod where
  toJSON RetentionPeriod' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("unlimited" Core..=) Prelude.<$> unlimited,
            ("numberOfDays" Core..=) Prelude.<$> numberOfDays
          ]
      )
