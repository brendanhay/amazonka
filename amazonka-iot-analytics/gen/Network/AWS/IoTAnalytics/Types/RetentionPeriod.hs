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
-- Module      : Network.AWS.IoTAnalytics.Types.RetentionPeriod
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.RetentionPeriod where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | How long, in days, message data is kept.
--
-- /See:/ 'newRetentionPeriod' smart constructor.
data RetentionPeriod = RetentionPeriod'
  { -- | The number of days that message data is kept. The @unlimited@ parameter
    -- must be false.
    numberOfDays :: Prelude.Maybe Prelude.Natural,
    -- | If true, message data is kept indefinitely.
    unlimited :: Prelude.Maybe Prelude.Bool
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
-- 'numberOfDays', 'retentionPeriod_numberOfDays' - The number of days that message data is kept. The @unlimited@ parameter
-- must be false.
--
-- 'unlimited', 'retentionPeriod_unlimited' - If true, message data is kept indefinitely.
newRetentionPeriod ::
  RetentionPeriod
newRetentionPeriod =
  RetentionPeriod'
    { numberOfDays = Prelude.Nothing,
      unlimited = Prelude.Nothing
    }

-- | The number of days that message data is kept. The @unlimited@ parameter
-- must be false.
retentionPeriod_numberOfDays :: Lens.Lens' RetentionPeriod (Prelude.Maybe Prelude.Natural)
retentionPeriod_numberOfDays = Lens.lens (\RetentionPeriod' {numberOfDays} -> numberOfDays) (\s@RetentionPeriod' {} a -> s {numberOfDays = a} :: RetentionPeriod)

-- | If true, message data is kept indefinitely.
retentionPeriod_unlimited :: Lens.Lens' RetentionPeriod (Prelude.Maybe Prelude.Bool)
retentionPeriod_unlimited = Lens.lens (\RetentionPeriod' {unlimited} -> unlimited) (\s@RetentionPeriod' {} a -> s {unlimited = a} :: RetentionPeriod)

instance Core.FromJSON RetentionPeriod where
  parseJSON =
    Core.withObject
      "RetentionPeriod"
      ( \x ->
          RetentionPeriod'
            Prelude.<$> (x Core..:? "numberOfDays")
            Prelude.<*> (x Core..:? "unlimited")
      )

instance Prelude.Hashable RetentionPeriod

instance Prelude.NFData RetentionPeriod

instance Core.ToJSON RetentionPeriod where
  toJSON RetentionPeriod' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("numberOfDays" Core..=) Prelude.<$> numberOfDays,
            ("unlimited" Core..=) Prelude.<$> unlimited
          ]
      )
