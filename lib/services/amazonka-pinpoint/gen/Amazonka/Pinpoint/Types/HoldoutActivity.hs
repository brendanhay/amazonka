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
-- Module      : Amazonka.Pinpoint.Types.HoldoutActivity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.HoldoutActivity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the settings for a holdout activity in a journey. This type of
-- activity stops a journey for a specified percentage of participants.
--
-- /See:/ 'newHoldoutActivity' smart constructor.
data HoldoutActivity = HoldoutActivity'
  { -- | The unique identifier for the next activity to perform, after performing
    -- the holdout activity.
    nextActivity :: Prelude.Maybe Prelude.Text,
    -- | The percentage of participants who shouldn\'t continue the journey.
    --
    -- To determine which participants are held out, Amazon Pinpoint applies a
    -- probability-based algorithm to the percentage that you specify.
    -- Therefore, the actual percentage of participants who are held out may
    -- not be equal to the percentage that you specify.
    percentage :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HoldoutActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextActivity', 'holdoutActivity_nextActivity' - The unique identifier for the next activity to perform, after performing
-- the holdout activity.
--
-- 'percentage', 'holdoutActivity_percentage' - The percentage of participants who shouldn\'t continue the journey.
--
-- To determine which participants are held out, Amazon Pinpoint applies a
-- probability-based algorithm to the percentage that you specify.
-- Therefore, the actual percentage of participants who are held out may
-- not be equal to the percentage that you specify.
newHoldoutActivity ::
  -- | 'percentage'
  Prelude.Int ->
  HoldoutActivity
newHoldoutActivity pPercentage_ =
  HoldoutActivity'
    { nextActivity = Prelude.Nothing,
      percentage = pPercentage_
    }

-- | The unique identifier for the next activity to perform, after performing
-- the holdout activity.
holdoutActivity_nextActivity :: Lens.Lens' HoldoutActivity (Prelude.Maybe Prelude.Text)
holdoutActivity_nextActivity = Lens.lens (\HoldoutActivity' {nextActivity} -> nextActivity) (\s@HoldoutActivity' {} a -> s {nextActivity = a} :: HoldoutActivity)

-- | The percentage of participants who shouldn\'t continue the journey.
--
-- To determine which participants are held out, Amazon Pinpoint applies a
-- probability-based algorithm to the percentage that you specify.
-- Therefore, the actual percentage of participants who are held out may
-- not be equal to the percentage that you specify.
holdoutActivity_percentage :: Lens.Lens' HoldoutActivity Prelude.Int
holdoutActivity_percentage = Lens.lens (\HoldoutActivity' {percentage} -> percentage) (\s@HoldoutActivity' {} a -> s {percentage = a} :: HoldoutActivity)

instance Data.FromJSON HoldoutActivity where
  parseJSON =
    Data.withObject
      "HoldoutActivity"
      ( \x ->
          HoldoutActivity'
            Prelude.<$> (x Data..:? "NextActivity")
            Prelude.<*> (x Data..: "Percentage")
      )

instance Prelude.Hashable HoldoutActivity where
  hashWithSalt _salt HoldoutActivity' {..} =
    _salt
      `Prelude.hashWithSalt` nextActivity
      `Prelude.hashWithSalt` percentage

instance Prelude.NFData HoldoutActivity where
  rnf HoldoutActivity' {..} =
    Prelude.rnf nextActivity
      `Prelude.seq` Prelude.rnf percentage

instance Data.ToJSON HoldoutActivity where
  toJSON HoldoutActivity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextActivity" Data..=) Prelude.<$> nextActivity,
            Prelude.Just ("Percentage" Data..= percentage)
          ]
      )
