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
-- Module      : Network.AWS.Pinpoint.Types.HoldoutActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.HoldoutActivity where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON HoldoutActivity where
  parseJSON =
    Prelude.withObject
      "HoldoutActivity"
      ( \x ->
          HoldoutActivity'
            Prelude.<$> (x Prelude..:? "NextActivity")
            Prelude.<*> (x Prelude..: "Percentage")
      )

instance Prelude.Hashable HoldoutActivity

instance Prelude.NFData HoldoutActivity

instance Prelude.ToJSON HoldoutActivity where
  toJSON HoldoutActivity' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextActivity" Prelude..=)
              Prelude.<$> nextActivity,
            Prelude.Just ("Percentage" Prelude..= percentage)
          ]
      )
