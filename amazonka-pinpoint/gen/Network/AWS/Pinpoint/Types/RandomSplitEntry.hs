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
-- Module      : Network.AWS.Pinpoint.Types.RandomSplitEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.RandomSplitEntry where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the settings for a path in a random split activity in a
-- journey.
--
-- /See:/ 'newRandomSplitEntry' smart constructor.
data RandomSplitEntry = RandomSplitEntry'
  { -- | The percentage of participants to send down the activity path.
    --
    -- To determine which participants are sent down each path, Amazon Pinpoint
    -- applies a probability-based algorithm to the percentages that you
    -- specify for the paths. Therefore, the actual percentage of participants
    -- who are sent down a path may not be equal to the percentage that you
    -- specify.
    percentage :: Prelude.Maybe Prelude.Int,
    -- | The unique identifier for the next activity to perform, after completing
    -- the activity for the path.
    nextActivity :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RandomSplitEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'percentage', 'randomSplitEntry_percentage' - The percentage of participants to send down the activity path.
--
-- To determine which participants are sent down each path, Amazon Pinpoint
-- applies a probability-based algorithm to the percentages that you
-- specify for the paths. Therefore, the actual percentage of participants
-- who are sent down a path may not be equal to the percentage that you
-- specify.
--
-- 'nextActivity', 'randomSplitEntry_nextActivity' - The unique identifier for the next activity to perform, after completing
-- the activity for the path.
newRandomSplitEntry ::
  RandomSplitEntry
newRandomSplitEntry =
  RandomSplitEntry'
    { percentage = Prelude.Nothing,
      nextActivity = Prelude.Nothing
    }

-- | The percentage of participants to send down the activity path.
--
-- To determine which participants are sent down each path, Amazon Pinpoint
-- applies a probability-based algorithm to the percentages that you
-- specify for the paths. Therefore, the actual percentage of participants
-- who are sent down a path may not be equal to the percentage that you
-- specify.
randomSplitEntry_percentage :: Lens.Lens' RandomSplitEntry (Prelude.Maybe Prelude.Int)
randomSplitEntry_percentage = Lens.lens (\RandomSplitEntry' {percentage} -> percentage) (\s@RandomSplitEntry' {} a -> s {percentage = a} :: RandomSplitEntry)

-- | The unique identifier for the next activity to perform, after completing
-- the activity for the path.
randomSplitEntry_nextActivity :: Lens.Lens' RandomSplitEntry (Prelude.Maybe Prelude.Text)
randomSplitEntry_nextActivity = Lens.lens (\RandomSplitEntry' {nextActivity} -> nextActivity) (\s@RandomSplitEntry' {} a -> s {nextActivity = a} :: RandomSplitEntry)

instance Prelude.FromJSON RandomSplitEntry where
  parseJSON =
    Prelude.withObject
      "RandomSplitEntry"
      ( \x ->
          RandomSplitEntry'
            Prelude.<$> (x Prelude..:? "Percentage")
            Prelude.<*> (x Prelude..:? "NextActivity")
      )

instance Prelude.Hashable RandomSplitEntry

instance Prelude.NFData RandomSplitEntry

instance Prelude.ToJSON RandomSplitEntry where
  toJSON RandomSplitEntry' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Percentage" Prelude..=) Prelude.<$> percentage,
            ("NextActivity" Prelude..=)
              Prelude.<$> nextActivity
          ]
      )
