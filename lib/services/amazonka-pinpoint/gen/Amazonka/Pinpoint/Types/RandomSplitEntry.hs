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
-- Module      : Amazonka.Pinpoint.Types.RandomSplitEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.RandomSplitEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the settings for a path in a random split activity in a
-- journey.
--
-- /See:/ 'newRandomSplitEntry' smart constructor.
data RandomSplitEntry = RandomSplitEntry'
  { -- | The unique identifier for the next activity to perform, after completing
    -- the activity for the path.
    nextActivity :: Prelude.Maybe Prelude.Text,
    -- | The percentage of participants to send down the activity path.
    --
    -- To determine which participants are sent down each path, Amazon Pinpoint
    -- applies a probability-based algorithm to the percentages that you
    -- specify for the paths. Therefore, the actual percentage of participants
    -- who are sent down a path may not be equal to the percentage that you
    -- specify.
    percentage :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RandomSplitEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextActivity', 'randomSplitEntry_nextActivity' - The unique identifier for the next activity to perform, after completing
-- the activity for the path.
--
-- 'percentage', 'randomSplitEntry_percentage' - The percentage of participants to send down the activity path.
--
-- To determine which participants are sent down each path, Amazon Pinpoint
-- applies a probability-based algorithm to the percentages that you
-- specify for the paths. Therefore, the actual percentage of participants
-- who are sent down a path may not be equal to the percentage that you
-- specify.
newRandomSplitEntry ::
  RandomSplitEntry
newRandomSplitEntry =
  RandomSplitEntry'
    { nextActivity = Prelude.Nothing,
      percentage = Prelude.Nothing
    }

-- | The unique identifier for the next activity to perform, after completing
-- the activity for the path.
randomSplitEntry_nextActivity :: Lens.Lens' RandomSplitEntry (Prelude.Maybe Prelude.Text)
randomSplitEntry_nextActivity = Lens.lens (\RandomSplitEntry' {nextActivity} -> nextActivity) (\s@RandomSplitEntry' {} a -> s {nextActivity = a} :: RandomSplitEntry)

-- | The percentage of participants to send down the activity path.
--
-- To determine which participants are sent down each path, Amazon Pinpoint
-- applies a probability-based algorithm to the percentages that you
-- specify for the paths. Therefore, the actual percentage of participants
-- who are sent down a path may not be equal to the percentage that you
-- specify.
randomSplitEntry_percentage :: Lens.Lens' RandomSplitEntry (Prelude.Maybe Prelude.Int)
randomSplitEntry_percentage = Lens.lens (\RandomSplitEntry' {percentage} -> percentage) (\s@RandomSplitEntry' {} a -> s {percentage = a} :: RandomSplitEntry)

instance Data.FromJSON RandomSplitEntry where
  parseJSON =
    Data.withObject
      "RandomSplitEntry"
      ( \x ->
          RandomSplitEntry'
            Prelude.<$> (x Data..:? "NextActivity")
            Prelude.<*> (x Data..:? "Percentage")
      )

instance Prelude.Hashable RandomSplitEntry where
  hashWithSalt _salt RandomSplitEntry' {..} =
    _salt
      `Prelude.hashWithSalt` nextActivity
      `Prelude.hashWithSalt` percentage

instance Prelude.NFData RandomSplitEntry where
  rnf RandomSplitEntry' {..} =
    Prelude.rnf nextActivity
      `Prelude.seq` Prelude.rnf percentage

instance Data.ToJSON RandomSplitEntry where
  toJSON RandomSplitEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextActivity" Data..=) Prelude.<$> nextActivity,
            ("Percentage" Data..=) Prelude.<$> percentage
          ]
      )
