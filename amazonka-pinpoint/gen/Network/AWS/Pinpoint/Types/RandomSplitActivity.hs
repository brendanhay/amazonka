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
-- Module      : Network.AWS.Pinpoint.Types.RandomSplitActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.RandomSplitActivity where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.RandomSplitEntry
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the settings for a random split activity in a journey. This
-- type of activity randomly sends specified percentages of participants
-- down one of as many as five paths in a journey, based on conditions that
-- you specify.
--
-- /See:/ 'newRandomSplitActivity' smart constructor.
data RandomSplitActivity = RandomSplitActivity'
  { -- | The paths for the activity, including the percentage of participants to
    -- enter each path and the activity to perform for each path.
    branches :: Prelude.Maybe [RandomSplitEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RandomSplitActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branches', 'randomSplitActivity_branches' - The paths for the activity, including the percentage of participants to
-- enter each path and the activity to perform for each path.
newRandomSplitActivity ::
  RandomSplitActivity
newRandomSplitActivity =
  RandomSplitActivity' {branches = Prelude.Nothing}

-- | The paths for the activity, including the percentage of participants to
-- enter each path and the activity to perform for each path.
randomSplitActivity_branches :: Lens.Lens' RandomSplitActivity (Prelude.Maybe [RandomSplitEntry])
randomSplitActivity_branches = Lens.lens (\RandomSplitActivity' {branches} -> branches) (\s@RandomSplitActivity' {} a -> s {branches = a} :: RandomSplitActivity) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON RandomSplitActivity where
  parseJSON =
    Prelude.withObject
      "RandomSplitActivity"
      ( \x ->
          RandomSplitActivity'
            Prelude.<$> ( x Prelude..:? "Branches"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable RandomSplitActivity

instance Prelude.NFData RandomSplitActivity

instance Prelude.ToJSON RandomSplitActivity where
  toJSON RandomSplitActivity' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Branches" Prelude..=) Prelude.<$> branches]
      )
