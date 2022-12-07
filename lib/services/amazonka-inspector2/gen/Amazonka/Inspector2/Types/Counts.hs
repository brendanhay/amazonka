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
-- Module      : Amazonka.Inspector2.Types.Counts
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.Counts where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.GroupKey
import qualified Amazonka.Prelude as Prelude

-- | a structure that contains information on the count of resources within a
-- group.
--
-- /See:/ 'newCounts' smart constructor.
data Counts = Counts'
  { -- | The key associated with this group
    groupKey :: Prelude.Maybe GroupKey,
    -- | The number of resources.
    count :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Counts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupKey', 'counts_groupKey' - The key associated with this group
--
-- 'count', 'counts_count' - The number of resources.
newCounts ::
  Counts
newCounts =
  Counts'
    { groupKey = Prelude.Nothing,
      count = Prelude.Nothing
    }

-- | The key associated with this group
counts_groupKey :: Lens.Lens' Counts (Prelude.Maybe GroupKey)
counts_groupKey = Lens.lens (\Counts' {groupKey} -> groupKey) (\s@Counts' {} a -> s {groupKey = a} :: Counts)

-- | The number of resources.
counts_count :: Lens.Lens' Counts (Prelude.Maybe Prelude.Integer)
counts_count = Lens.lens (\Counts' {count} -> count) (\s@Counts' {} a -> s {count = a} :: Counts)

instance Data.FromJSON Counts where
  parseJSON =
    Data.withObject
      "Counts"
      ( \x ->
          Counts'
            Prelude.<$> (x Data..:? "groupKey")
            Prelude.<*> (x Data..:? "count")
      )

instance Prelude.Hashable Counts where
  hashWithSalt _salt Counts' {..} =
    _salt `Prelude.hashWithSalt` groupKey
      `Prelude.hashWithSalt` count

instance Prelude.NFData Counts where
  rnf Counts' {..} =
    Prelude.rnf groupKey
      `Prelude.seq` Prelude.rnf count
