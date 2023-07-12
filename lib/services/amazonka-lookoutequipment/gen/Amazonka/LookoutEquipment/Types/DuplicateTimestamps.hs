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
-- Module      : Amazonka.LookoutEquipment.Types.DuplicateTimestamps
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.DuplicateTimestamps where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Entity that comprises information abount duplicate timestamps in the
-- dataset.
--
-- /See:/ 'newDuplicateTimestamps' smart constructor.
data DuplicateTimestamps = DuplicateTimestamps'
  { -- | Indicates the total number of duplicate timestamps.
    totalNumberOfDuplicateTimestamps :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DuplicateTimestamps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalNumberOfDuplicateTimestamps', 'duplicateTimestamps_totalNumberOfDuplicateTimestamps' - Indicates the total number of duplicate timestamps.
newDuplicateTimestamps ::
  -- | 'totalNumberOfDuplicateTimestamps'
  Prelude.Int ->
  DuplicateTimestamps
newDuplicateTimestamps
  pTotalNumberOfDuplicateTimestamps_ =
    DuplicateTimestamps'
      { totalNumberOfDuplicateTimestamps =
          pTotalNumberOfDuplicateTimestamps_
      }

-- | Indicates the total number of duplicate timestamps.
duplicateTimestamps_totalNumberOfDuplicateTimestamps :: Lens.Lens' DuplicateTimestamps Prelude.Int
duplicateTimestamps_totalNumberOfDuplicateTimestamps = Lens.lens (\DuplicateTimestamps' {totalNumberOfDuplicateTimestamps} -> totalNumberOfDuplicateTimestamps) (\s@DuplicateTimestamps' {} a -> s {totalNumberOfDuplicateTimestamps = a} :: DuplicateTimestamps)

instance Data.FromJSON DuplicateTimestamps where
  parseJSON =
    Data.withObject
      "DuplicateTimestamps"
      ( \x ->
          DuplicateTimestamps'
            Prelude.<$> (x Data..: "TotalNumberOfDuplicateTimestamps")
      )

instance Prelude.Hashable DuplicateTimestamps where
  hashWithSalt _salt DuplicateTimestamps' {..} =
    _salt
      `Prelude.hashWithSalt` totalNumberOfDuplicateTimestamps

instance Prelude.NFData DuplicateTimestamps where
  rnf DuplicateTimestamps' {..} =
    Prelude.rnf totalNumberOfDuplicateTimestamps
