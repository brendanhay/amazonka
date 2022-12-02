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
-- Module      : Amazonka.LookoutEquipment.Types.UnsupportedTimestamps
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.UnsupportedTimestamps where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Entity that comprises information abount unsupported timestamps in the
-- dataset.
--
-- /See:/ 'newUnsupportedTimestamps' smart constructor.
data UnsupportedTimestamps = UnsupportedTimestamps'
  { -- | Indicates the total number of unsupported timestamps across the ingested
    -- data.
    totalNumberOfUnsupportedTimestamps :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UnsupportedTimestamps' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalNumberOfUnsupportedTimestamps', 'unsupportedTimestamps_totalNumberOfUnsupportedTimestamps' - Indicates the total number of unsupported timestamps across the ingested
-- data.
newUnsupportedTimestamps ::
  -- | 'totalNumberOfUnsupportedTimestamps'
  Prelude.Int ->
  UnsupportedTimestamps
newUnsupportedTimestamps
  pTotalNumberOfUnsupportedTimestamps_ =
    UnsupportedTimestamps'
      { totalNumberOfUnsupportedTimestamps =
          pTotalNumberOfUnsupportedTimestamps_
      }

-- | Indicates the total number of unsupported timestamps across the ingested
-- data.
unsupportedTimestamps_totalNumberOfUnsupportedTimestamps :: Lens.Lens' UnsupportedTimestamps Prelude.Int
unsupportedTimestamps_totalNumberOfUnsupportedTimestamps = Lens.lens (\UnsupportedTimestamps' {totalNumberOfUnsupportedTimestamps} -> totalNumberOfUnsupportedTimestamps) (\s@UnsupportedTimestamps' {} a -> s {totalNumberOfUnsupportedTimestamps = a} :: UnsupportedTimestamps)

instance Data.FromJSON UnsupportedTimestamps where
  parseJSON =
    Data.withObject
      "UnsupportedTimestamps"
      ( \x ->
          UnsupportedTimestamps'
            Prelude.<$> (x Data..: "TotalNumberOfUnsupportedTimestamps")
      )

instance Prelude.Hashable UnsupportedTimestamps where
  hashWithSalt _salt UnsupportedTimestamps' {..} =
    _salt
      `Prelude.hashWithSalt` totalNumberOfUnsupportedTimestamps

instance Prelude.NFData UnsupportedTimestamps where
  rnf UnsupportedTimestamps' {..} =
    Prelude.rnf totalNumberOfUnsupportedTimestamps
