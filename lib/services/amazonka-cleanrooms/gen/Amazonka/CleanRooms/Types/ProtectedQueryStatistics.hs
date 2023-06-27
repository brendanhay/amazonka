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
-- Module      : Amazonka.CleanRooms.Types.ProtectedQueryStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.ProtectedQueryStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains statistics about the execution of the protected query.
--
-- /See:/ 'newProtectedQueryStatistics' smart constructor.
data ProtectedQueryStatistics = ProtectedQueryStatistics'
  { -- | The duration of the Protected Query, from creation until query
    -- completion.
    totalDurationInMillis :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProtectedQueryStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'totalDurationInMillis', 'protectedQueryStatistics_totalDurationInMillis' - The duration of the Protected Query, from creation until query
-- completion.
newProtectedQueryStatistics ::
  ProtectedQueryStatistics
newProtectedQueryStatistics =
  ProtectedQueryStatistics'
    { totalDurationInMillis =
        Prelude.Nothing
    }

-- | The duration of the Protected Query, from creation until query
-- completion.
protectedQueryStatistics_totalDurationInMillis :: Lens.Lens' ProtectedQueryStatistics (Prelude.Maybe Prelude.Integer)
protectedQueryStatistics_totalDurationInMillis = Lens.lens (\ProtectedQueryStatistics' {totalDurationInMillis} -> totalDurationInMillis) (\s@ProtectedQueryStatistics' {} a -> s {totalDurationInMillis = a} :: ProtectedQueryStatistics)

instance Data.FromJSON ProtectedQueryStatistics where
  parseJSON =
    Data.withObject
      "ProtectedQueryStatistics"
      ( \x ->
          ProtectedQueryStatistics'
            Prelude.<$> (x Data..:? "totalDurationInMillis")
      )

instance Prelude.Hashable ProtectedQueryStatistics where
  hashWithSalt _salt ProtectedQueryStatistics' {..} =
    _salt `Prelude.hashWithSalt` totalDurationInMillis

instance Prelude.NFData ProtectedQueryStatistics where
  rnf ProtectedQueryStatistics' {..} =
    Prelude.rnf totalDurationInMillis
