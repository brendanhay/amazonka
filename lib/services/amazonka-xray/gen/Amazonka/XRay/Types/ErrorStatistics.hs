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
-- Module      : Amazonka.XRay.Types.ErrorStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.ErrorStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about requests that failed with a 4xx Client Error status
-- code.
--
-- /See:/ 'newErrorStatistics' smart constructor.
data ErrorStatistics = ErrorStatistics'
  { -- | The number of requests that failed with untracked 4xx Client Error
    -- status codes.
    otherCount :: Prelude.Maybe Prelude.Integer,
    -- | The total number of requests that failed with a 4xx Client Error status
    -- code.
    totalCount :: Prelude.Maybe Prelude.Integer,
    -- | The number of requests that failed with a 419 throttling status code.
    throttleCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ErrorStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'otherCount', 'errorStatistics_otherCount' - The number of requests that failed with untracked 4xx Client Error
-- status codes.
--
-- 'totalCount', 'errorStatistics_totalCount' - The total number of requests that failed with a 4xx Client Error status
-- code.
--
-- 'throttleCount', 'errorStatistics_throttleCount' - The number of requests that failed with a 419 throttling status code.
newErrorStatistics ::
  ErrorStatistics
newErrorStatistics =
  ErrorStatistics'
    { otherCount = Prelude.Nothing,
      totalCount = Prelude.Nothing,
      throttleCount = Prelude.Nothing
    }

-- | The number of requests that failed with untracked 4xx Client Error
-- status codes.
errorStatistics_otherCount :: Lens.Lens' ErrorStatistics (Prelude.Maybe Prelude.Integer)
errorStatistics_otherCount = Lens.lens (\ErrorStatistics' {otherCount} -> otherCount) (\s@ErrorStatistics' {} a -> s {otherCount = a} :: ErrorStatistics)

-- | The total number of requests that failed with a 4xx Client Error status
-- code.
errorStatistics_totalCount :: Lens.Lens' ErrorStatistics (Prelude.Maybe Prelude.Integer)
errorStatistics_totalCount = Lens.lens (\ErrorStatistics' {totalCount} -> totalCount) (\s@ErrorStatistics' {} a -> s {totalCount = a} :: ErrorStatistics)

-- | The number of requests that failed with a 419 throttling status code.
errorStatistics_throttleCount :: Lens.Lens' ErrorStatistics (Prelude.Maybe Prelude.Integer)
errorStatistics_throttleCount = Lens.lens (\ErrorStatistics' {throttleCount} -> throttleCount) (\s@ErrorStatistics' {} a -> s {throttleCount = a} :: ErrorStatistics)

instance Core.FromJSON ErrorStatistics where
  parseJSON =
    Core.withObject
      "ErrorStatistics"
      ( \x ->
          ErrorStatistics'
            Prelude.<$> (x Core..:? "OtherCount")
            Prelude.<*> (x Core..:? "TotalCount")
            Prelude.<*> (x Core..:? "ThrottleCount")
      )

instance Prelude.Hashable ErrorStatistics where
  hashWithSalt _salt ErrorStatistics' {..} =
    _salt `Prelude.hashWithSalt` otherCount
      `Prelude.hashWithSalt` totalCount
      `Prelude.hashWithSalt` throttleCount

instance Prelude.NFData ErrorStatistics where
  rnf ErrorStatistics' {..} =
    Prelude.rnf otherCount
      `Prelude.seq` Prelude.rnf totalCount
      `Prelude.seq` Prelude.rnf throttleCount
