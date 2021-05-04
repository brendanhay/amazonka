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
-- Module      : Network.AWS.XRay.Types.FaultStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.FaultStatistics where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about requests that failed with a 5xx Server Error status
-- code.
--
-- /See:/ 'newFaultStatistics' smart constructor.
data FaultStatistics = FaultStatistics'
  { -- | The number of requests that failed with untracked 5xx Server Error
    -- status codes.
    otherCount :: Prelude.Maybe Prelude.Integer,
    -- | The total number of requests that failed with a 5xx Server Error status
    -- code.
    totalCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FaultStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'otherCount', 'faultStatistics_otherCount' - The number of requests that failed with untracked 5xx Server Error
-- status codes.
--
-- 'totalCount', 'faultStatistics_totalCount' - The total number of requests that failed with a 5xx Server Error status
-- code.
newFaultStatistics ::
  FaultStatistics
newFaultStatistics =
  FaultStatistics'
    { otherCount = Prelude.Nothing,
      totalCount = Prelude.Nothing
    }

-- | The number of requests that failed with untracked 5xx Server Error
-- status codes.
faultStatistics_otherCount :: Lens.Lens' FaultStatistics (Prelude.Maybe Prelude.Integer)
faultStatistics_otherCount = Lens.lens (\FaultStatistics' {otherCount} -> otherCount) (\s@FaultStatistics' {} a -> s {otherCount = a} :: FaultStatistics)

-- | The total number of requests that failed with a 5xx Server Error status
-- code.
faultStatistics_totalCount :: Lens.Lens' FaultStatistics (Prelude.Maybe Prelude.Integer)
faultStatistics_totalCount = Lens.lens (\FaultStatistics' {totalCount} -> totalCount) (\s@FaultStatistics' {} a -> s {totalCount = a} :: FaultStatistics)

instance Prelude.FromJSON FaultStatistics where
  parseJSON =
    Prelude.withObject
      "FaultStatistics"
      ( \x ->
          FaultStatistics'
            Prelude.<$> (x Prelude..:? "OtherCount")
            Prelude.<*> (x Prelude..:? "TotalCount")
      )

instance Prelude.Hashable FaultStatistics

instance Prelude.NFData FaultStatistics
