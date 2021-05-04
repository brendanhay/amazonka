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
-- Module      : Network.AWS.Config.Types.ComplianceContributorCount
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ComplianceContributorCount where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The number of AWS resources or AWS Config rules responsible for the
-- current compliance of the item, up to a maximum number.
--
-- /See:/ 'newComplianceContributorCount' smart constructor.
data ComplianceContributorCount = ComplianceContributorCount'
  { -- | Indicates whether the maximum count is reached.
    capExceeded :: Prelude.Maybe Prelude.Bool,
    -- | The number of AWS resources or AWS Config rules responsible for the
    -- current compliance of the item.
    cappedCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ComplianceContributorCount' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capExceeded', 'complianceContributorCount_capExceeded' - Indicates whether the maximum count is reached.
--
-- 'cappedCount', 'complianceContributorCount_cappedCount' - The number of AWS resources or AWS Config rules responsible for the
-- current compliance of the item.
newComplianceContributorCount ::
  ComplianceContributorCount
newComplianceContributorCount =
  ComplianceContributorCount'
    { capExceeded =
        Prelude.Nothing,
      cappedCount = Prelude.Nothing
    }

-- | Indicates whether the maximum count is reached.
complianceContributorCount_capExceeded :: Lens.Lens' ComplianceContributorCount (Prelude.Maybe Prelude.Bool)
complianceContributorCount_capExceeded = Lens.lens (\ComplianceContributorCount' {capExceeded} -> capExceeded) (\s@ComplianceContributorCount' {} a -> s {capExceeded = a} :: ComplianceContributorCount)

-- | The number of AWS resources or AWS Config rules responsible for the
-- current compliance of the item.
complianceContributorCount_cappedCount :: Lens.Lens' ComplianceContributorCount (Prelude.Maybe Prelude.Int)
complianceContributorCount_cappedCount = Lens.lens (\ComplianceContributorCount' {cappedCount} -> cappedCount) (\s@ComplianceContributorCount' {} a -> s {cappedCount = a} :: ComplianceContributorCount)

instance Prelude.FromJSON ComplianceContributorCount where
  parseJSON =
    Prelude.withObject
      "ComplianceContributorCount"
      ( \x ->
          ComplianceContributorCount'
            Prelude.<$> (x Prelude..:? "CapExceeded")
            Prelude.<*> (x Prelude..:? "CappedCount")
      )

instance Prelude.Hashable ComplianceContributorCount

instance Prelude.NFData ComplianceContributorCount
