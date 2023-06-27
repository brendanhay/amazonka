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
-- Module      : Amazonka.GuardDuty.Types.CoverageStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.CoverageStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.CoverageStatus
import Amazonka.GuardDuty.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | Information about the coverage statistics for a resource.
--
-- /See:/ 'newCoverageStatistics' smart constructor.
data CoverageStatistics = CoverageStatistics'
  { -- | Represents coverage statistics for EKS clusters aggregated by coverage
    -- status.
    countByCoverageStatus :: Prelude.Maybe (Prelude.HashMap CoverageStatus Prelude.Integer),
    -- | Represents coverage statistics for EKS clusters aggregated by resource
    -- type.
    countByResourceType :: Prelude.Maybe (Prelude.HashMap ResourceType Prelude.Integer)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoverageStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'countByCoverageStatus', 'coverageStatistics_countByCoverageStatus' - Represents coverage statistics for EKS clusters aggregated by coverage
-- status.
--
-- 'countByResourceType', 'coverageStatistics_countByResourceType' - Represents coverage statistics for EKS clusters aggregated by resource
-- type.
newCoverageStatistics ::
  CoverageStatistics
newCoverageStatistics =
  CoverageStatistics'
    { countByCoverageStatus =
        Prelude.Nothing,
      countByResourceType = Prelude.Nothing
    }

-- | Represents coverage statistics for EKS clusters aggregated by coverage
-- status.
coverageStatistics_countByCoverageStatus :: Lens.Lens' CoverageStatistics (Prelude.Maybe (Prelude.HashMap CoverageStatus Prelude.Integer))
coverageStatistics_countByCoverageStatus = Lens.lens (\CoverageStatistics' {countByCoverageStatus} -> countByCoverageStatus) (\s@CoverageStatistics' {} a -> s {countByCoverageStatus = a} :: CoverageStatistics) Prelude.. Lens.mapping Lens.coerced

-- | Represents coverage statistics for EKS clusters aggregated by resource
-- type.
coverageStatistics_countByResourceType :: Lens.Lens' CoverageStatistics (Prelude.Maybe (Prelude.HashMap ResourceType Prelude.Integer))
coverageStatistics_countByResourceType = Lens.lens (\CoverageStatistics' {countByResourceType} -> countByResourceType) (\s@CoverageStatistics' {} a -> s {countByResourceType = a} :: CoverageStatistics) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CoverageStatistics where
  parseJSON =
    Data.withObject
      "CoverageStatistics"
      ( \x ->
          CoverageStatistics'
            Prelude.<$> ( x
                            Data..:? "countByCoverageStatus"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "countByResourceType"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CoverageStatistics where
  hashWithSalt _salt CoverageStatistics' {..} =
    _salt
      `Prelude.hashWithSalt` countByCoverageStatus
      `Prelude.hashWithSalt` countByResourceType

instance Prelude.NFData CoverageStatistics where
  rnf CoverageStatistics' {..} =
    Prelude.rnf countByCoverageStatus
      `Prelude.seq` Prelude.rnf countByResourceType
