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
-- Module      : Amazonka.GuardDuty.Types.CoverageResourceDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.CoverageResourceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.CoverageEksClusterDetails
import Amazonka.GuardDuty.Types.ResourceType
import qualified Amazonka.Prelude as Prelude

-- | Information about the resource for each individual EKS cluster.
--
-- /See:/ 'newCoverageResourceDetails' smart constructor.
data CoverageResourceDetails = CoverageResourceDetails'
  { -- | EKS cluster details involved in the coverage statistics.
    eksClusterDetails :: Prelude.Maybe CoverageEksClusterDetails,
    -- | The type of Amazon Web Services resource.
    resourceType :: Prelude.Maybe ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoverageResourceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eksClusterDetails', 'coverageResourceDetails_eksClusterDetails' - EKS cluster details involved in the coverage statistics.
--
-- 'resourceType', 'coverageResourceDetails_resourceType' - The type of Amazon Web Services resource.
newCoverageResourceDetails ::
  CoverageResourceDetails
newCoverageResourceDetails =
  CoverageResourceDetails'
    { eksClusterDetails =
        Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | EKS cluster details involved in the coverage statistics.
coverageResourceDetails_eksClusterDetails :: Lens.Lens' CoverageResourceDetails (Prelude.Maybe CoverageEksClusterDetails)
coverageResourceDetails_eksClusterDetails = Lens.lens (\CoverageResourceDetails' {eksClusterDetails} -> eksClusterDetails) (\s@CoverageResourceDetails' {} a -> s {eksClusterDetails = a} :: CoverageResourceDetails)

-- | The type of Amazon Web Services resource.
coverageResourceDetails_resourceType :: Lens.Lens' CoverageResourceDetails (Prelude.Maybe ResourceType)
coverageResourceDetails_resourceType = Lens.lens (\CoverageResourceDetails' {resourceType} -> resourceType) (\s@CoverageResourceDetails' {} a -> s {resourceType = a} :: CoverageResourceDetails)

instance Data.FromJSON CoverageResourceDetails where
  parseJSON =
    Data.withObject
      "CoverageResourceDetails"
      ( \x ->
          CoverageResourceDetails'
            Prelude.<$> (x Data..:? "eksClusterDetails")
            Prelude.<*> (x Data..:? "resourceType")
      )

instance Prelude.Hashable CoverageResourceDetails where
  hashWithSalt _salt CoverageResourceDetails' {..} =
    _salt
      `Prelude.hashWithSalt` eksClusterDetails
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData CoverageResourceDetails where
  rnf CoverageResourceDetails' {..} =
    Prelude.rnf eksClusterDetails
      `Prelude.seq` Prelude.rnf resourceType
