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
-- Module      : Amazonka.GuardDuty.Types.CoverageEksClusterDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.CoverageEksClusterDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.AddonDetails
import qualified Amazonka.Prelude as Prelude

-- | Information about the EKS cluster that has a coverage status.
--
-- /See:/ 'newCoverageEksClusterDetails' smart constructor.
data CoverageEksClusterDetails = CoverageEksClusterDetails'
  { -- | Information about the installed EKS add-on.
    addonDetails :: Prelude.Maybe AddonDetails,
    -- | Name of the EKS cluster.
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | Represents all the nodes within the EKS cluster in your account.
    compatibleNodes :: Prelude.Maybe Prelude.Integer,
    -- | Represents the nodes within the EKS cluster that have a @HEALTHY@
    -- coverage status.
    coveredNodes :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoverageEksClusterDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addonDetails', 'coverageEksClusterDetails_addonDetails' - Information about the installed EKS add-on.
--
-- 'clusterName', 'coverageEksClusterDetails_clusterName' - Name of the EKS cluster.
--
-- 'compatibleNodes', 'coverageEksClusterDetails_compatibleNodes' - Represents all the nodes within the EKS cluster in your account.
--
-- 'coveredNodes', 'coverageEksClusterDetails_coveredNodes' - Represents the nodes within the EKS cluster that have a @HEALTHY@
-- coverage status.
newCoverageEksClusterDetails ::
  CoverageEksClusterDetails
newCoverageEksClusterDetails =
  CoverageEksClusterDetails'
    { addonDetails =
        Prelude.Nothing,
      clusterName = Prelude.Nothing,
      compatibleNodes = Prelude.Nothing,
      coveredNodes = Prelude.Nothing
    }

-- | Information about the installed EKS add-on.
coverageEksClusterDetails_addonDetails :: Lens.Lens' CoverageEksClusterDetails (Prelude.Maybe AddonDetails)
coverageEksClusterDetails_addonDetails = Lens.lens (\CoverageEksClusterDetails' {addonDetails} -> addonDetails) (\s@CoverageEksClusterDetails' {} a -> s {addonDetails = a} :: CoverageEksClusterDetails)

-- | Name of the EKS cluster.
coverageEksClusterDetails_clusterName :: Lens.Lens' CoverageEksClusterDetails (Prelude.Maybe Prelude.Text)
coverageEksClusterDetails_clusterName = Lens.lens (\CoverageEksClusterDetails' {clusterName} -> clusterName) (\s@CoverageEksClusterDetails' {} a -> s {clusterName = a} :: CoverageEksClusterDetails)

-- | Represents all the nodes within the EKS cluster in your account.
coverageEksClusterDetails_compatibleNodes :: Lens.Lens' CoverageEksClusterDetails (Prelude.Maybe Prelude.Integer)
coverageEksClusterDetails_compatibleNodes = Lens.lens (\CoverageEksClusterDetails' {compatibleNodes} -> compatibleNodes) (\s@CoverageEksClusterDetails' {} a -> s {compatibleNodes = a} :: CoverageEksClusterDetails)

-- | Represents the nodes within the EKS cluster that have a @HEALTHY@
-- coverage status.
coverageEksClusterDetails_coveredNodes :: Lens.Lens' CoverageEksClusterDetails (Prelude.Maybe Prelude.Integer)
coverageEksClusterDetails_coveredNodes = Lens.lens (\CoverageEksClusterDetails' {coveredNodes} -> coveredNodes) (\s@CoverageEksClusterDetails' {} a -> s {coveredNodes = a} :: CoverageEksClusterDetails)

instance Data.FromJSON CoverageEksClusterDetails where
  parseJSON =
    Data.withObject
      "CoverageEksClusterDetails"
      ( \x ->
          CoverageEksClusterDetails'
            Prelude.<$> (x Data..:? "addonDetails")
            Prelude.<*> (x Data..:? "clusterName")
            Prelude.<*> (x Data..:? "compatibleNodes")
            Prelude.<*> (x Data..:? "coveredNodes")
      )

instance Prelude.Hashable CoverageEksClusterDetails where
  hashWithSalt _salt CoverageEksClusterDetails' {..} =
    _salt
      `Prelude.hashWithSalt` addonDetails
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` compatibleNodes
      `Prelude.hashWithSalt` coveredNodes

instance Prelude.NFData CoverageEksClusterDetails where
  rnf CoverageEksClusterDetails' {..} =
    Prelude.rnf addonDetails
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf compatibleNodes
      `Prelude.seq` Prelude.rnf coveredNodes
