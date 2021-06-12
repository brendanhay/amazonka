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
-- Module      : Network.AWS.CostExplorer.Types.InstanceDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.InstanceDetails where

import qualified Network.AWS.Core as Core
import Network.AWS.CostExplorer.Types.EC2InstanceDetails
import Network.AWS.CostExplorer.Types.ESInstanceDetails
import Network.AWS.CostExplorer.Types.ElastiCacheInstanceDetails
import Network.AWS.CostExplorer.Types.RDSInstanceDetails
import Network.AWS.CostExplorer.Types.RedshiftInstanceDetails
import qualified Network.AWS.Lens as Lens

-- | Details about the instances that AWS recommends that you purchase.
--
-- /See:/ 'newInstanceDetails' smart constructor.
data InstanceDetails = InstanceDetails'
  { -- | The ElastiCache instances that AWS recommends that you purchase.
    elastiCacheInstanceDetails :: Core.Maybe ElastiCacheInstanceDetails,
    -- | The Amazon Redshift instances that AWS recommends that you purchase.
    redshiftInstanceDetails :: Core.Maybe RedshiftInstanceDetails,
    -- | The Amazon ES instances that AWS recommends that you purchase.
    eSInstanceDetails :: Core.Maybe ESInstanceDetails,
    -- | The Amazon EC2 instances that AWS recommends that you purchase.
    eC2InstanceDetails :: Core.Maybe EC2InstanceDetails,
    -- | The Amazon RDS instances that AWS recommends that you purchase.
    rDSInstanceDetails :: Core.Maybe RDSInstanceDetails
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'elastiCacheInstanceDetails', 'instanceDetails_elastiCacheInstanceDetails' - The ElastiCache instances that AWS recommends that you purchase.
--
-- 'redshiftInstanceDetails', 'instanceDetails_redshiftInstanceDetails' - The Amazon Redshift instances that AWS recommends that you purchase.
--
-- 'eSInstanceDetails', 'instanceDetails_eSInstanceDetails' - The Amazon ES instances that AWS recommends that you purchase.
--
-- 'eC2InstanceDetails', 'instanceDetails_eC2InstanceDetails' - The Amazon EC2 instances that AWS recommends that you purchase.
--
-- 'rDSInstanceDetails', 'instanceDetails_rDSInstanceDetails' - The Amazon RDS instances that AWS recommends that you purchase.
newInstanceDetails ::
  InstanceDetails
newInstanceDetails =
  InstanceDetails'
    { elastiCacheInstanceDetails =
        Core.Nothing,
      redshiftInstanceDetails = Core.Nothing,
      eSInstanceDetails = Core.Nothing,
      eC2InstanceDetails = Core.Nothing,
      rDSInstanceDetails = Core.Nothing
    }

-- | The ElastiCache instances that AWS recommends that you purchase.
instanceDetails_elastiCacheInstanceDetails :: Lens.Lens' InstanceDetails (Core.Maybe ElastiCacheInstanceDetails)
instanceDetails_elastiCacheInstanceDetails = Lens.lens (\InstanceDetails' {elastiCacheInstanceDetails} -> elastiCacheInstanceDetails) (\s@InstanceDetails' {} a -> s {elastiCacheInstanceDetails = a} :: InstanceDetails)

-- | The Amazon Redshift instances that AWS recommends that you purchase.
instanceDetails_redshiftInstanceDetails :: Lens.Lens' InstanceDetails (Core.Maybe RedshiftInstanceDetails)
instanceDetails_redshiftInstanceDetails = Lens.lens (\InstanceDetails' {redshiftInstanceDetails} -> redshiftInstanceDetails) (\s@InstanceDetails' {} a -> s {redshiftInstanceDetails = a} :: InstanceDetails)

-- | The Amazon ES instances that AWS recommends that you purchase.
instanceDetails_eSInstanceDetails :: Lens.Lens' InstanceDetails (Core.Maybe ESInstanceDetails)
instanceDetails_eSInstanceDetails = Lens.lens (\InstanceDetails' {eSInstanceDetails} -> eSInstanceDetails) (\s@InstanceDetails' {} a -> s {eSInstanceDetails = a} :: InstanceDetails)

-- | The Amazon EC2 instances that AWS recommends that you purchase.
instanceDetails_eC2InstanceDetails :: Lens.Lens' InstanceDetails (Core.Maybe EC2InstanceDetails)
instanceDetails_eC2InstanceDetails = Lens.lens (\InstanceDetails' {eC2InstanceDetails} -> eC2InstanceDetails) (\s@InstanceDetails' {} a -> s {eC2InstanceDetails = a} :: InstanceDetails)

-- | The Amazon RDS instances that AWS recommends that you purchase.
instanceDetails_rDSInstanceDetails :: Lens.Lens' InstanceDetails (Core.Maybe RDSInstanceDetails)
instanceDetails_rDSInstanceDetails = Lens.lens (\InstanceDetails' {rDSInstanceDetails} -> rDSInstanceDetails) (\s@InstanceDetails' {} a -> s {rDSInstanceDetails = a} :: InstanceDetails)

instance Core.FromJSON InstanceDetails where
  parseJSON =
    Core.withObject
      "InstanceDetails"
      ( \x ->
          InstanceDetails'
            Core.<$> (x Core..:? "ElastiCacheInstanceDetails")
            Core.<*> (x Core..:? "RedshiftInstanceDetails")
            Core.<*> (x Core..:? "ESInstanceDetails")
            Core.<*> (x Core..:? "EC2InstanceDetails")
            Core.<*> (x Core..:? "RDSInstanceDetails")
      )

instance Core.Hashable InstanceDetails

instance Core.NFData InstanceDetails
