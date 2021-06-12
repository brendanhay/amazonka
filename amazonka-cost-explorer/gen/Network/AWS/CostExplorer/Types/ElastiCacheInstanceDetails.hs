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
-- Module      : Network.AWS.CostExplorer.Types.ElastiCacheInstanceDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ElastiCacheInstanceDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details about the Amazon ElastiCache instances that AWS recommends that
-- you purchase.
--
-- /See:/ 'newElastiCacheInstanceDetails' smart constructor.
data ElastiCacheInstanceDetails = ElastiCacheInstanceDetails'
  { -- | Whether the recommendation is for a current generation instance.
    currentGeneration :: Core.Maybe Core.Bool,
    -- | Whether the recommended reservation is size flexible.
    sizeFlexEligible :: Core.Maybe Core.Bool,
    -- | The instance family of the recommended reservation.
    family :: Core.Maybe Core.Text,
    -- | The type of node that AWS recommends.
    nodeType :: Core.Maybe Core.Text,
    -- | The AWS Region of the recommended reservation.
    region :: Core.Maybe Core.Text,
    -- | The description of the recommended reservation.
    productDescription :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ElastiCacheInstanceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentGeneration', 'elastiCacheInstanceDetails_currentGeneration' - Whether the recommendation is for a current generation instance.
--
-- 'sizeFlexEligible', 'elastiCacheInstanceDetails_sizeFlexEligible' - Whether the recommended reservation is size flexible.
--
-- 'family', 'elastiCacheInstanceDetails_family' - The instance family of the recommended reservation.
--
-- 'nodeType', 'elastiCacheInstanceDetails_nodeType' - The type of node that AWS recommends.
--
-- 'region', 'elastiCacheInstanceDetails_region' - The AWS Region of the recommended reservation.
--
-- 'productDescription', 'elastiCacheInstanceDetails_productDescription' - The description of the recommended reservation.
newElastiCacheInstanceDetails ::
  ElastiCacheInstanceDetails
newElastiCacheInstanceDetails =
  ElastiCacheInstanceDetails'
    { currentGeneration =
        Core.Nothing,
      sizeFlexEligible = Core.Nothing,
      family = Core.Nothing,
      nodeType = Core.Nothing,
      region = Core.Nothing,
      productDescription = Core.Nothing
    }

-- | Whether the recommendation is for a current generation instance.
elastiCacheInstanceDetails_currentGeneration :: Lens.Lens' ElastiCacheInstanceDetails (Core.Maybe Core.Bool)
elastiCacheInstanceDetails_currentGeneration = Lens.lens (\ElastiCacheInstanceDetails' {currentGeneration} -> currentGeneration) (\s@ElastiCacheInstanceDetails' {} a -> s {currentGeneration = a} :: ElastiCacheInstanceDetails)

-- | Whether the recommended reservation is size flexible.
elastiCacheInstanceDetails_sizeFlexEligible :: Lens.Lens' ElastiCacheInstanceDetails (Core.Maybe Core.Bool)
elastiCacheInstanceDetails_sizeFlexEligible = Lens.lens (\ElastiCacheInstanceDetails' {sizeFlexEligible} -> sizeFlexEligible) (\s@ElastiCacheInstanceDetails' {} a -> s {sizeFlexEligible = a} :: ElastiCacheInstanceDetails)

-- | The instance family of the recommended reservation.
elastiCacheInstanceDetails_family :: Lens.Lens' ElastiCacheInstanceDetails (Core.Maybe Core.Text)
elastiCacheInstanceDetails_family = Lens.lens (\ElastiCacheInstanceDetails' {family} -> family) (\s@ElastiCacheInstanceDetails' {} a -> s {family = a} :: ElastiCacheInstanceDetails)

-- | The type of node that AWS recommends.
elastiCacheInstanceDetails_nodeType :: Lens.Lens' ElastiCacheInstanceDetails (Core.Maybe Core.Text)
elastiCacheInstanceDetails_nodeType = Lens.lens (\ElastiCacheInstanceDetails' {nodeType} -> nodeType) (\s@ElastiCacheInstanceDetails' {} a -> s {nodeType = a} :: ElastiCacheInstanceDetails)

-- | The AWS Region of the recommended reservation.
elastiCacheInstanceDetails_region :: Lens.Lens' ElastiCacheInstanceDetails (Core.Maybe Core.Text)
elastiCacheInstanceDetails_region = Lens.lens (\ElastiCacheInstanceDetails' {region} -> region) (\s@ElastiCacheInstanceDetails' {} a -> s {region = a} :: ElastiCacheInstanceDetails)

-- | The description of the recommended reservation.
elastiCacheInstanceDetails_productDescription :: Lens.Lens' ElastiCacheInstanceDetails (Core.Maybe Core.Text)
elastiCacheInstanceDetails_productDescription = Lens.lens (\ElastiCacheInstanceDetails' {productDescription} -> productDescription) (\s@ElastiCacheInstanceDetails' {} a -> s {productDescription = a} :: ElastiCacheInstanceDetails)

instance Core.FromJSON ElastiCacheInstanceDetails where
  parseJSON =
    Core.withObject
      "ElastiCacheInstanceDetails"
      ( \x ->
          ElastiCacheInstanceDetails'
            Core.<$> (x Core..:? "CurrentGeneration")
            Core.<*> (x Core..:? "SizeFlexEligible")
            Core.<*> (x Core..:? "Family")
            Core.<*> (x Core..:? "NodeType")
            Core.<*> (x Core..:? "Region")
            Core.<*> (x Core..:? "ProductDescription")
      )

instance Core.Hashable ElastiCacheInstanceDetails

instance Core.NFData ElastiCacheInstanceDetails
