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
-- Module      : Network.AWS.CostExplorer.Types.ElastiCacheInstanceDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ElastiCacheInstanceDetails where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about the Amazon ElastiCache instances that AWS recommends that
-- you purchase.
--
-- /See:/ 'newElastiCacheInstanceDetails' smart constructor.
data ElastiCacheInstanceDetails = ElastiCacheInstanceDetails'
  { -- | Whether the recommendation is for a current generation instance.
    currentGeneration :: Prelude.Maybe Prelude.Bool,
    -- | Whether the recommended reservation is size flexible.
    sizeFlexEligible :: Prelude.Maybe Prelude.Bool,
    -- | The instance family of the recommended reservation.
    family :: Prelude.Maybe Prelude.Text,
    -- | The type of node that AWS recommends.
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region of the recommended reservation.
    region :: Prelude.Maybe Prelude.Text,
    -- | The description of the recommended reservation.
    productDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      sizeFlexEligible = Prelude.Nothing,
      family = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      region = Prelude.Nothing,
      productDescription = Prelude.Nothing
    }

-- | Whether the recommendation is for a current generation instance.
elastiCacheInstanceDetails_currentGeneration :: Lens.Lens' ElastiCacheInstanceDetails (Prelude.Maybe Prelude.Bool)
elastiCacheInstanceDetails_currentGeneration = Lens.lens (\ElastiCacheInstanceDetails' {currentGeneration} -> currentGeneration) (\s@ElastiCacheInstanceDetails' {} a -> s {currentGeneration = a} :: ElastiCacheInstanceDetails)

-- | Whether the recommended reservation is size flexible.
elastiCacheInstanceDetails_sizeFlexEligible :: Lens.Lens' ElastiCacheInstanceDetails (Prelude.Maybe Prelude.Bool)
elastiCacheInstanceDetails_sizeFlexEligible = Lens.lens (\ElastiCacheInstanceDetails' {sizeFlexEligible} -> sizeFlexEligible) (\s@ElastiCacheInstanceDetails' {} a -> s {sizeFlexEligible = a} :: ElastiCacheInstanceDetails)

-- | The instance family of the recommended reservation.
elastiCacheInstanceDetails_family :: Lens.Lens' ElastiCacheInstanceDetails (Prelude.Maybe Prelude.Text)
elastiCacheInstanceDetails_family = Lens.lens (\ElastiCacheInstanceDetails' {family} -> family) (\s@ElastiCacheInstanceDetails' {} a -> s {family = a} :: ElastiCacheInstanceDetails)

-- | The type of node that AWS recommends.
elastiCacheInstanceDetails_nodeType :: Lens.Lens' ElastiCacheInstanceDetails (Prelude.Maybe Prelude.Text)
elastiCacheInstanceDetails_nodeType = Lens.lens (\ElastiCacheInstanceDetails' {nodeType} -> nodeType) (\s@ElastiCacheInstanceDetails' {} a -> s {nodeType = a} :: ElastiCacheInstanceDetails)

-- | The AWS Region of the recommended reservation.
elastiCacheInstanceDetails_region :: Lens.Lens' ElastiCacheInstanceDetails (Prelude.Maybe Prelude.Text)
elastiCacheInstanceDetails_region = Lens.lens (\ElastiCacheInstanceDetails' {region} -> region) (\s@ElastiCacheInstanceDetails' {} a -> s {region = a} :: ElastiCacheInstanceDetails)

-- | The description of the recommended reservation.
elastiCacheInstanceDetails_productDescription :: Lens.Lens' ElastiCacheInstanceDetails (Prelude.Maybe Prelude.Text)
elastiCacheInstanceDetails_productDescription = Lens.lens (\ElastiCacheInstanceDetails' {productDescription} -> productDescription) (\s@ElastiCacheInstanceDetails' {} a -> s {productDescription = a} :: ElastiCacheInstanceDetails)

instance Prelude.FromJSON ElastiCacheInstanceDetails where
  parseJSON =
    Prelude.withObject
      "ElastiCacheInstanceDetails"
      ( \x ->
          ElastiCacheInstanceDetails'
            Prelude.<$> (x Prelude..:? "CurrentGeneration")
            Prelude.<*> (x Prelude..:? "SizeFlexEligible")
            Prelude.<*> (x Prelude..:? "Family")
            Prelude.<*> (x Prelude..:? "NodeType")
            Prelude.<*> (x Prelude..:? "Region")
            Prelude.<*> (x Prelude..:? "ProductDescription")
      )

instance Prelude.Hashable ElastiCacheInstanceDetails

instance Prelude.NFData ElastiCacheInstanceDetails
