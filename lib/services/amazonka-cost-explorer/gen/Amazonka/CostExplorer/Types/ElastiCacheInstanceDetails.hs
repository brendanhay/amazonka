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
-- Module      : Amazonka.CostExplorer.Types.ElastiCacheInstanceDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.ElastiCacheInstanceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Details about the Amazon ElastiCache instances that Amazon Web Services
-- recommends that you purchase.
--
-- /See:/ 'newElastiCacheInstanceDetails' smart constructor.
data ElastiCacheInstanceDetails = ElastiCacheInstanceDetails'
  { -- | Determines whether the recommendation is for a current generation
    -- instance.
    currentGeneration :: Prelude.Maybe Prelude.Bool,
    -- | The instance family of the recommended reservation.
    family :: Prelude.Maybe Prelude.Text,
    -- | The type of node that Amazon Web Services recommends.
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | The description of the recommended reservation.
    productDescription :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region of the recommended reservation.
    region :: Prelude.Maybe Prelude.Text,
    -- | Determines whether the recommended reservation is size flexible.
    sizeFlexEligible :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ElastiCacheInstanceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentGeneration', 'elastiCacheInstanceDetails_currentGeneration' - Determines whether the recommendation is for a current generation
-- instance.
--
-- 'family', 'elastiCacheInstanceDetails_family' - The instance family of the recommended reservation.
--
-- 'nodeType', 'elastiCacheInstanceDetails_nodeType' - The type of node that Amazon Web Services recommends.
--
-- 'productDescription', 'elastiCacheInstanceDetails_productDescription' - The description of the recommended reservation.
--
-- 'region', 'elastiCacheInstanceDetails_region' - The Amazon Web Services Region of the recommended reservation.
--
-- 'sizeFlexEligible', 'elastiCacheInstanceDetails_sizeFlexEligible' - Determines whether the recommended reservation is size flexible.
newElastiCacheInstanceDetails ::
  ElastiCacheInstanceDetails
newElastiCacheInstanceDetails =
  ElastiCacheInstanceDetails'
    { currentGeneration =
        Prelude.Nothing,
      family = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      productDescription = Prelude.Nothing,
      region = Prelude.Nothing,
      sizeFlexEligible = Prelude.Nothing
    }

-- | Determines whether the recommendation is for a current generation
-- instance.
elastiCacheInstanceDetails_currentGeneration :: Lens.Lens' ElastiCacheInstanceDetails (Prelude.Maybe Prelude.Bool)
elastiCacheInstanceDetails_currentGeneration = Lens.lens (\ElastiCacheInstanceDetails' {currentGeneration} -> currentGeneration) (\s@ElastiCacheInstanceDetails' {} a -> s {currentGeneration = a} :: ElastiCacheInstanceDetails)

-- | The instance family of the recommended reservation.
elastiCacheInstanceDetails_family :: Lens.Lens' ElastiCacheInstanceDetails (Prelude.Maybe Prelude.Text)
elastiCacheInstanceDetails_family = Lens.lens (\ElastiCacheInstanceDetails' {family} -> family) (\s@ElastiCacheInstanceDetails' {} a -> s {family = a} :: ElastiCacheInstanceDetails)

-- | The type of node that Amazon Web Services recommends.
elastiCacheInstanceDetails_nodeType :: Lens.Lens' ElastiCacheInstanceDetails (Prelude.Maybe Prelude.Text)
elastiCacheInstanceDetails_nodeType = Lens.lens (\ElastiCacheInstanceDetails' {nodeType} -> nodeType) (\s@ElastiCacheInstanceDetails' {} a -> s {nodeType = a} :: ElastiCacheInstanceDetails)

-- | The description of the recommended reservation.
elastiCacheInstanceDetails_productDescription :: Lens.Lens' ElastiCacheInstanceDetails (Prelude.Maybe Prelude.Text)
elastiCacheInstanceDetails_productDescription = Lens.lens (\ElastiCacheInstanceDetails' {productDescription} -> productDescription) (\s@ElastiCacheInstanceDetails' {} a -> s {productDescription = a} :: ElastiCacheInstanceDetails)

-- | The Amazon Web Services Region of the recommended reservation.
elastiCacheInstanceDetails_region :: Lens.Lens' ElastiCacheInstanceDetails (Prelude.Maybe Prelude.Text)
elastiCacheInstanceDetails_region = Lens.lens (\ElastiCacheInstanceDetails' {region} -> region) (\s@ElastiCacheInstanceDetails' {} a -> s {region = a} :: ElastiCacheInstanceDetails)

-- | Determines whether the recommended reservation is size flexible.
elastiCacheInstanceDetails_sizeFlexEligible :: Lens.Lens' ElastiCacheInstanceDetails (Prelude.Maybe Prelude.Bool)
elastiCacheInstanceDetails_sizeFlexEligible = Lens.lens (\ElastiCacheInstanceDetails' {sizeFlexEligible} -> sizeFlexEligible) (\s@ElastiCacheInstanceDetails' {} a -> s {sizeFlexEligible = a} :: ElastiCacheInstanceDetails)

instance Data.FromJSON ElastiCacheInstanceDetails where
  parseJSON =
    Data.withObject
      "ElastiCacheInstanceDetails"
      ( \x ->
          ElastiCacheInstanceDetails'
            Prelude.<$> (x Data..:? "CurrentGeneration")
            Prelude.<*> (x Data..:? "Family")
            Prelude.<*> (x Data..:? "NodeType")
            Prelude.<*> (x Data..:? "ProductDescription")
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..:? "SizeFlexEligible")
      )

instance Prelude.Hashable ElastiCacheInstanceDetails where
  hashWithSalt _salt ElastiCacheInstanceDetails' {..} =
    _salt `Prelude.hashWithSalt` currentGeneration
      `Prelude.hashWithSalt` family
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` productDescription
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` sizeFlexEligible

instance Prelude.NFData ElastiCacheInstanceDetails where
  rnf ElastiCacheInstanceDetails' {..} =
    Prelude.rnf currentGeneration
      `Prelude.seq` Prelude.rnf family
      `Prelude.seq` Prelude.rnf nodeType
      `Prelude.seq` Prelude.rnf productDescription
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf sizeFlexEligible
