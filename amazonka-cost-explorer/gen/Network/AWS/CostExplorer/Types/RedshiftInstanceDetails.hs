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
-- Module      : Network.AWS.CostExplorer.Types.RedshiftInstanceDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RedshiftInstanceDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Details about the Amazon Redshift instances that Amazon Web Services
-- recommends that you purchase.
--
-- /See:/ 'newRedshiftInstanceDetails' smart constructor.
data RedshiftInstanceDetails = RedshiftInstanceDetails'
  { -- | Determines whether the recommended reservation is size flexible.
    sizeFlexEligible :: Prelude.Maybe Prelude.Bool,
    -- | Determines whether the recommendation is for a current-generation
    -- instance.
    currentGeneration :: Prelude.Maybe Prelude.Bool,
    -- | The instance family of the recommended reservation.
    family :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region of the recommended reservation.
    region :: Prelude.Maybe Prelude.Text,
    -- | The type of node that Amazon Web Services recommends.
    nodeType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedshiftInstanceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sizeFlexEligible', 'redshiftInstanceDetails_sizeFlexEligible' - Determines whether the recommended reservation is size flexible.
--
-- 'currentGeneration', 'redshiftInstanceDetails_currentGeneration' - Determines whether the recommendation is for a current-generation
-- instance.
--
-- 'family', 'redshiftInstanceDetails_family' - The instance family of the recommended reservation.
--
-- 'region', 'redshiftInstanceDetails_region' - The Amazon Web Services Region of the recommended reservation.
--
-- 'nodeType', 'redshiftInstanceDetails_nodeType' - The type of node that Amazon Web Services recommends.
newRedshiftInstanceDetails ::
  RedshiftInstanceDetails
newRedshiftInstanceDetails =
  RedshiftInstanceDetails'
    { sizeFlexEligible =
        Prelude.Nothing,
      currentGeneration = Prelude.Nothing,
      family = Prelude.Nothing,
      region = Prelude.Nothing,
      nodeType = Prelude.Nothing
    }

-- | Determines whether the recommended reservation is size flexible.
redshiftInstanceDetails_sizeFlexEligible :: Lens.Lens' RedshiftInstanceDetails (Prelude.Maybe Prelude.Bool)
redshiftInstanceDetails_sizeFlexEligible = Lens.lens (\RedshiftInstanceDetails' {sizeFlexEligible} -> sizeFlexEligible) (\s@RedshiftInstanceDetails' {} a -> s {sizeFlexEligible = a} :: RedshiftInstanceDetails)

-- | Determines whether the recommendation is for a current-generation
-- instance.
redshiftInstanceDetails_currentGeneration :: Lens.Lens' RedshiftInstanceDetails (Prelude.Maybe Prelude.Bool)
redshiftInstanceDetails_currentGeneration = Lens.lens (\RedshiftInstanceDetails' {currentGeneration} -> currentGeneration) (\s@RedshiftInstanceDetails' {} a -> s {currentGeneration = a} :: RedshiftInstanceDetails)

-- | The instance family of the recommended reservation.
redshiftInstanceDetails_family :: Lens.Lens' RedshiftInstanceDetails (Prelude.Maybe Prelude.Text)
redshiftInstanceDetails_family = Lens.lens (\RedshiftInstanceDetails' {family} -> family) (\s@RedshiftInstanceDetails' {} a -> s {family = a} :: RedshiftInstanceDetails)

-- | The Amazon Web Services Region of the recommended reservation.
redshiftInstanceDetails_region :: Lens.Lens' RedshiftInstanceDetails (Prelude.Maybe Prelude.Text)
redshiftInstanceDetails_region = Lens.lens (\RedshiftInstanceDetails' {region} -> region) (\s@RedshiftInstanceDetails' {} a -> s {region = a} :: RedshiftInstanceDetails)

-- | The type of node that Amazon Web Services recommends.
redshiftInstanceDetails_nodeType :: Lens.Lens' RedshiftInstanceDetails (Prelude.Maybe Prelude.Text)
redshiftInstanceDetails_nodeType = Lens.lens (\RedshiftInstanceDetails' {nodeType} -> nodeType) (\s@RedshiftInstanceDetails' {} a -> s {nodeType = a} :: RedshiftInstanceDetails)

instance Core.FromJSON RedshiftInstanceDetails where
  parseJSON =
    Core.withObject
      "RedshiftInstanceDetails"
      ( \x ->
          RedshiftInstanceDetails'
            Prelude.<$> (x Core..:? "SizeFlexEligible")
            Prelude.<*> (x Core..:? "CurrentGeneration")
            Prelude.<*> (x Core..:? "Family")
            Prelude.<*> (x Core..:? "Region")
            Prelude.<*> (x Core..:? "NodeType")
      )

instance Prelude.Hashable RedshiftInstanceDetails

instance Prelude.NFData RedshiftInstanceDetails
