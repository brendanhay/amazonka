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
-- Module      : Amazonka.MigrationHubStrategy.Types.BusinessGoals
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.BusinessGoals where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Business goals that you specify.
--
-- /See:/ 'newBusinessGoals' smart constructor.
data BusinessGoals = BusinessGoals'
  { -- | Business goal to reduce license costs.
    licenseCostReduction :: Prelude.Maybe Prelude.Natural,
    -- | Business goal to modernize infrastructure by moving to cloud native
    -- technologies.
    modernizeInfrastructureWithCloudNativeTechnologies :: Prelude.Maybe Prelude.Natural,
    -- | Business goal to reduce the operational overhead on the team by moving
    -- into managed services.
    reduceOperationalOverheadWithManagedServices :: Prelude.Maybe Prelude.Natural,
    -- | Business goal to achieve migration at a fast pace.
    speedOfMigration :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BusinessGoals' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseCostReduction', 'businessGoals_licenseCostReduction' - Business goal to reduce license costs.
--
-- 'modernizeInfrastructureWithCloudNativeTechnologies', 'businessGoals_modernizeInfrastructureWithCloudNativeTechnologies' - Business goal to modernize infrastructure by moving to cloud native
-- technologies.
--
-- 'reduceOperationalOverheadWithManagedServices', 'businessGoals_reduceOperationalOverheadWithManagedServices' - Business goal to reduce the operational overhead on the team by moving
-- into managed services.
--
-- 'speedOfMigration', 'businessGoals_speedOfMigration' - Business goal to achieve migration at a fast pace.
newBusinessGoals ::
  BusinessGoals
newBusinessGoals =
  BusinessGoals'
    { licenseCostReduction =
        Prelude.Nothing,
      modernizeInfrastructureWithCloudNativeTechnologies =
        Prelude.Nothing,
      reduceOperationalOverheadWithManagedServices =
        Prelude.Nothing,
      speedOfMigration = Prelude.Nothing
    }

-- | Business goal to reduce license costs.
businessGoals_licenseCostReduction :: Lens.Lens' BusinessGoals (Prelude.Maybe Prelude.Natural)
businessGoals_licenseCostReduction = Lens.lens (\BusinessGoals' {licenseCostReduction} -> licenseCostReduction) (\s@BusinessGoals' {} a -> s {licenseCostReduction = a} :: BusinessGoals)

-- | Business goal to modernize infrastructure by moving to cloud native
-- technologies.
businessGoals_modernizeInfrastructureWithCloudNativeTechnologies :: Lens.Lens' BusinessGoals (Prelude.Maybe Prelude.Natural)
businessGoals_modernizeInfrastructureWithCloudNativeTechnologies = Lens.lens (\BusinessGoals' {modernizeInfrastructureWithCloudNativeTechnologies} -> modernizeInfrastructureWithCloudNativeTechnologies) (\s@BusinessGoals' {} a -> s {modernizeInfrastructureWithCloudNativeTechnologies = a} :: BusinessGoals)

-- | Business goal to reduce the operational overhead on the team by moving
-- into managed services.
businessGoals_reduceOperationalOverheadWithManagedServices :: Lens.Lens' BusinessGoals (Prelude.Maybe Prelude.Natural)
businessGoals_reduceOperationalOverheadWithManagedServices = Lens.lens (\BusinessGoals' {reduceOperationalOverheadWithManagedServices} -> reduceOperationalOverheadWithManagedServices) (\s@BusinessGoals' {} a -> s {reduceOperationalOverheadWithManagedServices = a} :: BusinessGoals)

-- | Business goal to achieve migration at a fast pace.
businessGoals_speedOfMigration :: Lens.Lens' BusinessGoals (Prelude.Maybe Prelude.Natural)
businessGoals_speedOfMigration = Lens.lens (\BusinessGoals' {speedOfMigration} -> speedOfMigration) (\s@BusinessGoals' {} a -> s {speedOfMigration = a} :: BusinessGoals)

instance Data.FromJSON BusinessGoals where
  parseJSON =
    Data.withObject
      "BusinessGoals"
      ( \x ->
          BusinessGoals'
            Prelude.<$> (x Data..:? "licenseCostReduction")
            Prelude.<*> ( x
                            Data..:? "modernizeInfrastructureWithCloudNativeTechnologies"
                        )
            Prelude.<*> ( x
                            Data..:? "reduceOperationalOverheadWithManagedServices"
                        )
            Prelude.<*> (x Data..:? "speedOfMigration")
      )

instance Prelude.Hashable BusinessGoals where
  hashWithSalt _salt BusinessGoals' {..} =
    _salt `Prelude.hashWithSalt` licenseCostReduction
      `Prelude.hashWithSalt` modernizeInfrastructureWithCloudNativeTechnologies
      `Prelude.hashWithSalt` reduceOperationalOverheadWithManagedServices
      `Prelude.hashWithSalt` speedOfMigration

instance Prelude.NFData BusinessGoals where
  rnf BusinessGoals' {..} =
    Prelude.rnf licenseCostReduction
      `Prelude.seq` Prelude.rnf
        modernizeInfrastructureWithCloudNativeTechnologies
      `Prelude.seq` Prelude.rnf
        reduceOperationalOverheadWithManagedServices
      `Prelude.seq` Prelude.rnf speedOfMigration

instance Data.ToJSON BusinessGoals where
  toJSON BusinessGoals' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("licenseCostReduction" Data..=)
              Prelude.<$> licenseCostReduction,
            ( "modernizeInfrastructureWithCloudNativeTechnologies"
                Data..=
            )
              Prelude.<$> modernizeInfrastructureWithCloudNativeTechnologies,
            ( "reduceOperationalOverheadWithManagedServices"
                Data..=
            )
              Prelude.<$> reduceOperationalOverheadWithManagedServices,
            ("speedOfMigration" Data..=)
              Prelude.<$> speedOfMigration
          ]
      )
