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
import qualified Amazonka.Prelude as Prelude

-- | Business goals that you specify.
--
-- /See:/ 'newBusinessGoals' smart constructor.
data BusinessGoals = BusinessGoals'
  { -- | Business goal to reduce the operational overhead on the team by moving
    -- into managed services.
    reduceOperationalOverheadWithManagedServices :: Prelude.Maybe Prelude.Natural,
    -- | Business goal to reduce license costs.
    licenseCostReduction :: Prelude.Maybe Prelude.Natural,
    -- | Business goal to achieve migration at a fast pace.
    speedOfMigration :: Prelude.Maybe Prelude.Natural,
    -- | Business goal to modernize infrastructure by moving to cloud native
    -- technologies.
    modernizeInfrastructureWithCloudNativeTechnologies :: Prelude.Maybe Prelude.Natural
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
-- 'reduceOperationalOverheadWithManagedServices', 'businessGoals_reduceOperationalOverheadWithManagedServices' - Business goal to reduce the operational overhead on the team by moving
-- into managed services.
--
-- 'licenseCostReduction', 'businessGoals_licenseCostReduction' - Business goal to reduce license costs.
--
-- 'speedOfMigration', 'businessGoals_speedOfMigration' - Business goal to achieve migration at a fast pace.
--
-- 'modernizeInfrastructureWithCloudNativeTechnologies', 'businessGoals_modernizeInfrastructureWithCloudNativeTechnologies' - Business goal to modernize infrastructure by moving to cloud native
-- technologies.
newBusinessGoals ::
  BusinessGoals
newBusinessGoals =
  BusinessGoals'
    { reduceOperationalOverheadWithManagedServices =
        Prelude.Nothing,
      licenseCostReduction = Prelude.Nothing,
      speedOfMigration = Prelude.Nothing,
      modernizeInfrastructureWithCloudNativeTechnologies =
        Prelude.Nothing
    }

-- | Business goal to reduce the operational overhead on the team by moving
-- into managed services.
businessGoals_reduceOperationalOverheadWithManagedServices :: Lens.Lens' BusinessGoals (Prelude.Maybe Prelude.Natural)
businessGoals_reduceOperationalOverheadWithManagedServices = Lens.lens (\BusinessGoals' {reduceOperationalOverheadWithManagedServices} -> reduceOperationalOverheadWithManagedServices) (\s@BusinessGoals' {} a -> s {reduceOperationalOverheadWithManagedServices = a} :: BusinessGoals)

-- | Business goal to reduce license costs.
businessGoals_licenseCostReduction :: Lens.Lens' BusinessGoals (Prelude.Maybe Prelude.Natural)
businessGoals_licenseCostReduction = Lens.lens (\BusinessGoals' {licenseCostReduction} -> licenseCostReduction) (\s@BusinessGoals' {} a -> s {licenseCostReduction = a} :: BusinessGoals)

-- | Business goal to achieve migration at a fast pace.
businessGoals_speedOfMigration :: Lens.Lens' BusinessGoals (Prelude.Maybe Prelude.Natural)
businessGoals_speedOfMigration = Lens.lens (\BusinessGoals' {speedOfMigration} -> speedOfMigration) (\s@BusinessGoals' {} a -> s {speedOfMigration = a} :: BusinessGoals)

-- | Business goal to modernize infrastructure by moving to cloud native
-- technologies.
businessGoals_modernizeInfrastructureWithCloudNativeTechnologies :: Lens.Lens' BusinessGoals (Prelude.Maybe Prelude.Natural)
businessGoals_modernizeInfrastructureWithCloudNativeTechnologies = Lens.lens (\BusinessGoals' {modernizeInfrastructureWithCloudNativeTechnologies} -> modernizeInfrastructureWithCloudNativeTechnologies) (\s@BusinessGoals' {} a -> s {modernizeInfrastructureWithCloudNativeTechnologies = a} :: BusinessGoals)

instance Core.FromJSON BusinessGoals where
  parseJSON =
    Core.withObject
      "BusinessGoals"
      ( \x ->
          BusinessGoals'
            Prelude.<$> ( x
                            Core..:? "reduceOperationalOverheadWithManagedServices"
                        )
            Prelude.<*> (x Core..:? "licenseCostReduction")
            Prelude.<*> (x Core..:? "speedOfMigration")
            Prelude.<*> ( x
                            Core..:? "modernizeInfrastructureWithCloudNativeTechnologies"
                        )
      )

instance Prelude.Hashable BusinessGoals where
  hashWithSalt _salt BusinessGoals' {..} =
    _salt
      `Prelude.hashWithSalt` reduceOperationalOverheadWithManagedServices
      `Prelude.hashWithSalt` licenseCostReduction
      `Prelude.hashWithSalt` speedOfMigration
      `Prelude.hashWithSalt` modernizeInfrastructureWithCloudNativeTechnologies

instance Prelude.NFData BusinessGoals where
  rnf BusinessGoals' {..} =
    Prelude.rnf
      reduceOperationalOverheadWithManagedServices
      `Prelude.seq` Prelude.rnf licenseCostReduction
      `Prelude.seq` Prelude.rnf speedOfMigration
      `Prelude.seq` Prelude.rnf
        modernizeInfrastructureWithCloudNativeTechnologies

instance Core.ToJSON BusinessGoals where
  toJSON BusinessGoals' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ( "reduceOperationalOverheadWithManagedServices"
                Core..=
            )
              Prelude.<$> reduceOperationalOverheadWithManagedServices,
            ("licenseCostReduction" Core..=)
              Prelude.<$> licenseCostReduction,
            ("speedOfMigration" Core..=)
              Prelude.<$> speedOfMigration,
            ( "modernizeInfrastructureWithCloudNativeTechnologies"
                Core..=
            )
              Prelude.<$> modernizeInfrastructureWithCloudNativeTechnologies
          ]
      )
