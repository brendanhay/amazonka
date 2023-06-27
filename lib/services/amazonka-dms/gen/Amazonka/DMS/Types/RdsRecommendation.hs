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
-- Module      : Amazonka.DMS.Types.RdsRecommendation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.RdsRecommendation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types.RdsConfiguration
import Amazonka.DMS.Types.RdsRequirements
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information that describes a recommendation of a target engine
-- on Amazon RDS.
--
-- /See:/ 'newRdsRecommendation' smart constructor.
data RdsRecommendation = RdsRecommendation'
  { -- | Supplemental information about the requirements to the recommended
    -- target database on Amazon RDS.
    requirementsToTarget :: Prelude.Maybe RdsRequirements,
    -- | Supplemental information about the configuration of the recommended
    -- target database on Amazon RDS.
    targetConfiguration :: Prelude.Maybe RdsConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RdsRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requirementsToTarget', 'rdsRecommendation_requirementsToTarget' - Supplemental information about the requirements to the recommended
-- target database on Amazon RDS.
--
-- 'targetConfiguration', 'rdsRecommendation_targetConfiguration' - Supplemental information about the configuration of the recommended
-- target database on Amazon RDS.
newRdsRecommendation ::
  RdsRecommendation
newRdsRecommendation =
  RdsRecommendation'
    { requirementsToTarget =
        Prelude.Nothing,
      targetConfiguration = Prelude.Nothing
    }

-- | Supplemental information about the requirements to the recommended
-- target database on Amazon RDS.
rdsRecommendation_requirementsToTarget :: Lens.Lens' RdsRecommendation (Prelude.Maybe RdsRequirements)
rdsRecommendation_requirementsToTarget = Lens.lens (\RdsRecommendation' {requirementsToTarget} -> requirementsToTarget) (\s@RdsRecommendation' {} a -> s {requirementsToTarget = a} :: RdsRecommendation)

-- | Supplemental information about the configuration of the recommended
-- target database on Amazon RDS.
rdsRecommendation_targetConfiguration :: Lens.Lens' RdsRecommendation (Prelude.Maybe RdsConfiguration)
rdsRecommendation_targetConfiguration = Lens.lens (\RdsRecommendation' {targetConfiguration} -> targetConfiguration) (\s@RdsRecommendation' {} a -> s {targetConfiguration = a} :: RdsRecommendation)

instance Data.FromJSON RdsRecommendation where
  parseJSON =
    Data.withObject
      "RdsRecommendation"
      ( \x ->
          RdsRecommendation'
            Prelude.<$> (x Data..:? "RequirementsToTarget")
            Prelude.<*> (x Data..:? "TargetConfiguration")
      )

instance Prelude.Hashable RdsRecommendation where
  hashWithSalt _salt RdsRecommendation' {..} =
    _salt
      `Prelude.hashWithSalt` requirementsToTarget
      `Prelude.hashWithSalt` targetConfiguration

instance Prelude.NFData RdsRecommendation where
  rnf RdsRecommendation' {..} =
    Prelude.rnf requirementsToTarget
      `Prelude.seq` Prelude.rnf targetConfiguration
