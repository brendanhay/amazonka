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
-- Module      : Amazonka.MigrationHubStrategy.Types.ApplicationComponentStrategy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.ApplicationComponentStrategy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.RecommendationSet
import Amazonka.MigrationHubStrategy.Types.StrategyRecommendation
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a strategy recommendation for an application
-- component.
--
-- /See:/ 'newApplicationComponentStrategy' smart constructor.
data ApplicationComponentStrategy = ApplicationComponentStrategy'
  { -- | Set to true if the recommendation is set as preferred.
    isPreferred :: Prelude.Maybe Prelude.Bool,
    -- | Strategy recommendation for the application component.
    recommendation :: Prelude.Maybe RecommendationSet,
    -- | The recommendation status of a strategy for an application component.
    status :: Prelude.Maybe StrategyRecommendation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationComponentStrategy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isPreferred', 'applicationComponentStrategy_isPreferred' - Set to true if the recommendation is set as preferred.
--
-- 'recommendation', 'applicationComponentStrategy_recommendation' - Strategy recommendation for the application component.
--
-- 'status', 'applicationComponentStrategy_status' - The recommendation status of a strategy for an application component.
newApplicationComponentStrategy ::
  ApplicationComponentStrategy
newApplicationComponentStrategy =
  ApplicationComponentStrategy'
    { isPreferred =
        Prelude.Nothing,
      recommendation = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Set to true if the recommendation is set as preferred.
applicationComponentStrategy_isPreferred :: Lens.Lens' ApplicationComponentStrategy (Prelude.Maybe Prelude.Bool)
applicationComponentStrategy_isPreferred = Lens.lens (\ApplicationComponentStrategy' {isPreferred} -> isPreferred) (\s@ApplicationComponentStrategy' {} a -> s {isPreferred = a} :: ApplicationComponentStrategy)

-- | Strategy recommendation for the application component.
applicationComponentStrategy_recommendation :: Lens.Lens' ApplicationComponentStrategy (Prelude.Maybe RecommendationSet)
applicationComponentStrategy_recommendation = Lens.lens (\ApplicationComponentStrategy' {recommendation} -> recommendation) (\s@ApplicationComponentStrategy' {} a -> s {recommendation = a} :: ApplicationComponentStrategy)

-- | The recommendation status of a strategy for an application component.
applicationComponentStrategy_status :: Lens.Lens' ApplicationComponentStrategy (Prelude.Maybe StrategyRecommendation)
applicationComponentStrategy_status = Lens.lens (\ApplicationComponentStrategy' {status} -> status) (\s@ApplicationComponentStrategy' {} a -> s {status = a} :: ApplicationComponentStrategy)

instance Data.FromJSON ApplicationComponentStrategy where
  parseJSON =
    Data.withObject
      "ApplicationComponentStrategy"
      ( \x ->
          ApplicationComponentStrategy'
            Prelude.<$> (x Data..:? "isPreferred")
            Prelude.<*> (x Data..:? "recommendation")
            Prelude.<*> (x Data..:? "status")
      )

instance
  Prelude.Hashable
    ApplicationComponentStrategy
  where
  hashWithSalt _salt ApplicationComponentStrategy' {..} =
    _salt `Prelude.hashWithSalt` isPreferred
      `Prelude.hashWithSalt` recommendation
      `Prelude.hashWithSalt` status

instance Prelude.NFData ApplicationComponentStrategy where
  rnf ApplicationComponentStrategy' {..} =
    Prelude.rnf isPreferred
      `Prelude.seq` Prelude.rnf recommendation
      `Prelude.seq` Prelude.rnf status
