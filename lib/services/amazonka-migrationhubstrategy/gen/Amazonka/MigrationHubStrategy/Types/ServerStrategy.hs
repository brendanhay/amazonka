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
-- Module      : Amazonka.MigrationHubStrategy.Types.ServerStrategy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.ServerStrategy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.RecommendationSet
import Amazonka.MigrationHubStrategy.Types.StrategyRecommendation
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a strategy recommendation for a server.
--
-- /See:/ 'newServerStrategy' smart constructor.
data ServerStrategy = ServerStrategy'
  { -- | The recommendation status of the strategy for the server.
    status :: Prelude.Maybe StrategyRecommendation,
    -- | Strategy recommendation for the server.
    recommendation :: Prelude.Maybe RecommendationSet,
    -- | The number of application components with this strategy recommendation
    -- running on the server.
    numberOfApplicationComponents :: Prelude.Maybe Prelude.Int,
    -- | Set to true if the recommendation is set as preferred.
    isPreferred :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerStrategy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'serverStrategy_status' - The recommendation status of the strategy for the server.
--
-- 'recommendation', 'serverStrategy_recommendation' - Strategy recommendation for the server.
--
-- 'numberOfApplicationComponents', 'serverStrategy_numberOfApplicationComponents' - The number of application components with this strategy recommendation
-- running on the server.
--
-- 'isPreferred', 'serverStrategy_isPreferred' - Set to true if the recommendation is set as preferred.
newServerStrategy ::
  ServerStrategy
newServerStrategy =
  ServerStrategy'
    { status = Prelude.Nothing,
      recommendation = Prelude.Nothing,
      numberOfApplicationComponents = Prelude.Nothing,
      isPreferred = Prelude.Nothing
    }

-- | The recommendation status of the strategy for the server.
serverStrategy_status :: Lens.Lens' ServerStrategy (Prelude.Maybe StrategyRecommendation)
serverStrategy_status = Lens.lens (\ServerStrategy' {status} -> status) (\s@ServerStrategy' {} a -> s {status = a} :: ServerStrategy)

-- | Strategy recommendation for the server.
serverStrategy_recommendation :: Lens.Lens' ServerStrategy (Prelude.Maybe RecommendationSet)
serverStrategy_recommendation = Lens.lens (\ServerStrategy' {recommendation} -> recommendation) (\s@ServerStrategy' {} a -> s {recommendation = a} :: ServerStrategy)

-- | The number of application components with this strategy recommendation
-- running on the server.
serverStrategy_numberOfApplicationComponents :: Lens.Lens' ServerStrategy (Prelude.Maybe Prelude.Int)
serverStrategy_numberOfApplicationComponents = Lens.lens (\ServerStrategy' {numberOfApplicationComponents} -> numberOfApplicationComponents) (\s@ServerStrategy' {} a -> s {numberOfApplicationComponents = a} :: ServerStrategy)

-- | Set to true if the recommendation is set as preferred.
serverStrategy_isPreferred :: Lens.Lens' ServerStrategy (Prelude.Maybe Prelude.Bool)
serverStrategy_isPreferred = Lens.lens (\ServerStrategy' {isPreferred} -> isPreferred) (\s@ServerStrategy' {} a -> s {isPreferred = a} :: ServerStrategy)

instance Data.FromJSON ServerStrategy where
  parseJSON =
    Data.withObject
      "ServerStrategy"
      ( \x ->
          ServerStrategy'
            Prelude.<$> (x Data..:? "status")
            Prelude.<*> (x Data..:? "recommendation")
            Prelude.<*> (x Data..:? "numberOfApplicationComponents")
            Prelude.<*> (x Data..:? "isPreferred")
      )

instance Prelude.Hashable ServerStrategy where
  hashWithSalt _salt ServerStrategy' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` recommendation
      `Prelude.hashWithSalt` numberOfApplicationComponents
      `Prelude.hashWithSalt` isPreferred

instance Prelude.NFData ServerStrategy where
  rnf ServerStrategy' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf recommendation
      `Prelude.seq` Prelude.rnf numberOfApplicationComponents
      `Prelude.seq` Prelude.rnf isPreferred
