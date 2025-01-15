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
-- Module      : Amazonka.MigrationHubStrategy.Types.StrategyOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.StrategyOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.Strategy
import Amazonka.MigrationHubStrategy.Types.TargetDestination
import Amazonka.MigrationHubStrategy.Types.TransformationToolName
import qualified Amazonka.Prelude as Prelude

-- | Information about all the available strategy options for migrating and
-- modernizing an application component.
--
-- /See:/ 'newStrategyOption' smart constructor.
data StrategyOption = StrategyOption'
  { -- | Indicates if a specific strategy is preferred for the application
    -- component.
    isPreferred :: Prelude.Maybe Prelude.Bool,
    -- | Type of transformation. For example, Rehost, Replatform, and so on.
    strategy :: Prelude.Maybe Strategy,
    -- | Destination information about where the application component can
    -- migrate to. For example, @EC2@, @ECS@, and so on.
    targetDestination :: Prelude.Maybe TargetDestination,
    -- | The name of the tool that can be used to transform an application
    -- component using this strategy.
    toolName :: Prelude.Maybe TransformationToolName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StrategyOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isPreferred', 'strategyOption_isPreferred' - Indicates if a specific strategy is preferred for the application
-- component.
--
-- 'strategy', 'strategyOption_strategy' - Type of transformation. For example, Rehost, Replatform, and so on.
--
-- 'targetDestination', 'strategyOption_targetDestination' - Destination information about where the application component can
-- migrate to. For example, @EC2@, @ECS@, and so on.
--
-- 'toolName', 'strategyOption_toolName' - The name of the tool that can be used to transform an application
-- component using this strategy.
newStrategyOption ::
  StrategyOption
newStrategyOption =
  StrategyOption'
    { isPreferred = Prelude.Nothing,
      strategy = Prelude.Nothing,
      targetDestination = Prelude.Nothing,
      toolName = Prelude.Nothing
    }

-- | Indicates if a specific strategy is preferred for the application
-- component.
strategyOption_isPreferred :: Lens.Lens' StrategyOption (Prelude.Maybe Prelude.Bool)
strategyOption_isPreferred = Lens.lens (\StrategyOption' {isPreferred} -> isPreferred) (\s@StrategyOption' {} a -> s {isPreferred = a} :: StrategyOption)

-- | Type of transformation. For example, Rehost, Replatform, and so on.
strategyOption_strategy :: Lens.Lens' StrategyOption (Prelude.Maybe Strategy)
strategyOption_strategy = Lens.lens (\StrategyOption' {strategy} -> strategy) (\s@StrategyOption' {} a -> s {strategy = a} :: StrategyOption)

-- | Destination information about where the application component can
-- migrate to. For example, @EC2@, @ECS@, and so on.
strategyOption_targetDestination :: Lens.Lens' StrategyOption (Prelude.Maybe TargetDestination)
strategyOption_targetDestination = Lens.lens (\StrategyOption' {targetDestination} -> targetDestination) (\s@StrategyOption' {} a -> s {targetDestination = a} :: StrategyOption)

-- | The name of the tool that can be used to transform an application
-- component using this strategy.
strategyOption_toolName :: Lens.Lens' StrategyOption (Prelude.Maybe TransformationToolName)
strategyOption_toolName = Lens.lens (\StrategyOption' {toolName} -> toolName) (\s@StrategyOption' {} a -> s {toolName = a} :: StrategyOption)

instance Prelude.Hashable StrategyOption where
  hashWithSalt _salt StrategyOption' {..} =
    _salt
      `Prelude.hashWithSalt` isPreferred
      `Prelude.hashWithSalt` strategy
      `Prelude.hashWithSalt` targetDestination
      `Prelude.hashWithSalt` toolName

instance Prelude.NFData StrategyOption where
  rnf StrategyOption' {..} =
    Prelude.rnf isPreferred `Prelude.seq`
      Prelude.rnf strategy `Prelude.seq`
        Prelude.rnf targetDestination `Prelude.seq`
          Prelude.rnf toolName

instance Data.ToJSON StrategyOption where
  toJSON StrategyOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("isPreferred" Data..=) Prelude.<$> isPreferred,
            ("strategy" Data..=) Prelude.<$> strategy,
            ("targetDestination" Data..=)
              Prelude.<$> targetDestination,
            ("toolName" Data..=) Prelude.<$> toolName
          ]
      )
