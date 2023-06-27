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
-- Module      : Amazonka.StepFunctions.Types.RoutingConfigurationListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.RoutingConfigurationListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the routing configuration of a state machine
-- alias. In a routing configuration, you define an array of objects that
-- specify up to two state machine versions. You also specify the
-- percentage of traffic to be routed to each version.
--
-- /See:/ 'newRoutingConfigurationListItem' smart constructor.
data RoutingConfigurationListItem = RoutingConfigurationListItem'
  { -- | The Amazon Resource Name (ARN) that identifies one or two state machine
    -- versions defined in the routing configuration.
    --
    -- If you specify the ARN of a second version, it must belong to the same
    -- state machine as the first version.
    stateMachineVersionArn :: Prelude.Text,
    -- | The percentage of traffic you want to route to the second state machine
    -- version. The sum of the weights in the routing configuration must be
    -- equal to 100.
    weight :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RoutingConfigurationListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateMachineVersionArn', 'routingConfigurationListItem_stateMachineVersionArn' - The Amazon Resource Name (ARN) that identifies one or two state machine
-- versions defined in the routing configuration.
--
-- If you specify the ARN of a second version, it must belong to the same
-- state machine as the first version.
--
-- 'weight', 'routingConfigurationListItem_weight' - The percentage of traffic you want to route to the second state machine
-- version. The sum of the weights in the routing configuration must be
-- equal to 100.
newRoutingConfigurationListItem ::
  -- | 'stateMachineVersionArn'
  Prelude.Text ->
  -- | 'weight'
  Prelude.Natural ->
  RoutingConfigurationListItem
newRoutingConfigurationListItem
  pStateMachineVersionArn_
  pWeight_ =
    RoutingConfigurationListItem'
      { stateMachineVersionArn =
          pStateMachineVersionArn_,
        weight = pWeight_
      }

-- | The Amazon Resource Name (ARN) that identifies one or two state machine
-- versions defined in the routing configuration.
--
-- If you specify the ARN of a second version, it must belong to the same
-- state machine as the first version.
routingConfigurationListItem_stateMachineVersionArn :: Lens.Lens' RoutingConfigurationListItem Prelude.Text
routingConfigurationListItem_stateMachineVersionArn = Lens.lens (\RoutingConfigurationListItem' {stateMachineVersionArn} -> stateMachineVersionArn) (\s@RoutingConfigurationListItem' {} a -> s {stateMachineVersionArn = a} :: RoutingConfigurationListItem)

-- | The percentage of traffic you want to route to the second state machine
-- version. The sum of the weights in the routing configuration must be
-- equal to 100.
routingConfigurationListItem_weight :: Lens.Lens' RoutingConfigurationListItem Prelude.Natural
routingConfigurationListItem_weight = Lens.lens (\RoutingConfigurationListItem' {weight} -> weight) (\s@RoutingConfigurationListItem' {} a -> s {weight = a} :: RoutingConfigurationListItem)

instance Data.FromJSON RoutingConfigurationListItem where
  parseJSON =
    Data.withObject
      "RoutingConfigurationListItem"
      ( \x ->
          RoutingConfigurationListItem'
            Prelude.<$> (x Data..: "stateMachineVersionArn")
            Prelude.<*> (x Data..: "weight")
      )

instance
  Prelude.Hashable
    RoutingConfigurationListItem
  where
  hashWithSalt _salt RoutingConfigurationListItem' {..} =
    _salt
      `Prelude.hashWithSalt` stateMachineVersionArn
      `Prelude.hashWithSalt` weight

instance Prelude.NFData RoutingConfigurationListItem where
  rnf RoutingConfigurationListItem' {..} =
    Prelude.rnf stateMachineVersionArn
      `Prelude.seq` Prelude.rnf weight

instance Data.ToJSON RoutingConfigurationListItem where
  toJSON RoutingConfigurationListItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "stateMachineVersionArn"
                  Data..= stateMachineVersionArn
              ),
            Prelude.Just ("weight" Data..= weight)
          ]
      )
