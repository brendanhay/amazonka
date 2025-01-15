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
-- Module      : Amazonka.IoTFleetWise.Types.Node
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.Node where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types.Actuator
import Amazonka.IoTFleetWise.Types.Attribute
import Amazonka.IoTFleetWise.Types.Branch
import Amazonka.IoTFleetWise.Types.Sensor
import qualified Amazonka.Prelude as Prelude

-- | A general abstraction of a signal. A node can be specified as an
-- actuator, attribute, branch, or sensor.
--
-- /See:/ 'newNode' smart constructor.
data Node = Node'
  { -- | Information about a node specified as an actuator.
    --
    -- An actuator is a digital representation of a vehicle device.
    actuator :: Prelude.Maybe Actuator,
    -- | Information about a node specified as an attribute.
    --
    -- An attribute represents static information about a vehicle.
    attribute :: Prelude.Maybe Attribute,
    -- | Information about a node specified as a branch.
    --
    -- A group of signals that are defined in a hierarchical structure.
    branch :: Prelude.Maybe Branch,
    sensor :: Prelude.Maybe Sensor
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Node' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actuator', 'node_actuator' - Information about a node specified as an actuator.
--
-- An actuator is a digital representation of a vehicle device.
--
-- 'attribute', 'node_attribute' - Information about a node specified as an attribute.
--
-- An attribute represents static information about a vehicle.
--
-- 'branch', 'node_branch' - Information about a node specified as a branch.
--
-- A group of signals that are defined in a hierarchical structure.
--
-- 'sensor', 'node_sensor' - Undocumented member.
newNode ::
  Node
newNode =
  Node'
    { actuator = Prelude.Nothing,
      attribute = Prelude.Nothing,
      branch = Prelude.Nothing,
      sensor = Prelude.Nothing
    }

-- | Information about a node specified as an actuator.
--
-- An actuator is a digital representation of a vehicle device.
node_actuator :: Lens.Lens' Node (Prelude.Maybe Actuator)
node_actuator = Lens.lens (\Node' {actuator} -> actuator) (\s@Node' {} a -> s {actuator = a} :: Node)

-- | Information about a node specified as an attribute.
--
-- An attribute represents static information about a vehicle.
node_attribute :: Lens.Lens' Node (Prelude.Maybe Attribute)
node_attribute = Lens.lens (\Node' {attribute} -> attribute) (\s@Node' {} a -> s {attribute = a} :: Node)

-- | Information about a node specified as a branch.
--
-- A group of signals that are defined in a hierarchical structure.
node_branch :: Lens.Lens' Node (Prelude.Maybe Branch)
node_branch = Lens.lens (\Node' {branch} -> branch) (\s@Node' {} a -> s {branch = a} :: Node)

-- | Undocumented member.
node_sensor :: Lens.Lens' Node (Prelude.Maybe Sensor)
node_sensor = Lens.lens (\Node' {sensor} -> sensor) (\s@Node' {} a -> s {sensor = a} :: Node)

instance Data.FromJSON Node where
  parseJSON =
    Data.withObject
      "Node"
      ( \x ->
          Node'
            Prelude.<$> (x Data..:? "actuator")
            Prelude.<*> (x Data..:? "attribute")
            Prelude.<*> (x Data..:? "branch")
            Prelude.<*> (x Data..:? "sensor")
      )

instance Prelude.Hashable Node where
  hashWithSalt _salt Node' {..} =
    _salt
      `Prelude.hashWithSalt` actuator
      `Prelude.hashWithSalt` attribute
      `Prelude.hashWithSalt` branch
      `Prelude.hashWithSalt` sensor

instance Prelude.NFData Node where
  rnf Node' {..} =
    Prelude.rnf actuator `Prelude.seq`
      Prelude.rnf attribute `Prelude.seq`
        Prelude.rnf branch `Prelude.seq`
          Prelude.rnf sensor

instance Data.ToJSON Node where
  toJSON Node' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("actuator" Data..=) Prelude.<$> actuator,
            ("attribute" Data..=) Prelude.<$> attribute,
            ("branch" Data..=) Prelude.<$> branch,
            ("sensor" Data..=) Prelude.<$> sensor
          ]
      )
