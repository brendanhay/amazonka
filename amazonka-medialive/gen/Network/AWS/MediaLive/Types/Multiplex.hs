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
-- Module      : Network.AWS.MediaLive.Types.Multiplex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Multiplex where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.MultiplexOutputDestination
import Network.AWS.MediaLive.Types.MultiplexSettings
import Network.AWS.MediaLive.Types.MultiplexState

-- | The multiplex object.
--
-- /See:/ 'newMultiplex' smart constructor.
data Multiplex = Multiplex'
  { -- | A list of availability zones for the multiplex.
    availabilityZones :: Core.Maybe [Core.Text],
    -- | The unique arn of the multiplex.
    arn :: Core.Maybe Core.Text,
    -- | The unique id of the multiplex.
    id :: Core.Maybe Core.Text,
    -- | The number of currently healthy pipelines.
    pipelinesRunningCount :: Core.Maybe Core.Int,
    -- | The number of programs in the multiplex.
    programCount :: Core.Maybe Core.Int,
    -- | A list of the multiplex output destinations.
    destinations :: Core.Maybe [MultiplexOutputDestination],
    -- | The current state of the multiplex.
    state :: Core.Maybe MultiplexState,
    -- | The name of the multiplex.
    name :: Core.Maybe Core.Text,
    -- | A collection of key-value pairs.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | Configuration for a multiplex event.
    multiplexSettings :: Core.Maybe MultiplexSettings
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Multiplex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'multiplex_availabilityZones' - A list of availability zones for the multiplex.
--
-- 'arn', 'multiplex_arn' - The unique arn of the multiplex.
--
-- 'id', 'multiplex_id' - The unique id of the multiplex.
--
-- 'pipelinesRunningCount', 'multiplex_pipelinesRunningCount' - The number of currently healthy pipelines.
--
-- 'programCount', 'multiplex_programCount' - The number of programs in the multiplex.
--
-- 'destinations', 'multiplex_destinations' - A list of the multiplex output destinations.
--
-- 'state', 'multiplex_state' - The current state of the multiplex.
--
-- 'name', 'multiplex_name' - The name of the multiplex.
--
-- 'tags', 'multiplex_tags' - A collection of key-value pairs.
--
-- 'multiplexSettings', 'multiplex_multiplexSettings' - Configuration for a multiplex event.
newMultiplex ::
  Multiplex
newMultiplex =
  Multiplex'
    { availabilityZones = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      pipelinesRunningCount = Core.Nothing,
      programCount = Core.Nothing,
      destinations = Core.Nothing,
      state = Core.Nothing,
      name = Core.Nothing,
      tags = Core.Nothing,
      multiplexSettings = Core.Nothing
    }

-- | A list of availability zones for the multiplex.
multiplex_availabilityZones :: Lens.Lens' Multiplex (Core.Maybe [Core.Text])
multiplex_availabilityZones = Lens.lens (\Multiplex' {availabilityZones} -> availabilityZones) (\s@Multiplex' {} a -> s {availabilityZones = a} :: Multiplex) Core.. Lens.mapping Lens._Coerce

-- | The unique arn of the multiplex.
multiplex_arn :: Lens.Lens' Multiplex (Core.Maybe Core.Text)
multiplex_arn = Lens.lens (\Multiplex' {arn} -> arn) (\s@Multiplex' {} a -> s {arn = a} :: Multiplex)

-- | The unique id of the multiplex.
multiplex_id :: Lens.Lens' Multiplex (Core.Maybe Core.Text)
multiplex_id = Lens.lens (\Multiplex' {id} -> id) (\s@Multiplex' {} a -> s {id = a} :: Multiplex)

-- | The number of currently healthy pipelines.
multiplex_pipelinesRunningCount :: Lens.Lens' Multiplex (Core.Maybe Core.Int)
multiplex_pipelinesRunningCount = Lens.lens (\Multiplex' {pipelinesRunningCount} -> pipelinesRunningCount) (\s@Multiplex' {} a -> s {pipelinesRunningCount = a} :: Multiplex)

-- | The number of programs in the multiplex.
multiplex_programCount :: Lens.Lens' Multiplex (Core.Maybe Core.Int)
multiplex_programCount = Lens.lens (\Multiplex' {programCount} -> programCount) (\s@Multiplex' {} a -> s {programCount = a} :: Multiplex)

-- | A list of the multiplex output destinations.
multiplex_destinations :: Lens.Lens' Multiplex (Core.Maybe [MultiplexOutputDestination])
multiplex_destinations = Lens.lens (\Multiplex' {destinations} -> destinations) (\s@Multiplex' {} a -> s {destinations = a} :: Multiplex) Core.. Lens.mapping Lens._Coerce

-- | The current state of the multiplex.
multiplex_state :: Lens.Lens' Multiplex (Core.Maybe MultiplexState)
multiplex_state = Lens.lens (\Multiplex' {state} -> state) (\s@Multiplex' {} a -> s {state = a} :: Multiplex)

-- | The name of the multiplex.
multiplex_name :: Lens.Lens' Multiplex (Core.Maybe Core.Text)
multiplex_name = Lens.lens (\Multiplex' {name} -> name) (\s@Multiplex' {} a -> s {name = a} :: Multiplex)

-- | A collection of key-value pairs.
multiplex_tags :: Lens.Lens' Multiplex (Core.Maybe (Core.HashMap Core.Text Core.Text))
multiplex_tags = Lens.lens (\Multiplex' {tags} -> tags) (\s@Multiplex' {} a -> s {tags = a} :: Multiplex) Core.. Lens.mapping Lens._Coerce

-- | Configuration for a multiplex event.
multiplex_multiplexSettings :: Lens.Lens' Multiplex (Core.Maybe MultiplexSettings)
multiplex_multiplexSettings = Lens.lens (\Multiplex' {multiplexSettings} -> multiplexSettings) (\s@Multiplex' {} a -> s {multiplexSettings = a} :: Multiplex)

instance Core.FromJSON Multiplex where
  parseJSON =
    Core.withObject
      "Multiplex"
      ( \x ->
          Multiplex'
            Core.<$> (x Core..:? "availabilityZones" Core..!= Core.mempty)
            Core.<*> (x Core..:? "arn")
            Core.<*> (x Core..:? "id")
            Core.<*> (x Core..:? "pipelinesRunningCount")
            Core.<*> (x Core..:? "programCount")
            Core.<*> (x Core..:? "destinations" Core..!= Core.mempty)
            Core.<*> (x Core..:? "state")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "multiplexSettings")
      )

instance Core.Hashable Multiplex

instance Core.NFData Multiplex
